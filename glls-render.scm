(module glls-render (c-prefix
                     define-pipeline
                     export-pipeline)

(import chicken scheme)
(use (prefix glls glls:) glls-renderable (prefix gl-utils gl:))
(import-for-syntax (prefix glls glls:) (prefix glls-compiler glls:)
                   glls-renderable matchable miscmacros)

(reexport (except glls define-pipeline)
          (only glls-renderable renderable-size))

(begin-for-syntax
 (require-library glls-renderable)
 (define c-prefix (make-parameter '||))
 (define header-included? (make-parameter #f)))

(define c-prefix (make-parameter '||)) ; Needs to be defined twice so it can be manipulated upon export (for some reason)

(define-syntax renderable-setters
  (ir-macro-transformer
   (lambda (exp i compare)
     (match exp
      ((_ name uniforms)
       (let ((base-name (symbol-append 'set- name '-renderable-)))
         `(begin
            (define (,(symbol-append base-name 'vao!) renderable vao)
              (set-renderable-vao! renderable vao))
            (define (,(symbol-append base-name 'n-elements!) renderable n)
              (set-renderable-n-elements! renderable n))
            (define (,(symbol-append base-name 'element-type!) renderable type)
              (set-renderable-element-type! renderable type))
            (define (,(symbol-append base-name 'mode!) renderable mode)
              (set-renderable-mode! renderable mode))
            (define (,(symbol-append base-name 'offset!) renderable offset)
              (set-renderable-offset! renderable offset))
            ,@(let loop ((uniforms uniforms) (i 0))
                (if (null? uniforms)
                    '()
                    (cons `(define (,(symbol-append base-name (caar uniforms) '!)
                                    renderable value)
                             (set-renderable-uniform-value! renderable ,i value))
                          (loop (cdr uniforms) (add1 i))))))))
       (exp (syntax-error 'renderable-setters "Bad arguments" exp))))))

(define-for-syntax (get-uniforms s)
  (cond
   ((and (list? s)
       (list? (car s))
       (member (caar s) glls:shader-types))
    (get-keyword uniform: (cdar s) (lambda () '())))
   ((and (list? s) (>= (length s) 2) (member #:uniform s))
    (cdr (member #:uniform s)))
   (else (syntax-error 'define-pipeline "Only shaders that include uniform definitions may be used with glls-render" s))))

(define-syntax define-renderable-functions
  (ir-macro-transformer
   (lambda (exp i compare)
     (match exp
       ((_ name . shaders)
        (let* ((name (strip-syntax name))
               (uniforms (delete-duplicates
                          (concatenate (map get-uniforms (strip-syntax shaders)))
                          (lambda (a b) (eq? (car a) (car b))))))
          (let-values (((render-funs render-fun-name
                                     render-arrays-fun-name
                                     fast-fun-begin-name
                                     fast-fun-name fast-fun-end-name
                                     fast-fun-arrays-name)
                        (if (feature? compiling:)
                            (render-functions (c-prefix) name uniforms)
                            (values #f #f #f #f #f #f #f))))
            `(begin
               ,(if (feature? compiling:)
                    `(begin
                       ,(if (not (header-included?))
                            (begin 
                              (header-included? #t)
                              `(begin
                                 (import foreign)
                                 (foreign-declare ,gllsRender.h)))
                            #f)
                       (foreign-declare ,render-funs)
                       (define ,(symbol-append 'render- name)
                         (foreign-lambda void ,render-fun-name c-pointer))
                       (define ,(symbol-append 'render-arrays- name)
                         (foreign-lambda void ,render-arrays-fun-name c-pointer))
                       (define (,(symbol-append name '-fast-render-functions))
                         (values
                          (foreign-lambda void ,(symbol->string fast-fun-begin-name)
                                          c-pointer)
                          (foreign-lambda void ,(symbol->string fast-fun-name)
                                          c-pointer)
                          (foreign-lambda void ,(symbol->string fast-fun-end-name))
                          (foreign-lambda void ,(symbol->string fast-fun-arrays-name)
                                          c-pointer)
                          (foreign-value ,(string-append
                                           "&" (symbol->string fast-fun-begin-name))
                                         c-pointer)
                          (foreign-value ,(string-append
                                           "&" (symbol->string fast-fun-name))
                                         c-pointer)
                          (foreign-value ,(string-append
                                           "&" (symbol->string fast-fun-end-name))
                                         c-pointer)
                          (foreign-value ,(string-append
                                           "&" (symbol->string fast-fun-arrays-name))
                                         c-pointer))))
                    `(begin
                       (define (,(symbol-append 'render- name) renderable)
                         (render-renderable ',uniforms renderable #f))
                       (define (,(symbol-append 'render-arrays- name) renderable)
                         (render-renderable ',uniforms renderable #t))))
               (define (,(symbol-append 'make- name '-renderable) . args)
                 (apply make-renderable ,name args))
               (renderable-setters ,name ,uniforms)))))
       (expr (syntax-error 'define-pipeline "Invalid pipeline definition" expr))))))

(define-syntax define-pipeline
  (syntax-rules ()
    ((_ name  shaders ...)
     (begin (glls:define-pipeline name shaders ...)
            (define-renderable-functions name shaders ...)))
    ((_ . expr) (syntax-error 'define-pipeline "Invalide pipeline definition" expr))))

(define-syntax export-pipeline
  (ir-macro-transformer
   (lambda (expr i c)
     (if (and (not (= (length expr) 2))
            (symbol? (cadr expr)))
         (syntax-error 'export-shader "Expected a pipeline name" expr))
     (let* ((name (strip-syntax (cadr expr)))
            (render (symbol-append 'render- name))
            (make-renderable (symbol-append 'make- name '-renderable))
            (fast-funs (symbol-append name '-fast-render-functions)))
       `(export ,name ,render ,make-renderable ,fast-funs)))))

) ; glls-render
