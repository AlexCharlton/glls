(module glls-render (c-prefix
                     define-pipeline
                     load-ply-renderable)

(import chicken scheme)
(use (prefix glls glls:) glls-renderable (prefix gl-utils gl:))
(import-for-syntax (prefix glls glls:) glls-renderable matchable miscmacros)

(reexport (except glls define-pipeline))

(begin-for-syntax
 (require-library glls-renderable)
 (define c-prefix (make-parameter '||))
 (define header-included? (make-parameter #f)))

(define c-prefix (make-parameter '||)) ; Needs to be defined twice so it can be manipulated upon export (for some reason)

(define-syntax renderable-setters
  (ir-macro-transformer
   (lambda (exp i compare)
     (match exp
      [(_ name uniforms)
       (let ([base-name (symbol-append 'set- name '-renderable-)])
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
            ,@(let loop ([uniforms uniforms] [i 0])
                (if (null? uniforms)
                    '()
                    (cons `(define (,(symbol-append base-name (caar uniforms) '!)
                                    renderable value)
                             (set-renderable-uniform-value! renderable ,i value))
                          (loop (cdr uniforms) [add1 i]))))))]
       [exp (syntax-error 'renderable-setters "Bad arguments" exp)]))))

(define-for-syntax (get-uniforms s)
  (cond
   [(and (list? s)
       (= (length s) 5)
       (equal? (cadddr s) '->))
    (if* (member #:uniform (cadr s))
          (cdr it)
          '())]
   [(and (list? s) (>= (length s) 2) (member #:uniform s))
    (cdr (member #:uniform s))]
   [else (syntax-error 'define-pipeline "Only shaders that include uniform definitions may be used with glls-render" s)]))

(define-syntax define-renderable-functions
  (ir-macro-transformer
   (lambda (exp i compare)
     (match exp
       [(_ name . shaders)
        (let* ([name (strip-syntax name)]
               [uniforms (concatenate (map get-uniforms (strip-syntax shaders)))])
          (let-values ([(render-funs render-fun-name fast-fun-begin-name
                                     fast-fun-name fast-fun-end-name)
                        (if (feature? compiling:)
                            (render-functions (c-prefix) name uniforms)
                            (values #f #f #f #f #f))])
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
                       (define (,(symbol-append name '-fast-render-functions))
                         (values
                          (foreign-lambda void ,(symbol->string fast-fun-begin-name)
                                          c-pointer)
                          (foreign-lambda void ,(symbol->string fast-fun-name)
                                          c-pointer)
                          (foreign-lambda void ,(symbol->string fast-fun-end-name))
                          (foreign-value ,(string-append
                                           "&" (symbol->string fast-fun-begin-name))
                                         c-pointer)
                          (foreign-value ,(string-append
                                           "&" (symbol->string fast-fun-name))
                                         c-pointer)
                          (foreign-value ,(string-append
                                           "&" (symbol->string fast-fun-end-name))
                                         c-pointer))))
                    `(define (,(symbol-append 'render- name) renderable)
                       (render-renderable ',uniforms renderable)))
               (define (,(symbol-append 'make- name '-renderable) . args)
                 (apply make-renderable ,name args))
               (renderable-setters ,name ,uniforms))))]
       [expr (syntax-error 'define-pipeline "Invalid pipeline definition" expr)]))))

 (define-syntax define-pipeline
   (syntax-rules ()
     [(_ name  shaders ...)
      (begin (use glls)
             (glls:define-pipeline name shaders ...)
             (define-renderable-functions name shaders ...))]
     [(_ . expr) (syntax-error 'define-pipeline "Invalide pipeline definition" expr)]))

(define (load-ply-renderable ply renderable-maker . args)
  (define (get-arg arg)
    (get-keyword arg args
                 (lambda () (error 'load-vao-renderable "Expected keyword argument"
                              arg args))))
  (let-values ([(vao vertex-data index-data n-verts mode element-type)
                (gl:load-ply-vao ply
                                 vertex: (get-arg vertex:)
                                 face: (get-arg face:))])
    (values (apply renderable-maker
                   n-elements: n-verts
                   element-type: element-type
                   mode: mode
                   vao: vao
                   args)
            vertex-data
            index-data)))

) ; glls-render
