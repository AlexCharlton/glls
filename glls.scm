;;;; glls.scm
;;;;
;;;; Methods and macros for defining, compiling, and deleting shaders and pipelines.
;;;; Pipelines are the name that we are using to refer to OpenGL "programs"
;;;; which is an unfortunately vague term.

(module glls
  (make-pipeline
   pipeline-shaders
   pipeline-attributes
   pipeline-uniforms
   pipeline-program
   pipeline?
   create-pipeline
   pipelines
   define-pipeline
   define-shader
   create-shader
   compile-shader
   compile-pipeline
   compile-pipelines
   pipeline-uniform
   pipeline-attribute
   %delete-shader
   %delete-pipeline)

(import chicken scheme srfi-69 srfi-1 miscmacros)
(use glls-compiler (prefix opengl-glew gl:) matchable)
(import-for-syntax glls-compiler)

(reexport glls-compiler)

(begin-for-syntax
 (require-library glls-compiler))

(define-record pipeline
  shaders (setter attributes) (setter uniforms) (setter program))

(define-record-printer (pipeline p out)
  (fprintf out "#(pipeline ~S shaders:~S attributes:~S uniforms:~S)"
           (pipeline-program p) (map shader-type (pipeline-shaders p))
           (pipeline-attributes p) (pipeline-uniforms p)))

(define (%delete-shader shader)
  (gl:delete-shader (shader-program shader)))

(define (%delete-pipeline pipeline)
  (gl:delete-program (pipeline-program pipeline)))

(define-syntax define-shader
  (ir-macro-transformer
   (lambda (exp rename compare)
     (let* ([name (cadr exp)]
           [shader (%create-shader (strip-syntax (cddr exp)))]
           [shader-maker `(make-shader
                           ,(shader-type shader)
                           ,(shader-source shader) ',(shader-inputs shader)
                           ',(shader-outputs shader) ',(shader-uniforms shader)
                           0)])
       `(define ,name (set-finalizer! ,shader-maker %delete-shader))))))

(define (create-shader form)
  (set-finalizer! (%create-shader form) %delete-shader))

(define pipelines (make-parameter '()))

(define (shader-with-uniforms? s)
  (and (list? s)
     (= (length s) 3)
     (shader? (first s))
     (equal? (second s) uniforms:)))

(define (create-pipeline . shaders)
  (if (< (length shaders) 2)
      (syntax-error "Invalid pipeline definition:" shaders))
  (let* ([shaders (map (lambda (s)
                         (cond
                          [(shader? s) s]
                          [(shader-with-uniforms? s) (car s)]
                          [else (create-shader s)]))
                       shaders)]
         [attributes (apply append
                            (map shader-inputs
                                 (filter (lambda (s)
                                           (equal? (shader-type s) #:vertex))
                                         shaders)))]
         [uniforms (apply append (map shader-uniforms shaders))]
         [pipeline (make-pipeline shaders attributes uniforms 0)])
    (pipelines (cons pipeline (pipelines)))
    (set-finalizer! pipeline %delete-pipeline)
    pipeline))

(define-syntax define-pipeline
  (ir-macro-transformer
   (lambda (exp i compare)
     (if (< (length exp) 3)
         (syntax-error "Invalid pipeline definition:" exp))
     (let* ([new-shader? (lambda (s) (and (list? s)
                                   (= (length s) 5)
                                   (compare (cadddr s) '->)))]
            [name (cadr exp)]
            [shaders (map (lambda (s)
                            (if (new-shader? s)
                                (%create-shader s)
                                s))
                          (strip-syntax (cddr exp)))]
            [shader-makers (map (lambda (s)
                                  (if (shader? s)
                                      `(set-finalizer!
                                        (make-shader
                                         ,(shader-type s)
                                         ,(shader-source s) ',(shader-inputs s)
                                         ',(shader-outputs s) ',(shader-uniforms s)
                                         0)
                                        %delete-shader)
                                      s))
                                shaders)])
       `(define ,name
          (create-pipeline ,@shader-makers))))))


;;; GL compiling
(define (compile-shader shader)
  (define (shader-int-type shader)
    (ecase (shader-type shader)
           [(vertex:) gl:+vertex-shader+]
           [(tess-control:) gl:+tess-control-shader+]
           [(tess-evaluation:) gl:+tess-evaluation-shader+]
           [(geometry:) gl:+geometry-shader+]
           [(fragment:) gl:+fragment-shader+]
           [(compute:) gl:+compute-shader+]))
  (when (zero? (shader-program shader))
    (set! (shader-program shader) (gl:make-shader (shader-int-type shader) (shader-source shader)))))

(define (compile-pipeline pipeline)
  (when (zero? (pipeline-program pipeline))
    (for-each compile-shader (pipeline-shaders pipeline))
    (let* ([program (gl:make-program (map shader-program (pipeline-shaders pipeline)))]
           [attribute-location
            (lambda (a)
              (match-let* ([(name . type) a]
                           [location (gl:get-attrib-location
                                      program
                                      (symbol->string (symbol->glsl name)))])
                          (list name location type)))]
           [uniform-location
            (lambda (u)
              (match-let* ([(name . type) u]
                           [location (gl:get-uniform-location
                                      program
                                      (symbol->string (symbol->glsl name)))])
                          (list name location type)))])
      (for-each (lambda (s)
                  (gl:detach-shader program (shader-program s)))
                (pipeline-shaders pipeline))
      (set! (pipeline-program pipeline) program)
      (set! (pipeline-attributes pipeline)
            (map attribute-location (pipeline-attributes pipeline)))
      (set! (pipeline-uniforms pipeline)
            (map uniform-location (pipeline-uniforms pipeline))))))

(define (compile-pipelines)
  (for-each compile-pipeline (pipelines))
  (pipelines '()))

;; Accessors
(define (pipeline-uniform uniform pipeline)
  (let ([u (assoc uniform (pipeline-uniforms pipeline))])
    (unless u
      (error 'pipeline-uniform "No such uniform" uniform))
    (when (< (length u) 3)
      (error 'pipeline-uniform "Pipeline has not been compiled" pipeline))
    (cadr u)))

(define (pipeline-attribute attr pipeline)
  (let ([a (assoc attr (pipeline-attributes pipeline))])
    (unless a
      (error 'pipeline-attribute "No such attribute" attr))
    (when (< (length a) 3)
      (error 'pipeline-attribute "Pipeline has not been compiled" pipeline))
    (cadr a)))

) ; module end
