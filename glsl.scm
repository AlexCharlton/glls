;;;; glsl.scm
;;;;

(module glsl
  (make-pipeline
   pipeline-shaders
   pipeline-attributes
   pipeline-uniforms
   create-pipeline
   defpipeline
   defshader
   compile-pipeline
   gl-compile-shader)

(import chicken scheme srfi-69 srfi-1 miscmacros)
(use glsl-compiler (prefix opengl-glew gl:) matchable)
(import-for-syntax glsl-compiler)

(reexport glsl-compiler)

(begin-for-syntax
 (require-library glsl-compiler))

(define-record pipeline
  shaders (setter attributes) (setter uniforms) (setter program))

(define-record-printer (pipeline p out)
  (fprintf out "#(pipeline ~S shaders:~S attributes:~S uniforms:~S)"
           (pipeline-program p) (map shader-id (pipeline-shaders p))
           (pipeline-attributes p) (pipeline-uniforms p)))

(define-syntax begin-also-for-syntax
  (syntax-rules ()
    ((_ forms ...)
      (begin
        (begin-for-syntax (require-library glsl) forms ...)
        forms ...))))

(define-syntax defshader
  (er-macro-transformer
   (lambda (exp rename compare)
     (let* ([name (cadr exp)]
           [shader (create-shader (cddr exp))]
           [shader-maker `(,(rename 'make-shader)
                           ,(shader-type shader) ',(shader-id shader)
                           ,(shader-source shader) ',(shader-inputs shader)
                           ',(shader-outputs shader) ',(shader-uniforms shader)
                           0)])
       `(,(rename 'begin-also-for-syntax)
         (,(rename 'define) ,name ,shader-maker))))))

(define (create-pipeline . shaders)
  (if (< (length shaders) 2)
      (syntax-error "Invalid pipeline definition:" shaders))
  (let* ([shaders (map (lambda (s)
                         (if (and (list? s)
                                  (= (length s) 5)
                                  (equal? (cadddr s) '->))
                             (create-shader s)
                             (eval s)))
                       shaders)]
         [attributes (apply append
                            (map shader-inputs
                                 (filter (lambda (s)
                                           (equal? (shader-type s) #:vertex))
                                         shaders)))]
         [uniforms (apply append (map shader-uniforms shaders))])
    (make-pipeline shaders attributes uniforms 0)))

(define-syntax defpipeline
  (er-macro-transformer
   (lambda (exp rename compare)
     (if (< (length exp) 3)
         (syntax-error "Invalid pipeline definition:" exp))
     (let* ([name (cadr exp)]
            [shaders (map (lambda (s)
                            (if (and (list? s)
                                     (= (length s) 5)
                                     (equal? (cadddr s) '->))
                                (create-shader s)
                                (eval s)))
                          (cddr exp))]
            [shader-makers (map (lambda (s)
                                  `(,(rename 'make-shader)
                                   ,(shader-type s) ',(shader-id s)
                                   ,(shader-source s) ',(shader-inputs s)
                                   ',(shader-outputs s) ',(shader-uniforms s)
                                   0)) shaders)]
            [attributes (apply append
                               (map shader-inputs
                                    (filter (lambda (s)
                                              (equal? (shader-type s) #:vertex))
                                            shaders)))]
            [uniforms (apply append (map shader-uniforms shaders))])
       `(,(rename 'begin-also-for-syntax)
         (,(rename 'define) ,name
          (,(rename 'make-pipeline) (,(rename 'list) ,@shader-makers)
           ',attributes ',uniforms
           0)))))))


;;; GL compiling
(define *compiled-shaders* (make-hash-table))

(define (gl-compile-shader shader)
  (define (shader-int-type shader)
    (ecase (shader-type shader)
           [(vertex:) gl:+vertex-shader+]
           [(fragment:) gl:+fragment-shader+]
           [(geometry:) gl:+geometry-shader+]))
  (if* (hash-table-ref/default *compiled-shaders* (shader-id shader) #f)
       (begin
         (set! (shader-program shader) it)
              it)
       (begin
         (let ([s (gl:make-shader (shader-int-type shader) (shader-source shader))])
           (hash-table-set! *compiled-shaders* (shader-id shader) s)
           (set! (shader-program shader) s)
           s))))

(define (compile-pipeline pipeline)
  (for-each gl-compile-shader (pipeline-shaders pipeline))
  (let* ([program (gl:make-program (map shader-program (pipeline-shaders pipeline)))]
         [attribute-location
          (lambda (a)
            (match-let* ([(name . type) a]
                         [location (gl:get-attrib-location program name)])
                        (list name location type)))]
         [uniform-location
          (lambda (u)
            (match-let* ([(name . type) u]
                         [location (gl:get-uniform-location program name)])
                        (list name location type)))])
    (set! (pipeline-program pipeline) program)
    (set! (pipeline-attributes pipeline)
          (map attribute-location (pipeline-attributes pipeline)))
    (set! (pipeline-uniforms pipeline)
          (map uniform-location (pipeline-uniforms pipeline)))))

) ; module end
