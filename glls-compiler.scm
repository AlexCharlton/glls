;;;; glls-compiler.scm
;;;;
;;;; Functions responsible for taking glls forms and transforming them into GLSL

(module glls-compiler
  (compile-glls
   symbol->glsl
   compile-expr
   shader-types
   shader?
   shader-type
   shader-source
   shader-inputs
   shader-outputs
   shader-uniforms
   shader-imports
   shader-exports
   shader-program
   glsl-version
   %make-shader
   %create-shader)

(import chicken scheme data-structures srfi-1 srfi-69)

(use fmt fmt-c matchable srfi-42 miscmacros irregex)

;;; Shader record
(define-record shader
  type source inputs outputs uniforms imports exports (setter program))

(define-record-printer (shader s out)
  (fprintf out "#,(shader ~S ~S '~S '~S '~S '~S '~S ~S)"
           (shader-type s)  (shader-source s)
           (shader-inputs s) (shader-outputs s) (shader-uniforms s)
           (shader-imports s) (shader-exports s)
           (shader-program s)))

(define (%create-shader form)
  (let-values (((s i o u im ex) (compile-glls form)))
    (make-shader (caar form) s i o u im ex 0)))

(define (%make-shader type source inputs outputs uniforms imports exports program)
  (make-shader type (irregex-replace "<<imports>>" source
                                     (apply string-append
                                            (map shader-exports imports)))
               inputs outputs uniforms imports exports program))

(define shader-types
  '(#:vertex #:fragment #:geometry #:tess-control #:tess-evaluation #:compute))

(define glsl-version (make-parameter 330))
(define exports (make-parameter '()))
(define export-prototypes (make-parameter '()))

;;; Main compiling function
;; Takes a form with the glls syntax and returns values:
;; - The GLSL string
;; - A list of the inputs (string-name symbol-type)
;; - A list of the outputs (string-name symbol-type)
;; - A list of the uniforms (string-name symbol-type)
(define (compile-glls form)
  (define (shader-type? s) (member s shader-types))
  (define (valid-keys? keys)
    (for-each (lambda (k)
                (unless (or (not (keyword? k))
                           (member k '(input: output: uniform: version: extensions:
                                              pragmas: use: export:)))
                  (syntax-error "Key not recognized:" k)))
              keys))
  (define (compile type body #!key
                   (input '()) (output '()) (uniform '())
                   (version (glsl-version)) (extensions '()) (pragmas '())
                   (use '()) (export '()))
    (parameterize ((exports (map symbol->glsl export))
                   (export-prototypes '()))
      (let-values (((declarations in out uni) (compile-inputs input output uniform
                                                              version type)))
        (values (fmt #f "#version " (number->string version) "\n\n"
                     (fmt-join dsp
                               (list-ec (: e extensions)
                                        (fmt #f "#extension " e  #\newline)))
                     (fmt-join dsp
                               (list-ec (: p pragmas)
                                        (fmt #f "#pragma " p #\newline)))
                     (if (null? use)
                         ""
                         "<<imports>>\n")
                     (c-expr (cons '%begin
                                   (append declarations (map glsl->fmt body)))))
                in out uni use (apply string-append
                                (map (lambda (p) (fmt #f (c-expr p)))
                                     (export-prototypes)))))))
  (match form
    ((((? shader-type? shader-type) . (? valid-keys? keys)) body . body-rest)
     (apply compile shader-type (cons* body body-rest) keys))
    (_ (syntax-error "Poorly formed shader:" form))))

(define (compile-inputs in out uniform version shader-type)
  (define (in/out-type->glsl-type type)
    (cond
     ((or (>= version 330)
         (equal? type 'uniform)) type)
     ((and (equal? shader-type #:vertex)
         (equal? type 'in)) 'attribute)
     (else 'varying)))
  (define (params p type)
    (list-ec (: i p) (match-let (((t name) (glsl->fmt (parameter i))))
                       `(%var ,(list (in/out-type->glsl-type type) t) ,name))))
  (define (name-type p)
    (list-ec (: i p)
             (cons (car i)
                   (cadr i))))
  (values (append (params in 'in)
                  (params out 'out)
                  (params uniform 'uniform))
          (name-type in) (name-type out) (name-type uniform)))

(define (compile-expr expr)
  (fmt #f (c-expr (glsl->fmt expr))))

(define (glsl->fmt tree)
  (let ((t (list-ec (: el tree)
                    (cond
                     ((symbol? el) (symbol->glsl el))
                     ((boolean? el) (if el 'true 'false))
                     ((list? el) (glsl->fmt el))
                     (else el)))))
    (if* (hash-table-ref/default *special-functions* (car tree) #f)
         (apply it t)
         t)))

(define (symbol->glsl sym)
  (define (cammel-case str)
    (irregex-replace/all "[:-](.)" str
                         (lambda (m)
                           (let* ((s (irregex-match-substring m))
                                  (char1 (string-ref s 0))
                                  (char2 (char-upcase (string-ref s 1))))
                             (if (equal? char1 #\:)
                                 (string #\_ char2)
                                 (string char2))))))
  (define (dimensions str)
    (irregex-replace/all "[1-3]d" str
                         (lambda (m) (let* ((s (irregex-match-substring m))
                                       (char1 (string-ref s 0)))
                                  (string char1 #\D)))))
  (define (multi-sample str)
    (irregex-replace/all "DMs" str "DMS"))
  (define (all sym)
    (string->symbol (multi-sample (dimensions (cammel-case (symbol->string sym))))))
  (case sym
    ((emit-vertex) 'EmitVertex)
    ((end-primitive) 'EndPrimitive)
    (else (all sym))))

(define (replace symbol)
  (lambda (x . rest)
    (cons symbol rest)))

(define glsl:swizzle
  (match-lambda*
   ((_ vec . (? (list-of? symbol?) x)) `(%. ,vec ,(apply symbol-append x)))
   (args (syntax-error 'swizzle "Poorly formed arguments:" args))))

(define glsl:length
  (match-lambda*
   ((_ vec) `(%. ,vec (length)))
   (args (syntax-error 'length "Only one argument expected:" args))))

(define (type? x)
  (or (symbol? x)
      ((list-of? symbol?) x)))

(define parameter
  (match-lambda
   ((name ((or '#:array 'array) (? type? type) . size))
    `((%array ,type . ,size) ,name))
   ((name (? type? type))
    `(,type ,name))
   (p (syntax-error "Invalid parameter:" p))))

(define assignment
  (match-lambda*
   ((name ((or '#:array 'array) (? type? type) . size) . init)
    (when (member name (exports))
      (export-prototypes
       (cons `(%var (%array ,type . ,size) ,name)
             (export-prototypes))))
    `(%var (%array ,type . ,size) ,name . ,init))
   ((name (? type? type) . init)
    (when (member name (exports))
      (export-prototypes
       (cons `(%var ,type ,name)
             (export-prototypes))))
    `(%var ,type ,name . ,init))
   (expr (syntax-error "Poorly formed assignment:" expr))))

(define glsl:define
  (match-lambda*
   ((_ (name . params) (? type? return-type) body . body-rest)
    (when (member name (exports))
      (export-prototypes
       (cons `(%prototype ,return-type ,name ,(map parameter params))
             (export-prototypes))))
    `(%fun ,return-type ,name ,(map parameter params) ,body . ,body-rest))
   ((_ (name . params) (? type? return-type))
    `(%prototype ,return-type ,name ,(map parameter params)))
   ((_ . a) (apply assignment a))))

(define glsl:let
  (match-lambda*
   ((_ (? list? assignments)  body . body-rest)
    `(%begin ,@(map (lambda (a) (apply assignment a)) assignments)
             ,body . ,body-rest))
   (expr (syntax-error 'let "Poorly formed:" expr))))

(define glsl:struct
  (match-lambda*
   ((_ name . fields)
    `(struct ,name ,(map parameter fields)))
   (expr (syntax-error 'struct "Poorly formed:" expr))))

(define glsl:do-times
  (match-lambda*
   ((_ (var end) body . body-rest)
    `(for (%var int ,var 0) (< ,var ,end) (++ ,var) ,body . ,body-rest))
   ((_ (var start end) body . body-rest)
    `(for (%var int ,var ,start) (< ,var ,end) (++ ,var) ,body . ,body-rest))
   (expr (syntax-error 'do-times "Poorly formed:" expr))))

(define *special-functions*
  (alist->hash-table
   `((modulo . ,(replace '%))
     (equal? . ,(replace '==))
     (eqv? . ,(replace '==))
     (eq? . ,(replace '==))
     (= . ,(replace '==))
     (set! . ,(replace '=))
     (-= . ,(replace '-=)) ; Why does this need to be here?
     (and . ,(replace '&&))
     (or . ,(replace '%or))
     (not . ,(replace '!))
     (array-ref . ,(replace 'vector-ref))
     (field . ,(replace '%field))
     (.. . ,(replace '%field))
     (begin . ,(replace '%begin))
     (cond . ,(replace '%cond))
     (case . ,(replace '%switch))
     (expt . ,(replace 'pow))
     (discard . ,(lambda (a) 'discard))
     (length . ,glsl:length)
     (swizzle . ,glsl:swizzle)
     (~~ . ,glsl:swizzle)
     (define . ,glsl:define)
     (let . ,glsl:let)
     (let* . ,glsl:let)
     (define-record . ,glsl:struct)
     (struct . ,glsl:struct)
     (do-times . ,glsl:do-times))))

) ; end module
