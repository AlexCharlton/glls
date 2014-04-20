;;;; glls-compiler.scm
;;;;
;;;; Functions responsible for taking glls forms and transforming them into GLSL

(module glls-compiler
  (compile-glls
   symbol->glsl
   compile-expr
   shader-type
   shader-id
   shader-source
   shader-inputs
   shader-outputs
   shader-uniforms
   shader-program
   make-shader
   create-shader)

(import chicken scheme data-structures srfi-1 srfi-69 irregex)

(use fmt fmt-c matchable srfi-42 miscmacros)

;;; Shader record
(define-record shader
  type id source inputs outputs uniforms (setter program))

(define-record-printer (shader s out)
  (fprintf out "#,(shader ~S '~S ~S '~S '~S '~S ~S)"
           (shader-type s) (shader-id s) (shader-source s)
           (shader-inputs s) (shader-outputs s) (shader-uniforms s)
           (shader-program s)))

(define (create-shader form #!key [inputs '()])
  (let-values ([(s i o u) (compile-glls form inputs: inputs)])
    (make-shader (car form) (gensym "shader") s i o u 0)))

(define shader-types
  '(#:vertex #:fragment #:geometry #:tess-control #:tess-evaluation #:compute))

;;; Main compiling function
;; Takes a form with the glls syntax and returns values:
;; - The GLSL string
;; - A list of the inputs (string-name symbol-type)
;; - A list of the outputs (string-name symbol-type)
;; - A list of the uniforms (string-name symbol-type)
(define (compile-glls form #!key [inputs '()])
  (define (shader-type? s) (member s shader-types))
  (define (compile type input body output #!optional [version 330])
    (let-values ([(sl in out uni) (compile-inputs (append inputs input) output)])
      (values (string-append  "#version " (number->string version) "\n\n"
                              (fmt #f (c-expr `(%begin ,@sl ,(glsl->fmt body)))))
              in out uni)))
  (match form
    [((? shader-type? shader-type) input body -> output)
     (compile shader-type input body output)]
    [((? shader-type? shader-type) (? integer? version) input body -> output)
     (compile shader-type input body output version)]
    [_ (syntax-error "Poorly formed shader:" form)]))

(define (prn x) (newline) (newline) (print x) (newline) x)

(define (compile-inputs in out)
  (define (params p type)
    (list-ec (: i p) (match-let ([(t name) (glsl->fmt (parameter i))])
                       `(%var ,(list type t) ,name))))
  (define (name-type p)
    (list-ec (: i p)
             (cons (symbol->glsl (car i))
                   (cadr i))))
  (let* ([uniforms (if* (member #:uniform in)
                        it
                        '(_))]
         [in (if (not (equal? uniforms '(_)))
                 (take in (- (length in) (length uniforms)))
                 in)])
    (values (append (params in 'in)
                    (params out 'out)
                    (params (cdr uniforms) 'uniform))
            (name-type in) (name-type out) (name-type (cdr uniforms)))))

(define (compile-expr expr)
  (fmt #f (c-expr (glsl->fmt expr))))

(define (glsl->fmt tree)
  (let ([t (list-ec (: el tree)
                    (cond
                     [(symbol? el) (symbol->glsl el)]
                     [(list? el) (glsl->fmt el)]
                     [else el]))])
    (if* (hash-table-ref/default *special-functions* (car tree) #f)
         (apply it t)
         t)))

(define (symbol->glsl sym)
  (define cammel-case
    (irregex-replace/all "[:-](.)" (symbol->string sym)
                         (lambda (m)
                           (let* ([s (irregex-match-substring m)]
                                  [char1 (string-ref s 0)]
                                  [char2 (char-upcase (string-ref s 1))])
                             (if (equal? char1 #\:)
                                 (string #\_ char2)
                                 (string char2))))))
  (define dimensions
    (irregex-replace/all "[1-3]d" cammel-case
                         (lambda (m) (let* ([s (irregex-match-substring m)]
                                       [char1 (string-ref s 0)])
                                  (string char1 #\D)))))
  (string->symbol (irregex-replace/all "DMs" dimensions "DMS")))

(define (replace symbol)
  (lambda (x . rest)
    (cons symbol rest)))

(define glsl:swizzle
  (match-lambda*
   [(_ (? symbol? vec) . (? (list-of? symbol?) x)) `(%. ,vec ,(apply symbol-append x))]
   [args (syntax-error 'swizzle "Poorly formed arguments:" args)]))

(define glsl:length
  (match-lambda*
   [(_ vec) `(%. ,vec (length))]
   [args (syntax-error 'length "Only one argument expected:" args)]))

(define (type? x)
  (or (symbol? x)
      ((list-of? symbol?) x)))

(define parameter
  (match-lambda
   [(name (#:array (? type? type) . size))
    `((%array ,type . ,size) ,name)]
   [(name (? type? type))
    `(,type ,name)]
   [p (syntax-error "Invalid parameter:" p)]))

(define assignment
  (match-lambda*
   [(name (#:array (? type? type) . size) . init)
    `(%var (%array ,type . ,size) ,name . ,init)]
   [(name (? type? type) . init)
    `(%var ,type ,name . ,init)]
   [expr (syntax-error "Poorly formed assignment:" expr)]))

(define glsl:define
  (match-lambda*
   [(_ (name . params) (? type? return-type) body . body-rest)
    `(%fun ,return-type ,name ,(map parameter params) ,body . ,body-rest)]
   [(_ . a) (apply assignment a)]))

(define glsl:let
  (match-lambda*
   [(_ (? list? assignments)  body . body-rest)
    `(%begin ,@(map (lambda (a) (apply assignment a)) assignments)
             ,body . ,body-rest)]
   [expr (syntax-error 'let "Poorly formed:" expr)]))

(define glsl:struct
  (match-lambda*
   [(_ name . fields)
    `(struct name ,(map parameter fields))]
   [expr (syntax-error 'struct "Poorly formed:" expr)]))

(define glsl:dotimes
  (match-lambda*
   [(_ (var end) body . body-rest)
    `(for (= ,var 0) (< ,var ,end) (++ ,var) ,body . ,body-rest)]
   [(_ (var start end) body . body-rest)
    `(for (= ,var ,start) (< ,var ,end) (++ ,var) ,body . ,body-rest)]))

(define *special-functions*
  (alist->hash-table
   `((modulo . ,(replace '%))
     (equal? . ,(replace '==))
     (eqv? . ,(replace '==))
     (eq? . ,(replace '==))
     (= . ,(replace '==))
     (set! . ,(replace '=))
     (and . ,(replace '&&))
     (or . ,(replace '%or))
     (not . ,(replace '!))
     (bitwise-or . ,(replace 'bitwise-ior))
     (array-ref . ,(replace 'vector-ref))
     (field . ,(replace '%field))
     (begin . ,(replace '%begin))
     (cond . ,(replace '%cond))
     (discard . ,(lambda (a) 'discard))
     (length . ,glsl:length)
     (swizzle . ,glsl:swizzle)
     (~ . ,glsl:swizzle)
     (define . ,glsl:define)
     (let . ,glsl:let)
     (record . ,glsl:struct)
     (struct . ,glsl:struct)
     (dotimes . ,glsl:dotimes))))

) ; end module
