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

(import chicken scheme)

(use extras data-structures srfi-1 srfi-69
     fmt fmt-c matchable miscmacros irregex)

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

(define glsl-version (make-parameter (cond-expand
                                       (gles 120)
                                       (else 330))))
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
                            (member k '(input: output: uniform: version: use: export: prelude:)))
                  (syntax-error "Key not recognized:" k)))
              keys))
  (define (normalize-version version)
    (match version
      ( (? number? version) (list version))
      ( ((? number? version) rest ...) (cons version rest))
      (else (error "invalid glsl version (expecting number or '(number ...))" version))))
  (define (compile type body #!key
                   (input '()) (output '()) (uniform '())
                   prelude
                   (version (glsl-version))
                   (use '()) (export '()))
    (parameterize ((exports export)
                   (export-prototypes '()))
      (let ((version-spec (normalize-version version)))
        (let-values (((declarations in out uni) (compile-inputs input output uniform
                                                                version-spec type)))
          (values (fmt #f (cat "#version " (fmt-join dsp version-spec " ") "\n\n"
                               (or prelude "")
                               (if (null? use)
                                   ""
                                   "<<imports>>\n")
                               (if (null? declarations)
                                   ""
                                   (apply c-begin declarations))
                               (apply c-begin (map glsl->fmt body))))
                  in out uni use (fmt #f (apply-cat (export-prototypes))))))))
  (match form
    ((((? shader-type? shader-type) . (? valid-keys? keys)) body . body-rest)
     (apply compile shader-type (cons* body body-rest) keys))
    (_ (syntax-error "Poorly formed shader:" form))))

(define (compile-inputs in out uniform version-spec shader-type)
  (define (in/out-type->glsl-type type)
    (match (cons* shader-type type version-spec)
      ((shader-type type (? (cut >= <> 130) version) _ ...) type)
      ((shader-type type (? (cut >= <> 100) version) 'es _ ...) type)
      ((shader-type 'uniform _ ...) 'uniform)
      (('#:vertex 'in _ ...) 'attribute)
      (else 'varying)))
  (define (params p type)
    (map (lambda (param)
           (let ((p (parameter param)))
             (c-var (list (in/out-type->glsl-type type)
                          (first p))
                    (second p))))
         p))
  (define (name-type p)
    (map (lambda (p) (cons (first p) (second p)))
         p))
  (values (append (params in 'in)
                  (params out 'out)
                  (params uniform 'uniform))
          (name-type in) (name-type out) (name-type uniform)))

(define (compile-expr expr)
  (fmt #f (glsl->fmt expr)))

(define (glsl->fmt x)
  (cond
   ((symbol? x) (c-wrap-stmt (symbol->glsl x)))
   ((boolean? x) (c-wrap-stmt (dsp (if x 'true 'false))))
   ((list? x)
    (if* (hash-table-ref/default *special-syntax* (car x) #f)
         (apply it (cdr x))
         (if* (hash-table-ref/default *special-functions* (car x) #f)
              (apply it (map glsl->fmt (cdr x)))
              (c-apply (map glsl->fmt x)))))
   (else (c-wrap-stmt (dsp x)))))

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
  (define (illegal-chars str)
    (if (hash-table-ref/default *special-functions* (string->symbol str) #f)
        str
        (irregex-replace/all "[!@$%^&*><+=-]" str "_")))
  (define (multi-sample str)
    (irregex-replace/all "DMs" str "DMS"))
  (define (all sym)
    (string->symbol (multi-sample (dimensions (illegal-chars
                                               (cammel-case (symbol->string sym)))))))
  (case sym
    ((emit-vertex) 'EmitVertex)
    ((end-primitive) 'EndPrimitive)
    (else (all sym))))

(define (replace symbol)
  (lambda (x . rest)
    (cons symbol rest)))

(define glsl:length
  (match-lambda*
    ((vec) (c-wrap-stmt (cat vec ".length()")))
    (args (syntax-error 'length "Only one argument expected:" args))))

(define (type? x)
  (or (symbol? x)
      ((list-of? symbol?) x)))

(define (type->glsl x)
  (if (list? x)
      (map symbol->glsl x)
      (symbol->glsl x)))

(define parameter
  (match-lambda
    ((name ((or '#:array 'array) (? type? type) . size))
     (list (cons* '%array (type->glsl type) size) (symbol->glsl name)))
    ((name (? type? type))
     (list (type->glsl type) (symbol->glsl name)))
    (p (syntax-error "Invalid parameter:" p))))

(define assignment
  (match-lambda*
    ((name ((or '#:array 'array) (? type? type) . size) . init)
     (when (member name (exports))
       (export-prototypes
        (cons (c-var `(%array ,(type->glsl type) . ,size) (symbol->glsl name))
              (export-prototypes))))
     (apply c-var `(%array ,(type->glsl type) . ,size)
            (symbol->glsl name)
            (cond
             ((null? init)'())
             ((vector? (car init)) 
              (list (cat (type->glsl type)
                         "[]("
                         (fmt-join c-expr
                                   (map glsl->fmt
                                        (vector->list (car init)))
                                   ", ")
                         ")")))
             (else (list (apply glsl->fmt init))))))
    ((name (? type? type) . init)
     (when (member name (exports))
       (export-prototypes
        (cons (c-var (type->glsl type) (symbol->glsl name))
              (export-prototypes))))
     (apply c-var (type->glsl type)
            (symbol->glsl name)
            (if (null? init)
                '()
                (list (apply glsl->fmt init)))))
    (expr (syntax-error "Poorly formed assignment:" expr))))

(define glsl:define
  (match-lambda*
    (((name . params) (? type? return-type) body . body-rest)
     (when (member name (exports))
       (export-prototypes
        (cons (c-prototype (type->glsl return-type) (symbol->glsl name)
                           (map parameter params))
              (export-prototypes))))
     (apply c-fun (type->glsl return-type) (glsl->fmt name)
            (map parameter params)
            (map glsl->fmt (cons body body-rest))))
    (((name . params) (? type? return-type))
     (c-prototype (type->glsl return-type) (symbol->glsl name)
                  (map parameter params)))
    (a (apply assignment a))))

(define glsl:let
  (match-lambda*
    (((? list? assignments)  body . body-rest)
     (apply c-begin (append (map (lambda (a) (apply assignment a)) assignments)
                            (map glsl->fmt (cons body body-rest)))))
    (expr (syntax-error 'let "Poorly formed:" expr))))

(define glsl:struct
  (match-lambda*
    ((name . fields)
     (c-struct (symbol->glsl name) (map parameter fields)))
    (expr (syntax-error 'struct "Poorly formed:" expr))))

(define glsl:do-times
  (match-lambda*
    (((var end) body . body-rest)
     (let ((v (symbol->glsl var)))
       (apply c-for (c-var 'int v 0)
              (c< v (glsl->fmt end))
              (c++ v)
              (map glsl->fmt (cons body body-rest)))))
    (((var start end) body . body-rest)
     (let ((v (symbol->glsl var)))
       (apply c-for (c-var 'int v (glsl->fmt start))
              (c< v (glsl->fmt end))
              (c++ v)
              (map glsl->fmt (cons body body-rest)))))
    (expr (syntax-error 'do-times "Poorly formed:" expr))))

(define glsl:swizzle
  (match-lambda*
    ((vec . (? (list-of? symbol?) x))
     (c-wrap-stmt (cat (symbol->glsl vec) "." (apply symbol-append x))))
    (args (syntax-error 'swizzle "Poorly formed arguments:" args))))

(define glsl:array-ref
  (match-lambda*
    ((array ref) (c-wrap-stmt (cat array "[" ref "]")))
    (args (syntax-error 'array-ref "Poorly formed arguments:" args))))

(define glsl:array-set!
  (match-lambda*
    ((array ref val) (c-wrap-stmt (cat array "[" ref "] = " val)))
    (args (syntax-error 'array-set! "Poorly formed arguments:" args))))

(define (glsl:cond . x)
  ;; Adapted from fmt-c
  (let loop ((ls x) (res '()))
    (if (null? ls)
        (glsl->fmt (cons 'if (delete 'else (reverse res))))
        (loop (cdr ls)
              (cons (if (pair? (cddar ls))
                        (cons 'begin (cdar ls))
                        (cadar ls))
                    (cons (caar ls) res))))))

(define *special-syntax*
  (alist->hash-table
   `((cond . ,glsl:cond)
     (define . ,glsl:define)
     (let . ,glsl:let)
     (let* . ,glsl:let)
     (swizzle . ,glsl:swizzle)
     (~~ . ,glsl:swizzle)
     (struct . ,glsl:struct)
     (define-record . ,glsl:struct)
     (do-times . ,glsl:do-times)
     (%extension . ,(lambda (name behaviour) (cat "#extension " name
                                             " : " behaviour #\newline)))
     (%pragma . ,(match-lambda*
                   ((#:stdgl (n . b)) (cat "#pragma STDGL "
                                           n "(" (fmt-join dsp b ", ")
                                           ")" #\newline))
                   ((n . b) (cat "#pragma "
                                n "(" (fmt-join dsp b ", ")
                                ")" #\newline)))))))

(define *special-functions*
  (alist->hash-table
   `((array-ref . ,glsl:array-ref)
     (vector-ref . ,glsl:array-ref)
     (array-set! . ,glsl:array-set!)
     (vector-set! . ,glsl:array-set!)
     (expt . ,(lambda e (c-apply (cons 'pow e))))
     (length . ,glsl:length)

     (discard . ,(lambda () (c-wrap-stmt (dsp "discard"))))
     (break . ,(lambda () (c-wrap-stmt (dsp "break"))))
     (continue . ,(lambda () (c-wrap-stmt (dsp "continue"))))
     (return . ,c-return)

     (begin . ,c-begin)
     (while . ,c-while)
     (for . ,c-for)
     (if . ,c-if)
     (switch . ,c-switch)
     (case . ,c-case)
     (case/fallthrough . ,c-case/fallthrough)
     (default . ,c-default)

     (++ . ,c++)
     (-- . ,c--)
     (+ . ,c+)
     (- . ,c-)
     (* . ,c*)
     (/ . ,c/)
     (modulo . ,c%)
     (% . ,c%)
     (bitwise-and . ,c&)
     (& . ,c&)
     (bitwise-xor . ,c^)
     (^ . ,c^)
     (bitwise-not . ,c~)
     (~ . ,c~)
     (not . ,c!)
     (! . ,c!)
     (and . ,c&&)
     (&& . ,c&&)
     (arithmetic-shift . ,c<<)
     (<< . ,c<<)
     (>> . ,c>>)
     (bitwise-ior . ,c-bit-or)
     (or . ,c-or)

     (== . ,c==)
     (equal? . ,c==)
     (eqv? . ,c==)
     (eq? . ,c==)
     (= . ,c==)

     (!= . ,c!=)
     (< . ,c<)
     (> . ,c>)
     (<= . ,c<=)
     (>= . ,c>=)
     (set! . ,c=)
     (= . ,c=)
     (+= . ,c+=)
     (-= . ,c-=)
     (*= . ,c*=)
     (/= . ,c/=)
     (modulo= . ,c%=)
     (%= . ,c%=)
     (bitwise-and= . ,c&=)
     (&= . ,c&=)
     (bitwise-xor= . ,c^=)
     (^= . ,c^=)
     (bitwise-ior= . ,c-bit-or=)
     (arithmetic-shift= . ,c<<=)
     (<<= . ,c<<=)
     (>>= . ,c>>=)
     (++/post . ,c++/post)
     (--/post . ,c--/post)

     (.. . ,c.)
     (field . ,c.)

     (%define . ,cpp-define)
     (%if . ,cpp-if)
     (%ifdef . ,cpp-ifdef)
     (%ifndef . ,cpp-ifndef)
     (%elif . ,cpp-elif)
     (%else . ,cpp-else)
     (%error . ,cpp-error))))

) ; end module
