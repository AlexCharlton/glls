;;;; compiler.scm
;;;;
;;;; Functions responsible for taking scheme-glsl forms and transforming them into glsl
(module glsl-compiler
  (compile-glsl
   symbol->glsl
   compile-expr
   compile-inputs
   shader-type
   shader-id
   shader-source
   shader-inputs
   shader-outputs
   shader-uniforms
   shader-program
   make-shader
   create-shader)

(import chicken scheme)

(use irregex srfi-1 srfi-13 srfi-42 srfi-69 matchable format data-structures
     miscmacros)

;;; Shader record
(define-record shader
  type id source inputs outputs uniforms (setter program))

(define-record-printer (shader s out)
  (fprintf out "#,(shader ~S '~S ~S '~S '~S '~S ~S)"
           (shader-type s) (shader-id s) (shader-source s)
           (shader-inputs s) (shader-outputs s) (shader-uniforms s)
           (shader-program s)))

(define (create-shader form #!key [inputs '()])
  (let-values ([(s i o u) (compile-glsl form inputs: inputs)])
    (make-shader (car form) (gensym "shader") s i o u 0)))

;;; Main compiling function
;; Takes a form with the scheme-glsl syntax and returns values:
;; - The glsl string
;; - A list of the inputs (string-name symbol-type)
;; - A list of the outputs (string-name symbol-type)
;; - A list of the uniforms (string-name symbol-type)
(define (compile-glsl form #!key [inputs '()])
  (define (shader-type? s) (member s '(#:vertex #:fragment #:geometry)))
  (match form
    [((? shader-type? shader-type) input body -> output)
     (let-values ([(sl in out uni) (compile-inputs (append inputs input) output)])
       (values (format #f "#version 330~%~%~a~%~a~%"
                       sl (compile-expr body))
               in out uni))]
    [_ (syntax-error "Poorly formed shader:" form)]))

(define (compile-inputs in out)
  (let* ([uniforms (if* (member #:uniform in)
                        it
                        '(_))]
         [in (if (not (equal? uniforms '(_)))
                 (take in (- (length in) (length uniforms)))
                 in)])
    (values (apply string-append
                   (flatten (list-ec (: i in) (format "in ~a;~%" (assignment i)))
                            (list-ec (: o out) (format "out ~a;~%" (assignment o)))
                            (list-ec (: u (cdr uniforms))
                                     (format "uniform ~a;~%" (assignment u)))))
            (list-ec (: a in)
                     (cons (symbol->glsl (car a))
                           (cadr a)))
            (list-ec (: o out)
                     (cons (symbol->glsl (car o))
                           (cadr o)))
            (list-ec (: u (cdr uniforms))
                     (cons (symbol->glsl (car u))
                           (cadr u))))))

;; Given a scheme-glsl expression, return the glsl string
(define compile-expr
  (match-lambda
   [(? special-fun? expr) ((hash-table-ref *special-functions* (first expr))
                           (cdr expr))]
   [(? list? expr) (function-call expr)]
   [(? symbol? s) (symbol->glsl s)]
   [expr (format #f "~a" expr)]))

(define (special-fun? expr)
  (and (list? expr)
       (hash-table-ref/default *special-functions* (first expr) #f)))

(define (function-call form)
  (match-let (((fun . args) form))
    (format #f "~a(~{~a~^, ~})" (symbol->glsl fun) (map compile-expr args))))

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
  (irregex-replace/all "DMs" dimensions "DMS"))

;;; GLSL functions, operators
(define (infix-fun-multiple-args sym)
  (lambda (args)
    (when (< (length args) 2)
      (syntax-error sym "Expected two or more arguments:" args))
    (format #f (format #f "(~~{~~a~~^ ~a ~~})" sym) (map compile-expr args))))

(define (infix-fun sym)
  (lambda (args)
    (unless (= (length args) 2)
      (syntax-error sym "Expected two arguments:" args))
    (format #f (format #f "(~~{~~a~~^ ~a ~~})" sym) (map compile-expr args))))

(define (prefix-fun sym)
  (lambda (args)
    (unless (= (length args) 1)
      (syntax-error sym "Expected one argument:" args))
    (format #f "(~a ~a)" sym (compile-expr (first args)))))

(define glsl:array-ref
  (match-lambda
   [(a i) (format #f "~a[~a]" (compile-expr a) (compile-expr i))]
   [args (syntax-error 'array-ref "Expected two arguments:" args)]))

(define glsl:length
  (match-lambda
   [(a) (format #f "~a.length()" (compile-expr a))]
   [args (syntax-error 'length "Expected one argument:" args)]))

(define glsl:swizzle
  (match-lambda
   [(a b . c) (format #f "~a.~a~{~a~}" (compile-expr a) b c)]
   [args (syntax-error 'swizzle "Poorly formed arguments:" args)]))

(define glsl:assign
  (match-lambda
   [(a b) (format #f "~a = ~a" (compile-expr a) (compile-expr b))]
   [args (syntax-error 'set! "Expected two arguments:" args)]))

(define glsl:if
  (match-lambda
   [(test true) (format #f "if (~a){~%~a;}~%" (compile-expr test) (compile-expr true))]
   [(test true false) (format #f "if (~a){~%~a;} else {~%~a;}~%"
                              (compile-expr test) (compile-expr true)
                              (compile-expr false))]
   [args (syntax-error 'if "Poorly formed:" args)]))

(define (glsl:begin expr)
  (apply string-append (list-ec (: e expr)
                                (string-append (compile-expr e) ";\n"))))

(define glsl:cond
  (match-lambda
   [((test body . body-rest) . rest)
    (apply string-append
     (format #f "if (~a){~%~a;~%~{~a;~%~}} " (compile-expr test) (compile-expr body)
             (map compile-expr body-rest))
     (list-ec (: e rest)
              (cond
               [(or (not (list? e))
                    (< (length e) 2))
                (syntax-error 'cond "Poorly formed:" (cons* (cons* test body body-rest)
                                                            rest))]
               [(equal? (car e) 'else)
                (format #f "else {~%~{~a;~%~}}~%" (map compile-expr (cdr e)))] 
               [else (format #f "else if (~a){~%~{~a;~%~}} "
                             (compile-expr (car e))
                             (map compile-expr (cdr e)))])))]
   [args (syntax-error 'cond "Poorly formed:" args)]))

(define (compile-type type name)
  (match type
    [(#:array (? keyword? type) (? number? n))
     (format #f "~a ~a[~a]"
             (compile-expr type) (compile-expr name) n)]
    [(#:array (? keyword? type))
     (format #f "~a ~a[]"
             (compile-expr type) (compile-expr name))]
    [(? keyword? type)
     (format #f "~a ~a" (compile-expr type) (compile-expr name))]
    [type (syntax-error "Bad type:" type)]))

(define assignment
  (match-lambda
   [(var (#:array (? keyword? type) (? number? n)))
    (compile-type (list #:array type n) var)]
   [(var (#:array (? keyword? type) (? number? n)) (? (lambda (v)
                                                        (and (list? v)
                                                             (= (length v) n)))
                                                      val))
    (format #f "~a = ~a[](~{~a~^, ~})"
            (compile-type (list #:array type n) var) (compile-expr type)
            (map compile-expr val))]
   [(var (#:array (? keyword? type)))
    (format #f "~a" (compile-type (list #:array type) var))]
   [(var (? keyword? type) val)
    (format #f "~a = ~a" (compile-type type var) (compile-expr val))]
   [(var (? keyword? type)) (compile-type type var)]
   [expr (syntax-error "Poorly formed assignment:" expr)]))

(define glsl:let
  (match-lambda
   [((? list? assignments) . body)
    (apply string-append
           (flatten (map (lambda (a) (list (assignment a) ";\n")) assignments)
                    (compile-expr (cons 'begin body))))]
   [expr (syntax-error 'let "Poorly formed:" expr)]))

(define parameter
  (match-lambda
   [((? symbol? name) (? symbol? type))
    (compile-type type name)]
   [((? (lambda (q) (member q '(#:in #:out #:inout))) qualifier)
     (? symbol? name) (? symbol? type))
    (format #f "~a ~a" (compile-expr qualifier) (compile-type type name))]
   [expr (syntax-error "Bad function parameter:" expr)]))

(define glsl:define
  (match-lambda
   [((name . params) return-type body . body-rest)
    (format #f "~a ~a(~{~a~^, ~}){~%~a;~%~{~a;~%~}}~%"
            (compile-type return-type "") (compile-expr name) (map parameter params)
            (compile-expr body)
            (map compile-expr body-rest))]
   [a (format #f "~a;~%" (assignment a))]))

(define glsl:dotimes
  (match-lambda
   [((var end) body . body-rest)
    (let ([var (compile-expr var)])
      (format #f "for (int ~a = 0; ~a < ~a ; ~a++){~%~a;~%~{~a;~%~}}~%"
              var var (compile-expr end) var (compile-expr body)
              (map compile-expr body-rest)))]
   [((var start end) body . body-rest)
    (let ([var (compile-expr var)])
      (format #f "for (int ~a = ~a; ~a < ~a ; ~a++){~a;~%~%~{~a;~%~}}~%"
              var (compile-expr start) var (compile-expr end) var
              (compile-expr body)
              (map compile-expr body-rest)))]
   [expr (syntax-error 'dotimes "Poorly formed:" expr)]))

(define glsl:while
  (match-lambda
   [(test body . body-rest)
    (format #f "while (~a){~%~a;~%~{~a;~%~}}~%"
            (compile-expr test) (compile-expr body) (map compile-expr body-rest))]
   [expr (syntax-error 'while "Poorly formed:" expr)]))

(define glsl:return
  (match-lambda
   [() "return"]
   [(ret) (format #f "return ~a" (compile-expr ret))]
   [args (syntax-error 'return "Can only return zero or one things:" args)]))

(define *special-functions*
  (alist->hash-table
   `((+ . ,(infix-fun-multiple-args '+))
     (- . ,(infix-fun-multiple-args '-))
     (* . ,(infix-fun-multiple-args '*))
     (/ . ,(infix-fun-multiple-args '/))
     (modulo . ,(infix-fun '%))
     (equal? . ,(infix-fun '==))
     (= . ,(infix-fun '==))
     (< . ,(infix-fun '<))
     (> . ,(infix-fun '>))
     (<= . ,(infix-fun '<=))
     (>= . ,(infix-fun '>=))
     (<< . ,(infix-fun '<<))
     (>> . ,(infix-fun '>>))
     (and . ,(infix-fun '&&))
     (or . ,(infix-fun '|\|\||))
     (not . ,(prefix-fun '!))
     (bitwise-and . ,(infix-fun '&))
     (bitwise-not . ,(prefix-fun '~))
     (bitwise-or . ,(infix-fun '|\||))
     (bitwise-xor . ,(infix-fun '^))
     (array-ref . ,glsl:array-ref)
     (length . ,glsl:length)
     (swizzle . ,glsl:swizzle)
     (~ . ,glsl:swizzle)
     (set! . ,glsl:assign)
     (if . ,glsl:if)
     (begin . ,glsl:begin)
     (cond . ,glsl:cond)
     (let . ,glsl:let)
     (define . ,glsl:define)
     (dotimes . ,glsl:dotimes)
     (while . ,glsl:while)
     (break . ,(lambda (a) "break"))
     (continue . ,(lambda (a) "continue"))
     (discard . ,(lambda (a) "discard"))
     (return . ,glsl:return))))

) ; end module
