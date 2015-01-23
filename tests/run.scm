(use test glls-compiler glls)

(test-group "renaming"
  (test 'gl_Position (symbol->glsl 'gl:position))
  (test 'floatBitsToUint (symbol->glsl 'float-bits-to-uint))
  (test 'shadow2DProjLod (symbol->glsl 'shadow-2d-proj-lod))
  (test 'sampler2DMSArray (symbol->glsl 'sampler-2d-ms-array))
  (test 'EmitVertex (symbol->glsl 'emit-vertex))
  (test 'EndPrimitive (symbol->glsl 'end-primitive)))

(test-group "expressions"
  (test "vec4(position, 0.0, 1.0);\n" (compile-expr '(vec4 position 0.0 1.0)))
  (test "1 + 2;\n" (compile-expr '(+ 1 2)))
  (test "pow(a, 2);\n"
        (compile-expr '(expt a 2)))
  (test "vec4(position, 0.0, (0.5 + x + y));\n"
        (compile-expr '(vec4 position 0.0 (+ 0.5 x y))))
  (test "position.xyz;\n"
        (compile-expr '(swizzle position x y z)))
  (test "array.length();\n"
        (compile-expr '(length array)))
  (test "if (i == 0) {\n    foo = 4;\n    bar = 5;\n} else {\n    foo = 4.0;\n}\n"
        (compile-expr '(if (= i 0) 
                           (begin (set! foo 4) (set! bar 5))
                           (set! foo 4.0))))
  (test "int foo = 1;\nfoo;\n"
        (compile-expr '(let ((foo #:int 1))
                         foo)))
  (test "int foo;\nint bar = 4;\nvec4 quox[];\nvec4 baz[4];\nvec4 box[4] = 1(3, 3, 4);\nif (foo == 1) {\n    foo = 4;\n}\n"
        (compile-expr '(let ((foo #:int)
                             (bar #:int 4)
                             (quox (#:array #:vec4))
                             (baz (#:array #:vec4 4))
                             (box (#:array #:vec4 4) (1 3 3 4)))
                         (cond ((= foo 1) (set! foo 4))))))
  (test "if (x < 0) {\n    y = 1;\n} else if (x < 5) {\n    x = 1;\n    y = 2;\n} else {\n    y = 3;\n}\n"
        (compile-expr '(cond
                        ((< x 0) (set! y 1))
                        ((< x 5)
                         (set! x 1)
                         (set! y 2))
                        (else (set! y 3)))))
  (test "vec3 foo (in int x, int y) {\n    x = y;\n    return bar;\n}\n"
        (compile-expr '(define (foo (x (in: #:int)) (y #:int)) #:vec3
                  (set! x y)
                  bar)))
  (test "for (int i = 0; i < 5; ++i) {\n    foo(i);\n}\n"
        (compile-expr '(do-times (i 5)
                                (foo i))))
  (test "while (i > 4) {\n    if (thing) {\n        break;\n    }\n    foo(i);\n}\n"
        (compile-expr '(while (> i 4)
                         (if thing (break))
                         (foo i))))
  
  (test-error (compile-expr '(let ((foo (#:array #:int 4) (1 2))))))

  (test "a[1];\n"
        (compile-expr '(array-ref a 1)))
  (test "a[1] = x;\n"
        (compile-expr '(array-set! a 1 x)))
  (test "vec4(position, 0.0, 1.0);\n"
        (compile-expr '(vec4 position 0.0 1.0)))
  (test "for (int i = 2; i < 4; ++i) {\n    break;\n}\n"
        (compile-expr '(do-times (i 2 4) (break))))
  (test "for (i = 0; i < 4; ++i) {\n    break;\n}\n"
        (compile-expr '(for (set! i 0) (< i 4) (++ i) (break))))
  (test "struct foo {\n    int x;\n    int y[];\n};\n"
        (compile-expr '(define-record foo (x int:) (y (array: int:)))))
  (test "int x = 4;\nint y;\ny = 1;\nx + y;\n"
        (compile-expr '(let ((x int 4) (y int)) (set! y 1) (+ x y))))
  (test "int x = 4;\nx + 1;\n"
        (compile-expr '(let ((x int 4)) (+ x 1))))
  (test "int x;\nint y[];\nx + y;\n"
        (compile-expr '(let ((x int) (y (array: int))) (+ x y))))
  (test "int foo (int x, int y) {\n    return x + y;\n}\n"
        (compile-expr '(define (foo (x int) (y int)) int (+ x y))))
  (test "int foo () {\n    return 5;\n}\n"
        (compile-expr '(define (foo) int 5)))
  (test "int foo (int x) {\n    return x;\n}\n"
        (compile-expr '(define (foo (x int)) int x)))
  (test "int foo[5] = int[](1, bar(1), 3, 4, 5);\n"
        (compile-expr '(define foo (array: int 5) #(1 (bar 1) 3 4 5))))
  (test "#define foo 1\n"
        (compile-expr '(%define foo 1)))
  (test "#pragma STDGL invariant(all)\n"
        (compile-expr '(%pragma #:stdgl (invariant all))))
  (test "#extension GL_ARB_arrays_of_arrays : enable\n"
        (compile-expr '(%extension GL_ARB_arrays_of_arrays enable)))
    ); end test-group "expressions"

(test-group "shaders"
  (test  "#version 410\n\nin vec2 vertex;\nin vec3 color;\nout vec3 c;\nuniform mat4 viewMatrix;\nvoid main () {\n    gl_Position = viewMatrix * vec4(vertex, 0.0, 1.0);\n    c = color;\n}\n"
         (compile-glls
          '((#:vertex input: ((vertex #:vec2) (color #:vec3))
                      uniform: ((view-matrix #:mat4))
                      output: ((c #:vec3))
                      version: 410)
            (define (main) #:void
              (set! gl:position (* view-matrix (vec4 vertex 0.0 1.0)))
              (set! c color)))))
  (test  "#version 130\n\nattribute vec2 vertex;\nattribute vec3 color;\nvarying vec3 c;\nuniform mat4 viewMatrix;\nvoid main () {\n    gl_Position = viewMatrix * vec4(vertex, 0.0, 1.0);\n    c = color;\n}\n"
         (compile-glls
          '((#:vertex input: ((vertex #:vec2) (color #:vec3))
                      uniform: ((view-matrix #:mat4))
                      output: ((c #:vec3))
                      version: 130)
            (define (main) #:void
              (set! gl:position (* view-matrix (vec4 vertex 0.0 1.0)))
              (set! c color)))))
  (test "#version 130\n\nvarying vec3 c;\nvoid main () {\n    gl_FragColor = vec4(c, 1.0);\n}\n"
        (compile-glls '((#:fragment input: ((c #:vec3))
                                    #:version 130) 
                        (define (main) #:void
                          (set! gl:frag-color (vec4 c 1.0))))))
  ) ; end test-group shaders

(define-shader foo
    (#:vertex export: (quox))
  (define (quox (x #:int)) #:float
    (* x 2.0)))

(define-shader bar
    (#:vertex use: (foo))
  (define (main) #:void
    (quox 1)))

(test-group "defines"
   (test  "#version 330\n\nfloat quox (int x);\n\nvoid main () {\n    quox(1);\n}\n"
         (shader-source bar)))

(test-exit)
