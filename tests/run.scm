(use test glsl-compiler)

(test-group "renaming"
  (test "gl_Position" (symbol->glsl 'gl:position))
  (test "floatBitsToUint" (symbol->glsl 'float-bits-to-uint)))

(test-group "expressions"
  (test "vec4(position, 0.0, 1.0)" (compile-expr '(vec4 position 0.0 1.0)))
  (test "(1 + 2)" (compile-expr '(+ 1 2)))
  (test "vec4(position, 0.0, (0.5 + x + y))"
        (compile-expr '(vec4 position 0.0 (+ 0.5 x y))))
  (test "position.xyz"
        (compile-expr '(swizzle position x y z)))
  (test "array.length()"
        (compile-expr '(length array)))
  (test "if ((i == 0)){\nfoo = 4;\nbar = 5;\n;} else {\nfoo = 4.0;}\n"
        (compile-expr '(if (= i 0) 
                           (begin (set! foo 4) (set! bar 5))
                           (set! foo 4.0))))
  (test "int foo;\nint bar = 4;\nvec4 quox[];\nvec4 baz[4];\nvec4 box[4] = vec4[](1, 3, 3, 4);\nif ((foo == 1)){\nfoo = 4;\n} ;\n"
        (compile-expr '(let ((foo #:int)
                             (bar #:int 4)
                             (quox (#:array #:vec4))
                             (baz (#:array #:vec4 4))
                             (box (#:array #:vec4 4) (1 3 3 4)))
                         (cond ((= foo 1) (set! foo 4))))))
  (test "if ((x < 0)){\ny = 1;\n} else if ((x < 5)){\ny = 2;\n} else {\ny = 3;\n}\n"
        (compile-expr '(cond
                        ((< x 0) (set! y 1))
                        ((< x 5) (set! y 2))
                        (else (set! y 3)))))
  (test "vec3 [4] foo(in int x, int y){\nx = y;\nreturn bar;\n}\n"
        (compile-expr '(define (foo (#:in x #:int) (y #:int)) (#:array #:vec3 4)
                         (set! x y)
                         (return bar))))
  (test "for (int i = 0; i < 5 ; i++){\nfoo(i);\n}\n"
        (compile-expr '(dotimes (i 5)
                                (foo i))))
  (test "while ((i < 4)){\nif (thing){\nbreak;}\n;\nfoo(i);\n}\n"
        (compile-expr '(while (< i 4)
                         (if thing (break))
                         (foo i))))
  (test "in vec3 foo;\nin vec3 bar;\nout mat4 quox;\nuniform int baz[];\n"
        (compile-inputs '((foo #:vec3) (bar #:vec3) #:uniform (baz (#:array #:int)))
                        '((quox #:mat4))))
  (test "#version 330\n\nin vec2 vertex;\nin vec3 color;\nout vec3 c;\nuniform mat4 viewMatrix;\n\nvoid  main(){\ngl_Position = (viewMatrix * vec4(vertex, 0.0, 1.0));\nc = color;\n}\n\n"
        (compile-shader
         '(#:vertex ((vertex #:vec2) (color #:vec3) #:uniform (view-matrix #:mat4))
                    (define (main) #:void
                      (set! gl:position (* view-matrix (vec4 vertex 0.0 1.0)))
                      (set! c color))
                    -> ((c #:vec3)))))
  (test-error (compile-expr '(let ((foo (#:array #:int 4) (1 2))))))
  (test-error (compile-expr '(+ 0.5))))

(test-exit)
