(import chicken scheme)

(use glsl (prefix glfw3 glfw:) (prefix opengl-glew gl:))

(defpipeline foo 
  (#:vertex ((vertex #:vec2) (color #:vec3) #:uniform (view-matrix #:mat4))
     (define (main) #:void
       (set! gl:position (* view-matrix (vec4 vertex 0.0 1.0)))
       (set! c color))
     -> ((c #:vec3)))
  (#:fragment ((c #:vec3))
     (define (main) #:void
       (set! frag-color (vec4 c 1.0)))
     -> ((frag-color #:vec4))))

(defshader bar #:vertex
    ((vertex #:vec2) (color #:vec3) #:uniform (view-matrix #:mat4))
  (define (main) #:void
    (set! gl:position (* view-matrix (vec4 vertex 0.0 1.0)))
    (set! c color))
  -> ((c #:vec3)))

(defpipeline baz 
  bar
  (cadr (pipeline-shaders foo)))

(glfw:with-window (640 480 "Example" resizable: #f)
   (compile-pipeline foo)
   (compile-pipeline baz)
   (print foo)
   (print baz))
