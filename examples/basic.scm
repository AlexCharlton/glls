(module basic-shader-example *

(import chicken scheme)

(use glls (prefix glfw3 glfw:) (prefix opengl-glew gl:))

(defpipeline foo 
  ((#:vertex) ((vertex #:vec2) (color #:vec3) #:uniform (mvp #:mat4))
     (define (main) #:void
       (set! gl:position (* mvp (vec4 vertex 0.0 1.0)))
       (set! c color))
     -> ((c #:vec3)))
  ((#:fragment) ((c #:vec3))
     (define (main) #:void
       (set! frag-color (vec4 c 1.0)))
     -> ((frag-color #:vec4))))

(defshader bar (#:vertex)
    ((vertex #:vec2) (color #:vec3) #:uniform (mvp #:mat4))
  (define (main) #:void
    (set! gl:position (* mvp (vec4 vertex 0.0 1.0)))
    (set! c color))
  -> ((c #:vec3)))

(defpipeline baz 
  `(,bar uniforms: ((mvp #:mat4)))
  (cadr (pipeline-shaders foo)))

(glfw:with-window (640 480 "Example" resizable: #f)
   (gl:init)
   (compile-pipelines)
   (print foo)
   (print baz))
)
