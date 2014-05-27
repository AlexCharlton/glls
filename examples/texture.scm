;;;; texture.scm

;;;;
;;;; NOTE:
;;;; This uses glls-render, so if this file is compiled it must be linked with OpenGL
;;;; E.g.:
;;;; csc -lGL simple.scm

(module texture-glls-example *

(import chicken scheme srfi-4)
(use glls-render (prefix glfw3 glfw:) (prefix opengl-glew gl:) gl-math gl-utils soil)

;;; VAO data
(define vertex-data (f32vector -1 -1  0  1
                                1 -1  1  1
                                1  1  1  0
                               -1  1  0  0))

(define index-data (u16vector 0 1 2
                              0 2 3))

(define vao (make-parameter #f))

;;; Matrices
(define projection-matrix
  (perspective 640 480 0.1 100 70))

(define view-matrix
  (look-at 1 0 3
           0 0 0
           0 1 0))

(define model-matrix (mat4-identity))

(define mvp (m* projection-matrix
                (m* view-matrix model-matrix)))


;;; Pipeline definition
(define-pipeline sprite-shader
  ((#:vertex) ((vertex #:vec2) (tex-coord #:vec2) #:uniform (mvp #:mat4))
     (define (main) #:void
       (set! gl:position (* mvp (vec4 vertex 0.0 1.0)))
       (set! tex-c tex-coord))
     -> ((tex-c #:vec2)))
  ((#:fragment) ((tex-c #:vec2) #:uniform (tex #:sampler-2d))
     (define (main) #:void
       (set! frag-color (texture tex tex-c)))
     -> ((frag-color #:vec4))))

(define renderable (make-parameter #f))

;;; Initialization and main loop
(glfw:with-window (640 480 "Example" resizable: #f)
  (gl:init)
  (compile-pipelines)
  (let ([vao (make-vao (f32vector->blob vertex-data) (u16vector->blob index-data)
                       `((,(pipeline-attribute 'vertex sprite-shader) float: 2)
                         (,(pipeline-attribute 'tex-coord sprite-shader) float: 2)))]
        [texture (load-ogl-texture "img_test.png" 0 0 0)])
    (renderable (make-sprite-shader-renderable
                 n-elements: (u16vector-length index-data)
                 element-type: (type->gl-type ushort:)
                 vao: vao
                 tex: texture
                 mvp: mvp)))
  (let loop ()
     (glfw:swap-buffers (glfw:window))
     (gl:clear (bitwise-ior gl:+color-buffer-bit+ gl:+depth-buffer-bit+))
     (render-sprite-shader (renderable))
     (glfw:poll-events)
     (unless (glfw:window-should-close (glfw:window))
       (loop))))

) ; end module
