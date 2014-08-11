;;;; exports.scm

;;;; This example illustrates the ability of shaders to export functions.
;;;; Note that shaders that are indirectly referenced will still be included in a pipeline
;;;; E.g.: In the example below, 'shift' is not 'use'd by 'simple-shader', but still gets compiled into the pipeline because it is 'use'd by 'colorer'

;;;; NOTE:
;;;; This uses glls-render, so if this file is compiled it must be linked with OpenGL
;;;; E.g.:
;;;; csc -lGL texture.scm

(module shader-export-example *

(import chicken scheme)
(use glls-render (prefix glfw3 glfw:) (prefix opengl-glew gl:) gl-math gl-utils)

;;; VAO data
(define vertex-data (f32vector -1 -1
                               1 -1
                               1 1
                               -1 1))

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
                (m* view-matrix model-matrix)
                #t ; Matrix should be in a non-GC'd area
                ))

(define-shader shift
  (#:fragment export: (shift)) ()
  (define (shift (x #:float)) #:float
    (+ (* x 0.5)
       0.5))
  -> ())

(define-shader colorer
    (#:fragment use: (shift) export: (colors)) ()
  (define (colors (position #:vec2)) #:vec4
    (vec4 (shift (.. position x))
          (shift (.. position y))
          (shift (- (.. position x)
                    (.. position y)))
          1))
  -> ())

;;; Pipeline definition
(define-pipeline simple-shader
  ((#:vertex) ((vertex #:vec2) #:uniform (mvp #:mat4))
     (define (main) #:void
       (set! gl:position (* mvp (vec4 vertex 0.0 1.0)))
       (set! pos vertex))
     -> ((pos #:vec2)))
  ((#:fragment use: (colorer)) ((pos #:vec2))
     (define (main) #:void
       (set! frag-color (colors pos)))
     -> ((frag-color #:vec4))))

;;; Initialization and main loop
(glfw:with-window (640 480 "Example" resizable: #f)
  (gl:init)
  (compile-pipelines)
  (let* ((vao (make-vao vertex-data index-data
                        `((,(pipeline-attribute 'vertex simple-shader) float: 2))))
         (renderable (make-simple-shader-renderable
                      n-elements: (u16vector-length index-data)
                      element-type: (type->gl-type ushort:)
                      vao: vao
                      mvp: mvp)))
    (let loop ()
      (glfw:swap-buffers (glfw:window))
      (gl:clear (bitwise-ior gl:+color-buffer-bit+ gl:+depth-buffer-bit+))
      (render-simple-shader renderable)
      (gl:check-error)
      (glfw:poll-events)
      (unless (glfw:window-should-close (glfw:window))
        (loop)))))

) ; end module
