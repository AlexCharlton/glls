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
(define rect (make-mesh vertices: '(attributes: ((position #:float 2))
                                    initial-elements: ((position . (-1 -1
                                                                     1 -1
                                                                     1  1
                                                                    -1  1))))
                        indices: '(type: #:ushort
                                   initial-elements: (0 1 2
                                                      0 2 3))))
;;; Matrices
(define projection-matrix
  (perspective 640 480 0.1 100 70))

(define view-matrix
  (look-at (make-point 1 0 3)
           (make-point 0 0 0)
           (make-point 0 1 0)))

(define model-matrix (mat4-identity))

(define mvp (m* projection-matrix
                (m* view-matrix model-matrix)
                #t ; Matrix should be in a non-GC'd area
                ))

(define-shader shift
  (#:fragment export: (shift))
  (define (shift (x #:float)) #:float
    (+ (* x 0.5)
       0.5)))

(define-shader colorer
    (#:fragment use: (shift) export: (colors))
  (define (colors (position #:vec2)) #:vec4
    (vec4 (shift (.. position x))
          (shift (.. position y))
          (shift (- (.. position x)
                    (.. position y)))
          1)))

;;; Pipeline definition
(define-pipeline simple-shader
  ((#:vertex input: ((position #:vec2))
             uniform: ((mvp #:mat4))
             output: ((pos #:vec2)))
   (define (main) #:void
     (set! gl:position (* mvp (vec4 position 0.0 1.0)))
     (set! pos position)))
  ((#:fragment input: ((pos #:vec2))
               output: ((frag-color #:vec4))
               use: (colorer))
   (define (main) #:void
     (set! frag-color (colors pos)))))

;;; Initialization and main loop
(glfw:with-window (640 480 "Example" resizable: #f)
  (gl:init)
  (compile-pipelines)
  (mesh-attribute-locations-set! rect (pipeline-mesh-attributes simple-shader))
  (mesh-make-vao rect)
  (let* ((renderable (make-simple-shader-renderable mesh: rect
                                                    mvp: mvp)))
    (let loop ()
      (glfw:swap-buffers (glfw:window))
      (gl:clear (bitwise-ior gl:+color-buffer-bit+ gl:+depth-buffer-bit+))
      (render-simple-shader renderable)
      (check-error)
      (glfw:poll-events)
      (unless (glfw:window-should-close (glfw:window))
        (loop)))))

) ; end module
