;;;; particles.scm

;;;; This example illustrates 2D particle affect without the need for
;;;; mvp matrices, and manipulates particle vertex data at runtime.

;;;; NOTE:
;;;; This uses glls-render, so if this file is compiled it must be linked with OpenGL
;;;; E.g.:
;;;; csc -lGL complete.scm

(import chicken scheme)

;; note we don't need gl-math in this example
(use glls-render (prefix glfw3 glfw:) (prefix opengl-glew gl:) gl-utils
     srfi-18 miscmacros)

;; a shader that draws points as blurry circles. this form be
;; re-evaluated at runtime (and its shaders immediately take effect).
(define-pipeline point-shader
    ((#:vertex version: 130
               input: ((position #:vec2)))
     (define (main) #:void
       (set! gl:position (vec4 position 0.0 1.0))
       (set! gl:point-size 50.0)))
    ((#:fragment version: 130
                 output: ((frag-color #:vec4)))
     (define (main) #:void
       (define r #:vec2 (- gl:point-coord (vec2 0.5)))
       (define c #:float (- 1 (* 2 (sqrt (dot r r)))))
       (set! frag-color (vec4 1 1 0 c)))))

(define numpoints 1000)

(define point-mesh
  (make-mesh vertices: `(attributes:
                         ((position #:float 2))
                         initial-elements:
                         ((position . , (make-list (* 2 numpoints) 0))))
             indices: `(type: #:ushort initial-elements: , (iota numpoints ))
             mode: #:points))

;; you can change the movement speed here and redefine this top-level
;; function. less impressive though than redefining the shader live, I
;; know.
(define (move-points-randomly)
  ;; at the end of with-mesh, all our vertices should be copied over
  ;; on to the GPU (#:stream).
  (with-mesh point-mesh
             (lambda ()
               (dotimes (it (mesh-n-vertices point-mesh))
                        (let ((p (mesh-vertex-ref point-mesh 'position it)))
                          (f32vector-set! p 0 (+ (f32vector-ref p 0)
                                                 (/ (- (random 1000) 500) 200000)))
                          (f32vector-set! p 1 (+ (f32vector-ref p 1)
                                                 (/ (- (random 1000) 500) 200000)))
                          (mesh-vertex-set! point-mesh 'position it p))))))


(define renderable #f)

(define (render)
  ;; trying to be clever about how we initialize our point-mesh,
  ;; allowing redefining point-mesh above seamlessly
  (unless (mesh-vao point-mesh)
    (mesh-make-vao! point-mesh (pipeline-mesh-attributes point-shader) #:stream)
    (set! renderable (make-point-shader-renderable mesh: point-mesh)))

  (move-points-randomly)
  (render-point-shader renderable))

;;; Initialization and main loop
;;; Run in a thread so that you can still use the REPL
(thread-start!
 (lambda ()
   (glfw:with-window (640 480 "Example" resizable: #f)
     (gl:init)

     (gl:enable gl:+vertex-program-point-size+)
     (gl:enable gl:+blend+)
     (gl:blend-func gl:+src-alpha+ gl:+one-minus-src-alpha+)
     (compile-pipelines)

     (let loop ()
       (glfw:swap-buffers (glfw:window))
       (gl:clear (bitwise-ior gl:+color-buffer-bit+ gl:+depth-buffer-bit+))
       (render)
       (thread-yield!) ; Let the main thread eval stuff
       (glfw:poll-events)
       (unless (glfw:window-should-close? (glfw:window))
         (loop))))))


