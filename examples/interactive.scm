;;;; interactive.scm

;;;; An example of an interactive glls environment
;;;; Run with csi

(import chicken scheme)
(use glls-render (prefix glfw3 glfw:) (prefix opengl-glew gl:) gl-math gl-utils
     srfi-18)

;;; VAO data
(define rect (make-mesh vertices: '(attributes: ((position #:float 2)
                                                 (color #:unsigned-byte 3
                                                        normalized: #t))
                                    initial-elements: ((position . (-1 -1
                                                                     1 -1
                                                                     1  1
                                                                    -1  1))
                                                       (color . (1 0 0
                                                                 0 1 0
                                                                 0 0 1
                                                                 1 0 1))))
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


;;; Pipeline definition
;; Change me, then re-evaluate this form!
;; If you change attributes and uniforms, though, you'll need to create a new renderable.
(define-pipeline simple-shader
  ((#:vertex input: ((position #:vec2) (color #:vec3))
             uniform: ((mvp #:mat4))
             output: ((c #:vec3)))
   (define (main) #:void
     (set! gl:position (* mvp (vec4 position 0.0 1.0)))
     (set! c color)))
  ((#:fragment input: ((c #:vec3))
               output: ((frag-color #:vec4))) 
   (define (main) #:void
     (set! frag-color (vec4 c 1.0)))))

;;; Initialization and main loop
;;; Run in a thread so that you can still use the REPL
(thread-start!
 (lambda ()
   (glfw:with-window (640 480 "Example" resizable: #f)
     (gl:init)
     (compile-pipelines)
     (mesh-attribute-locations-set! rect (pipeline-mesh-attributes simple-shader))
     (mesh-make-vao rect)
     (let ((renderable (make-simple-shader-renderable mesh: rect
                                                      mvp: mvp)))
       (let loop ()
         (glfw:swap-buffers (glfw:window))
         (gl:clear (bitwise-ior gl:+color-buffer-bit+ gl:+depth-buffer-bit+))
         (render-simple-shader renderable)
         (thread-yield!) ; Let the main thread eval stuff
         (glfw:poll-events)
         (unless (glfw:window-should-close (glfw:window))
           (loop)))))))
