;;;; complete.scm

;;;; This example illustrates phong-shading in glls with a model
;;;; loaded by opengl-glew. The file "horse.ply.gz" must be in
;;;; the same directory as this program.

;;;; Due to the use of 'define-external', this file must be compiled.
;;;; This can be done by:
;;;;     csc complete.scm

(import chicken scheme)
(use glls gl-math gl-utils (prefix glfw3 glfw:) (prefix opengl-glew gl:))

;;; Matrices
(define projection-matrix (perspective 640 480 0.01 100 70))
(define view-matrix (make-parameter #f))
(define model-matrix (rotate-y (degrees->radians 90)
                               (rotate-x (degrees->radians -90)
                                         (mat4-identity))))
(define inverse-transpose-model
  (inverse (transpose model-matrix)))

;;; Camera movement
(define pan (make-parameter 0))
(define zoom (make-parameter 0))
(define angle (make-parameter 0))
(define distance (make-parameter 0.2))
(define camera-position (make-parameter (make-f32vector 3 0)))

(define-external (keyCallback (c-pointer window)
                              (int key) (int scancode) (int action) (int mods))
    void
  (cond
   [(and (eq? key glfw:+key-escape+) (eq? action glfw:+press+))
    (glfw:set-window-should-close window 1)]
   [(and (eq? key glfw:+key-left+) (eq? action glfw:+press+))
    (pan (sub1 (pan)))]
   [(and (eq? key glfw:+key-right+) (eq? action glfw:+press+))
    (pan (add1 (pan)))]
   [(and (eq? key glfw:+key-left+) (eq? action glfw:+release+))
    (pan (add1 (pan)))]
   [(and (eq? key glfw:+key-right+) (eq? action glfw:+release+))
    (pan (sub1 (pan)))]
   [(and (eq? key glfw:+key-up+) (eq? action glfw:+press+))
    (zoom (sub1 (zoom)))]
   [(and (eq? key glfw:+key-down+) (eq? action glfw:+press+))
    (zoom (add1 (zoom)))]
   [(and (eq? key glfw:+key-up+) (eq? action glfw:+release+))
    (zoom (add1 (zoom)))]
   [(and (eq? key glfw:+key-down+) (eq? action glfw:+release+))
    (zoom (sub1 (zoom)))]))

(define (update)
  (angle (+ (angle) (/ (pan) 30)))
  (if (positive? (+ (distance) (* (zoom) 0.005)))
      (distance (+ (distance) (* (zoom) 0.005))))
   (let ([camera-x (* (distance) (sin (angle)))]
         [camera-z (* (distance) (cos (angle)))])
     (f32vector-set! (camera-position) 0 camera-x)
     (f32vector-set! (camera-position) 2 camera-z)
     (view-matrix (look-at camera-x 0 camera-z
                           0 0 0
                           0 1 0))))

;;; Rendering
(defpipeline phong-shader 
  ((#:vertex) ((vertex #:vec3) (normal #:vec3)
               #:uniform (mvp #:mat4) (model #:mat4) (inv-transpose-model #:mat4))
     (define (main) #:void
       (set! gl:position (* mvp (vec4 vertex 1.0)))
       (set! p (vec3 (* model (vec4 vertex 1))))
       (set! n (- ; Normals facing in for this model
                (normalize (vec3 (* inv-transpose-model (vec4 normal 0)))))))
     -> ((p #:vec3) (n #:vec3)))
  ((#:fragment) ((n #:vec3) (p #:vec3) #:uniform (camera-position #:vec3))
   (let ((light-position #:vec3 (vec3 0 0 2))
         (light-diffuse #:vec3 (vec3 0.7 0.7 0.7))
         (light-specular #:vec3 (vec3 1 1 1))
         (ambient #:vec3 (vec3 0.2 0.2 0.2))
         (surface-ambient #:vec3 (vec3 0.2 0.1 0))
         (surface-diffuse #:vec3 (vec3 0.2 0.1 0.04))
         (surface-specular #:vec3 (vec3 0.4 0.4 0.4))
         (specular-exponent #:float 100.0))
     (define (main) #:void
       (let* ((ambient-intensity #:vec3 (* ambient surface-ambient))
              (to-light #:vec3 (normalize (- light-position p)))
              (diffuse-intensity #:vec3 (* light-diffuse surface-diffuse
                                           (max (dot to-light n) 0)))
              (spec #:float (max (dot (reflect (- to-light) n)
                                      (normalize (- camera-position p)))
                                 0))
              (specular-intensity #:vec3 (* light-specular surface-specular
                                            (expt spec specular-exponent))))
         (set! frag-color
               (vec4 (+ ambient-intensity diffuse-intensity specular-intensity)
                     (swizzle n x))))))
   -> ((frag-color #:vec4))))

(define vao (make-parameter #f))

(define (render)
  (gl:use-program (pipeline-program phong-shader))
  (gl:uniform-matrix4fv (pipeline-uniform 'mvp phong-shader) 1 #f
                        (m* projection-matrix
                            (m* (view-matrix) model-matrix)))
  (gl:uniform-matrix4fv (pipeline-uniform 'model phong-shader) 1 #f
                        model-matrix)
  (gl:uniform-matrix4fv (pipeline-uniform 'inv-transpose-model phong-shader) 1 #f
                        inverse-transpose-model)
  (gl:uniform3fv (pipeline-uniform 'camera-position phong-shader) 1
                  (camera-position))
  (gl:bind-vertex-array (vao))
  (gl:draw-elements-base-vertex gl:+triangles+ 290898 (type->gl-type uint:) #f 0)

  (gl:check-error)
  (gl:bind-vertex-array 0))

;;; Initialize and main loop
(glfw:with-window (640 480 "Example" resizable: #f)
   (glfw:set-key-callback (glfw:window) #$keyCallback)
   (gl:enable gl:+depth-test+)
   (gl:depth-func gl:+less+)
   (compile-pipelines)
   (map (lambda (s) (print (shader-source s))) (pipeline-shaders phong-shader))
   (vao (load-ply-vao "horse.ply.gz"
                  vertex: `((,(pipeline-attribute 'vertex phong-shader) x y z)
                            (,(pipeline-attribute 'normal phong-shader) nx ny nz))
                  face: 'vertex_indices))
   (let loop ()
     (glfw:swap-buffers (glfw:window))
     (gl:clear (bitwise-ior gl:+color-buffer-bit+ gl:+depth-buffer-bit+))
     (update)
     (render)
     (glfw:poll-events)
     (unless (glfw:window-should-close (glfw:window))
       (loop))))
