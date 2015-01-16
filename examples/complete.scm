;;;; complete.scm

;;;; This example illustrates phong-shading in glls with a model
;;;; loaded by opengl-glew. The file "horse.ply.gz" must be in
;;;; the same directory as this program.

;;;; NOTE:
;;;; This uses glls-render, so if this file is compiled it must be linked with OpenGL
;;;; E.g.:
;;;; csc -lGL complete.scm

;;;; Use arrow keys to rotate, zoom camera.

(import chicken scheme)
(use glls-render gl-math gl-utils (prefix glfw3 glfw:) (prefix opengl-glew gl:) srfi-4)

;;; Matrices
(define projection-matrix (perspective 640 480 0.01 100 70))
(define view-matrix (make-parameter #f))
(define model-matrix (rotate-y (degrees->radians 90)
                               (rotate-x (degrees->radians -90)
                                         (mat4-identity #t))))
(define mvp (make-parameter (make-f32vector 16 #f #t)))
(define inverse-transpose-model
  (transpose (inverse model-matrix) #t))

;;; Camera movement
(define pan (make-parameter 0))
(define zoom (make-parameter 0))
(define angle (make-parameter 0))
(define distance (make-parameter 0.2))
(define camera-position (make-parameter (make-point 0 0 0 #t)))

(glfw:key-callback
 (lambda (window key scancode action mods)
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
     (zoom (sub1 (zoom)))])))

(define (update)
  (angle (+ (angle) (/ (pan) 30)))
  (if (positive? (+ (distance) (* (zoom) 0.005)))
      (distance (+ (distance) (* (zoom) 0.005))))
  (point-x-set! (camera-position) (* (distance) (sin (angle))))
  (point-z-set! (camera-position) (* (distance) (cos (angle))))
  (view-matrix (look-at (camera-position)
                        (make-point 0 0 0)
                        (make-point 0 1 0)))
   (mvp (m* projection-matrix
            (m* (view-matrix) model-matrix)
            (mvp))))

;;; Rendering
(define-pipeline phong-shader 
  ((#:vertex input: ((position #:vec3) (normal #:vec3))
             uniform: ((mvp #:mat4) (model #:mat4) (inv-transpose-model #:mat4))
             output: ((p #:vec3) (n #:vec3))
             version: 120)
   (define (main) #:void
     (set! gl:position (* mvp (vec4 position 1.0)))
     (set! p (vec3 (* model (vec4 position 1))))
     (set! n (- ; Normals facing in for this model
              (normalize (vec3 (* inv-transpose-model (vec4 normal 0))))))))
  ((#:fragment input: ((n #:vec3) (p #:vec3))
               uniform: ((camera-position #:vec3))
               version: 120)
   (let ((light-position #:vec3 (vec3 0 2 2))
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
              (diffuse-intensity #:float (max (dot to-light n) 0))
              (diffuse #:vec3 (* light-diffuse surface-diffuse
                                 diffuse-intensity))
              (specular-intensity
               #:float (if (> diffuse-intensity 0)
                           (max (dot (normalize (- camera-position p))
                                     (reflect (- to-light) n))
                                0)
                           0))
              (specular #:vec3 (* light-specular surface-specular
                                  (expt specular-intensity specular-exponent))))
         (set! gl:frag-color
           (vec4 (+ ambient-intensity diffuse specular)
                 (swizzle n x))))))))

(define horse-mesh (load-ply-mesh
                    "horse.ply.gz"
                    vertex: '((position
                               x y z)
                              (normal
                               nx ny nz))
                    face: 'vertex_indices))

;;; Initialize and main loop
(glfw:with-window (640 480 "Example" resizable: #f)
  (gl:init)
   (gl:enable gl:+depth-test+)
   (gl:depth-func gl:+less+)
   (compile-pipelines)
   (mesh-make-vao! horse-mesh (pipeline-mesh-attributes phong-shader))
   (map (lambda (s) (print (shader-source s))) (pipeline-shaders phong-shader))
   (let ((renderable (make-phong-shader-renderable
                      mesh: horse-mesh
                      mvp: (mvp)
                      model: model-matrix
                      camera-position: (camera-position)
                      inv-transpose-model: inverse-transpose-model)))
     (let loop ()
      (glfw:swap-buffers (glfw:window))
      (gl:clear (bitwise-ior gl:+color-buffer-bit+ gl:+depth-buffer-bit+))
      (update)
      (render-phong-shader renderable)
      (check-error)
      (glfw:poll-events)
      (unless (glfw:window-should-close (glfw:window))
        (loop)))))
