(module glls-renderable (render-functions
                         make-renderable
                         render-renderable
                         set-renderable-vao!
                         set-renderable-n-elements!
                         set-renderable-element-type!
                         set-renderable-mode!
                         set-renderable-offset!
                         set-renderable-uniform-value!
                         gllsRender.h
                         renderable-size)

(import glls chicken scheme foreign lolevel foreign irregex srfi-1 srfi-4 extras)
(use glls-compiler fmt fmt-c miscmacros (prefix opengl-glew gl:)
     (prefix gl-utils-core gl:) (prefix gl-utils-mesh gl:))

(foreign-declare "#include <math.h>")
(foreign-declare "#include \"gllsRender.h\"")

(define set-renderable-program!
  (foreign-lambda* void ((c-pointer data) (unsigned-int program))
    "GLLSrenderable2 *renderable = (GLLSrenderable2 *) data;
     renderable->program = program;"))

(define set-renderable-vao!
  (foreign-lambda* void ((c-pointer data) (unsigned-int vao))
    "GLLSrenderable2 *renderable = (GLLSrenderable2 *) data;
     renderable->vao = vao;"))

(define set-renderable-n-elements!
  (foreign-lambda* void ((c-pointer data) (unsigned-int n))
    "GLLSrenderable2 *renderable = (GLLSrenderable2 *) data;
     renderable->nElements = n;"))

(define set-renderable-element-type!
  (foreign-lambda* void ((c-pointer data) (unsigned-int type))
    "GLLSrenderable2 *renderable = (GLLSrenderable2 *) data;
     renderable->elementType = type;"))

(define set-renderable-mode!
  (foreign-lambda* void ((c-pointer data) (unsigned-int mode))
    "GLLSrenderable2 *renderable = (GLLSrenderable2 *) data;
     renderable->mode = mode;"))

(define set-renderable-offset!
  (foreign-lambda* void ((c-pointer data) (c-pointer offset))
    "GLLSrenderable2 *renderable = (GLLSrenderable2 *) data;
     renderable->offset = offset;"))

(define (set-renderable-uniform-value! data i value)
  (cond
   ((f32vector? value)
    ((foreign-lambda* void
         ((c-pointer data) (int i) (f32vector value))
       "GLLSrenderable1024 *renderable = (GLLSrenderable1024 *) data;
        renderable->uniformValues[i] = value;") data i value))
   ((s32vector? value)
    ((foreign-lambda* void
         ((c-pointer data) (int i) (s32vector value))
       "GLLSrenderable1024 *renderable = (GLLSrenderable1024 *) data;
        renderable->uniformValues[i] = value;") data i value))
   ((u32vector? value)
    ((foreign-lambda* void
         ((c-pointer data) (int i) (u32vector value))
       "GLLSrenderable1024 *renderable = (GLLSrenderable1024 *) data;
        renderable->uniformValues[i] = value;") data i value))
   ((pointer? value)
    ((foreign-lambda* void
         ((c-pointer data) (int i) (c-pointer value))
       "GLLSrenderable1024 *renderable = (GLLSrenderable1024 *) data;
        renderable->uniformValues[i] = value;") data i value))
   ((fixnum? value)
    ((foreign-lambda* void
         ((c-pointer data) (int i) (int value))
       "GLLSrenderable1024 *renderable = (GLLSrenderable1024 *) data;
       renderable->uniformValues[i] = (void *) value;") data i value))
   (else (error 'set-renderable-uniform-value! "Invalid type" value))))

(define set-renderable-uniform-location!
  (foreign-lambda* void ((c-pointer data) (int size) (int i) (int location))
    "GLLSrenderable2 *renderable = (GLLSrenderable2 *) data;
     void *values = &renderable->uniformValues[0];
     int *locations  = (int *) (values + (size * sizeof(void*)));
     locations[i] = location;"))

(define get-renderable-program
  (foreign-lambda* unsigned-int ((c-pointer data))
    "GLLSrenderable2 *renderable = (GLLSrenderable2 *) data;
     C_return(renderable->program);"))

(define get-renderable-vao
  (foreign-lambda* unsigned-int ((c-pointer data))
    "GLLSrenderable2 *renderable = (GLLSrenderable2 *) data;
     C_return(renderable->vao);"))

(define get-renderable-n-elements
  (foreign-lambda* unsigned-int ((c-pointer data))
    "GLLSrenderable2 *renderable = (GLLSrenderable2 *) data;
     C_return(renderable->nElements);"))

(define get-renderable-element-type
  (foreign-lambda* unsigned-int ((c-pointer data))
    "GLLSrenderable2 *renderable = (GLLSrenderable2 *) data;
     C_return(renderable->elementType);"))

(define get-renderable-mode
  (foreign-lambda* unsigned-int ((c-pointer data))
    "GLLSrenderable2 *renderable = (GLLSrenderable2 *) data;
     C_return(renderable->mode);"))

(define get-renderable-offset
  (foreign-lambda* c-pointer ((c-pointer data))
    "GLLSrenderable2 *renderable = (GLLSrenderable2 *) data;
     C_return(renderable->offset);"))

(define get-renderable-uniform-value
  (foreign-lambda* c-pointer ((c-pointer data) (unsigned-int i))
    "GLLSrenderable2 *renderable = (GLLSrenderable1024 *) data;
     C_return(renderable->uniformValues[i]);"))

(define get-renderable-uniform-location
  (foreign-lambda* int ((c-pointer data) (int size) (int i))
    "GLLSrenderable2 *renderable = (GLLSrenderable2 *) data;
     void *values = &renderable->uniformValues[0];
     int *locations  = (int *) (values + (size * sizeof(void*)));
     C_return(locations[i]);"))

(define (renderable-size pipeline)
  ((foreign-lambda* unsigned-int ((int n))
     "int i = pow(2, ceil(log(n)/log(2)));
     if (i <= 2) C_return(sizeof(GLLSrenderable2));
     else if (i <= 4) C_return(sizeof(GLLSrenderable4));
     else if (i <= 8) C_return(sizeof(GLLSrenderable8));
     else if (i <= 16) C_return(sizeof(GLLSrenderable16));
     else if (i <= 32) C_return(sizeof(GLLSrenderable32));
     else if (i <= 64) C_return(sizeof(GLLSrenderable64));
     else if (i <= 128) C_return(sizeof(GLLSrenderable128));
     else if (i <= 256) C_return(sizeof(GLLSrenderable256));
     else if (i <= 1024) C_return(sizeof(GLLSrenderable1024));
     fprintf(stderr, \"Error GLLSrenerables cannot hold this many uniforms: ~d\\n\", i);
     C_return(0);")
   (length (pipeline-uniforms pipeline))))

(define (allocate-renderable pipeline)
  (allocate (renderable-size pipeline)))

(define (symbol->c-symbol sym)
  (define (cammel-case str)
    (irregex-replace/all "[:-](.)" str
                         (lambda (m)
                           (let* ((s (irregex-match-substring m))
                                  (char1 (string-ref s 0))
                                  (char2 (char-upcase (string-ref s 1))))
                             (if (equal? char1 #\:)
                                 (string #\_ char2)
                                 (string char2))))))
  (define (strip-illegal-chars str)
    (irregex-replace/all '(: bos num)
                         (irregex-replace/all '(~ (or alphanum "_")) str "")
                         ""))
  (let ((str (symbol->string sym)))
    (string->symbol (strip-illegal-chars (cammel-case str)))))

(define-syntax def-glls-render
  (ir-macro-transformer
   (lambda (e r c)
     (let ((glls-render (with-input-from-file "gllsRender.h"
                          (lambda ()
                            (read-string)))))
       `(define gllsRender.h ,glls-render)))))

(def-glls-render)

(define-syntax define-uniform
  (ir-macro-transformer
   (lambda (e r c)
     (let ((name (strip-syntax (cadr e))))
       `(define ,name
          (foreign-lambda* void
              ((unsigned-int loc) (unsigned-int count) (c-pointer value))
            ,(string-append (symbol->string name)
                            "(loc, count, value);")))))))

(define-syntax define-uniform-matrix
  (ir-macro-transformer
   (lambda (e r c)
     (let ((name (strip-syntax (cadr e))))
       `(define ,name
          (foreign-lambda* void
              ((unsigned-int loc) (unsigned-int count) (c-pointer value))
            ,(string-append (symbol->string name)
                            "(loc, count, 0, value);")))))))

(define-uniform glUniform1fv)
(define-uniform glUniform1iv)
(define-uniform glUniform1uiv)
(define-uniform glUniform2fv)
(define-uniform glUniform2iv)
(define-uniform glUniform2uiv)
(define-uniform glUniform3fv)
(define-uniform glUniform3iv)
(define-uniform glUniform3uiv)
(define-uniform glUniform4fv)
(define-uniform glUniform4iv)
(define-uniform glUniform4uiv)
(define-uniform-matrix glUniformMatrix2fv)
(define-uniform-matrix glUniformMatrix3fv)
(define-uniform-matrix glUniformMatrix4fv)
(define-uniform-matrix glUniformMatrix2x3fv)
(define-uniform-matrix glUniformMatrix3x2fv)
(define-uniform-matrix glUniformMatrix2x4fv)
(define-uniform-matrix glUniformMatrix4x2fv)
(define-uniform-matrix glUniformMatrix3x4fv)
(define-uniform-matrix glUniformMatrix4x3fv)

(define dynamic? (make-parameter #f))

(define (glsl-type->uniform-fun type)
  (define (dynamic/static d s)
    (if (dynamic?) d s))
  (case (symbol->glsl type)
    ((float) (dynamic/static glUniform1fv 'glUniform1fv))
    ((bool int) (dynamic/static glUniform1iv 'glUniform1iv))
    ((uint) (dynamic/static glUniform1uiv 'glUniform1uiv))
    ((vec2) (dynamic/static glUniform2fv 'glUniform2fv))
    ((bvec2 ivec2) (dynamic/static glUniform2iv 'glUniform2iv))
    ((uvec2) (dynamic/static glUniform2uiv 'glUniform2uiv))
    ((vec3) (dynamic/static glUniform3fv 'glUniform3fv))
    ((bvec3 ivec3) (dynamic/static glUniform3iv 'glUniform3iv))
    ((uvec3) (dynamic/static glUniform3uiv 'glUniform3uiv))
    ((vec4) (dynamic/static glUniform4fv 'glUniform4fv))
    ((bvec4 ivec4) (dynamic/static glUniform4iv 'glUniform4iv))
    ((uvec4) (dynamic/static glUniform4uiv 'glUniform4uiv))
    ((mat2 mat2x2) (dynamic/static glUniformMatrix2fv 'glUniformMatrix2fv))
    ((mat3 mat3x3) (dynamic/static glUniformMatrix3fv 'glUniformMatrix3fv))
    ((mat4 mat4x4) (dynamic/static glUniformMatrix4fv 'glUniformMatrix4fv))
    ((mat2x3) (dynamic/static glUniformMatrix2x3fv 'glUniformMatrix2x3fv))
    ((mat3x2) (dynamic/static glUniformMatrix3x2fv 'glUniformMatrix3x2fv))
    ((mat2x4) (dynamic/static glUniformMatrix2x4fv 'glUniformMatrix2x4fv))
    ((mat4x2) (dynamic/static glUniformMatrix4x2fv 'glUniformMatrix4x2fv))
    ((mat3x4) (dynamic/static glUniformMatrix3x4fv 'glUniformMatrix3x4fv))
    ((mat4x3) (dynamic/static glUniformMatrix4x3fv 'glUniformMatrix4x3fv))
    (else (error "Not a GLSL type" type))))

(define (matrix? type)
  (case (symbol->glsl type)
    ((mat2 mat2x2 mat3 mat3x3 mat4 mat4x4 mat2x3 mat3x2 mat2x4 mat4x2 mat3x4 mat4x3)
     #t)
    (else #f)))

(define (sampler? type)
  (case (symbol->glsl type)
    ((sampler1D sampler2D sampler3D samplerCube sampler2DRect sampler1DShadow sampler2DShadow sampler2DRectShadow sampler1DArray sampler2DArray sampler1DArrayShadow sampler2DArrayShadow samplerBuffer sampler2DMS sampler2DMSArray
                isampler1D isampler2D isampler3D isamplerCube isampler2DRect isampler1DArray isampler2DArray isamplerBuffer isampler2DMS isampler2DMSArray
                usampler1D usampler2D usampler3D usamplerCube usampler2DRect usampler1DArray usampler2DArray usamplerBuffer usampler2DMS usampler2DMSArray)
     #t)
    (else #f)))

(define (sampler->texture type)
  (case (symbol->glsl type)
    ((sampler1D sampler1DShadow isampler1D usampler1D)
     gl:+texture-1d+)
    ((sampler2D sampler2DShadow isampler2D usampler2D)
     gl:+texture-2d+)
    ((sampler3D isampler3D usampler3D)
     gl:+texture-3d+)
    ((samplerCube isamplerCube usamplerCube)
     gl:+texture-cube-map+)
    ((sampler2DRect sampler2DRectShadow isampler2DRect usampler2DRect)
     gl:+texture-rectangle+)
    ((sampler1DArray sampler1DArrayShadow isampler1DArray usampler1DArray)
     gl:+texture-1d-array+)
    ((sampler2DArray sampler2DArrayShadow isampler2DArray usampler2DArray)
     gl:+texture-2d-array+)
    ((samplerBuffer isamplerBuffer usamplerBuffer)
     gl:+texture-buffer+)
    ((sampler2DMS isampler2DMS usampler2DMS)
     gl:+texture-2d-multisample+)
    ((sampler2DMSArray isampler2DMSArray usampler2DMSArray)
     gl:+texture-2d-multisample-array+)
    (else (error "No such sampler type" type))))

(define (uniform->binder type n i)
  (if (dynamic?)
      (if (matrix? type)
          (lambda (renderable size)
            ((glsl-type->uniform-fun type)
             (get-renderable-uniform-location renderable size i)
             n
             (get-renderable-uniform-value renderable i)))
          (lambda (renderable size)
            ((glsl-type->uniform-fun type)
             (get-renderable-uniform-location renderable size i)
             n
             (get-renderable-uniform-value renderable i))))
      (if (matrix? type)
       `(,(glsl-type->uniform-fun type)
         (vector-ref (%-> data uniformLocations) ,i)
         ,n
         #f
         (vector-ref (%-> data uniformValues) ,i))
       `(,(glsl-type->uniform-fun type)
         (vector-ref (%-> data uniformLocations) ,i)
         ,n
         (vector-ref (%-> data uniformValues) ,i)))))

(define (sampler->binder type i texture-id)
  (if (dynamic?)
      (values
       (lambda (renderable size)
         (gl:active-texture (+ gl:+texture0+ texture-id))
         (gl:bind-texture (sampler->texture type)
                          (pointer->address
                           (get-renderable-uniform-value renderable i)))
         (gl:uniform1i (get-renderable-uniform-location renderable size i)
                       texture-id))
       (lambda ()
         (gl:bind-texture (sampler->texture type) 0)))
      (values
       `(%begin
         (glActiveTexture (+ GL_TEXTURE0 ,texture-id))
         (glBindTexture ,(sampler->texture type)
                        (%cast (unsigned int)
                               (vector-ref (%-> data uniformValues) ,i)))
         (glUniform1i (vector-ref (%-> data uniformLocations) ,i)
                      ,texture-id))
       `(glBindTexture ,(sampler->texture type) 0))))

(define texture-id (make-parameter 0))

(define (uniform-binders uniforms)
  (let ((uniform-type/number
         (lambda (u) (cond ((and (list? u)
                          (equal? (first u) array:)
                          (= (length u) 3)
                          (number? (third u)))
                       (values (second u) (third u)))
                      ((list? u) (error 'define-render-functions
                                        "Invalid uniform" u))
                      (else (values u 1))))))
    (let loop ((uniforms uniforms) (i 0)
               (uniform-binders '()) (sampler-binders '()) (sampler-unbinders '()))
      (if (null? uniforms)
          (values uniform-binders sampler-binders sampler-unbinders)
          (let-values (((type n) (uniform-type/number (cadar uniforms))))
            (if (sampler? type)
                (let-values (((binder unbinder) (sampler->binder type i (texture-id))))
                  (parameterize ((texture-id (add1 (texture-id))))
                    (loop (cdr uniforms) (add1 i)
                          uniform-binders
                          (cons binder sampler-binders)
                          (lset-adjoin equal? sampler-unbinders unbinder))))
                (loop (cdr uniforms) (add1 i)
                      (cons (uniform->binder type n i)
                            uniform-binders)
                      sampler-binders
                      sampler-unbinders)))))))

(define (next-power-of-two n)
  (inexact->exact (expt 2 (ceiling (/ (log n)
                                      (log 2))))))

(define (renderable-slots uniforms)
  (next-power-of-two (if (>= (length uniforms) 2)
                         (length uniforms)
                         2)))

(define (render-functions prefix name uniforms)
  (let* ((renderable-struct (string->symbol
                             (string-append
                              "GLLSrenderable"
                              (number->string (renderable-slots uniforms)))))
         (fun-name (symbol->c-symbol (symbol-append prefix
                                                    'render- name)))
         (fast-fun-name (symbol->c-symbol (symbol-append prefix
                                                         'fast-render- name)))
         (fast-fun-begin-name (symbol->c-symbol (symbol-append prefix
                                                               'fast-render- name
                                                               '-begin)))
         (fast-fun-end-name (symbol->c-symbol (symbol-append prefix
                                                             'fast-render- name
                                                             '-end))))
    (let-values (((uniform-binders sampler-binders sampler-unbinders)
                  (uniform-binders uniforms)))
      (values
       (fmt #f (c-expr
                `(%begin
                  (%fun void ,fast-fun-begin-name (((const ,renderable-struct *)
                                                    data))
                        (glUseProgram (%-> data program))
                        ,@sampler-binders)
                  (%fun void ,fast-fun-name (((const ,renderable-struct *)
                                              data))
                        ,@uniform-binders
                        (glBindVertexArray (%-> data vao))
                        (glDrawElements (%-> data mode)
                                        (%-> data nElements)
                                        (%-> data elementType)
                                        (%-> data offset)))
                  (%fun void ,fast-fun-end-name ()
                        ,@sampler-unbinders
                        (glBindVertexArray 0))
                  (%fun void ,fun-name (((const ,renderable-struct *) data))
                        (,fast-fun-begin-name data)
                        (,fast-fun-name data)
                        (,fast-fun-end-name)))))
       fun-name
       fast-fun-begin-name
       fast-fun-name
       fast-fun-end-name))))

(define (render-renderable uniforms renderable)
  (parameterize ((dynamic? #t))
    (let-values (((uniform-binders sampler-binders sampler-unbinders)
                  (uniform-binders uniforms))
                 ((n) (renderable-slots uniforms)))
      (gl:use-program (get-renderable-program renderable))
      (for-each (lambda (b) (b renderable n)) uniform-binders)
      (for-each (lambda (b) (b renderable n)) sampler-binders)
      (gl:bind-vertex-array (get-renderable-vao renderable))
      (gl:draw-elements (get-renderable-mode renderable)
                        (get-renderable-n-elements renderable)
                        (get-renderable-element-type renderable)
                        (get-renderable-offset renderable))
      (for-each (lambda (b) (b)) sampler-unbinders)
      (gl:bind-vertex-array 0)
      (gl:check-error))))

(define (symbol->keyword sym)
    (string->keyword (symbol->string sym)))

(define (make-renderable pipeline . args)
  (define (get-arg arg #!optional default)
    (get-keyword arg args
                 (lambda () (if default
                           (default)
                           (error 'make-renderable "Expected keyword argument"
                                  arg args)))))
  (let* ((uniforms (pipeline-uniforms pipeline))
         (n (renderable-slots uniforms))
         (renderable (get-arg #:data (lambda ()
                                       (set-finalizer! (allocate-renderable pipeline)
                                                       free)))))
    (set-renderable-program! renderable (pipeline-program pipeline))
    (if* (get-arg #:mesh (lambda () #f))
        (begin
          (set-renderable-vao! renderable (gl:mesh-vao it))
          (set-renderable-mode! renderable (gl:mode->gl (gl:mesh-mode it)))
          (set-renderable-element-type! renderable
                                        (gl:type->gl (gl:mesh-index-type it)))
          (set-renderable-n-elements! renderable (gl:mesh-n-indices it)))
        (begin
          (set-renderable-vao! renderable (get-arg #:vao))
          (set-renderable-mode! renderable (get-arg #:mode (lambda () gl:+triangles+)))
          (set-renderable-element-type! renderable (get-arg #:element-type))
          (set-renderable-n-elements! renderable (get-arg #:n-elements))))
    (set-renderable-offset! renderable (get-arg #:offset (lambda () #f)))
    (let loop ((uniforms uniforms) (i 0))
      (unless (null? uniforms)
        (let* ((u (car uniforms))
               (name (car u))
               (loc (cadr u)))
          (set-renderable-uniform-location! renderable n i loc)
          (set-renderable-uniform-value! renderable i
                                         (get-arg (symbol->keyword name))))
        (loop (cdr uniforms) (add1 i))))
    renderable))

) ; glls-renderable
