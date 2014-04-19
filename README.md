# glls
glls (GL Lisp Shaders) lets you write GLSL (OpenGL Shader Language) shaders in a convenient pseudo-scheme language in Chicken Scheme. The compilation into GLSL happens at compile-time for zero runtime cost.

The idea for glls was hugely inspired by [Varjo](https://github.com/cbaggers/varjo). Before learning about Varjo, I had never considered the possibility of writing shaders in anything but the GLSL. Seeing them being written in Lisp was a major, "Of course!" moment.

That said, while this library bears some superficial resemblance to Varjo, the approach is quite different. While Varjo does a lot of work to validate the the lispy-glls expressions (including type checking), glls only performs cursory syntactic checking. The result of this is that one could probably write shaders in Varjo without knowing the GLSL and could be reasonably sure that those shaders would always compile to something that would mostly work. glls makes no such promises, so it is entirely possible to generate GLSL that won't compile. Being able to understand GLSL code is therefore a prerequisite for successful shader debugging. The GLSL code output by glls is not overly obtuse, but you'll have to forgive the lack of indentation and the occasional extra semi-colon or parentheses (better safe than sorry).

## Installation
This repository is a [Chicken Scheme](http://call-cc.org/) egg.

It is part of the [Chicken egg index](http://wiki.call-cc.org/chicken-projects/egg-index-4.html) and can be installed with `chicken-install glls`.

## Requirements
* format
* matchable
* miscmacros
* opengl-glew

## Documentation
### Shaders
    [record] (shader TYPE ID SOURCE INPUTS OUTPUTS UNIFORMS PROGRAM)

Used to represent shaders. Returned by `defshader` and `create-shader`. It should not be necessary to access the slots of this record.

    [macro] (defshader NAME GLLS-SHADER)

Defines, for syntax and run-time, a new `shader` named `NAME`. The (unquoted) form `GLLS-SHADER` should conform to language defined in the section [The glls shader language](#the-glls-shader-language). Before shaders are used, they must be compiled by OpenGL with `compile-shader`.

    [procedure] (create-shader GLLS-SHADER #!key INPUTS)

Creates a new `shader`. The form `GLLS-SHADER` should conform to language defined in the section [The glls shader language](#the-glls-shader-language). The key `INPUTS` can be used to include additional inputs to the shader. Before shaders are used, they must be compiled by OpenGL with `compile-shader`.

    [procedure] (compile-glls GLLS-SHADER #!key INPUTS)

Returns the source string for a shader. The form `GLLS-SHADER` should conform to language defined in the section [The glls shader language](#the-glls-shader-language). The key `INPUTS` can be used to include additional inputs to the shader.

    [procedure] (compile-shader SHADER)

Compile (in OpenGL) `SHADER`. Nothing is done if the shader has already been compiled. This typically does not need to be called, since `compile-pipeline` does so. Must be called while there is an active OpenGL context.

    [procedure] (delete-shader SHADER)

Delete (from OpenGL) the program of `SHADER`. Must be called while there is an active OpenGL context.


### Pipelines
*Pipelines* are the term that glsl uses to describe a collection of shaders that will be linked together. This is equivalent to a GL *program*, just less ambiguously named.

    [record] (pipeline SHADERS ATTRIBUTES UNIFORMS PROGRAM)

Created with `defpipeline` or `create-pipeline`, contains the data needed for a pipeline. `SHADERS` is the list of shader records. `ATTRIBUTES` and `UNIFORMS` are lists of the attributes and uniforms of the shader, specified as (name . type) pairs before compilation (with `compile-pipeline` or `compile-pipelines`) and (name location type) lists after compilation. `PROGRAM` is the GL ID of the program (always 0 before compilation).

    [macro] (defpipeline NAME . SHADERS)

Defines, for syntax and run-time, a new `pipeline` named `NAME`. The `SHADERS` should either be forms conforming to language defined in the section [The glls shader language](#the-glls-shader-language), `shader`s defined by `defshader`, or a mix of the two. Pipelines must have at least one vertex and one fragment shader to be able to compile. Before pipelines are used, they must be compiled by OpenGL with `compile-pipeline` or `compile-pipelines`.

    [procedure] (create-pipeline . SHADERS)

Creates a new `pipeline`. The `SHADERS` should either be forms conforming to language defined in the section [The glls shader language](#the-glls-shader-language), `shader`s, or a mix of the two. Pipelines must have at least one vertex and one fragment shader to be able to compile. Before pipelines are used, they must be compiled by OpenGL with `compile-pipeline` or `compile-pipelines`.

    [procedure] (compile-pipeline PIPELINE)

Compile (in OpenGL) the `PIPELINE` and sets its `PROGRAM` slot to the OpenGL program ID. Compiles all of the pipeline's shaders with `compile-shader`. Must be called while there is an active OpenGL context.

    [procedure] (compile-pipelines)

Compile (as per `compile-pipeline`) all the pipelines defined by `defpipeline` and `create-pipeline`. Must be called while there is an active OpenGL context.

    [procedure] (delete-pipeline PIPELINE)

Delete (from OpenGL) the program of `PIPELINE`. Must be called while there is an active OpenGL context.


### The glls shader language


## Examples
These examples depends on the [glfw3](http://wiki.call-cc.org/eggref/4/glfw3) egg for window and context creation.

Aside from knowing how to write glls shaders, only one macro, one function, and one record is necessary to use glls: `defpipeline`, `compile-pipelines`, and the record `pipeline`. 

``` Scheme
(import chicken scheme)

(use glls (prefix glfw3 glfw:) (prefix opengl-glew gl:))

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

(glfw:with-window (640 480 "Example" resizable: #f)
   (compile-pipelines)
   (print foo)
   (gl:use-program (pipeline-program foo)))
```

This example is similar to the first, but also illustrates the ability to define pipelines in different ways.

``` Scheme
(import chicken scheme)

(use glls (prefix glfw3 glfw:) (prefix opengl-glew gl:))

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
   (compile-pipelines)
   (print foo)
   (print baz)
   (delete-pipeline foo)
```

## Version history
### Version 0.1.0
* Initial release

## Source repository
Source available on [GitHub](https://github.com/AlexCharlton/glls).

Bug reports and patches welcome! Bugs can be reported via GitHub or to alex.n.charlton at gmail.

## Author
Alex Charlton

## Licence
BSD
