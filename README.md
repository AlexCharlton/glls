# glls
glls (GL Lisp Shaders) lets you write [GLSL](https://www.opengl.org/documentation/glsl/) (OpenGL Shader Language) shaders in a convenient pseudo-scheme language in Chicken Scheme. The compilation into GLSL happens at compile-time for zero run-time cost. Run-time compilation and dynamic recompilation is also supported. To those that want to dynamically construct shaders: I solute you.

In addition to the eponymous module, glls also provides the `glls-render` module. `glls-render` enhances glls to create automatic rendering functions for each pipeline. When compiled, these rendering functions are created in efficient C, although dynamic functions are also provided. See the section [Automatic render functions](#automatic-render-functions) for details.

The idea for glls was hugely inspired by [Varjo](https://github.com/cbaggers/varjo). Before learning about Varjo, I had never considered the possibility of writing shaders in anything but the GLSL. Seeing them being written in Lisp was a major, "Of course!" moment.

That said, while this library bears some superficial resemblance to Varjo, the approach is quite different. While Varjo does a lot of work to validate the the lispy-glls expressions (including type checking), glls only performs cursory syntactic checking. The result of this is that one could probably write shaders in Varjo without knowing the GLSL and could be reasonably sure that those shaders would always compile to something that would mostly work. glls makes no such promises, so it is entirely possible to generate GLSL that won’t compile. Being able to understand GLSL code is therefore a prerequisite for successful shader debugging. The GLSL code output by glls is beautifully formatted, thanks to Alex Shinn’s amazing [fmt](http://synthcode.com/scheme/fmt/) library. fmt is responsible for far more than just the GLSL formatting, since it is basically a compiler of its own. The compilation portion of glsl is more or less a thin layer on top of fmt.

glls should work on Linux, Mac OS X, Windows, and with OpenGL ES. glls will automatically compile with ES support on ARM hardware, or when `gles` is defined during compilation (e.g. `chicken-install -D gles`).

## Installation
This repository is a [Chicken Scheme](http://call-cc.org/) egg.

It is part of the [Chicken egg index](http://wiki.call-cc.org/chicken-projects/egg-index-4.html) and can be installed with `chicken-install glls`.

## Requirements
* make
* fmt
* matchable
* miscmacros
* opengl-glew
* srfi-42

## Documentation
glls contains three modules: glls-render, glls, and glls-compiler. glls-render rexports glls and glls-compiler and is used when you want the functionality of glls plus the addition of [automatically generated render functions](#automatic-render-functions). glls is the primary module, providing the main interface to shaders and pipelines, which also rexports glls-compiler. glls-compiler provides the functions used to compile shaders.

    [parameter] glsl-version

The default GLSL version used by shaders. Defaults to `120` on GL ES platforms, `330` otherwise. Can also be a list, see `<version>` under [Shader syntax](#shader-syntax). When compiling a file with a shader, modifying this parameter will only take effect if you change it before the compilation phase. E.g.:

``` Scheme
(use-for-syntax glls)
(begin-for-syntax (glsl-version 300))
```

### Shaders
    [record] (shader TYPE SOURCE INPUTS OUTPUTS UNIFORMS PROGRAM)

Used to represent shaders. Returned by `define-shader` and `create-shader`. It should not typically be necessary to access the slots of this record.

    [macro] (define-shader SHADER-NAME GLLS-SHADER)

Defines a new `shader` named `NAME`. The (unquoted) form `GLLS-SHADER` should conform to language defined in the section [The glls shader language](#the-glls-shader-language). Before shaders are used, they must be compiled by OpenGL with `compile-shader`.

    [procedure] (create-shader GLLS-SHADER )

Creates (at run-time) a new `shader`. The form `GLLS-SHADER` should conform to language defined in the section [The glls shader language](#the-glls-shader-language). Before shaders are used, they must be compiled by OpenGL with `compile-shader`.

    [procedure] (compile-glls GLLS-SHADER)

Returns the source string for a shader. The form `GLLS-SHADER` should conform to language defined in the section [The glls shader language](#the-glls-shader-language).

    [procedure] (compile-shader SHADER)

Compile (in OpenGL) `SHADER`. Nothing is done if the shader has already been compiled. This typically does not need to be called, since `compile-pipeline` does so. Must be called while there is an active OpenGL context.


### Pipelines
*Pipelines* are the term that glsl uses to describe a collection of shaders that will be linked together. This is equivalent to a GL *program*, just less ambiguously named.

    [record] (pipeline SHADERS ATTRIBUTES UNIFORMS PROGRAM)

Created with `define-pipeline` or `create-pipeline`, contains the data needed for a pipeline. `SHADERS` is the list of shader records. `ATTRIBUTES` and `UNIFORMS` are lists of the attributes and uniforms of the shader, specified as `(name . type)` pairs before compilation (with `compile-pipeline` or `compile-pipelines`) and `(name location type)` lists after compilation. `PROGRAM` is the GL ID of the program (always 0 before compilation).

    [macro] (define-pipeline PIPELINE-NAME . SHADERS)

Defines a new `pipeline` named `NAME`. The `SHADERS` should either be forms conforming to language defined in the section [The glls shader language](#the-glls-shader-language), `shader`s defined by `define-shader`, or a mix of the two. Pipelines must have at least one vertex and one fragment shader to be able to compile. Before pipelines are used, they must be compiled by OpenGL with `compile-pipeline` or `compile-pipelines`.

`define-pipeline` behaves differently when it is being evaluated *and* when a given pipeline is being redefined. In this case, the new pipeline inherits the GL program ID of the old one. Additionally, the pipeline is compiled by OpenGL right away (and as a consequence, so are any pipelines that are pending compilation). This is done so that pipelines can be edited and reevaluated in a REPL session and one’s scene will be updated as expected. See the [interactive example](https://github.com/AlexCharlton/glls/blob/master/examples/interactive.scm) for an example of how this can be accomplished.

`define-pipeline` has additional effects when used with the `glls-render` module (see [Automatic render functions](#automatic-render-functions)).

    [procedure] (create-pipeline . SHADERS)

Creates (at run-time) a new `pipeline`. The `SHADERS` should either be forms conforming to language defined in the section [The glls shader language](#the-glls-shader-language), `shader`s, or a mix of the two. Pipelines must have at least one vertex and one fragment shader to be able to compile. Before pipelines are used, they must be compiled by OpenGL with `compile-pipeline` or `compile-pipelines`.

    [procedure] (compile-pipeline PIPELINE)

Compile (in OpenGL) the `PIPELINE` and sets its `PROGRAM` slot to the OpenGL program ID. If the pipeline’s `PROGRAM` slot is already set to a non-zero value, this ID will be reused for the new program. Compiles all of the pipeline’s shaders with `compile-shader`. Must be called while there is an active OpenGL context.

    [procedure] (compile-pipelines)

Compile (as per `compile-pipeline`) all the pipelines defined by `define-pipeline` and `create-pipeline`. Must be called while there is an active OpenGL context.

    [procedure] (pipeline-uniform UNIFORM PIPELINE)

Return the location of `UNIFORM`. The `PIPELINE` must be compiled before this function can be used.

    [procedure] (pipeline-attribute ATTRIBUTE PIPELINE)

Return the location of `ATTRIBUTE`. The `PIPELINE` must be compiled before this function can be used.

    [procedure] (pipeline-mesh-attributes PIPELINE)

Return a list of `(ATTRIBUTE-NAME . LOCATION)` pairs, suitable for passing to [gl-utils’](http://wiki.call-cc.org/eggref/4/gl-utils) `mesh-make-vao!`.

### The glls shader language
#### Shader syntax
The shaders of glls – the forms that `define-shader`, `define-pipeline`, etc. expect – have the following syntax:

    (<type> [input: <inputs>] [uniform: <uniforms>] [output: <outputs>]
            [version: <version>] [use: <imports>] [export: <exports])
    <body> ...

`type` is the keyword type of the shader. It must be one of `#:vertex`, `#:fragment`, `#:geometry`, `#:tess-control`, `#:tess-evaluation`, or `#:compute`.

`inputs` is a list of the input variables for the shader. These are given in `(name type)` lists.

`uniforms` is a list of the uniform variables for the shader. These are given in `(name type)` lists.

`outputs` is a list of the output variables from the shader. These are given in `(name type)` lists.

`version` is the integer version number of the shader, i.e. the number you would write at the top of the shader source (e.g. `#version 410`). Defaults to the `glsl-version` parameter. This can also be a list of the form `'(<version> <specifiers> ...)` where the specifiers will be appended to the `#version` line when compiled to glsl. For example, `'(300 es)` becomes `#version 300 es`.

`imports` is the list of shaders that the current shader depends on. See the section [Shaders that export](#shaders-that-export) for more details. Defaults to `()`

`exports` is the list of symbols that the current shader exports. See the section [Shaders that export](#shaders-that-export) for more details. Defaults to `()`

`body` is the form representing the code of the shader. See the section [Shader Lisp](#shader-lisp) for an explanation of the kind of code that is expected.


#### Shader Lisp
For the most part, the Lisp used to define glls shaders looks like Scheme with one notable difference: types must be specified whenever a variable or function is defined. Under the hood, forms are being passed to [fmt](https://wiki.call-cc.org/eggref/4/fmt#c-as-s-expressions), so everything that you can do there will work in glls. Details of the Lisp used for shaders is provided in the following sections.

It should be possible to do almost anything in glls that you would want to do with the GLSL. Known exceptions to this is are: layout qualifiers (which I don’t feel are terribly relevant in the context of Scheme, at least not until uniform locations become prevalent), do-while loops (which have no Scheme analog), and uniform blocks, `#line`, `#undef`, and struct uniforms all for no good reason. Let me know if there are any features that you find lacking.

Keep in mind that glls cannot do anything that the GLSL can’t, such as making anonymous or recursive functions.

##### Variables and naming
Symbols in glls are transformed from a Scheme style into the C style used in the GLSL. Letters after dashes are uppercased (i.e., symbols become camelCased). Symbols prefixed by `gl:` in glls become prefixed by `gl_` in GLSL. `#f` and `#t` may be used instead of `false` and `true`.

For programmer-defined variables this has little consequence. The importance of learning the renaming conventions comes when you want to call GLSL functions or variables. Examples of mappings between glls and GLSL names are: `gl:position` → `gl_Position`, `float-bits-to-uint` → `floatBitsToUint`, `shadow-2d-proj-lod` → `shadow2DProjLod`, and `sampler-2d-ms-array` → `sampler2DMSArray`. Two special cases are `emit-vertex` and `end-primitive` which are translated into the functions `EmitVertex` and `EndPrimitive` respectively (which, for some reason, go against the usual GLSL naming conventions).

##### Types
When defining variables or functions in glls, types must be supplied. Basic types (e.g. `int`, `mat2x2`) are given either as a symbol or keyword (e.g. `int`, `#:mat2x2`), whichever is preferred. Types with qualifiers (e.g. `lowp float`, `out mediump vec2`) are given as lists (e.g. `(lowp float)`, `(out mediump vec2)`).

Arrays are specified as lists beginning with the keyword `#:array`. The next element in the list is the type, while the optional third element is the size. E.g. `(#:array int 5)`. When used with qualifiers, the array takes the place of the type, e.g. `(highp (#:array float))`.

##### Functions
GLSL functions and operators are all called like normal Lisp functions. In almost all cases the GLSL symbol (taking into account the renaming described in [Variables and naming](#variables-and-naming) can be used, while many operators can be called with their Scheme counterpart. The only operators that may not be used directly are `|`, `||`, `|=`, `.`, `=`, and array reference which must be called with their counterparts.

The following is a mapping between glls aliases for GLSL functions and operators:

* `modulo`: `%`
* `expt`: `pow`
* `equal?`, `eqv?`, `eq`, `=`: `==`
* `set!`: `=`
* `and`: `&&`
* `or`: `||`
* `not`: `!`
* `bitwise-and`: `&`
* `bitwise-ior`: `|`
* `bitwise-xor`: `^`
* `bitwise-not`: `~`
* `arithmetic-shift`: `<<`
* `bitwise-and=`: `&=`
* `bitwise-ior=`: `|=`
* `bitwise-xor=`: `^=`
* `arithmetic-shift=`: `<<=`
* `++/post`: Postfix `++`
* `--/post`: Postfix `--`
* `field`, `..`: `.` (struct field reference, e.g. `(field point x)` → `point.x`)
* `swizzle`, `~~`: `.` (vector swizzling, e.g. `(swizzle color r g)` → `color.rg`)
* `array-ref`, `vector-ref`: `[]` (array reference, e.g. `(array-ref a 4)` → `a[4]`)
* `array-set!`, `vector-set!`: `[] =` (array setting, e.g. `(array-set! a 4 x)` → `a[4] = x`)
* `length`: `.length()` (vector length, e.g. `(length vec)` → `vec.length()`)

##### Definition
Variables, functions, and records (structs) are defined much like they are in Scheme, with the additional requirement of including types.

    (define <name> <type> [<value>])

Defines the variable `name`. When `type` is an array, a vector literal (eg. `#(1 2 3)`) may be used as the initial `value`.

    (define (<name> [(<parameter> <type>) ...]) <return-type> <body> ...)

Defines the function `name`. The last expression in the body of a non-void function is automatically returned. If `body` is omitted, a function prototype is created.

    (let ((<name> <type> [<value>]) ...) <body> ...)

Defines the supplied variables. When `type` is an array, a vector literal (eg. `#(1 2 3)`) may be used as the initial `value`. Note that, unlike Scheme, the variables created will continue to exist outside of the `let` (until the extent of whatever lexical scope the `let` exists within). In other words, `let` does not introduce scope. Note also that variables defined in `let` are within the scope of variables that are subsequently defined in the same `let` (i.e. `let` functions like `let*` in Scheme, and in fact `let*` may be used if preferred).

    (define-record <name> (<type> <field>) ...)

Defines the struct `name`.

##### Control
The following can be used with identical syntax to scheme:

    (if <test> <true> [<false>])

    (cond (<test> <result> ...) ... (else <result>))

    (begin <body> ...)

Keep in mind that they may only be used in the same place as their corresponding GLSL statements, with the exception of `begin`, which can only be used where it is possible to have multiple expressions.

Additionally, the following forms can be used for switch/case statements in the GLSL fashion:

    (c-switch <clause> ...)

    (c-case <values> <body> ...)

    (c-case/fallthrough <values> <body> ...)

    (c-default <body> ...) 

##### Iteration
    (for <init> <condition> <update> <body> ...)

GLSL style `for` loop.

    (do-times (<var> [<start>] <end>) <body> ...)

Equivalent to `(for (define <var> #:int <start>) (< <var> <end>) (++ <var>) <body> ...)`. `start` defaults to 0.

    (while <condition> <body> ...)

GLSL style `while` loop.

##### Jumps
All GLSL jumps (`continue`, `break`, `return`, `discard`) can be called like functions. Return may accept one argument. Keep in mind that the last expression in a non void function is automatically returned.

##### Pre-processor
The following forms can be used to add pre-processor directives:

    (%define <name> [<value>])

    (%if <test> <true> [<false>])

    (%ifdef <value> <true> [<false>])

    (%ifndef <value> <true> [<false>])

    (%elif <value> <true> [<false>])

    (%else <fallback>)

    (%error <value> ...)

    (%pragma [#:stdgl] <name> <behaviour>)

The optional `#:stdgl` keyword is used when a `STDGL` pragma is desired. `<name>` and `<behaviour>` will be formatted without modification as `<name>(<behaviour>)`.

    (%extension <name> <behaviour>)

`<name>` and `<behaviour>` will be formatted without modification as `<name> : <behaviour>`.


### Shaders that export
It is often desirable to have shaders that contain generic, reusable functions. These shaders are linked into the pipeline (or program, in GLSL parlance) so that they can be accessed by other shaders. In order for another shader to reuse a function, it first has to (as in C) include a function prototype. glsl automates this process.

glls lets you define shaders that export symbols through the use of the [`export` keyword](#shader-syntax). These shaders can then be imported by others (through the [`use` keyword](#shader-syntax)). Prototypes are automatically generated for top-level functions or variables whose names match the symbols in the `export` keyword list. These prototypes are then inserted into shaders that `use` the exporting shader. Shaders that are `use`d by another are automatically linked into the resulting pipeline.

Shaders that export should not have any inputs or outputs.

See the example [exports.scm](https://github.com/AlexCharlton/glls/blob/master/examples/exports.scm) to see this in action.

### Automatic render functions
By using the `glls-render` module, you can have glls automatically generate functions that will render an object with your glls pipeline. `glls-render` wraps `define-pipeline` so that it also defines a set of functions used for rendering and managing the *renderable* objects that are specific to that pipeline: one to create them, several to render them, and others to manipulate them. `glls-render` should not be used with the `glls` module: It reexports everything that you need from `glls`.

Recalling `define-pipeline`:

    (define-pipeline PIPELINE-NAME . SHADERS)

There is one difference with `glls-render`’s `define-pipeline`: All shaders must include a list of the shader’s uniforms since the uniforms are the important information needed to derive rendering functions. This means that if you previously define some shaders (for example: `my-vertex-shader` and `my-fragment-shader`) and you wish to combine them in a pipeline, you *must* include the uniforms in the pipeline definition. This is done with a list that takes the form `(SHADER uniform: [UNIFORM] ...)`. This list must be present even if the shader does not use any uniforms. For example:

    (define-pipeline my-pipeline
      (my-vertex-shader uniform: mvp-matrix inverse-transpose-matrix)
      (my-fragment-shader uniform:))

Of course, if you are defining the shaders in the pipeline, then a separate list of uniforms is not necessary.

#### Renderables
`glls-render`’s `define-piplelines` defines a function for creating renderable objects:

    (make-PIPELINE-NAME-renderable [vao: VAO] [mode: MODE] [n-elements: N-ELEMENTS] [element-type: ELEMENT-TYPE] [mesh: MESH] [offset: OFFSET] [data: DATA] + PIPELINE-SPECIFIC-KEYWORDS)

Where `PIPELINE-NAME` is the name of the pipeline who’s renderables are being created.

- `VAO` – A VAO such as those returned by [opengl-glew’s `make-vao`](http://api.call-cc.org/doc/opengl-glew/make-vao). I.e.: A VAO that binds an array of attributes – for each element in the pipeline – as well as an element array.
- `MODE` – The drawing mode to use when drawing the elements of the VAO. Must be mode that is accepted by (gl-utils’) [mode->gl](http://api.call-cc.org/doc/gl-utils/mode-%3Egl). Defaults to `#:triangles`.
- `N-ELEMENTS` – The number of elements (vertices) to draw.
- `ELEMENT-TYPE` – The type of the values in the VAO’s element array. Must be one of `#:unsigned-byte`, `#:unsigned-short`, or `#:unsigned-int`. Not required if the VAO has no element array (i.e. `render-arrays-PIPELINE-NAME` is being used to render).
- `MESH` – A [gl-utils](http://wiki.call-cc.org/eggref/4/gl-utils) mesh, provided in place of `VAO`, `MODE`, `N-ELEMENTS`, and `ELEMENT-TYPE`.
- `OFFSET` – A byte offset to the location of the desired indices to draw.
- `DATA` – An optional pointer to an appropriate glls renderable object. If not provided, a fresh renderable object will be created. [gllsRenderable.h](https://github.com/AlexCharlton/glls/blob/master/gllsRender.h) defines the structs used for renderables. Which struct is used for a given pipeline is chosen based on the number of uniforms present in the pipeline.

See the [`glDrawElements` documentation](https://www.opengl.org/sdk/docs/man/html/glDrawElements.xhtml) for more information about these expected arguments.

`make-PIPELINE-NAME-renderable` also expects one keyword argument for each uniform in the pipeline. These arguments should either be an f32vector, an s32vector, a u32vector, a pointer to the uniform data, or – in the case of a texture – a fixnum. Even if the uniform is a single value (e.g. a float), it must still be passed as a vector (or a pointer). This lets the value of the uniform be updated independently of the renderable.

Additionally, there are a number of renderable setters for each of the keyword arguments accepted by `make-PIPELINE-NAME-renderable`:

    [procedure] (set-renderable-vao! RENDERABLE VAO)
    [procedure] (set-renderable-n-elements! RENDERABLE N-ELEMENTS)
    [procedure] (set-renderable-element-type! RENDERABLE TYPE)
    [procedure] (set-renderable-mode! RENDERABLE MODE)
    [procedure] (set-renderable-offset! RENDERABLE OFFSET)

    (set-PIPELINE-NAME-renderable-UNIFORM-NAME! RENDERABLE UNIFORM-VALUE)

These setters accept two arguments: a renderable and a value. The values correspond to those that `make-PIPELINE-NAME-renderable` accepts. For each uniform in the pipeline, a function named `set-PIPELINE-NAME-renderable-UNIFORM-NAME!` is created.

    [procedure] (renderable-size PIPELINE)

Returns the size, in bytes, of the memory needed for a renderable belonging to `PIPELINE`.

#### Rendering renderables
    (render-PIPELINE-NAME RENDERABLE)
    (render-arrays-PIPELINE-NAME RENDERABLE)

Where `PIPELINE-NAME` is the name of the pipeline who’s renderables are being rendered. `render-PIPELINE-NAME` and `render-arrays-PIPELINE-NAME` both render the given renderable.

These render functions work differently depending on whether the `define-pipeline` has been compiled or interpreted (although the end results should look the same). When `define-pipeline` is compiled, the resulting render functions are compiled directly to efficient (non-branching) C. When `define-pipeline` is interpreted, the render functions call a generic rendering function that is not nearly as fast.

The `render-PIPELINE-NAME` function draws the renderable using `draw-elements`. While this is typically the most efficient way to render a set of vertices, sometimes `draw-arrays` is preferable. `render-arrays-PIPELINE-NAME` functions identically except for calling `draw-arrays` instead of `draw-elements`.

##### Fast render functions
When compiled, the render function defined by `define-pipeline` is actually a combination of three “fast” render functions: a *begin-render* function, a *render* function, and an *end-render* function. The array rendering function is a similar combination: the *begin-render* function, an *array-render* function and the *end-render* function. This is done so that, if desired, all of the renderables that belong to the same pipeline may be rendered at the same time, without needing to perform expensive calls like program changes or texture binding more than once. To use these functions, call the *begin-render* function with the first renderable, then call the *render* (or *array-render*) function on all renderables (including the first), finally calling the *end-render* function (with no arguments) to clean up.

    (PIPELINE-NAME-fast-render-functions)

`define-pipeline` does not define all of the render functions separately, but instead defines a single function with which to access them: `PIPELINE-NAME-fast-render-functions`, where `PIPELINE-NAME` is the name of the pipeline. This function returns eight values: the *begin-render* function, the *render* function, the *end-render* function, the *array-render* function, and pointers to those same C functions in that order.

    [parameter] unique-textures?

This parameter, defaulting to `#t`, controls where textures are bound in the fast render functions. When `unique-textures?` is `#t`, textures are bound in the main render function. When `unique-textures?` is `#f`, textures are bound in the begin render function. In other words: when `unique-textures?` is `#f`, it is assumed that that all of the renderables belonging to the same pipeline share a common “sprite sheet” (or other shared texture type). This parameter must be set for syntax (i.e. in a `begin-for-syntax` form) in order to have an effect.

#### Utilities
    [macro] (export-pipeline . PIPELINES)

Since glls-render causes `define-pipeline` to define multiple functions, this macro exports everything related to each pipeline in `PIPELINES`, except for the `set-PIPELINE-NAME-renderable-UNIFORM!` setters. These must be exported individually.


## Examples
These examples depends on the [glfw3](http://wiki.call-cc.org/eggref/4/glfw3) egg for window and context creation. The examples presented here illustrate only very basic shader definition and loading. For more complete examples, see the [examples directory](https://github.com/AlexCharlton/glls/tree/master/examples) of the source.

Aside from knowing how to write glls shaders, only one macro, one function, and one record is necessary to use glls: `define-pipeline`, `compile-pipelines`, and the record `pipeline`. This example illustrates this minimal pipeline creation

``` Scheme
(import chicken scheme)

(use glls (prefix glfw3 glfw:) (prefix opengl-glew gl:))

(define-pipeline foo 
  ((#:vertex input: ((vertex #:vec2) (color #:vec3))
             uniform: ((mvp #:mat4))
             output: ((c #:vec3)))
   (define (main) #:void
     (set! gl:position (* mvp (vec4 vertex 0.0 1.0)))
     (set! c color)))
  ((#:fragment input: ((c #:vec3))
               output: ((frag-color #:vec4)))
   (define (main) #:void
     (set! frag-color (vec4 c 1.0)))))

(glfw:with-window (640 480 "Example" resizable: #f)
   (gl:init)
   (compile-pipelines)
   (print foo)
   (gl:use-program (pipeline-program foo)))
```

This example is similar to the first, but also illustrates the ability to define pipelines in different ways.

``` Scheme
(import chicken scheme)

(use glls (prefix glfw3 glfw:) (prefix opengl-glew gl:))

(define-pipeline foo 
  ((#:vertex input: ((vertex #:vec2) (color #:vec3))
                             uniform: ((mvp #:mat4))
                             output: ((c #:vec3)))
   (define (main) #:void
     (set! gl:position (* mvp (vec4 vertex 0.0 1.0)))
     (set! c color)))
  ((#:fragment input: ((c #:vec3))
               output: ((frag-color #:vec4)))
   (define (main) #:void
     (set! frag-color (vec4 c 1.0)))))

(define-shader bar (#:vertex input: ((vertex #:vec2) (color #:vec3))
                             uniform: ((mvp #:mat4))
                             output: ((c #:vec3)))
  (define (main) #:void
    (set! gl:position (* mvp (vec4 vertex 0.0 1.0)))
    (set! c color)))

(define-pipeline baz 
  bar
  (cadr (pipeline-shaders foo)))

(glfw:with-window (640 480 "Example" resizable: #f)
   (gl:init)
   (compile-pipelines)
   (print foo)
   (print baz))
```

## Version history
### Version 0.11.0
23 January 2014

- Overhaul compiler so new syntax can be introduced, and so existing syntax more closely maps to GLSL
- Add array initialization
- Move `pragma` and `extension` from keywords to forms

### Version 0.10.0
25 December 2014

- Export `set-renderable-*!` functions

### Version 0.9.0
24 December 2014

- Add `unique-textures?` parameter
- `export-pipeline` exports primary setters
- Better renderable setter error values

### Version 0.8.0
8 December 2014

- Add `draw-array` render functions
- Support OpenGL ES
- Bug fixes

### Version 0.7.0
20 October 2014

- Renderable element-type and mode setters take keywords instead of integers

### Version 0.6.0
10 September 2014

- Integrate with gl-utils’ meshes
- Remove obsolete `load-ply-renderable`

### Version 0.5.2
24 August 2014

- Update to new gl-utils

**Version 0.5.1**

- Allow `#t`, `#f`
- Fix swizzle


**Version 0.5.0**

14 August 2014

- New shader syntax

### Version 0.4.1
12 August 2014

- Fix export name matching

**Version 0.4.0**

11 August 2014

- Add shader exports
- Add function prototyping
- Add `export-pipeline`
- Add aliases to `field`, `swizzle` (`..`, `~~`)
- Export `renderable-size` rather than `allocate-renderable`
- Improve error messages
- Bug fixes

### Version 0.3.3
4 June 2014

- Bug fixes relating to dynamic reevaluated pipelines

**Version 0.3.2**

3 June 2014

- Update examples for GC safety

**Version 0.3.1**

2 June 2014

- Fix `load-ply-renderable`
- Fix gllsRender.h

**Version 0.3.0**

30 May 2014

- Support dynamic reevaluation of pipelines

### Version 0.2.2
29 May 2014

- Add make as a dependency

**Version 0.2.1**

- Fix bug affecting dynamic use of glls-render

**Version 0.2.0**

28 May 2014

- Automatic render function generation
- Removed `eval` from `defpipeline` (which broke some things when used in modules)
- Renamed `defpipeline`, `defshader`

### Version 0.1.0
* Initial release

## Source repository
Source available on [GitHub](https://github.com/AlexCharlton/glls).

Bug reports and patches welcome! Bugs can be reported via GitHub or to alex.n.charlton at gmail.

## Author
Alex Charlton

## Licence
BSD
