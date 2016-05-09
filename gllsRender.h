#if defined (__APPLE__)
#include <OpenGL/gl.h>
#elif defined (GLES)
#include <GLES3/gl3.h>
#elif defined (_WIN32)
#include <GL/glew.h>
#else
#include <GL/gl.h>
#endif

typedef struct GLLSrenderable2{
    unsigned int program, vao, nElements, elementType, mode;
    void *offset;
    void *uniformValues[2];
    int uniformLocations[2];
} GLLSrenderable2;

typedef struct GLLSrenderable4{
    unsigned int program, vao, nElements, elementType, mode;
    void *offset;
    void *uniformValues[4];
    int uniformLocations[4];
} GLLSrenderable4;

typedef struct GLLSrenderable8{
    unsigned int program, vao, nElements, elementType, mode;
    void *offset;
    void *uniformValues[8];
    int uniformLocations[8];
} GLLSrenderable8;

typedef struct GLLSrenderable16{
    unsigned int program, vao, nElements, elementType, mode;
    void *offset;
    void *uniformValues[16];
    int uniformLocations[16];
} GLLSrenderable16;

typedef struct GLLSrenderable32{
    unsigned int program, vao, nElements, elementType, mode;
    void *offset;
    void *uniformValues[32];
    int uniformLocations[32];
} GLLSrenderable32;

typedef struct GLLSrenderable64{
    unsigned int program, vao, nElements, elementType, mode;
    void *offset;
    void *uniformValues[64];
    int uniformLocations[64];
} GLLSrenderable64;

typedef struct GLLSrenderable128{
    unsigned int program, vao, nElements, elementType, mode;
    void *offset;
    void *uniformValues[128];
    int uniformLocations[128];
} GLLSrenderable128;

typedef struct GLLSrenderable256{
    unsigned int program, vao, nElements, elementType, mode;
    void *offset;
    void *uniformValues[256];
    int uniformLocations[256];
} GLLSrenderable256;

typedef struct GLLSrenderable1024{
    unsigned int program, vao, nElements, elementType, mode;
    void *offset;
    void *uniformValues[1024];
    int uniformLocations[1024];
} GLLSrenderable1024;
