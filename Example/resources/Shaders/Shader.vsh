#if __VERSION__ >= 140

in vec4 position;
in vec2 texCoords;

out vec2 texCoordsVarying;
#else
attribute vec4 position;
attribute vec2 texCoords;

varying mediump vec2 texCoordsVarying;
#endif

uniform mat4 modelMat;
uniform mat4 viewMat;

void main()
{
    gl_Position = viewMat * modelMat * position;
    texCoordsVarying = texCoords;
}
