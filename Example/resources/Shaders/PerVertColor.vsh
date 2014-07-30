#if __VERSION__ >= 140

in vec2 position;
in vec2 texCoords;
in vec4 color;

out vec2 texCoordsVarying;
out vec4 colorVarying;
#else
attribute vec2 position;
attribute vec2 texCoords;
attribute vec4 color;

varying mediump vec2 texCoordsVarying;
varying lowp vec4 colorVarying;
#endif

uniform mat4 modelMat;
uniform mat4 viewMat;

void main()
{
    gl_Position = viewMat * modelMat * vec4(position, 0.0, 1.0);
    texCoordsVarying = texCoords;
    colorVarying = color;
}
