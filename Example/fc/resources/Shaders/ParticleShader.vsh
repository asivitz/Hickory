#ifdef GL_ES
uniform lowp vec4 color;
attribute vec3 position;
#else
uniform vec4 color;
in vec3 position;
#endif
uniform mat4 modelMat;
uniform mat4 viewMat;
uniform float size;

void main()
{
    gl_Position = viewMat * modelMat * vec4(position, 1.0);

    gl_PointSize = size;
}
