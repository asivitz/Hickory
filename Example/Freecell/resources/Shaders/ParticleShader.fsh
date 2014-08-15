#ifdef GL_ES
uniform lowp vec4 color;
uniform lowp vec4 color2;
#else
uniform vec4 color;
uniform vec4 color2;
out vec4 outcolor;
#endif

void main()
{
#ifdef GL_ES
    gl_FragColor = color;
#else
    outcolor = color;
#endif
