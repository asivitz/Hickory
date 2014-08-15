#ifdef GL_ES
uniform lowp vec4 color;
uniform lowp sampler2D tex;

varying mediump vec2 texCoordsVarying;
#else
uniform vec4 color;
uniform sampler2D tex;

in vec2 texCoordsVarying;
out vec4 outcolor;
#endif


void main()
{
#ifdef GL_ES
   gl_FragColor = texture2D(tex, texCoordsVarying) * color;
#else
   outcolor = texture(tex, texCoordsVarying) * color;
#endif

}
