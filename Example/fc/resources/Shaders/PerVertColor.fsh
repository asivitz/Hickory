#ifdef GL_ES
uniform lowp sampler2D tex;

varying mediump vec2 texCoordsVarying;
varying lowp vec4 colorVarying;
#else
uniform sampler2D tex;

in vec2 texCoordsVarying;
in vec4 colorVarying;
out vec4 outcolor;
#endif


void main()
{
#ifdef GL_ES
   gl_FragColor = texture2D(tex, texCoordsVarying) * colorVarying;
#else
   outcolor = texture(tex, texCoordsVarying) * colorVarying;
#endif

}
