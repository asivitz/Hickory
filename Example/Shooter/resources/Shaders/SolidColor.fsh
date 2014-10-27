#if __VERSION__ >= 140
uniform vec4 color;
out vec4 outcolor;
#else
uniform lowp vec4 color;
#endif

void main()
{

#if __VERSION__ >= 140
   outcolor = color;
#else
   gl_FragColor = color;
#endif
}
