#include <stdarg.h>
#include <stdio.h>
#include <unistd.h>

   //NSString *filePath = [[NSBundle mainBundle] resourcePath];
   //[filePath getCString:buf maxLength:maxLength encoding:NSASCIIStringEncoding];

#if TARGET_OS_IPHONE
#else
void getResourcePath(char * buf, int maxLength)
{
   char wd[1024];
   getcwd(wd, 1024);
   snprintf(buf, maxLength, "%s/resources", wd);
}
#endif

   //NSString * docPath = NSSearchPathForDirectoriesInDomains(NSDocumentDirectory, NSUserDomainMask, YES)[0];
   //return [docPath cStringUsingEncoding:NSASCIIStringEncoding];
#if TARGET_OS_IPHONE
#else
const char * documentsPath()
{
   return "/tmp/cb/";
}
#endif

void dlog(const char *format , ... )
{
   char tolog[512];
   
   va_list arglist;
   va_start( arglist, format );
   vprintf( format, arglist );
   va_end( arglist );
   printf("\n");
}

