#include <stdarg.h>
#include <stdio.h>
#include <unistd.h>

void getResourcePath(char * buf, int maxLength)
{
#if TARGET_OS_IPHONE
   NSString *filePath = [[NSBundle mainBundle] resourcePath];
   [filePath getCString:buf maxLength:maxLength encoding:NSASCIIStringEncoding];
#else
   char wd[1024];
   getcwd(wd, 1024);
   snprintf(buf, maxLength, "%s/resources", wd);
#endif
}

const char * documentsPath()
{
#if TARGET_OS_IPHONE
   NSString * docPath = NSSearchPathForDirectoriesInDomains(NSDocumentDirectory, NSUserDomainMask, YES)[0];
   return [docPath cStringUsingEncoding:NSASCIIStringEncoding];
#else
   return "/tmp/cb/";
#endif
}

void dlog(const char *format , ... )
{
   char tolog[512];
   
   va_list arglist;
   va_start( arglist, format );
   vprintf( format, arglist );
   va_end( arglist );
   printf("\n");
}

