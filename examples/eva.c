#define EVALUATE_IMPLEMENTATION
#include "evaluate.h"

#define VERSION   "0.1"
#define COPYRIGHT "Copyright (c) 2020 Martin Rizzo"
#define MIN_FILE_SIZE        (0)           /* < minimum size for loadable files (in bytes)            */
#define MAX_FILE_SIZE        (1024L*1024L) /* < maximum size for loadable files (in bytes)            */
#define MAX_FILES 256  /* maximum number of files to evaluate */

#define skipendofline(ptr) (ptr[0]=='\n' && ptr[1]=='\r') || (ptr[0]=='\r' && ptr[1]=='\n') ? ptr+=2 : ++ptr;
#define isOption(param,name1,name2) (strcmp(param,name1)==0 || strcmp(param,name2)==0)



static Bool evaluateTextLine(const utf8* ptr, const utf8** out_endptr, EVCTX* ctx) {
    Bool printByDefault = FALSE;
    Bool moreArguments;
    EvVariant* variant;
    utf8 name[128], *dest, *destend;

    /* skip all blank spaces at the beginning of the line */
    while ( isblank(*ptr) ) { ++ptr; }

    /* extract name */
    safecpy_begin( dest, destend, name, sizeof(name) );
    while ( !isendofcode(*ptr) && !isblank(*ptr) ) { safecpy_upper(dest,destend,ptr);  }
    safecpy_end( dest, destend );
    while (isblank(*ptr)) { ++ptr; }

    /* detect var assignation "=" */
    if ( *ptr=='=' ) {
        variant = evEvaluateExpression(ptr+1,&ptr,ctx);
        evAddConstant(name, variant, ctx);
    }
    /* detect output directive: "PRINT", "?" */
    else if (0==strcmp(name,"PRINT") || 0==strcmp(name,"?") || printByDefault) {
        moreArguments=TRUE; while(moreArguments) {
            variant       = evEvaluateExpression(ptr,&ptr,ctx);
            moreArguments = (*ptr==EVCH_PARAM_SEP); if (moreArguments) { ++ptr; }
            evDeferVariant(variant, !moreArguments, NULL, ctx);
        }
    }

    /* place pointer in the first character of the next line and return it */
    while (!isendofline(*ptr)) { ++ptr; } skipendofline(ptr);
    if (out_endptr) { (*out_endptr) = ptr; }
    return TRUE;
}


static Bool evaluateFile(const utf8* filePath, EVCTX* ctx) {
    FILE* file=NULL; long fileSize=0; utf8 *fileBuffer=NULL; const utf8 *ptr; int err=0, lineNumber;
    assert( filePath!=NULL );
    
    /* try to load the entire file to a buffer */
    if (!err) {
        file=fopen(filePath,"rb"); if (!file) { err=evErr(EVERR_FILE_NOT_FOUND,filePath,ctx); }
    }
    if (!err) {
        fseek(file,0L,SEEK_END); fileSize=ftell(file); rewind(file);
        if ( fileSize>MAX_FILE_SIZE ) { err=evErr(EVERR_FILE_TOO_LARGE,filePath,ctx); }
    }
    if (!err) {
        fileBuffer = malloc(fileSize+1); if (!fileBuffer) { err=evErr(EVERR_NOT_ENOUGH_MEMORY,0,ctx); }
    }
    if (!err) {
        if (fileSize!=fread(fileBuffer, 1, fileSize, file)) { err=evErr(EVERR_CANNOT_READ_FILE,filePath,ctx); }
        else { fileBuffer[fileSize]=EVCH_ENDOFFILE; }
    }
    if (file) { fclose(file); }
    
    /* if the file is loaded in buffer    */
    /* then evaluate all lines one by one */
    if (!err) {
        evErrBeginFile(filePath,ctx);
        lineNumber=0; ptr=fileBuffer; while ( *ptr!=EVCH_ENDOFFILE ) {
            evErrSetLineNumber(++lineNumber,ctx);
            evaluateTextLine(ptr,&ptr,ctx);
        }
        evErrEndFile(filePath,ctx);
    }
    /* release resources and return */
    free(fileBuffer);
    return !err ? TRUE : FALSE;
}


static void printLine(const EvDeferredVariant* deferred) {
    utf8 str[256]; const EvVariant* variant;
    assert( deferred!=NULL );
    
    variant = &deferred->variant;
    switch (variant->evtype) {
        case EVTYPE_EMPTY:    /* printf("<empty>"); */ break;
        case EVTYPE_UNSOLVED: printf("<..?..>"); break;
        case EVTYPE_INUMBER:  printf("%d",variant->inumber.value); break;
        case EVTYPE_FNUMBER:  printf("%g",variant->fnumber.value); break;
        case EVTYPE_ASTRING:
        case EVTYPE_CSTRING:
            ev_variantToString(variant,str,sizeof(str)); printf("%s",str); break;
    }
    printf(deferred->userValue==1 ? "\n" : " ");
}



int main(int argc, char *argv[]) {
    int i; EvDeferredVariant* deferred;
    const utf8 *param;
    const utf8 *help[] = {
        "USAGE: qeval [options] file-to-evaluate","",
        "  OPTIONS:",
        "    -h, --help            display this help and exit",
        "    -v, --version         output version information and exit",
        NULL
    };
    
    EVCTX* ctx               = evCreateContext();
    int  numberOfFiles       = 0;
    Bool printHelpAndExit    = argc<=1;
    Bool printVersionAndExit = FALSE;
    const utf8 *filePaths[MAX_FILES]; memset(filePaths,0,sizeof(filePaths));
    
    ctx = evCreateContext();
    
    /* process all parameters */
    for (i=1; i<argc; ++i) { param=argv[i];
        if ( param[0]!='-' ) { if (numberOfFiles<MAX_FILES) { filePaths[numberOfFiles++]=param; } }
        else if ( isOption(param,"-h","--help" ) )   { printHelpAndExit = TRUE; }
        else if ( isOption(param,"-v","--version") ) { printVersionAndExit = TRUE; }

    }
    
    /* print help or version if requested */
    if ( printHelpAndExit    ) { i=0; while (help[i]!=NULL) { printf("%s\n",help[i++]); } return 0; }
    if ( printVersionAndExit ) { printf("QEVAL version %s\n%s\n", VERSION, COPYRIGHT);    return 0; }
    
    /* evaluate any file provided in the command line */
    for (i=0; i<numberOfFiles; ++i) {
        evaluateFile(filePaths[i],ctx);
    }
    /* print all lines stored as deferred variants */
    for ( deferred=evGetFirstDeferredVariant(ctx); deferred; deferred=evGetNextDeferredVariant(deferred,ctx) ) {
        printLine(deferred);
    }
    evErrPrintErrors(ctx);
    evDestroyContext(ctx);
    return 0;
}




