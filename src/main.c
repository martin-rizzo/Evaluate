#include "qeval.h"
#define MAX_FILES 256  /* maximum number of files to evaluate */
#define isOption(param,name1,name2) (strcmp(param,name1)==0 || strcmp(param,name2)==0)



static Bool evaluateTextLine(const utf8* ptr, const utf8** out_endptr) {
    Bool printByDefault = FALSE;
    Bool continueScanning;
    Variant* variant;
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
        variant = evaluateExpression(ptr+1,&ptr);
        addVariantToMap(&theNameMap, variant, name);
    }
    /* detect output directive: "PRINT", "?" */
    else if (0==strcmp(name,"PRINT") || 0==strcmp(name,"?") || printByDefault) {
        do {
            variant          = evaluateExpression(ptr,&ptr);
            continueScanning = (*ptr==CH_PARAM_SEP); if (continueScanning) { ++ptr; }
            addDeferredOutput(!continueScanning,variant);
        } while (continueScanning);
    }

    /* place pointer in the first character of the next line and return it */
    while (!isendofline(*ptr)) { ++ptr; } skipendofline(ptr);
    if (out_endptr) { (*out_endptr) = ptr; }
    return TRUE;
}

static Bool evaluateFile(const utf8* filePath) {
    FILE* file=NULL; long fileSize=0; utf8 *fileBuffer=NULL; const utf8 *ptr;
    const utf8* prevFilePath; int prevLineNumber;
    assert( filePath!=NULL );
    
    /* try to load the entire file to a buffer */
    if (success) {
        file=fopen(filePath,"rb"); if (!file) { error(ERR_FILE_NOT_FOUND,filePath); }
    }
    if (success) {
        fseek(file,0L,SEEK_END); fileSize=ftell(file); rewind(file);
        if ( fileSize>MAX_FILE_SIZE ) { error(ERR_FILE_TOO_LARGE,filePath); }
    }
    if (success) {
        fileBuffer = malloc(fileSize+1); if (!fileBuffer) { error(ERR_NOT_ENOUGH_MEMORY,0); }
    }
    if (success) {
        if (fileSize!=fread(fileBuffer, 1, fileSize, file)) { error(ERR_CANNOT_READ_FILE,filePath); }
        else { fileBuffer[fileSize]=CH_ENDFILE; }
    }
    if (file) { fclose(file); }
    
    /* if the file is loaded in buffer    */
    /* then evaluate all lines one by one */
    if (success) {
        /* q_beginFile(filePath); */
        prevFilePath   = g.curFilePath   ; g.curFilePath   = filePath;
        prevLineNumber = g.curLineNumber ; g.curLineNumber = 1;
        ptr = fileBuffer; while ( *ptr!=CH_ENDFILE && success ) {
            /* q_evaluateTextLine(++curLineNumber,ptr,&ptr); */
            evaluateTextLine(ptr,&ptr);
            ++g.curLineNumber;
        }
        /* q_endFile(filePath); */
        g.curFilePath   = prevFilePath;
        g.curLineNumber = prevLineNumber;
    }
    /* release resources and return */
    free(fileBuffer);
    return success ? TRUE : FALSE;
}


int main(int argc, char *argv[]) {
    int i;
    const utf8 *param;
    const utf8 *help[] = {
        "USAGE: qeval [options] file-to-evaluate","",
        "  OPTIONS:",
        "    -h, --help            display this help and exit",
        "    -v, --version         output version information and exit",
        NULL
    };
    
    int  numberOfFiles       = 0;
    Bool printHelpAndExit    = argc<=1;
    Bool printVersionAndExit = FALSE;
    const utf8 *filePaths[MAX_FILES]; memset(filePaths,0,sizeof(filePaths));
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
    if (numberOfFiles>0) {
        init();
        for (i=0; i<numberOfFiles; ++i) { evaluateFile(filePaths[i]); }
        if (!printErrorMessages()) { printDeferredOutput(); }
    }
    stallocFreeAll();
    return success ? 0 : -1;
}




