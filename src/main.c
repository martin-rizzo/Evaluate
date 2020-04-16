#include "qeval.h"
#define MAX_FILES 256  /* maximum number of files to evaluate */
#define isOption(param,name1,name2) (strcmp(param,name1)==0 || strcmp(param,name2)==0)



static Bool evaluateTextLine(const utf8* ptr, const utf8** out_endptr) {
    Bool printByDefault = FALSE;
    Bool moreArguments;
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
        variant = qEvaluateExpression(ptr+1,&ptr);
        qAddConstant(name, variant);
    }
    /* detect output directive: "PRINT", "?" */
    else if (0==strcmp(name,"PRINT") || 0==strcmp(name,"?") || printByDefault) {
        moreArguments=TRUE; while(moreArguments) {
            variant       = qEvaluateExpression(ptr,&ptr);
            moreArguments = (*ptr==CH_PARAM_SEP); if (moreArguments) { ++ptr; }
            addDeferredOutput(!moreArguments,variant);
        }
    }

    /* place pointer in the first character of the next line and return it */
    while (!isendofline(*ptr)) { ++ptr; } skipendofline(ptr);
    if (out_endptr) { (*out_endptr) = ptr; }
    return TRUE;
}


static Bool evaluateFile(const utf8* filePath) {
    FILE* file=NULL; long fileSize=0; utf8 *fileBuffer=NULL; const utf8 *ptr; int err=0, lineNumber;
    assert( filePath!=NULL );
    
    /* try to load the entire file to a buffer */
    if (!err) {
        file=fopen(filePath,"rb"); if (!file) { err=qerror(ERR_FILE_NOT_FOUND,filePath); }
    }
    if (!err) {
        fseek(file,0L,SEEK_END); fileSize=ftell(file); rewind(file);
        if ( fileSize>MAX_FILE_SIZE ) { err=qerror(ERR_FILE_TOO_LARGE,filePath); }
    }
    if (!err) {
        fileBuffer = malloc(fileSize+1); if (!fileBuffer) { err=qerror(ERR_NOT_ENOUGH_MEMORY,0); }
    }
    if (!err) {
        if (fileSize!=fread(fileBuffer, 1, fileSize, file)) { err=qerror(ERR_CANNOT_READ_FILE,filePath); }
        else { fileBuffer[fileSize]=CH_ENDFILE; }
    }
    if (file) { fclose(file); }
    
    /* if the file is loaded in buffer    */
    /* then evaluate all lines one by one */
    if (!err) {
        qerrorBeginFile(filePath);
        lineNumber=0; ptr=fileBuffer; while ( *ptr!=CH_ENDFILE ) {
            qerrorSetLineNumber(++lineNumber);
            evaluateTextLine(ptr,&ptr);
        }
        qerrorEndFile(filePath);
    }
    /* release resources and return */
    free(fileBuffer);
    return !err ? TRUE : FALSE;
}


static void printDeferredOutput() {
    utf8 str[256]; const DeferredOutput* output;
    for ( output=getFirstDeferredOutput(); output; output=getNextDeferredOutput(output) ) {
        switch (output->variant.type) {
            case TYPE_EMPTY:    /* printf("<empty>"); */ break;
            case TYPE_UNSOLVED: printf("<..?..>"); break;
            case TYPE_INUMBER:  printf("%d",output->variant.inumber.value); break;
            case TYPE_FNUMBER:  printf("%g",output->variant.fnumber.value); break;
            case TYPE_ASTRING:
            case TYPE_CSTRING:
                variantToString(&output->variant,str,sizeof(str));
                printf("%s",str);
                break;
        }
        printf(output->userValue==1 ? "\n" : " ");
    }
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
        for (i=0; i<numberOfFiles; ++i) { evaluateFile(filePaths[i]);  }
        if (!qPrintAllErrors()) { printDeferredOutput(); }
    }
    permallocFreeAll();
    return 0;
}




