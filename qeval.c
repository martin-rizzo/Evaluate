/**
 * @file       qeval.c
 * @date       Jan 39, 2020
 * @author     Martin Rizzo | <martinrizzo@gmail.com>
 * @copyright  Copyright (c) 2020 Martin Rizzo.
 *             This project is released under the MIT License.
 * -------------------------------------------------------------------------
 *  QEVAL - Quick eval
 * -------------------------------------------------------------------------
 *  Copyright (c) 2020 Martin Rizzo
 *
 *  Permission is hereby granted, free of charge, to any person obtaining
 *  a copy of this software and associated documentation files (the
 *  "Software"), to deal in the Software without restriction, including
 *  without limitation the rights to use, copy, modify, merge, publish,
 *  distribute, sublicense, and/or sell copies of the Software, and to
 *  permit persons to whom the Software is furnished to do so, subject to
 *  the following conditions:
 *
 *  The above copyright notice and this permission notice shall be
 *  included in all copies or substantial portions of the Software.
 *
 *  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 *  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 *  MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 *  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
 *  CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
 *  TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
 *  SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 * -------------------------------------------------------------------------
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#define VERSION "0.1"               /* < QEVAL current version                      */
#define MIN_FILE_SIZE (0L)          /* < minimum size for loadable files (in bytes) */
#define MAX_FILE_SIZE (1024L*1024L) /* < maximum size for loadable files (in bytes) */


#define PARAM_IS(param,name1,name2) (strcmp(param,name1)==0 || strcmp(param,name2)==0)

typedef char           utf8;              /* < unicode variable width character encoding */
typedef int Bool; enum { FALSE=0, TRUE }; /* < Boolean */


/* some standard characters */
enum EvalCharacter {
    CH_ENDFILE       ='\0' , CH_ENDLABEL    =':' , CH_STARTCOMMENT=';'  , CH_SPACE    =' '  ,
    CH_PARAM_SEP     =','  , CH_EXT_SEP     ='.' , CH_PATH_SEP    ='/'  , CH_PATH_SEP2='\\' ,
    CH_STRING        ='\'' , CH_CSTR        ='"' , CH_STRINGESC   ='\\' ,
    CH_HEXPREFIX_ADDR='$'  , CH_HEXPREFIX2  ='#' , CH_BINPREFIX   ='%'  ,
    CH_BEG_STRING = '\''   , CH_END_STRING  = '\'',
    CH_BEG_NAME   = '$'    , CH_END_NAME    = ';',
    CH_OPTIONAL_DIREC_PREFIX = '.'
};

/* supported errors */
enum Error {
    SUCCESS=0, ERR_FILE_NOT_FOUND=-1000, ERR_FILE_TOO_BIG, ERR_NOT_ENOUGH_MEMORY, ERR_INT8_OUT_OF_RANGE,
    ERR_INT16_OUT_OF_RANGE, ERR_BITNUM_OUT_OF_RANGE, ERR_INVALID_EXPRESSION, ERR_INVALID_IMODE,
    ERR_UNEXPECTED_PARENTHESIS, ERR_INVALID_RST_ADDRESS, ERR_DISP_MUST_BE_ZERO,
    ERR_DISP_OUT_OF_RANGE, ERR_UNKNOWN_PARAM
} error;



/*=================================================================================================================*/
#pragma mark - > MAIN


static void main_EvaluateFile(const utf8 *filename) {
    FILE *file; long fileSize;
    utf8 *fileBuffer;

    /* load the whole file into the buffer */
    file = fopen(filename,"rb");
    if ( !file ) { error=ERR_FILE_NOT_FOUND; return; }
    fseek(file, 0L, SEEK_END); fileSize = ftell(file); rewind(file);
    if ( fileSize>MAX_FILE_SIZE ) { fclose(file); error=ERR_FILE_TOO_BIG; return; }
    fileBuffer = malloc(fileSize+1);
    if ( !fileBuffer ) { fclose(file); error=ERR_NOT_ENOUGH_MEMORY; return; }
    fread(fileBuffer, fileSize, 1, file);
    fileBuffer[fileSize]=CH_ENDFILE;
    fclose(file);

    /* evaluate code line by line */

    /* ... */

    /* deallocate the buffer */
    free(fileBuffer);
}


/*=================================================================================================================*/
#pragma mark - > MAIN

int main(int argc, char *argv[]) {
    int i;
    Bool printHelpAndExit=FALSE, printVersionAndExit=FALSE;
    const utf8 *filename=NULL, *param;
    const utf8 *help[] = {
        "USAGE: qeval [options] file-to-evaluate","",
        "  OPTIONS:",
        "    -h, --help            display this help and exit",
        "    -v, --version         output version information and exit",
        NULL
    };

    /* process all parameters */
    for (i=1; i<argc; ++i) { param=argv[i];
        if ( param[0]!='-' ) { filename=param; }
        else if ( PARAM_IS(param,"-h","--help" ) )   { printHelpAndExit = TRUE; }
        else if ( PARAM_IS(param,"-v","--version") ) { printVersionAndExit = TRUE; }

    }

    if ( printHelpAndExit ) {
        i=0; while (help[i]!=NULL) { printf("%s\n",help[i++]); }
        return 0;
    }
    if ( printVersionAndExit ) {
        printf("QEVAL version %s\n", VERSION);
        printf("Copyright (c) 2020 Martin Rizzo\n");
        return 0;
    }
    /* if a filename was provided then evaluate it! */
    if (filename) {
        main_EvaluateFile(filename);
    }
    return 0;
}




