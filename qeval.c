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
#define VERSION "0.1"               /* < QEVAL current version                        */
#define MIN_FILE_SIZE (0L)          /* < minimum size for loadable files (in bytes)   */
#define MAX_FILE_SIZE (1024L*1024L) /* < maximum size for loadable files (in bytes)   */
#define MAX_ERROR_COUNT 32          /* < maximum number of errors able to be reported */


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
typedef enum ErrorID_ {
    SUCCESS=0, ERR_FILE_NOT_FOUND=-1000, ERR_FILE_TOO_BIG, ERR_NOT_ENOUGH_MEMORY, ERR_INT8_OUT_OF_RANGE,
    ERR_INT16_OUT_OF_RANGE, ERR_BITNUM_OUT_OF_RANGE, ERR_INVALID_EXPRESSION, ERR_INVALID_IMODE,
    ERR_UNEXPECTED_PARENTHESIS, ERR_INVALID_RST_ADDRESS, ERR_DISP_MUST_BE_ZERO,
    ERR_DISP_OUT_OF_RANGE, ERR_UNKNOWN_PARAM
} ErrorID;

typedef struct Error_ {
    const utf8 *filename;
    int         line;
    ErrorID     id;
} Error;

typedef enum {
    UNSOLVED, STRING, INTEGER
} ExpressionType;

typedef struct Expression_ {
    ExpressionType type;
    const utf8    *start;
    const utf8    *end;
    int            stack1len;
    int            stack2len;
    char           stacks[1];
    int            user1;
    int            user2;
    struct Expression_  *nextDelayed;
} Expression;


/*=================================================================================================================*/
#pragma mark - > HANDLING ERRORS

static Error       theErrors[MAX_ERROR_COUNT];
static const utf8 *theFileName   = NULL;
static int         theLineNumber = 0;
static int         theErrorCount = 0;

static Bool err(ErrorID errorID) {
    Error *error;
    if (theErrorCount<MAX_ERROR_COUNT) {
        error = &theErrors[theErrorCount++];
        error->filename = theFileName;
        error->line     = theLineNumber;
        error->id       = errorID;
    }
    return (errorID==SUCCESS);
}

static Bool err_PrintErrorMessages(void) {
    const utf8 *message; Error *error=theErrors; int count=theErrorCount;
    while (--count>=0) {
        switch (error->id) {
            case SUCCESS:                message = "SUCCESS"; break;
            case ERR_FILE_NOT_FOUND:     message = "File not found";    break;
            case ERR_FILE_TOO_BIG:       message = "File too big";      break;
            case ERR_NOT_ENOUGH_MEMORY:  message = "Not enough memory"; break;
            case ERR_INT8_OUT_OF_RANGE:  message = "The 8-bit value is out of range"; break;
            case ERR_INT16_OUT_OF_RANGE: message = "The 16-bit value is out of range"; break;
            case ERR_BITNUM_OUT_OF_RANGE:message = "The bit number is out of range (valid range is: 0-7)"; break;
            case ERR_INVALID_EXPRESSION: message = "Invalid expression"; break;
            case ERR_INVALID_IMODE:      message = "Invalid interruption mode"; break;
            case ERR_UNEXPECTED_PARENTHESIS: message = "Unexpected ')' (closing parenthesis)"; break;
            case ERR_INVALID_RST_ADDRESS:message = "Invalid RST address"; break;
            case ERR_DISP_MUST_BE_ZERO:  message = "No displacement can be added to the index register"; break;
            case ERR_DISP_OUT_OF_RANGE:  message = "The displacement is out of range"; break;
            case ERR_UNKNOWN_PARAM:      message = "Unknown parameter"; break;
            default:                     message = "Unknown error";     break;
        }
        if (error->line) { printf("%s:%d: %s\n", error->filename, error->line, message); }
        else             { printf("%s\n", message); }
        ++error;
    }
    return (theErrorCount>0);
}

/*=================================================================================================================*/
#pragma mark - > EXPRESSION

Expression * theDelayedList;
Expression * theUnsolvedList;


static void exp_AddDelayedExpression(Expression *expression) {

}

static void exp_AddUnsolvedExpression(Expression *expression) {

}

static void exp_AddNamedExpression(const utf8 *name, Expression *expression) {

}

#define exp_GetFirstDelayedExpression() theDelayedList

#define exp_GetNextDelayedExpression(expression) ((expression)->nextDelayed)


/*=================================================================================================================*/
#pragma mark - > QEVAL

/*
    named-map     :  name -> [Expression]
    delayed-list  :  [Expressions]
    unsolved-list :  [Expressions]


    LD HL, delayed           -> exp = Solve("delayed"); AddToDelayList(exp, address, linenum)
    delayed  EQU constant*2  -> exp = Solve("constant*2"); AddToDelayList(exp,0,linenum); SetConst(delayed, exp)
    constant EQU 100+10      -> exp = Solve("100+10"); SetConst(constant, exp)
    LD HL, constant          -> exp = Solve("constant"); (*address) = ToInt16(exp)


*/



static Expression qeval_BeginSolving(const utf8 *exp_start, const utf8 *exp_end) {
    Expression expression;
    expression.type = UNSOLVED;
    return expression;

}

static void qeval_ContinueSolving(Expression expression) {
}

static void qeval_AddToSolveList(Expression *expression, int user1, int user2) {
    expression->user1 = user1;
    expression->user2 = user2;
    /*
    expression->prev = last;
    last->next = expression;
    */

}

static void qeval_SetConst(const utf8 *name, Expression *expression) {

    if ( expression->type == UNSOLVED ) { exp_AddDelayedExpression(expression); }
    exp_AddNamedExpression(name, expression);
}



static Bool qeval_ToInt16(const Expression *expression, int *out_integer) {
    (*out_integer) = 1;
    return TRUE;
}

static const Bool qeval_ToString(const Expression *expression, utf8 *buffer, int bufferSize) {
    strcpy(buffer,"");
    return TRUE;
}


/*=================================================================================================================*/
#pragma mark - > MAIN


static void main_InitGlobals() {
    theFileName     = NULL;
    theLineNumber   = 0;
    theErrorCount   = 0;
    theDelayedList  = NULL;
    theUnsolvedList = NULL;
}

static Bool main_EvaluateTextLine(const utf8 *start, const utf8 **end) {

    /* detect output directive: "PRINT", "?" */

    /* detect var assignation "=" */


    if (end) { *end = start+1; }
    return TRUE;
}

static Bool main_EvaluateFile(const utf8 *filename) {
    FILE *file; long fileSize;
    utf8 *fileBuffer;
    const utf8 *ptr;

    /* reset globals */
    theFileName   = filename;
    theLineNumber = 0;

    /* load the whole file into the buffer */
    file = fopen(filename,"rb");
    if ( !file ) { return err(ERR_FILE_NOT_FOUND); }
    fseek(file, 0L, SEEK_END); fileSize = ftell(file); rewind(file);
    if ( fileSize>MAX_FILE_SIZE ) { fclose(file); return err(ERR_FILE_TOO_BIG); }
    fileBuffer = malloc(fileSize+1);
    if ( !fileBuffer ) { fclose(file); return err(ERR_NOT_ENOUGH_MEMORY); }
    fread(fileBuffer, fileSize, 1, file);
    fileBuffer[fileSize]=CH_ENDFILE;
    fclose(file);

    /* evaluate code line by line */
    ptr=fileBuffer; theLineNumber=1;
    while ( *ptr!=CH_ENDFILE && theErrorCount==0 ) {
        main_EvaluateTextLine(ptr,&ptr);
        ++theLineNumber;
    }

    /* deallocate the buffer and print any error */
    free(fileBuffer);
    return TRUE;
}

static void main_PrintDelayedExpressions() {
    utf8 str[1024];
    Expression *expression = exp_GetFirstDelayedExpression();
    while (expression) {
        if ( qeval_ToString(expression,str,sizeof(str)) ) { printf("%s",str); }
        else                                              { printf("<?>");  }
        expression = exp_GetNextDelayedExpression(expression);
    }
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
        main_InitGlobals();
        main_EvaluateFile(filename);
        if (!err_PrintErrorMessages()) {
             main_PrintDelayedExpressions();
        }
    }
    return 0;
}




