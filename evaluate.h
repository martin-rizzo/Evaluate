/**
 * @file       evaluate.h
 * @date       Jan 29, 2020
 * @author     Martin Rizzo | <martinrizzo@gmail.com>
 * @copyright  Copyright (c) 2020 Martin Rizzo.
 *             This project is released under the MIT License.
 * -------------------------------------------------------------------------
 *  Evaluate - A portable, one-header library to evaluate math expressions
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
#ifndef EVALUATE_H_INCLUDED
#define EVALUATE_H_INCLUDED

#ifdef NDEBUG
#    define EVLOG(x)
#else
#    define EVLOG(x) printf x; printf("\n")
#endif

typedef char utf8;                         /* < unicode variable width character encoding */
typedef int Bool; enum { FALSE=0, TRUE };  /* < Boolean */

/** Identifier of errors generated during expression evaluation */
typedef enum EVERR {
    EVERR_NO_ERROR=0,          EVERR_FILE_NOT_FOUND=-1000, EVERR_FILE_TOO_LARGE,     EVERR_CANNOT_READ_FILE,
    EVERR_NOT_ENOUGH_MEMORY,   EVERR_INVALID_EXPRESSION,   EVERR_UNEXPECTED_PTHESIS, EVERR_TOO_MANY_OPEN_PTHESES
} EVERR;


/** Identifier of the type of value contained in a EvVariant */
typedef enum  EVTYPE { EVTYPE_UNSOLVED, EVTYPE_EMPTY, EVTYPE_ASTRING, EVTYPE_CSTRING, EVTYPE_INUMBER, EVTYPE_FNUMBER } EVTYPE;

/** Union that can be used to represent any data type (integer number, float number, c-string, asm-string, etc... */
typedef union EvVariant {
    EVTYPE evtype;
    struct { EVTYPE evtype;                          } empty;     /* < empty data             */
    struct { EVTYPE evtype; int   value;             } inumber;   /* < integer number         */
    struct { EVTYPE evtype; float value;             } fnumber;   /* < floating-point number  */
    struct { EVTYPE evtype; const utf8 *begin, *end; } astring;   /* < assembler string       */
    struct { EVTYPE evtype; const utf8 *begin, *end; } cstring;   /* < C string               */
    struct { EVTYPE evtype; const utf8 *begin, *end; } unsolved;  /* < unsolved expression    */
} EvVariant;

typedef struct EvDeferredVariant {
    EvVariant                 variant;
    int                       userValue;
    void*                     userPtr;
    struct EvDeferredVariant* next;
} EvDeferredVariant;

#define EVCTX void

extern EVCTX * evCreateContext(void);
extern void    evDestroyContext(EVCTX* ctx);

extern void        evAddConstant(const utf8* name, const EvVariant* value, EVCTX* ctx);
extern EvVariant * evEvaluateExpression(const utf8 *start, const utf8 **out_end, EVCTX* ctx);

extern EvDeferredVariant* evDeferVariant(const EvVariant* variant, int userValue, void* userPtr, EVCTX* ctx);
extern EvDeferredVariant* evGetFirstDeferredVariant(EVCTX* ctx);
extern EvDeferredVariant* evGetNextDeferredVariant(EvDeferredVariant* deferred, EVCTX* ctx);

extern int  evErr(EVERR everr, const utf8 *str, EVCTX* ctx);
extern void evErrBeginFile(const utf8* filePath, EVCTX* ctx);
extern void evErrEndFile(const utf8* filePath, EVCTX* ctx);
extern void evErrSetLineNumber(int lineNumber, EVCTX* ctx);
extern Bool evErrPrintErrors(EVCTX* ctx);


/*=================================================================================================================*/
#pragma mark - > IMPLEMENTATION

#ifdef EVALUATE_IMPLEMENTATION
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>

#define EV_PERMALLOC_CHUNK_SIZE (64*1024)     /**< size of each chunk of memory managed by permalloc   */
#define EV_MAX_NAME_LEN         (63)          /**< maximum length for names of vars (in bytes)         */
#define EV_CALC_STACK_SIZE      (256)         /**< the calculator's stack size in number of elements   */
#define EV_MIN_PRECEDENCE       (1)           /**< the minimum valid operator predecence               */
#define EV_NUMBER_OF_MAP_SLOTS  (77)          /**< number of slots used in the hashmap                 */

/** Some special characters used in expressions */
typedef enum EVCH {
    EVCH_ENDOFFILE     ='\0' , EVCH_STARTCOMMENT=';',  EVCH_PARAM_SEP    =',',
    EVCH_ASMSTRING     ='\'' , EVCH_CSTRING     ='"' , EVCH_STRING_ESCAPE='\\' ,
    EVCH_HEXPREFIX_ADDR='$'  , EVCH_HEXPREFIX   ='#' , EVCH_BINPREFIX    ='%'
} StdChars_;

/** The information hidden behind the EVCTX pointer */
typedef struct Ev_Context {
    struct Ev_PermallocContext* permactx;
    struct Ev_VariantMap*       constantsMap;
    struct Ev_DeferredList*     deferredList;
    struct Ev_ErrorLine*        curErrorLine;
    struct Ev_Error*            firstError;
    struct Ev_Error*            lastError;
} Ev_Context;


/*=================================================================================================================*/
#pragma mark - > HELPER FUNCTIONS

#define INRANGE(c,min,max) (min<=c && c<=max)

#define safecpy_begin(dest,destend,buf,size) dest=&buf[0]; destend=&buf[size-1];
#define safecpy(dest,destend,ptr)  if (dest<destend) { *dest++ = *ptr; } ++ptr;
#define safecpy_char(dest,destend,ch) if (dest<destend) { *dest++ = ch; }
#define safecpy_two(dest,destend,ptr) if (dest<(destend-1)) { *dest++=ptr[0]; *dest++=ptr[1]; } ptr+=2;
#define safecpy_upper(dest,destend,ptr) if (dest<destend) { *dest++ = toupper(*ptr); } ++ptr;
#define safecpy_end(dest,destend) dest[0]='\0'

/* identifying characters */
static int isbin      (int ch) { return ch=='0' || ch=='1'; }
static int ishex      (int ch) { return INRANGE(ch,'0','9') || INRANGE(ch,'A','F') || INRANGE(ch,'a','f'); }
static int isname     (int ch) { return isalnum(ch) || ch=='_' || ch=='@' || ch=='.' || ch=='$' || ch=='#'; }
static int isliteral  (int ch) { return isalnum(ch) || ch=='_' || ch==EVCH_HEXPREFIX_ADDR || ch==EVCH_HEXPREFIX || ch==EVCH_BINPREFIX; }
static int isendofcode(int ch) { return ch=='\r' || ch=='\n' || ch==EVCH_STARTCOMMENT || ch==EVCH_ENDOFFILE; }
static int isendofline(int ch) { return ch==EVCH_ENDOFFILE || ch=='\r' || ch=='\n'; }
static int hexvalue   (int ch) { return INRANGE(ch,'0','9') ? ch-'0' : INRANGE(ch,'A','F') ? ch-'A'+10 : INRANGE(ch,'a','f') ? ch-'a'+10 : -1; }
static int octvalue   (int ch) { return INRANGE(ch,'0','7') ? ch-'0' : -1; }
#define skipblankspaces(ptr) while ( isblank(*ptr) ) { ++ptr; }


/*=================================================================================================================*/
#pragma mark - > PERMANENT ALLOCATION (PERMALLOC)
#define CTX(x) (ctx->x)

typedef struct Ev_PermallocChunk {
    char data[EV_PERMALLOC_CHUNK_SIZE]; struct Ev_PermallocChunk* next; } Ev_PermallocChunk;

typedef struct Ev_PermallocContext {
    Ev_PermallocChunk* chunk; char* ptr; int remaining; } Ev_PermallocContext;

/**
 * Creates and returns a new permanent allocation context
 *
 * Each memory block allocated with 'ev_permalloc(..)' will belong to a context
 * and when a context is destroyed all memory blocks that belong to it are freed.
 * That is why permanent allocated memory don't need to be manually deallocated.
 * Note: in multi-thread code, you should use a different context for each thread.
 */
static Ev_PermallocContext* ev_permallocCreateContext() {
    Ev_PermallocContext* ctx = malloc(sizeof(Ev_PermallocContext));
    CTX(chunk)     = NULL;
    CTX(ptr)       = NULL;
    CTX(remaining) = 0;
    return ctx;
}

/**
 * Destroys a permanent allocation context and frees all memory blocks that belong to it
 */
static void ev_permallocDestroyContext(Ev_PermallocContext* ctx) {
    Ev_PermallocChunk *chunk, *nextChunk=NULL;
    if (ctx==NULL) { return; }
    for (chunk=CTX(chunk); chunk; chunk=nextChunk) { nextChunk=chunk->next; free(chunk); }
    free(ctx);
}

/**
 * Allocates a block of memory that stays valid until the context is destroyed
 *
 * @param size The size of the memory block to alloc, in bytes
 * @param ctx  The context where the memory block will be allocated
 * @return A pointer to the beginning of the new allocated memory block
 */
static void* ev_permalloc(int size, Ev_PermallocContext* ctx) {
    Ev_PermallocChunk* prevPermallocChunk; void* ptr;
    if ( CTX(remaining)<size ) {
        prevPermallocChunk=CTX(chunk); CTX(chunk)=malloc(sizeof(Ev_PermallocChunk));
        if (prevPermallocChunk) { prevPermallocChunk->next = CTX(chunk); }
        CTX(ptr)       = CTX(chunk)->data;
        CTX(remaining) = EV_PERMALLOC_CHUNK_SIZE;
    }
    /* allocates the first 'size' bytes counting from 'thePermallocPtr' */
    ptr=CTX(ptr); CTX(ptr)+=size; CTX(remaining)-=size;
    return ptr;
}

/**
 * Allocates a string that is the result of replace a character from the provided replacement
 *
 * If the replacement pointer is NULL then the original string will be duplicated without any modification.
 * As any permalloc operation, the allocated string stays valid until the context is destroyed.
 * @param string         The original string
 * @param charToReplace  The character to be replaced
 * @param replacement    The string that replaces the found character (optional, can be NULL)
 * @param ctx            The context where the string will be allocated
 * @returns A pointer to the new allocated string
 */
static const utf8* ev_permallocString(const utf8* string, int charToReplace, const utf8* replacement, Ev_PermallocContext* ctx) {
    utf8 *buffer, *dest; const utf8 *ptr=string;
    assert( string!=NULL );
    
    dest = buffer = ev_permalloc( (int)strlen(string) + (replacement!=NULL ? (int)strlen(replacement) : 0) + 1, ctx);
    ptr=string; while (*ptr!='\0' && *ptr!=charToReplace) { *dest++=*ptr++; }
    if (*ptr==charToReplace && replacement!=NULL) { ++ptr; while (*replacement!='\0') { *dest++=*replacement++; } }
    while (*ptr!='\0') { *dest++=*ptr++; }
    *dest='\0'; return buffer;
}

#undef CTX


/*=================================================================================================================*/
#pragma mark - > HANDLING ERRORS
#define CTX(member) (((Ev_Context*)ctx)->member)

typedef struct Ev_ErrorLine { const utf8* permaPath; int number; struct Ev_ErrorLine* prev;    } Ev_ErrorLine;
typedef struct Ev_Error { EVERR id; const utf8* str; Ev_ErrorLine line; struct Ev_Error* next; } Ev_Error;

/**
 * Reports a error
 * @param everr    The evaluation error identifier, ex: EVERR_INVALID_EXPRESSION
 * @param str      The optional text attached to the error reported (it can be NULL)
 */
int evErr(EVERR everr, const utf8 *str, EVCTX* ctx) {
    const utf8* message; Ev_Error* newError;
    assert( ctx!=NULL );
    switch (everr) {
        case EVERR_NO_ERROR:             message = "SUCCESS"; break;
        case EVERR_FILE_NOT_FOUND:       message = "file not found";    break;
        case EVERR_FILE_TOO_LARGE:       message = "the file '$' is too large"; break;
        case EVERR_CANNOT_READ_FILE:     message = "can not read file '$'"; break;
        case EVERR_NOT_ENOUGH_MEMORY:    message = "not enough memory"; break;
        case EVERR_INVALID_EXPRESSION:   message = "invalid expression"; break;
        case EVERR_UNEXPECTED_PTHESIS:   message = "unexpected ')' (closing parenthesis)"; break;
        case EVERR_TOO_MANY_OPEN_PTHESES:message = "too many open parentheses"; break;
        default:                         message = "unknown error";     break;
    }
    newError = ev_permalloc(sizeof(Ev_Error), CTX(permactx));
    newError->id             = everr;
    newError->line.permaPath = CTX(curErrorLine) ? CTX(curErrorLine)->permaPath : 0;
    newError->line.number    = CTX(curErrorLine) ? CTX(curErrorLine)->number    : 0;
    newError->str            = ev_permallocString(message,'$',str,CTX(permactx));
    newError->next           = NULL;
    if (!CTX(firstError)) { CTX(firstError)=newError; }
    CTX(lastError) = CTX(lastError) ? (CTX(lastError)->next=newError) : newError;
    return everr;
}

void evErrBeginFile(const utf8* filePath, EVCTX* ctx) {
    Ev_ErrorLine* newErrorLine;
    assert( filePath!=NULL && ctx!=NULL );
    newErrorLine = malloc(sizeof(Ev_ErrorLine));
    newErrorLine->permaPath = ev_permallocString(filePath,0,0,CTX(permactx));
    newErrorLine->number    = 0;
    newErrorLine->prev      = CTX(curErrorLine);
    CTX(curErrorLine) = newErrorLine;
}

void evErrEndFile(const utf8* filePath, EVCTX* ctx) {
    Ev_ErrorLine* prevErrorLine;
    assert( filePath!=NULL && ctx!=NULL );
    assert( CTX(curErrorLine)!=NULL && strcmp(CTX(curErrorLine)->permaPath,filePath)==0 );
    prevErrorLine = CTX(curErrorLine)->prev;
    free(CTX(curErrorLine)); CTX(curErrorLine)=prevErrorLine;
}

void evErrSetLineNumber(int lineNumber, EVCTX* ctx) {
    assert( ctx!=NULL );
    CTX(curErrorLine)->number = lineNumber;
}

Bool evErrPrintErrors(EVCTX* ctx) {
    Ev_Error* error; const int column=0;
    assert( ctx!=NULL );
    for (error=CTX(firstError); error; error=error->next) {
        if (error->line.permaPath!=NULL && error->line.number>0) {
            if (column>0) { printf("%s:%d:%d: ", error->line.permaPath, error->line.number, column); }
            else          { printf("%s:%d: "   , error->line.permaPath, error->line.number);         }
        }
        printf("%s %s\n", "error:", error->str);
    }
    return (CTX(firstError)!=NULL);
}

/*=================================================================================================================*/
#pragma mark - > OPERATORS

/** Identifier of supported operators */
typedef enum EVOP {
    EVOP_PLUS, EVOP_MINUS, EVOP_NOT, EVOP_LNOT, EVOP_ADD, EVOP_SUB, EVOP_MUL, EVOP_DIV, EVOP_MOD,
    EVOP_SHL, EVOP_SHR, EVOP_EQ, EVOP_NE, EVOP_LT, EVOP_LE, EVOP_GT, EVOP_GE,
    EVOP_AND, EVOP_XOR, EVOP_OR, EVOP_LAND, EVOP_LOR, EVOP_INVALID
} EVOP;

/** Structure containing information about an operator (id, precedence, etc..) */
typedef struct Ev_Operator { EVOP id; const utf8* str; int preced; } Ev_Operator;

/** Array containing info about each UNARY operator. The last element in {0,0,0} */
static const Ev_Operator Ev_UnaryOperators[5] = {
    {EVOP_PLUS,"+ ",12}, {EVOP_MINUS,"- ",12}, {EVOP_NOT,"~ ",12}, {EVOP_LNOT,"! ",12},
    {0,0,0}
};
/** Array containing info about each BINARY operator. The last element is {0,0,0} */
static const Ev_Operator Ev_BinaryOperators[19] = {
    {EVOP_SHL ,"<<", 9}, {EVOP_SHR   ,">>", 9},
    {EVOP_LE  ,"<=", 8}, {EVOP_GE    ,">=", 8},
    {EVOP_EQ  ,"==", 7}, {EVOP_NE    ,"!=", 7},
    {EVOP_LAND,"&&", 3},
    {EVOP_LOR ,"||", 2},
    {EVOP_MUL ,"* ",11}, {EVOP_DIV,"/ ",11}, {EVOP_MOD,"% ",11},
    {EVOP_ADD ,"+ ",10}, {EVOP_SUB,"- ",10},
    {EVOP_LT  ,"< ", 8}, {EVOP_GT ,"> ", 8},
    {EVOP_AND ,"& ", 6},
    {EVOP_XOR ,"^ ", 5},
    {EVOP_OR  ,"| ", 4},
    /* {EVOP_TERN,"?",1} */
    {0,0,0}
};

/** Special operator object used to mark parenthesis */
static const Ev_Operator Ev_Parenthesis = {EVOP_INVALID,"",(EV_MIN_PRECEDENCE-1)};

/** Special operator object used to mark limits in the evaluator stack */
static const Ev_Operator Ev_SafeGuard = {EVOP_INVALID,"",(EV_MIN_PRECEDENCE-1)};


/*=================================================================================================================*/
#pragma mark - > VARIANTS

/** Special variant object used as placeholder and delimiter */
static const EvVariant Ev_EmptyVariant = { EVTYPE_EMPTY };

static void ev_copyVariant(EvVariant* dest, const EvVariant* sour, EVCTX* ctx) {
    int length; utf8* begin;
    assert( dest!=NULL && sour!=NULL && ctx!=NULL );
    switch ( (dest->evtype=sour->evtype) ) {
        case EVTYPE_EMPTY: break;
        case EVTYPE_INUMBER: dest->inumber.value = sour->inumber.value; break;
        case EVTYPE_FNUMBER: dest->fnumber.value = sour->fnumber.value; break;
        case EVTYPE_ASTRING:
        case EVTYPE_CSTRING:
        case EVTYPE_UNSOLVED:
            length = (int)(sour->astring.end - sour->astring.begin);
            dest->astring.begin = (begin = ev_permalloc(length+1,CTX(permactx)));
            dest->astring.end   = begin + length;
            memcpy(begin, sour->astring.begin, length); begin[length]='\0';
            break;
    }
}

static Bool ev_variantToInt(const EvVariant* variant, int* i) {
    switch( variant->evtype ) {
        case EVTYPE_INUMBER: (*i)=variant->inumber.value;    return TRUE;
        case EVTYPE_ASTRING: (*i)=variant->astring.begin[0]; return TRUE;
        case EVTYPE_EMPTY:   return TRUE;
        default: return FALSE;
    }
}

static Bool ev_variantToFloat(const EvVariant* variant, float* f) {
    switch( variant->evtype ) {
        case EVTYPE_FNUMBER: (*f)=variant->fnumber.value;        return TRUE;
        case EVTYPE_INUMBER: (*f)=(float)variant->inumber.value; return TRUE;
        case EVTYPE_EMPTY:   return TRUE;
        default: return FALSE;
    }
}

static Bool ev_variantToString(const EvVariant* variant, utf8* buffer, int bufferSize) {
    utf8 *dest, *destend; const utf8 *ptr, *ptrend; int ch, digit;
    assert( variant!=NULL && buffer!=NULL && bufferSize>0 );
    switch (variant->evtype) {
            
        case EVTYPE_ASTRING:
            safecpy_begin(dest,destend,buffer,bufferSize);
            ptr=variant->astring.begin; ptrend=variant->astring.end;
            do {
                while (ptr<ptrend && *ptr!=EVCH_ASMSTRING) { safecpy(dest,destend,ptr); }
                if  (ptr<ptrend) { safecpy_char(dest,destend,EVCH_ASMSTRING); ptr+=2; }
            } while (ptr<ptrend);
            safecpy_end(dest,destend);
            return TRUE;

        case EVTYPE_CSTRING:
            safecpy_begin(dest,destend,buffer,bufferSize);
            ptr=variant->cstring.begin; while(ptr<variant->cstring.end) {
                if (ptr[0]!=EVCH_STRING_ESCAPE) { safecpy(dest,destend,ptr); }
                else {
                    switch (ptr[1]) {
                        case 'a' : safecpy_char(dest,destend,0x07); ptr+=2; break; /* audible bell */
                        case 'b' : safecpy_char(dest,destend,0x08); ptr+=2; break; /* backspce */
                        case 'e' : safecpy_char(dest,destend,0x1B); ptr+=2; break; /* escape character */
                        case 'f' : safecpy_char(dest,destend,0x0C); ptr+=2; break; /* form feed - new page */
                        case 'n' : safecpy_char(dest,destend,0x0A); ptr+=2; break; /* line feed - new line */
                        case 'r' : safecpy_char(dest,destend,0x0D); ptr+=2; break; /* carriage return */
                        case 't' : safecpy_char(dest,destend,0x09); ptr+=2; break; /* horizontal tab */
                        case 'v' : safecpy_char(dest,destend,0x0B); ptr+=2; break; /* vertical tab */
                        case '\\': safecpy_char(dest,destend,0x5C); ptr+=2; break; /* backslash */
                        case '\'': safecpy_char(dest,destend,0x27); ptr+=2; break; /* single quote */
                        case '"' : safecpy_char(dest,destend,0x22); ptr+=2; break; /* double quote */
                        case 'x' : /* a hexadecimal number */
                            if ( (digit=hexvalue(ptr[2]))>=0 ) { ch=digit; ptr+=3;
                                if ( (digit=hexvalue(*ptr))>=0 ) { ch=digit+(ch*16); ++ptr; }
                                safecpy_char(dest,destend,ch);
                            }
                            break;
                        default: /* a octal number */
                            if ( (digit=octvalue(ptr[1]))>=0 ) { ch=digit; ptr+=2;
                                if ( (digit=octvalue(*ptr))>=0 ) { ch=digit+(ch*8); ++ptr;
                                    if ( (digit=octvalue(*ptr))>=0 ) { ch=digit+(ch*8); ++ptr; }
                                }
                                safecpy_char(dest,destend,ch);
                            } else {
                                /* a unknown escape sequence */
                                safecpy_two(dest,destend,ptr);
                            }
                            break;
                    }
                }
            }
            safecpy_end(dest,destend);
            return TRUE;

        default:
            return FALSE;
    }
}

#define ev_return_INumber1(v,       op, right) v->evtype=EVTYPE_INUMBER; v->inumber.value=      op right; return 0
#define ev_return_INumber2(v, left, op, right) v->evtype=EVTYPE_INUMBER; v->inumber.value= left op right; return 1
#define ev_return_FNumber1(v,       op, right) v->evtype=EVTYPE_FNUMBER; v->fnumber.value=      op right; return 0
#define ev_return_FNumber2(v, left, op, right) v->evtype=EVTYPE_FNUMBER; v->fnumber.value= left op right; return 1

/**
 * Calculates a simple mathematical operation between two variants applying the provided operator
 * @param[out] v          Pointer to the variant where the calculation result will be stored
 * @param[in]  left       A variant containing the left operand
 * @param[in]  operator   The operation to apply
 * @param[in]  right      A variant containing the right operand
 */
static int ev_calculate(EvVariant* v, const EvVariant* left, const Ev_Operator *operator, const EvVariant* right) {
    int intL,intR; float float1,float2;
    assert( operator!=NULL && left!=NULL && right!=NULL );
    
    if ( ev_variantToInt(right,&intR) && ev_variantToInt(left,&intL) ) { switch(operator->id) {
        case EVOP_PLUS : ev_return_INumber1(v, +, intR);
        case EVOP_MINUS: ev_return_INumber1(v, -, intR);
        case EVOP_NOT  : ev_return_INumber1(v, ~, intR);
        case EVOP_LNOT : ev_return_INumber1(v, !, intR);
        case EVOP_ADD  : ev_return_INumber2(v, intL, +,intR); case EVOP_SUB: ev_return_INumber2(v, intL, -,intR);
        case EVOP_MUL  : ev_return_INumber2(v, intL, *,intR); case EVOP_DIV: ev_return_INumber2(v, intL, /,intR);
        case EVOP_SHL  : ev_return_INumber2(v, intL,<<,intR); case EVOP_SHR: ev_return_INumber2(v, intL,>>,intR);
        case EVOP_AND  : ev_return_INumber2(v, intL, &,intR); case EVOP_OR:  ev_return_INumber2(v, intL, |,intR);
        case EVOP_XOR  : ev_return_INumber2(v, intL, ^,intR); case EVOP_MOD: ev_return_INumber2(v, intL, %,intR);
        case EVOP_EQ   : ev_return_INumber2(v, intL,==,intR); case EVOP_NE : ev_return_INumber2(v, intL,!=,intR);
        case EVOP_LT   : ev_return_INumber2(v, intL,< ,intR); case EVOP_GT : ev_return_INumber2(v, intL,> ,intR);
        case EVOP_LE   : ev_return_INumber2(v, intL,<=,intR); case EVOP_GE : ev_return_INumber2(v, intL,>=,intR);
        default: assert(FALSE); break;
    } }
    else if ( ev_variantToFloat(left,&float1) && ev_variantToFloat(right,&float2) ) { switch (operator->id) {
        case EVOP_ADD: ev_return_FNumber2(v, float1, +,float2); case EVOP_SUB: ev_return_FNumber2(v, float1, -,float2);
        case EVOP_MUL: ev_return_FNumber2(v, float1, *,float2); case EVOP_DIV: ev_return_FNumber2(v, float1, /,float2);
        default: assert(FALSE); break;
    } }
    (*v) = (*right);
    return 0;
}


/*=================================================================================================================*/
#pragma mark - > VARIANT CONTAINERS

typedef struct Ev_VariantElement {
    union { int integer; const utf8* string; } key;
    EvVariant                                  variant;
    struct Ev_VariantElement*                  next;
} Ev_VariantElement;

typedef struct Ev_DeferredList { EvDeferredVariant *first, *last;              } Ev_DeferredList;
typedef struct Ev_VariantList  { Ev_VariantElement *first, *last;              } Ev_VariantList;
typedef struct Ev_VariantMap   { Ev_VariantList slots[EV_NUMBER_OF_MAP_SLOTS]; } Ev_VariantMap;


#define ev_calculateHash(hash,ptr,string) \
    hash=5381; ptr=(unsigned char*)string; while (*ptr) { hash = ((hash<<5)+hash) ^ *ptr++; }

static Ev_DeferredList* ev_permallocDeferredList(Ev_PermallocContext* ctx) {
    Ev_DeferredList* list = ev_permalloc(sizeof(Ev_DeferredList), ctx);
    list->first = list->last = NULL;
    return list;
}

static Ev_VariantMap* ev_permallocVariantMap(Ev_PermallocContext* ctx) {
    Ev_VariantMap* map = ev_permalloc(sizeof(Ev_VariantMap), ctx);
    memset(map->slots, 0, EV_NUMBER_OF_MAP_SLOTS*sizeof(Ev_VariantList));
    return map;
}

static void ev_addVariantElementToList(Ev_VariantList* list, Ev_VariantElement* elementToAdd) {
    if (list->last) { list->last = (list->last->next = elementToAdd); }
    else            { list->last = (list->first = elementToAdd);      }
}

static void ev_addVariantToListKS(Ev_VariantList* list, const EvVariant* variantToAdd, const utf8* stringKey, EVCTX* ctx) {
    Ev_VariantElement* element; int sizeofStringKey; utf8* copyofStringKey;
    assert( list!=NULL && variantToAdd!=NULL && stringKey!=NULL && stringKey[0]!='\0' && ctx!=NULL );
    
    sizeofStringKey = (int)strlen(stringKey)+1;
    copyofStringKey = ev_permalloc(sizeofStringKey,CTX(permactx));
    memcpy(copyofStringKey, stringKey, sizeofStringKey);
    
    element             = ev_permalloc(sizeof(Ev_VariantElement),CTX(permactx));
    element->key.string = copyofStringKey;
    ev_copyVariant(&element->variant, variantToAdd, ctx);
    ev_addVariantElementToList(list, element);
}

static void ev_addVariantToMap(Ev_VariantMap* map, const EvVariant* variantToAdd, const utf8* stringKey, EVCTX* ctx) {
    unsigned hash; unsigned char* tmp;
    assert( map!=NULL && variantToAdd!=NULL && stringKey!=NULL && stringKey[0]!='\0' );
    ev_calculateHash(hash,tmp,stringKey);
    ev_addVariantToListKS(&map->slots[hash%EV_NUMBER_OF_MAP_SLOTS], variantToAdd, stringKey, ctx);
}

static const EvVariant* ev_findVariantInMap(Ev_VariantMap* map, const utf8* stringKey) {
    unsigned hash; Ev_VariantElement* element; unsigned char* tmp;
    assert( map!=NULL && stringKey!=NULL && stringKey[0]!='\0' );
    ev_calculateHash(hash,tmp,stringKey);
    element = map->slots[hash%EV_NUMBER_OF_MAP_SLOTS].first;
    while (element) {
        if ( 0==strcmp(element->key.string,stringKey) ) { return &element->variant; }
        element = element->next;
    }
    return NULL;
}


/*=================================================================================================================*/
#pragma mark - > READING ELEMENTS FROM THE TEXT BUFFER

/**
 * Try to read the value of a integer literal
 * @param[out]  out_v       (output) Pointer to the variant where the read value will be stored
 * @param[in]   ptr         Pointer to the begin of the string containing the value to read
 * @param[out]  out_endptr  (optional output) Ref to the pointer where the end of reading will be stored, can be NULL
 * @returns
 *    FALSE when no value can be read because the string format does not match with an integer number
 */
static Bool ev_readNumber(EvVariant* out_v, const utf8* ptr, const utf8** out_endptr) {
    const utf8 *last, *type; int base, value, digit; double fvalue, fdivisor;
    assert( out_v!=NULL && (ptr!=NULL && *ptr!=EVCH_PARAM_SEP && *ptr!='\0') );
    
    if ( !isliteral(*ptr) && *ptr!='.' ) { return FALSE; }
    base = 0;

    /* try to recognize the prefix that define the number representation (base) */
    switch ( ptr[0] ) {
        /* differentiate the current address "$" from the hex prefix "$" */
        case EVCH_HEXPREFIX_ADDR: if (ishex(ptr[1])) { base=16; ++ptr; }
                                  /* else               { (*out_value)=(theCurrAddress&0xFFFF); (*out_end)=ptr+1; return TRUE; } */
                                  break;
        case EVCH_HEXPREFIX:      if (ishex(ptr[1])) { base=16; ++ptr; } break; /* < hex type defined with "$" */
        case EVCH_BINPREFIX:      if (isbin(ptr[1])) { base= 2; ++ptr; } break; /* < bin type defined with "%" */
    }
    /* find the last character */
    last=ptr; while ( isliteral(*last) || *last=='.' ) { ++last; } --last;
    
    /* try to find the base of a possible number (if it wasn't already found) */
    if ( base==0 && (isdigit(*ptr) || *ptr=='.') ) {
        if      ( isalpha(*last) )                 { type=last;    --last;  } /* < type defined as suffix       */
        else if ( ptr[0]=='0' && isalpha(ptr[1]) ) { type=(ptr+1); ptr+=2;  } /* < type defined as 0x prefix    */
        else                                       { type=NULL;    base=10; } /* < no type (default to decimal) */
        if (type!=NULL) { switch (tolower(*type)) {
            case 'h': case 'x':   base=16; break;
            case 'o': case 'q':   base= 8; break;
            case 'b': case 'y':   base= 2; break;
            case 'd': case 't':   base=10; break;
            default: base=0; break;
        } }
    }
    if ( base==0 ) { return FALSE; }
    /* calculate the literal value using the found base */
    value=0; while ( ptr<=last && *ptr!='.' )
    { digit=hexvalue(*ptr++); if (0<=digit && digit<base) { value*=base; value+=digit; } }
    
    /* if the number contains a dot (.) then process it as a floating-point */
    if (*ptr=='.') {
        if (base!=10) { return FALSE; }
        fvalue=(double)value; fdivisor=1.0;
        ++ptr; while ('0'<=*ptr && *ptr<='9') { fvalue = fvalue * 10.0 + (*ptr++ - '0'); fdivisor*=10.0; }
        if (*ptr=='.') { return FALSE; }
        out_v->fnumber.evtype = EVTYPE_FNUMBER;
        out_v->fnumber.value  = (float)(fvalue/fdivisor);
    }
    /* the number does not contain a dot (.), it's a integer */
    else {
        out_v->inumber.evtype = EVTYPE_INUMBER;
        out_v->inumber.value  = value;
    }
    /* everything ok!, return end pointer & value */
    while ( isliteral(*ptr) ) { ++ptr; }
    if (out_endptr) { (*out_endptr)=ptr; }
    return TRUE;
}

/**
 * Try to read a operator
 * @param[out] out_op    (output) Ref to the pointer where returns a pointer to a structure with info about the operator that was read in
 * @param[in]  ptr       pointer to the begin of the string containing the operator to read
 * @param[out] out_endptr   (optional output) Ref to the pointer where the end of reading will be stored, can be NULL
 * @returns
 *    FALSE when no operator can be read because the string format does not match with any known operator
 */
static Bool ev_readOperator(const Ev_Operator** out_op, Bool precededByNumber, const utf8* ptr, const utf8** out_endptr) {
    const Ev_Operator* op; int firstchar;
    assert( out_op!=NULL && ptr!=NULL );

    firstchar = ptr[0];
    if ( firstchar==EVCH_PARAM_SEP ) { return FALSE; }
    op = (precededByNumber ? Ev_BinaryOperators : Ev_UnaryOperators); while (op->str!=NULL) {
        if ( op->str[0]==firstchar ) {
            if ( op->str[1]==' '    ) { *out_op=op; if (out_endptr) { (*out_endptr)=ptr+1; } return TRUE; }
            if ( op->str[1]==ptr[1] ) { *out_op=op; if (out_endptr) { (*out_endptr)=ptr+2; } return TRUE; }
        }
        ++op;
    }
    /* the operator wasn't correctly identified */
    return FALSE;
}

/**
 * Try to read any name (vars, labels, etc) from the provided string
 * @param[in]  buffer     the buffer that will be filled in with the label name
 * @param[in]  bufferSize the buffer size in bytes
 * @param[in]  ptr        pointer to the string to read
 * @param[out] out_end    (output) returns a pointer to the next char to read immediately after the label
 */
static Bool ev_readName(utf8 *buffer, int bufferSize, const utf8 *ptr, const utf8 **out_end) {
    utf8 *dest, *destend;
    assert( ptr!=NULL && out_end!=NULL && buffer!=NULL && bufferSize>0 );
    
    /* skip the character signaling the beginning of name */
    if ( !isname(*ptr) ) { return FALSE; }
    /* copy name */
    safecpy_begin(dest,destend,buffer,bufferSize);
    while ( isname(*ptr) ) { safecpy_upper(dest,destend,ptr); }
    safecpy_end(dest,destend);
    /* label is copied!, return pointer to the next char to read */
    (*out_end)=ptr; return TRUE;
}


/*=================================================================================================================*/
#pragma mark - > EXPRESION EVALUATOR

static EvVariant theVariant;
struct { const Ev_Operator* array[EV_CALC_STACK_SIZE]; int i; } opStack;
struct { EvVariant          array[EV_CALC_STACK_SIZE]; int i; } vStack;


/** Macros to manage calculator's stacks */
#define cINITSTACK(stack,value0,value1) stack.array[0]=value0; stack.array[1]=value1; stack.i=2;
#define cPUSH(stack,value)              (stack.array[stack.i++]=value)
#define cPOP(stack)                     (stack.array[--stack.i])
#define cPEEK(stack)                    (stack.array[stack.i-1])
#define cEXECUTE(f, tmp, oStack, vStack) \
    vStack.i -= f(&tmp, &vStack.array[vStack.i-2], oStack.array[--oStack.i], &vStack.array[vStack.i-1]); \
    vStack.array[vStack.i-1] = tmp;

#define cWHILE_PRECEDENCE(cond, f, tmp, oStack, vStack) \
    while (cPEEK(oStack)->preced cond) { cEXECUTE(f,tmp,oStack,vStack); }


EvVariant * evEvaluateExpression(const utf8 *start, const utf8 **out_end, EVCTX* ctx) {
    const utf8 *ptr = start; int err;
    const Ev_Operator* op; EvVariant variant, tmp; const EvVariant *variantRef;
    utf8 name[EV_MAX_NAME_LEN]; Bool continueScanning, precededByNumber, opPushed=TRUE;
    assert( start!=NULL && *start!=EVCH_PARAM_SEP && *start!=EVCH_ENDOFFILE );
    assert( out_end!=NULL );
    assert( ctx!=NULL );

    err = 0;
    theVariant.evtype = EVTYPE_EMPTY;
    theVariant.unsolved.begin = ptr;
    skipblankspaces(ptr);
    
    /*-- evaluate empty  ----------------------*/
    if ( *ptr==EVCH_PARAM_SEP || isendofline(*ptr) ) {
        theVariant.empty.evtype = EVTYPE_EMPTY;
    }
    /*-- evaluate C string --------------------*/
    else if ( *ptr==EVCH_CSTRING ) {
        theVariant.cstring.evtype = EVTYPE_CSTRING;
        theVariant.cstring.begin  = ++ptr;
        do {
            while ( !isendofcode(*ptr) && *ptr!=EVCH_CSTRING) { ++ptr; }
            continueScanning = ptr[0]==EVCH_CSTRING && *(ptr-1)==EVCH_STRING_ESCAPE;
            if  (continueScanning) { ptr+=2; }
        } while (continueScanning);
        theVariant.cstring.end = ptr;
        if (*ptr==EVCH_CSTRING) { ++ptr; }
    }
    /*-- evaluate ASM string ------------------*/
    else if ( *ptr==EVCH_ASMSTRING ) {
        theVariant.astring.evtype = EVTYPE_ASTRING;
        theVariant.astring.begin  = ++ptr;
        do {
            while ( !isendofcode(*ptr) && *ptr!=EVCH_ASMSTRING) { ++ptr; }
            continueScanning = ptr[0]==EVCH_ASMSTRING && ptr[1]==EVCH_ASMSTRING;
            if  (continueScanning) { ptr+=2; }
        } while (continueScanning);
        theVariant.astring.end = ptr;
        if (*ptr==EVCH_ASMSTRING) { ++ptr; }
    }
    /*-- evaluate expression ------------------*/
    else {
        cINITSTACK( opStack,   &Ev_SafeGuard,   &Ev_SafeGuard   );
        cINITSTACK( vStack,    Ev_EmptyVariant, Ev_EmptyVariant );
        while (*ptr!=EVCH_PARAM_SEP && !isendofcode(*ptr)) {
            precededByNumber=!opPushed; opPushed=FALSE;
            if (*ptr=='(') {
                cPUSH(opStack,&Ev_Parenthesis); opPushed=TRUE; ++ptr;
            }
            else if (*ptr==')') {
                cWHILE_PRECEDENCE(>=EV_MIN_PRECEDENCE, ev_calculate,tmp,opStack,vStack);
                if (cPOP(opStack)!=&Ev_Parenthesis) { err=evErr(EVERR_UNEXPECTED_PTHESIS,0,ctx); }
                ++ptr;
            }
            else if ( ev_readOperator(&op,precededByNumber,ptr,&ptr) ) {
                cWHILE_PRECEDENCE(>=op->preced, ev_calculate,tmp,opStack,vStack);
                cPUSH(opStack, op); opPushed=TRUE;
                
            }
            else if ( ev_readNumber(&variant,ptr,&ptr) ) { cPUSH(vStack, variant); }
            else if ( ev_readName(name,sizeof(name),ptr,&ptr) ) {
                variantRef = ev_findVariantInMap(CTX(constantsMap), name);
                if (variantRef==NULL) {
                    while (*ptr!=EVCH_PARAM_SEP && !isendofline(*ptr)) { ++ptr; }
                    theVariant.unsolved.evtype = EVTYPE_UNSOLVED;
                    theVariant.unsolved.end    = (*out_end) = ptr;
                    return &theVariant;
                }
                cPUSH(vStack, (*variantRef));
            }
            else { err=evErr(EVERR_INVALID_EXPRESSION,0,ctx); }
            skipblankspaces(ptr);
        }

        /* process any pending operator before return */
        if (!err) { cWHILE_PRECEDENCE(>=EV_MIN_PRECEDENCE, ev_calculate,tmp,opStack,vStack); }
        if (!err && cPEEK(opStack)==&Ev_Parenthesis) { err=evErr(EVERR_TOO_MANY_OPEN_PTHESES,0,ctx); }
        if (!err && (vStack.i!=3 || opStack.i!=2) )  { err=evErr(EVERR_INVALID_EXPRESSION,0,ctx); }
        if (!err) { theVariant = cPOP(vStack); }
    }

    while (*ptr!=EVCH_PARAM_SEP && !isendofline(*ptr)) { ++ptr; }
    (*out_end) = ptr;
    return &theVariant;
}

void evAddConstant(const utf8* name, const EvVariant* value, EVCTX* ctx) {
    assert( name!=NULL && value!=NULL && ctx!=NULL );
    ev_addVariantToMap(CTX(constantsMap), value, name, ctx);
}

/*=================================================================================================================*/
#pragma mark - > CONTEXT CREATION / DESTRUCTION

EVCTX* evCreateContext(void) {
    Ev_Context* ctx = malloc(sizeof(Ev_Context));
    CTX(permactx)     = ev_permallocCreateContext();
    CTX(deferredList) = ev_permallocDeferredList(CTX(permactx));
    CTX(constantsMap) = ev_permallocVariantMap(CTX(permactx));
    CTX(curErrorLine) = NULL;
    CTX(firstError)   = NULL;
    CTX(lastError)    = NULL;
    return ctx;
}

void evDestroyContext(EVCTX* ctx) {
    if (ctx==NULL) { return; }
    ev_permallocDestroyContext(CTX(permactx));
    free((Ev_Context*)ctx);
}


/*=================================================================================================================*/
#pragma mark - > DEFERRED EVALUATIONS

#define ev_addToList(list,element) \
    if ((list)->last) { (list)->last = ((list)->last->next = (element)); } \
    else              { (list)->last = ((list)->first      = (element)); }

EvDeferredVariant* evDeferVariant(const EvVariant* variant, int userValue, void* userPtr, EVCTX* ctx) {
    EvDeferredVariant* deferred;
    assert( variant!=NULL && ctx!=NULL );
    deferred = ev_permalloc(sizeof(EvDeferredVariant),CTX(permactx));
    deferred->userValue = userValue;
    deferred->userPtr   = userPtr;
    ev_copyVariant(&deferred->variant, variant, ctx);
    ev_addToList(CTX(deferredList),deferred);
    return deferred;
}

EvDeferredVariant* evGetFirstDeferredVariant(EVCTX* ctx) {
    assert( ctx!=NULL );
    if (CTX(firstError)!=NULL) { return NULL; }
    /* TODO: evaluate all deferred variants! */
    return CTX(deferredList)->first;
}

EvDeferredVariant* evGetNextDeferredVariant(EvDeferredVariant* deferred, EVCTX* ctx) {
    assert( deferred!=NULL && ctx!=NULL );
    return deferred->next;
}


/*=================================================================================================================*/
/*
    PUBLIC FUNCTIONS
    ----------------
 
        EVERR
        EVCTX
        EvVariant
        EvVariantType
        EvDeferredVariant
 
        CREATION / DESTRUCTION
            EVCTX* evCreateContext( );
            void   evDestroyContext( EVCTX* );
 
        EVALUATION OF EXPRESSIONS
            EvVariant* evAddConstant( name, EvVariant* value, EVCTX* );
            EvVariant* evEvaluateExpression( const utf8* expression, &out_end, EVCTX* );
 
        DEFERRED EVALUATIONS
            EvDeferredVariant* evDeferVariant( EvVariant*, userValue, userPtr, EVCTX* );
            void               evEvaluateDeferredVariants( EVCTX* );
            EvDeferredVariant* evGetFirstDeferredVariant( EVCTX* );
            EvDeferredVariant* evGetNextDeferredVariant( EvDeferredVariant*, EVCTX* );

        EVALUATION ERROR REPORT // optional //
            Bool evErr( EVERR, str, EVCTX* );
            void evErrBeginFile( filePath, EVCTX* );
            void evErrEndFile( EVCTX* );
            void evErrSetLineNumber( lineNumber, EVCTX* );
            Bool evErrPrintErrors( EVCTX* )

 */


#endif /* ifdef EVALUATE_IMPLEMENTATION */

#endif /* ifndef EVALUATE_H_INCLUDED */

