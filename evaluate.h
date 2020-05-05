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


extern void evCreateContext(void);
extern void evDestroyContext(void);

extern EvDeferredVariant* evDeferVariant(const EvVariant* variant, int userValue, void* userPtr);
extern EvDeferredVariant* evGetFirstDeferredVariant(void);
extern EvDeferredVariant* evGetNextDeferredVariant(EvDeferredVariant* deferred);

extern int  everr(EVERR everr, const utf8 *str);
extern void everrBeginFile(const utf8* filePath);
extern void everrEndFile(const utf8* filePath);
extern void everrSetLineNumber(int lineNumber);
extern Bool everrPrintErrors(void);


/*=================================================================================================================*/
#pragma mark - > IMPLEMENTATION

#ifdef EVALUATE_IMPLEMENTATION
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>

#define EV_PERMALLOC_CHUNK_SIZE (64*1024)     /* < size of each chunk of memory allocated by autoalloc   */
#define EV_MAX_NAME_LEN         (63)          /* < maximum length for names of vars (in bytes)           */
#define EV_CALC_STACK_SIZE      (256)         /* < the calculator's stack size in number of elements     */
#define EV_MIN_PRECEDENCE       (1)           /* < the minimum valid operator predecence                 */
#define EV_NUMBER_OF_MAP_SLOTS  (77)          /* < number of slots used in the hashmap                   */

/** Some special characters used in expressions */
typedef enum EVCH {
    EVCH_ENDOFFILE     ='\0' , EVCH_STARTCOMMENT=';',  EVCH_PARAM_SEP    =',',
    EVCH_ASMSTRING     ='\'' , EVCH_CSTRING     ='"' , EVCH_STRING_ESCAPE='\\' ,
    EVCH_HEXPREFIX_ADDR='$'  , EVCH_HEXPREFIX   ='#' , EVCH_BINPREFIX    ='%'
} StdChars_;



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
#pragma mark - > PERMANENT DYNAMIC ALLOCATION (PERMALLOC)

typedef struct Ev_PermallocChunk {
    struct Ev_PermallocChunk* next;
    char                      data[EV_PERMALLOC_CHUNK_SIZE];
} Ev_PermallocChunk;

typedef struct Ev_PermallocContext {
    Ev_PermallocChunk* chunk;
    char*              ptr;
    int                remaining;
} Ev_PermallocContext;


Ev_PermallocContext* thePermalloc = NULL;


#define CTX(x) ctx->x

static Ev_PermallocContext* ev_permallocInit() {
    Ev_PermallocContext* ctx = malloc(sizeof(Ev_PermallocContext));
    CTX(chunk)     = NULL;
    CTX(ptr)       = NULL;
    CTX(remaining) = 0;
    return ctx;
}

/**
 * Allocates a block of memory that stays valid until the end of the program
 * @param size The size of the memory block to alloc, in bytes
 * @return     A pointer to the beginning of the block
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
 * If the replacement pointer is NULL then the original string will be duplicated without any modification
 * @param string         The original string
 * @param charToReplace  The character to be replaced
 * @param replacement    The string that replaces the found character (optional, can be NULL)
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

/**
 * Deallocates all memory that previously was allocated with 'permalloc(..)'
 */
static void ev_permallocDealloc(Ev_PermallocContext* ctx) {
    Ev_PermallocChunk *chunk, *nextChunk=NULL;
    if (ctx==NULL) { return; }
    for (chunk=CTX(chunk); chunk; chunk=nextChunk) { nextChunk=chunk->next; free(chunk); }
    free(ctx);
}


/*=================================================================================================================*/
#pragma mark - > HANDLING ERRORS

typedef struct Ev_ErrorLine { const utf8* permaPath; int number; struct Ev_ErrorLine* prev;    } Ev_ErrorLine;
typedef struct Ev_Error { EVERR id; const utf8* str; Ev_ErrorLine line; struct Ev_Error* next; } Ev_Error;

static Ev_ErrorLine* theCurErrorLine = NULL;
static Ev_Error*     theFirstError   = NULL;
static Ev_Error*     theLastError    = NULL;

/**
 * Reports a error
 * @param everr    The evaluation error identifier, ex: EVERR_INVALID_EXPRESSION
 * @param str      The optional text attached to the error reported (it can be NULL)
 */
int everr(EVERR everr, const utf8 *str) {
    const utf8* message; Ev_Error* newError;
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
    newError = ev_permalloc(sizeof(Ev_Error), thePermalloc);
    newError->id             = everr;
    newError->line.permaPath = theCurErrorLine ? theCurErrorLine->permaPath : 0;
    newError->line.number    = theCurErrorLine ? theCurErrorLine->number    : 0;
    newError->str            = ev_permallocString(message,'$',str,thePermalloc);
    newError->next           = NULL;
    if (!theFirstError) { theFirstError=newError; }
    theLastError = theLastError ? (theLastError->next=newError) : newError;
    return everr;
}

void everrBeginFile(const utf8* filePath) {
    Ev_ErrorLine* newErrorLine;
    assert(filePath!=NULL);
    newErrorLine = malloc(sizeof(Ev_ErrorLine));
    newErrorLine->permaPath = ev_permallocString(filePath,0,0,thePermalloc);
    newErrorLine->number    = 0;
    newErrorLine->prev      = theCurErrorLine;
    theCurErrorLine = newErrorLine;
}

void everrEndFile(const utf8* filePath) {
    Ev_ErrorLine* prevErrorLine;
    assert( filePath!=NULL );
    assert( theCurErrorLine!=NULL && strcmp(theCurErrorLine->permaPath,filePath)==0 );
    prevErrorLine = theCurErrorLine->prev;
    free(theCurErrorLine); theCurErrorLine=prevErrorLine;
}

void everrSetLineNumber(int lineNumber) {
    theCurErrorLine->number = lineNumber;
}

Bool everrPrintErrors(void) {
    Ev_Error* error; const int column=0;
    for (error=theFirstError; error; error=error->next) {
        if (error->line.permaPath!=NULL && error->line.number>0) {
            if (column>0) { printf("%s:%d:%d: ", error->line.permaPath, error->line.number, column); }
            else          { printf("%s:%d: "   , error->line.permaPath, error->line.number);         }
        }
        printf("%s %s\n", "error:", error->str);
    }
    return (theFirstError!=NULL);
}

/*=================================================================================================================*/
#pragma mark - > OPERATORS

/** IDs of supported operators */
typedef enum OperatorID {
    OP_PLUS, OP_MINUS, OP_NOT, OP_LNOT, OP_ADD, OP_SUB, OP_MUL, OP_DIV, OP_MOD,
    OP_SHL, OP_SHR, OP_EQ, OP_NE, OP_LT, OP_LE, OP_GT, OP_GE,
    OP_AND, OP_XOR, OP_OR, OP_LAND, OP_LOR, OP_INVALID
} OperatorID;

/** Structure containing information about an operator (id, precedence, etc..) */
typedef struct Operator { OperatorID id; const utf8* str; int preced; } Operator;

/** Array containing info about each UNARY operator. The last element in {0,0,0} */
static const Operator UnaryOperators[5] = {
    {OP_PLUS,"+ ",12}, {OP_MINUS,"- ",12}, {OP_NOT,"~ ",12}, {OP_LNOT,"! ",12},
    {0,0,0}
};
/** Array containing info about each BINARY operator. The last element is {0,0,0} */
static const Operator BinaryOperators[19] = {
    {OP_SHL ,"<<", 9}, {OP_SHR   ,">>", 9},
    {OP_LE  ,"<=", 8}, {OP_GE    ,">=", 8},
    {OP_EQ  ,"==", 7}, {OP_NE    ,"!=", 7},
    {OP_LAND,"&&", 3},
    {OP_LOR ,"||", 2},
    {OP_MUL ,"* ",11}, {OP_DIV,"/ ",11}, {OP_MOD,"% ",11},
    {OP_ADD ,"+ ",10}, {OP_SUB,"- ",10},
    {OP_LT  ,"< ", 8}, {OP_GT ,"> ", 8},
    {OP_AND ,"& ", 6},
    {OP_XOR ,"^ ", 5},
    {OP_OR  ,"| ", 4},
    /* {OP_TERN,"?",1} */
    {0,0,0}
};

/** Special operator object used to mark parenthesis */
static const Operator OpParenthesis = {OP_INVALID,"",(EV_MIN_PRECEDENCE-1)};

/** Special operator object used to mark limits in the evaluator stack */
static const Operator OpSafeGuard   = {OP_INVALID,"",(EV_MIN_PRECEDENCE-1)};


/*=================================================================================================================*/
#pragma mark - > VARIANTS

static const EvVariant theEmptyVariant = { EVTYPE_EMPTY };

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
static int ev_calculate(EvVariant* v, const EvVariant* left, const Operator *operator, const EvVariant* right) {
    int intL,intR; float float1,float2;
    assert( operator!=NULL && left!=NULL && right!=NULL );
    
    if ( ev_variantToInt(right,&intR) && ev_variantToInt(left,&intL) ) { switch(operator->id) {
        case OP_PLUS : ev_return_INumber1(v, +, intR);
        case OP_MINUS: ev_return_INumber1(v, -, intR);
        case OP_NOT  : ev_return_INumber1(v, ~, intR);
        case OP_LNOT : ev_return_INumber1(v, !, intR);
        case OP_ADD  : ev_return_INumber2(v, intL, +,intR); case OP_SUB: ev_return_INumber2(v, intL, -,intR);
        case OP_MUL  : ev_return_INumber2(v, intL, *,intR); case OP_DIV: ev_return_INumber2(v, intL, /,intR);
        case OP_SHL  : ev_return_INumber2(v, intL,<<,intR); case OP_SHR: ev_return_INumber2(v, intL,>>,intR);
        case OP_AND  : ev_return_INumber2(v, intL, &,intR); case OP_OR:  ev_return_INumber2(v, intL, |,intR);
        case OP_XOR  : ev_return_INumber2(v, intL, ^,intR); case OP_MOD: ev_return_INumber2(v, intL, %,intR);
        case OP_EQ   : ev_return_INumber2(v, intL,==,intR); case OP_NE : ev_return_INumber2(v, intL,!=,intR);
        case OP_LT   : ev_return_INumber2(v, intL,< ,intR); case OP_GT : ev_return_INumber2(v, intL,> ,intR);
        case OP_LE   : ev_return_INumber2(v, intL,<=,intR); case OP_GE : ev_return_INumber2(v, intL,>=,intR);
        default: assert(FALSE); break;
    } }
    else if ( ev_variantToFloat(left,&float1) && ev_variantToFloat(right,&float2) ) { switch (operator->id) {
        case OP_ADD: ev_return_FNumber2(v, float1, +,float2); case OP_SUB: ev_return_FNumber2(v, float1, -,float2);
        case OP_MUL: ev_return_FNumber2(v, float1, *,float2); case OP_DIV: ev_return_FNumber2(v, float1, /,float2);
        default: assert(FALSE); break;
    } }
    (*v) = (*right);
    return 0;
}


/*=================================================================================================================*/
#pragma mark - > VARIANT CONTAINERS

#define ev_calculateHash(hash,ptr,string) \
    hash=5381; ptr=(unsigned char*)string; while (*ptr) { hash = ((hash<<5)+hash) ^ *ptr++; }

typedef struct Ev_VariantElement {
    EvVariant variant; union { int integer; const utf8* string; } key;
    struct Ev_VariantElement* next;
} Ev_VariantElement;

typedef struct Ev_VariantList { Ev_VariantElement *first, *last; } Ev_VariantList;
typedef struct Ev_VariantList Ev_VariantMapSlot;
typedef struct Ev_VariantMap  { Ev_VariantMapSlot* slots; } VariantMap;


static void ev_copyVariant(EvVariant* dest, const EvVariant* sour) {
    int length; utf8* begin;
    assert( dest!=NULL && sour!=NULL );
    switch ( (dest->evtype=sour->evtype) ) {
        case EVTYPE_EMPTY: break;
        case EVTYPE_INUMBER: dest->inumber.value = sour->inumber.value; break;
        case EVTYPE_FNUMBER: dest->fnumber.value = sour->fnumber.value; break;
        case EVTYPE_ASTRING:
        case EVTYPE_CSTRING:
        case EVTYPE_UNSOLVED:
            length = (int)(sour->astring.end - sour->astring.begin);
            dest->astring.begin = (begin = ev_permalloc(length+1,thePermalloc));
            dest->astring.end   = begin + length;
            memcpy(begin, sour->astring.begin, length); begin[length]='\0';
            break;
    }
}

static void ev_addVariantElementToList(Ev_VariantList* list, Ev_VariantElement* elementToAdd) {
    if (list->last) { list->last = (list->last->next = elementToAdd); }
    else            { list->last = (list->first = elementToAdd);      }
}

static void ev_addVariantToListKS(Ev_VariantList* list, const EvVariant* variantToAdd, const utf8* stringKey) {
    Ev_VariantElement* element; int sizeofStringKey; utf8* copyofStringKey;
    assert( list!=NULL && variantToAdd!=NULL && stringKey!=NULL && stringKey[0]!='\0' );
    
    sizeofStringKey = (int)strlen(stringKey)+1;
    copyofStringKey = ev_permalloc(sizeofStringKey,thePermalloc);
    memcpy(copyofStringKey, stringKey, sizeofStringKey);
    
    element             = ev_permalloc(sizeof(Ev_VariantElement),thePermalloc);
    element->key.string = copyofStringKey;
    ev_copyVariant(&element->variant, variantToAdd);
    ev_addVariantElementToList(list, element);
}

static void ev_addVariantToMap(VariantMap* map, const EvVariant* variantToAdd, const utf8* stringKey) {
    unsigned hash; unsigned char* tmp;
    assert( map!=NULL && variantToAdd!=NULL && stringKey!=NULL && stringKey[0]!='\0' );
    ev_calculateHash(hash,tmp,stringKey);
    if (map->slots==NULL) { map->slots=ev_permalloc(EV_NUMBER_OF_MAP_SLOTS*sizeof(Ev_VariantMapSlot),thePermalloc); }
    ev_addVariantToListKS(&map->slots[hash%EV_NUMBER_OF_MAP_SLOTS], variantToAdd, stringKey);
}

static const EvVariant* ev_findVariantInMap(VariantMap* map, const utf8* stringKey) {
    unsigned hash; Ev_VariantElement* element; unsigned char* tmp;
    assert( map!=NULL && stringKey!=NULL && stringKey[0]!='\0' );
    ev_calculateHash(hash,tmp,stringKey);
    element = map->slots ? map->slots[hash%EV_NUMBER_OF_MAP_SLOTS].first : NULL;
    while (element) {
        if ( 0==strcmp(element->key.string,stringKey) ) { return &element->variant; }
        element = element->next;
    }
    return NULL;
}


/*=================================================================================================================*/
#pragma mark - > READING ELEMENTS FROM THE TEXT buffer

/**
 * Try to read the value of a integer literal
 * @param[out]  out_v       (output) Pointer to the variant where the read value will be stored
 * @param[in]   ptr         Pointer to the begin of the string containing the value to read
 * @param[out]  out_endptr  (optional output) Ref to the pointer where the end of reading will be stored, can be NULL
 * @returns
 *    FALSE when no value can be read because the string format does not match with an integer number
 */
static Bool readNumber(EvVariant* out_v, const utf8* ptr, const utf8** out_endptr) {
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
static Bool readOperator(const Operator** out_op, Bool precededByNumber, const utf8* ptr, const utf8** out_endptr) {
    const Operator* op; int firstchar;
    assert( out_op!=NULL && ptr!=NULL );

    firstchar = ptr[0];
    if ( firstchar==EVCH_PARAM_SEP ) { return FALSE; }
    op = (precededByNumber ? BinaryOperators : UnaryOperators); while (op->str!=NULL) {
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
static Bool readName(utf8 *buffer, int bufferSize, const utf8 *ptr, const utf8 **out_end) {
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

static VariantMap theConstantsMap = { 0 };

static EvVariant theVariant;
struct { const Operator* array[EV_CALC_STACK_SIZE]; int i; } opStack;
struct { EvVariant       array[EV_CALC_STACK_SIZE]; int i; } vStack;


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


static EvVariant * qEvaluateExpression(const utf8 *start, const utf8 **out_end) {
    const utf8 *ptr = start; int err;
    const Operator* op; EvVariant variant, tmp; const EvVariant *variantRef;
    utf8 name[EV_MAX_NAME_LEN]; Bool continueScanning, precededByNumber, opPushed=TRUE;
    assert( start!=NULL ); assert( out_end!=NULL );
    assert( *start!=EVCH_PARAM_SEP ); assert( *start!=EVCH_ENDOFFILE );

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
        cINITSTACK( opStack,   &OpSafeGuard,    &OpSafeGuard    );
        cINITSTACK( vStack,    theEmptyVariant, theEmptyVariant );
        while (*ptr!=EVCH_PARAM_SEP && !isendofcode(*ptr)) {
            precededByNumber=!opPushed; opPushed=FALSE;
            if (*ptr=='(') {
                cPUSH(opStack,&OpParenthesis); opPushed=TRUE; ++ptr;
            }
            else if (*ptr==')') {
                cWHILE_PRECEDENCE(>=EV_MIN_PRECEDENCE, ev_calculate,tmp,opStack,vStack);
                if (cPOP(opStack)!=&OpParenthesis) { err=everr(EVERR_UNEXPECTED_PTHESIS,0); }
                ++ptr;
            }
            else if ( readOperator(&op,precededByNumber,ptr,&ptr) ) {
                cWHILE_PRECEDENCE(>=op->preced, ev_calculate,tmp,opStack,vStack);
                cPUSH(opStack, op); opPushed=TRUE;
                
            }
            else if ( readNumber(&variant,ptr,&ptr) ) { cPUSH(vStack, variant); }
            else if ( readName(name,sizeof(name),ptr,&ptr) ) {
                variantRef = ev_findVariantInMap(&theConstantsMap, name);
                if (variantRef==NULL) {
                    while (*ptr!=EVCH_PARAM_SEP && !isendofline(*ptr)) { ++ptr; }
                    theVariant.unsolved.evtype = EVTYPE_UNSOLVED;
                    theVariant.unsolved.end    = (*out_end) = ptr;
                    return &theVariant;
                }
                cPUSH(vStack, (*variantRef));
            }
            else { err=everr(EVERR_INVALID_EXPRESSION,0); }
            skipblankspaces(ptr);
        }

        /* process any pending operator before return */
        if (!err) { cWHILE_PRECEDENCE(>=EV_MIN_PRECEDENCE, ev_calculate,tmp,opStack,vStack); }
        if (!err && cPEEK(opStack)==&OpParenthesis) { err=everr(EVERR_TOO_MANY_OPEN_PTHESES,0); }
        if (!err && (vStack.i!=3 || opStack.i!=2) ) { err=everr(EVERR_INVALID_EXPRESSION,0); }
        if (!err) { theVariant = cPOP(vStack); }
    }

    while (*ptr!=EVCH_PARAM_SEP && !isendofline(*ptr)) { ++ptr; }
    (*out_end) = ptr;
    return &theVariant;
}

static void qAddConstant(const utf8* name, const EvVariant* value) {
    ev_addVariantToMap(&theConstantsMap, value, name);
}

/*=================================================================================================================*/
#pragma mark - > CREATION / DESTRUCTION

void evCreateContext(void) {
    thePermalloc = ev_permallocInit();
}

void evDestroyContext(void) {
    ev_permallocDealloc(thePermalloc);
}


/*=================================================================================================================*/
#pragma mark - > DEFERRED EVALUATIONS

#define ev_addToList(list,element) \
    if ((list)->last) { (list)->last = ((list)->last->next = (element)); } \
    else              { (list)->last = ((list)->first      = (element)); }


typedef struct Ev_DeferredList { EvDeferredVariant *first, *last; } Ev_DeferredList;

static Ev_DeferredList theDeferredList = {0,0};

EvDeferredVariant* evDeferVariant(const EvVariant* variant, int userValue, void* userPtr) {
    EvDeferredVariant* deferred = ev_permalloc(sizeof(EvDeferredVariant),thePermalloc);
    deferred->userValue = userValue;
    deferred->userPtr   = userPtr;
    ev_copyVariant(&deferred->variant, variant);
    ev_addToList(&theDeferredList,deferred);
    return deferred;
}

EvDeferredVariant* evGetFirstDeferredVariant(void) {
    if (theFirstError!=NULL) { return NULL; }
    /* TODO: evaluate all deferred variants! */
    return theDeferredList.first;
}

EvDeferredVariant* evGetNextDeferredVariant(EvDeferredVariant* deferred) {
    return deferred->next;
}


/*=================================================================================================================*/
/*
    PUBLIC FUNCTIONS
    ----------------
 
        EVERR
        EvVariant
        EvVariantType
        EvDeferredVariant
 
        CREATION / DESTRUCTION
            evCreateContext( );
            evDestroyContext( );
 
        EVALUATION OF EXPRESSIONS
            EvVariant* evAddConstant(name, EvVariant* value)
            EvVariant* evEvaluateExpression(..)
 
        DEFERRED EVALUATIONS
            EvDeferredVariant* evDeferVariant( EvVariant*, userValue, userPtr );
            void               evEvaluateDeferredVariants( );
            EvDeferredVariant* evGetFirstDeferredVariant( );
            EvDeferredVariant* evGetNextDeferredVariant( EvDeferredVariant* );

        EVALUATION ERROR REPORT (optional)
            Bool evErr(EVERR,str);
            void evErrBeginFile(filePath);
            void evErrEndFile();
            void evErrSetLineNumber(lineNumber);
            Bool evErrPrintErrors(..)

 */


#endif /* ifdef EVALUATE_IMPLEMENTATION */

#endif /* ifndef EVALUATE_H_INCLUDED */

