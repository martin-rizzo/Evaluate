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
#include <ctype.h>
#include <assert.h>
#define VERSION   "0.1"
#define COPYRIGHT "Copyright (c) 2020 Martin Rizzo"

#define MIN_FILE_SIZE       (0)           /* < minimum size for loadable files (in bytes)            */
#define MAX_FILE_SIZE       (1024L*1024L) /* < maximum size for loadable files (in bytes)            */
#define MAX_ERROR_COUNT     (32)          /* < maximum number of errors able to be reported          */
#define MAX_NAME_LEN        (63)          /* < maximum length for names of vars (in bytes)           */
#define MAX_ERRMSG_LEN      (63)          /* < maximum length for error messages (in bytes)          */
#define MAX_ERRSTR_LEN      (63)          /* < maximum length for the optional error text (in bytes) */
#define CALC_STACK_SIZE     (256)         /* < the calculator's stack size in number of elements     */
#define STALLOC_CHUNK_SIZE  (64*1024)     /* < size of each chunk of memory allocated by autoalloc   */
#define MIN_PRECEDENCE      (1)           /* < the minimum valid operator predecence                 */
#define NUMBER_OF_MAP_SLOTS (77)          /* < number of slots used in the hashmap                   */



typedef char           utf8;              /* < unicode variable width character encoding */
typedef int Bool; enum { FALSE=0, TRUE }; /* < Boolean */


/* some standard characters */
typedef enum StdChars_ {
    CH_ENDFILE       ='\0' , CH_ENDLABEL    =':' , CH_STARTCOMMENT  =';'  , CH_SPACE    =' '  ,
    CH_PARAM_SEP     =','  , CH_EXT_SEP     ='.' , CH_PATH_SEP      ='/'  , CH_PATH_SEP2='\\' ,
    CH_ASMSTRING     ='\'' , CH_CSTRING     ='"' , CH_STRING_ESCAPE ='\\' ,
    CH_HEXPREFIX_ADDR='$'  , CH_HEXPREFIX   ='#' , CH_BINPREFIX   ='%'  ,
    CH_OPTIONAL_DIREC_PREFIX = '.'
} StdChars_;

/* supported errors */
typedef enum ErrorID_ {
    ERR_NO_ERROR=0, ERR_FILE_NOT_FOUND=-1000, ERR_FILE_TOO_LARGE, ERR_CANNOT_READ_FILE, ERR_NOT_ENOUGH_MEMORY,
    ERR_INT8_OUT_OF_RANGE,  ERR_INT16_OUT_OF_RANGE, ERR_BITNUM_OUT_OF_RANGE, ERR_INVALID_EXPRESSION,
    ERR_INVALID_IMODE, ERR_UNEXPECTED_PTHESIS, ERR_TOO_MANY_OPEN_PTHESES, ERR_INVALID_RST_ADDRESS,
    ERR_DISP_MUST_BE_ZERO, ERR_DISP_OUT_OF_RANGE, ERR_UNKNOWN_PARAM
} ErrorID;



typedef struct Error { ErrorID id; const utf8 *filename, *str; int line; } Error;






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
static int isliteral  (int ch) { return isalnum(ch) || ch=='_' || ch==CH_HEXPREFIX_ADDR || ch==CH_HEXPREFIX || ch==CH_BINPREFIX; }
static int isendofcode(int ch) { return ch=='\r' || ch=='\n' || ch==CH_STARTCOMMENT || ch==CH_ENDFILE; }
static int isendofline(int ch) { return ch==CH_ENDFILE || ch=='\r' || ch=='\n'; }
static int hexvalue   (int ch) { return INRANGE(ch,'0','9') ? ch-'0' : INRANGE(ch,'A','F') ? ch-'A'+10 : INRANGE(ch,'a','f') ? ch-'a'+10 : -1; }
static int octvalue   (int ch) { return INRANGE(ch,'0','7') ? ch-'0' : -1; }
#define skipendofline(ptr) (ptr[0]=='\n' && ptr[1]=='\r') || (ptr[0]=='\r' && ptr[1]=='\n') ? ptr+=2 : ++ptr;
#define skipblankspaces(ptr) while ( isblank(*ptr) ) { ++ptr; }

static const utf8 * stralloc(const utf8 *str, int maxlen) {
    utf8 *dest; size_t len;
    if (!str) { return NULL; }
    len=strlen(str); if (maxlen>0 && len>maxlen) { len=maxlen; }
    dest=malloc(len+1); memcpy(dest,str,len); dest[len]='\0';
    if (len==maxlen && len>=2) { dest[len-2]=dest[len-1]='.'; }
    return dest;
}

static const utf8 * strreplace(utf8 *buffer, const utf8 *message, int charToReplace, const utf8 *replacement) {
    utf8 *dest=buffer; const utf8 *ptr=message, *sour;
    assert( buffer!=NULL && message!=NULL );
    while (*ptr!='\0') {
        if (*ptr==charToReplace && replacement) { ++ptr; sour=replacement; while (*sour!='\0') { *dest++=*sour++; } }
        else { *dest++=*ptr++; }
    }
    *dest='\0'; return buffer;
}


/*=================================================================================================================*/
#pragma mark - > PSEUDO STATIC ALLOCATION

typedef struct StallocChunk { struct StallocChunk* next; char data[STALLOC_CHUNK_SIZE]; } StallocChunk;
StallocChunk* theStallocChunk     = NULL;
char*         theStallocPtr       = NULL;
int           theStallocRemaining = 0;

/**
 * Allocates a block of memory that stays valid until the end of the program
 * @param size The size of the memory block to alloc, in bytes
 * @return     A pointer to the beginning of the block
 */
static void* stalloc(int size) {
    StallocChunk* prevStallocChunk; void* ptr;
    if (theStallocRemaining<size) {
        prevStallocChunk=theStallocChunk; theStallocChunk=malloc(sizeof(StallocChunk));
        if (prevStallocChunk) { prevStallocChunk->next = theStallocChunk; }
        theStallocPtr       = theStallocChunk->data;
        theStallocRemaining = STALLOC_CHUNK_SIZE;
    }
    /* allocates the first 'size' bytes counting from 'theStallocPtr' */
    ptr=theStallocPtr; theStallocPtr+=size; theStallocRemaining-=size;
    return ptr;
}

/**
 * Deallocates all memory that previously was allocated with 'stalloc(..)'
 */
static void stallocFreeAll() {
    StallocChunk *chunk, *nextChunk=NULL;
    for (chunk=theStallocChunk; chunk; chunk=nextChunk) { nextChunk=chunk->next; free(chunk); }
    theStallocPtr=NULL; theStallocChunk=NULL; theStallocRemaining=0;
}


/*=================================================================================================================*/
#pragma mark - > HANDLING ERRORS

static struct Globals {
    const utf8* curFilePath;
    int         curLineNumber;
    Error       errors[MAX_ERROR_COUNT];
    int         errorCount;
} g;

#ifdef NDEBUG
#    define DLOG(x)
#else
#    define DLOG(x) printf x; printf("\n")
#endif

/**
 * Global var that indicates whether the program is being successfully executed.
 * It is TRUE while no error is reported.
 */
#define success (g.errorCount==0)

/**
 * Reports a error
 * @param errorID  The error identifier, ex: ERR_CANNOT_READ_FILE
 * @param str      The optional text attached to the error reported (it can be NULL)
 */
static Bool error(ErrorID errorID, const utf8 *str) {
    Error *error; if (g.errorCount<MAX_ERROR_COUNT) {
        error = &g.errors[g.errorCount++];
        error->id=errorID;
        error->filename=stralloc(g.curFilePath,-1);
        error->line=g.curLineNumber;
        error->str=stralloc(str,MAX_ERRSTR_LEN);
    }
    return (errorID==ERR_NO_ERROR);
}

static Bool printErrorMessages(void) {
    const utf8 *message; Error *error=g.errors; int count=g.errorCount;
    utf8 buffer[ (MAX_ERRMSG_LEN+1)+(MAX_ERRSTR_LEN+1) ];

    while (--count>=0) {
        switch (error->id) {
            case ERR_NO_ERROR:             message = "SUCCESS"; break;
            case ERR_FILE_NOT_FOUND:       message = "file not found";    break;
            case ERR_FILE_TOO_LARGE:       message = "the file '$' is too large"; break;
            case ERR_NOT_ENOUGH_MEMORY:    message = "not enough memory"; break;
            case ERR_INT8_OUT_OF_RANGE:    message = "the 8-bit value is out of range"; break;
            case ERR_INT16_OUT_OF_RANGE:   message = "the 16-bit value is out of range"; break;
            case ERR_BITNUM_OUT_OF_RANGE:  message = "the bit number is out of range (valid range is: 0-7)"; break;
            case ERR_INVALID_EXPRESSION:   message = "invalid expression"; break;
            case ERR_INVALID_IMODE:        message = "invalid interruption mode"; break;
            case ERR_UNEXPECTED_PTHESIS:   message = "unexpected ')' (closing parenthesis)"; break;
            case ERR_TOO_MANY_OPEN_PTHESES:message = "too many open parentheses"; break;
            case ERR_INVALID_RST_ADDRESS:  message = "invalid RST address"; break;
            case ERR_DISP_MUST_BE_ZERO:    message = "no displacement can be added to the index register"; break;
            case ERR_DISP_OUT_OF_RANGE:    message = "the displacement is out of range"; break;
            case ERR_UNKNOWN_PARAM:        message = "unknown parameter"; break;
            default:                       message = "unknown error";     break;
        }
        message=strreplace(buffer,message,'$',error->str);
        if (error->line) { printf("%s:%d: %s %s\n", error->filename, error->line, "error:", message); }
        else             { printf("%s %s\n", "error:", message); }
        ++error;
    }
    return (g.errorCount>0);
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
static const Operator OpParenthesis = {OP_INVALID,"",(MIN_PRECEDENCE-1)};

/** Special operator object used to mark limits in the evaluator stack */
static const Operator OpSafeGuard   = {OP_INVALID,"",(MIN_PRECEDENCE-1)};


/*=================================================================================================================*/
#pragma mark - > VARIANTS

typedef enum   VariantType { TYPE_UNSOLVED, TYPE_EMPTY, TYPE_ASTRING, TYPE_CSTRING, TYPE_INUMBER, TYPE_FNUMBER } VariantType;
typedef union  Variant {
    VariantType type;
    struct { VariantType type;                          } empty;     /* < empty data             */
    struct { VariantType type; int   value;             } inumber;   /* < integer number         */
    struct { VariantType type; float value;             } fnumber;   /* < floating-point number  */
    struct { VariantType type; const utf8 *begin, *end; } astring;   /* < assembler string       */
    struct { VariantType type; const utf8 *begin, *end; } cstring;   /* < C string               */
    struct { VariantType type; const utf8 *begin, *end; } unsolved;  /* < unsolved expression    */
} Variant;

static const Variant EmptyVariant = { TYPE_EMPTY };

static Bool variantToInt(const Variant* variant, int* i) {
    switch( variant->type ) {
        case TYPE_INUMBER: (*i)=variant->inumber.value;    return TRUE;
        case TYPE_ASTRING: (*i)=variant->astring.begin[0]; return TRUE;
        case TYPE_EMPTY:   return TRUE;
        default: return FALSE;
    }
}

static Bool variantToFloat(const Variant* variant, float* f) {
    switch( variant->type ) {
        case TYPE_FNUMBER: (*f)=variant->fnumber.value;        return TRUE;
        case TYPE_INUMBER: (*f)=(float)variant->inumber.value; return TRUE;
        case TYPE_EMPTY:   return TRUE;
        default: return FALSE;
    }
}

static Bool variantToString(const Variant* variant, utf8* buffer, int bufferSize) {
    utf8 *dest, *destend; const utf8 *ptr, *ptrend; int ch, digit;
    assert( variant!=NULL && buffer!=NULL && bufferSize>0 );
    switch (variant->type) {
            
        case TYPE_ASTRING:
            safecpy_begin(dest,destend,buffer,bufferSize);
            ptr=variant->astring.begin; ptrend=variant->astring.end;
            do {
                while (ptr<ptrend && *ptr!=CH_ASMSTRING) { safecpy(dest,destend,ptr); }
                if  (ptr<ptrend) { safecpy_char(dest,destend,CH_ASMSTRING); ptr+=2; }
            } while (ptr<ptrend);
            safecpy_end(dest,destend);
            return TRUE;

        case TYPE_CSTRING:
            safecpy_begin(dest,destend,buffer,bufferSize);
            ptr=variant->cstring.begin; while(ptr<variant->cstring.end) {
                if (ptr[0]!=CH_STRING_ESCAPE) { safecpy(dest,destend,ptr); }
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

#define return_INumber1(v,       op, right) v->type=TYPE_INUMBER; v->inumber.value=      op right; return 0
#define return_INumber2(v, left, op, right) v->type=TYPE_INUMBER; v->inumber.value= left op right; return 1
#define return_FNumber1(v,       op, right) v->type=TYPE_FNUMBER; v->fnumber.value=      op right; return 0
#define return_FNumber2(v, left, op, right) v->type=TYPE_FNUMBER; v->fnumber.value= left op right; return 1

/**
 * Calculates a simple mathematical operation between two variants applying the provided operator
 * @param[out] v          Pointer to the variant where the calculation result will be stored
 * @param[in]  left       A variant containing the left operand
 * @param[in]  operator   The operation to apply
 * @param[in]  right      A variant containing the right operand
 */
static int calcOperation(Variant* v, const Variant* left, const Operator *operator, const Variant* right) {
    int intL,intR; float float1,float2;
    assert( operator!=NULL && left!=NULL && right!=NULL );
    
    if ( variantToInt(right,&intR) && variantToInt(left,&intL) ) { switch(operator->id) {
        case OP_PLUS : return_INumber1(v, +, intR);
        case OP_MINUS: return_INumber1(v, -, intR);
        case OP_NOT  : return_INumber1(v, ~, intR);
        case OP_LNOT : return_INumber1(v, !, intR);
        case OP_ADD  : return_INumber2(v, intL, +,intR); case OP_SUB: return_INumber2(v, intL, -,intR);
        case OP_MUL  : return_INumber2(v, intL, *,intR); case OP_DIV: return_INumber2(v, intL, /,intR);
        case OP_SHL  : return_INumber2(v, intL,<<,intR); case OP_SHR: return_INumber2(v, intL,>>,intR);
        case OP_AND  : return_INumber2(v, intL, &,intR); case OP_OR:  return_INumber2(v, intL, |,intR);
        case OP_XOR  : return_INumber2(v, intL, ^,intR); case OP_MOD: return_INumber2(v, intL, %,intR);
        case OP_EQ   : return_INumber2(v, intL,==,intR); case OP_NE : return_INumber2(v, intL,!=,intR);
        case OP_LT   : return_INumber2(v, intL,< ,intR); case OP_GT : return_INumber2(v, intL,> ,intR);
        case OP_LE   : return_INumber2(v, intL,<=,intR); case OP_GE : return_INumber2(v, intL,>=,intR);
        default: assert(FALSE); break;
    } }
    else if ( variantToFloat(left,&float1) && variantToFloat(right,&float2) ) { switch (operator->id) {
        case OP_ADD: return_FNumber2(v, float1, +,float2); case OP_SUB: return_FNumber2(v, float1, -,float2);
        case OP_MUL: return_FNumber2(v, float1, *,float2); case OP_DIV: return_FNumber2(v, float1, /,float2);
        default: assert(FALSE); break;
    } }
    (*v) = (*right);
    return 0;
}


/*=================================================================================================================*/
#pragma mark - > VARIANT CONTAINERS

#define calculateHash(hash,ptr,string) \
    hash=5381; ptr=(unsigned char*)string; while (*ptr) { hash = ((hash<<5)+hash) ^ *ptr++; }

#define EmptyVariantList { 0, 0 }
#define EmptyVariantMap  { 0 }

typedef struct VariantElement {
    Variant variant; union { int integer; const utf8* string; } key;
    struct VariantElement* next;
} VariantElement;

typedef struct VariantList { VariantElement *first, *last; } VariantList;
typedef struct VariantList VariantMapSlot;
typedef struct VariantMap  { VariantMapSlot* slots; } VariantMap;


static void copyVariant(Variant* dest, const Variant* sour) {
    int length; utf8* begin;
    assert( dest!=NULL && sour!=NULL );
    switch ( (dest->type=sour->type) ) {
        case TYPE_EMPTY: break;
        case TYPE_INUMBER: dest->inumber.value = sour->inumber.value; break;
        case TYPE_FNUMBER: dest->fnumber.value = sour->fnumber.value; break;
        case TYPE_ASTRING:
        case TYPE_CSTRING:
        case TYPE_UNSOLVED:
            length = (int)(sour->astring.end - sour->astring.begin);
            dest->astring.begin = (begin = stalloc(length+1));
            dest->astring.end   = begin + length;
            memcpy(begin, sour->astring.begin, length); begin[length]='\0';
            break;
    }
}

static void addVariantElementToList(VariantList* list, VariantElement* elementToAdd) {
    if (list->last) { list->last = (list->last->next = elementToAdd); }
    else            { list->last = (list->first = elementToAdd);      }
}

static void addVariantToListKI(VariantList* list, const Variant* variantToAdd, int integerKey) {
    VariantElement* element = stalloc(sizeof(VariantElement));
    element->key.integer = integerKey;
    copyVariant(&element->variant, variantToAdd);
    addVariantElementToList(list, element);
}

static void addVariantToListKS(VariantList* list, const Variant* variantToAdd, const utf8* stringKey) {
    VariantElement* element; int sizeofStringKey; utf8* copyofStringKey;
    assert( list!=NULL && variantToAdd!=NULL && stringKey!=NULL && stringKey[0]!='\0' );
    
    sizeofStringKey = (int)strlen(stringKey)+1;
    copyofStringKey = stalloc(sizeofStringKey);
    memcpy(copyofStringKey, stringKey, sizeofStringKey);
    
    element             = stalloc(sizeof(VariantElement));
    element->key.string = copyofStringKey;
    copyVariant(&element->variant, variantToAdd);
    addVariantElementToList(list, element);
}

static void addVariantToMap(VariantMap* map, Variant* variantToAdd, const utf8* stringKey) {
    unsigned hash; unsigned char* tmp;
    assert( map!=NULL && variantToAdd!=NULL && stringKey!=NULL && stringKey[0]!='\0' );
    calculateHash(hash,tmp,stringKey);
    if (map->slots==NULL) { map->slots=stalloc(NUMBER_OF_MAP_SLOTS*sizeof(VariantMapSlot)); }
    addVariantToListKS(&map->slots[hash%NUMBER_OF_MAP_SLOTS], variantToAdd, stringKey);
}

static const Variant* findVariantInMap(VariantMap* map, const utf8* stringKey) {
    unsigned hash; VariantElement* element; unsigned char* tmp;
    assert( map!=NULL && stringKey!=NULL && stringKey[0]!='\0' );
    calculateHash(hash,tmp,stringKey);
    element = map->slots ? map->slots[hash%NUMBER_OF_MAP_SLOTS].first : NULL;
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
static Bool readINumber(Variant* out_v, const utf8* ptr, const utf8** out_endptr) {
    const utf8 *last, *type; int base, value, digit;
    assert( out_v!=NULL && (ptr!=NULL && *ptr!=CH_PARAM_SEP && *ptr!='\0') );
    
    if ( !isliteral(*ptr) ) { return FALSE; }
    base = 0;

    /* try to recognize the prefix that define the number representation (base) */
    switch ( ptr[0] ) {
        /* differentiate the current address "$" from the hex prefix "$" */
        case CH_HEXPREFIX_ADDR: if (ishex(ptr[1])) { base=16; ++ptr; }
                                /* else               { (*out_value)=(theCurrAddress&0xFFFF); (*out_end)=ptr+1; return TRUE; } */
                                break;
        case CH_HEXPREFIX:      if (ishex(ptr[1])) { base=16; ++ptr; } break; /* < hex type defined with "$" */
        case CH_BINPREFIX:      if (isbin(ptr[1])) { base= 2; ++ptr; } break; /* < bin type defined with "%" */
    }
    /* find the last character */
    last=ptr; while ( isliteral(*last) ) { ++last; } --last;
    
    /* try to find the base of a possible number (if it wasn't already found) */
    if ( base==0 && isdigit(*ptr) ) {
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
    value=0; while ( ptr<=last )
    { digit=hexvalue(*ptr++); if (0<=digit && digit<base) { value*=base; value+=digit; } }
    while ( isliteral(*ptr) ) { ++ptr; }
    
    /* ATTENTION!: test if the number is a floating-point */
    if (*ptr=='.') { return FALSE; }
    
    /* everything ok!, return end pointer & value */
    if (out_endptr) { (*out_endptr)=ptr; }
    out_v->inumber.type  = TYPE_INUMBER;
    out_v->inumber.value = value;
    return TRUE;
}

/**
 * Try to read the value of a floating-point literal
 * @param[out]  out_v       (output) Pointer to the variant where the read value will be stored
 * @param[in]   ptr         Pointer to the begin of the string containing the value to read
 * @param[out]  out_endptr  (optional output) Ref to the pointer where the end of reading will be stored, can be NULL
 * @returns
 *    FALSE when no value can be read because the string format does not match with a floating-point number
 */
static Bool readFNumber(Variant* out_v, const utf8* ptr, const utf8** out_endptr) {
    
    double value=0, divisor=1;
    while ('0'<=*ptr && *ptr<='9') { value = value * 10.0 + (*ptr++ - '0'); }
    if (*ptr=='.') { ++ptr; } else { return FALSE; }
    while ('0'<=*ptr && *ptr<='9') { value = value * 10.0 + (*ptr++ - '0'); divisor*=10.0; }
    if (*ptr=='.' || divisor==0.0 ) { return FALSE; }
    
    /* everything ok!, return end pointer & value */
    if (out_endptr) { (*out_endptr)=ptr; }
    out_v->fnumber.type  = TYPE_FNUMBER;
    out_v->fnumber.value = (float)(value/divisor);
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
    if ( firstchar==CH_PARAM_SEP ) { return FALSE; }
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

static VariantMap theNameMap = EmptyVariantMap;

static Variant theVariant;
struct { const Operator* array[CALC_STACK_SIZE]; int i; } opStack;
struct { Variant         array[CALC_STACK_SIZE]; int i; } vStack;


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

static Variant * evaluateExpression(const utf8 *start, const utf8 **out_end) {
    const utf8 *ptr = start;
    const Operator* op; Variant variant, tmp; const Variant *variantRef;
    utf8 name[MAX_NAME_LEN]; Bool continueScanning, precededByNumber, opPushed=TRUE;
    assert( start!=NULL ); assert( out_end!=NULL );
    assert( *start!=CH_PARAM_SEP ); assert( *start!=CH_ENDFILE );

    theVariant.unsolved.type  = TYPE_UNSOLVED;
    theVariant.unsolved.begin = ptr;
    skipblankspaces(ptr);
    
    /*-- evaluate empty  ----------------------*/
    if ( *ptr==CH_PARAM_SEP || isendofline(*ptr) ) {
        theVariant.empty.type = TYPE_EMPTY;
    }
    /*-- evaluate C string --------------------*/
    else if ( *ptr==CH_CSTRING ) {
        theVariant.cstring.type  = TYPE_CSTRING;
        theVariant.cstring.begin = ++ptr;
        do {
            while ( !isendofcode(*ptr) && *ptr!=CH_CSTRING) { ++ptr; }
            continueScanning = ptr[0]==CH_CSTRING && *(ptr-1)==CH_STRING_ESCAPE;
            if  (continueScanning) { ptr+=2; }
        } while (continueScanning);
        theVariant.cstring.end = ptr;
        if (*ptr==CH_CSTRING) { ++ptr; }
    }
    /*-- evaluate ASM string ------------------*/
    else if ( *ptr==CH_ASMSTRING ) {
        theVariant.astring.type  = TYPE_ASTRING;
        theVariant.astring.begin = ++ptr;
        do {
            while ( !isendofcode(*ptr) && *ptr!=CH_ASMSTRING) { ++ptr; }
            continueScanning = ptr[0]==CH_ASMSTRING && ptr[1]==CH_ASMSTRING;
            if  (continueScanning) { ptr+=2; }
        } while (continueScanning);
        theVariant.astring.end = ptr;
        if (*ptr==CH_ASMSTRING) { ++ptr; }
    }
    /*-- evaluate expression ------------------*/
    else {
        cINITSTACK(opStack,   &OpSafeGuard, &OpSafeGuard);
        cINITSTACK(vStack,    EmptyVariant, EmptyVariant);
        while (*ptr!=CH_PARAM_SEP && !isendofcode(*ptr)) {
            precededByNumber=!opPushed; opPushed=FALSE;
            if (*ptr=='(') {
                cPUSH(opStack,&OpParenthesis); opPushed=TRUE; ++ptr;
            }
            else if (*ptr==')') {
                cWHILE_PRECEDENCE(>=MIN_PRECEDENCE, calcOperation,tmp,opStack,vStack);
                if (cPOP(opStack)!=&OpParenthesis) { error(ERR_UNEXPECTED_PTHESIS,0); return NULL; }
                ++ptr;
            }
            else if ( readOperator(&op,precededByNumber,ptr,&ptr) ) {
                cWHILE_PRECEDENCE(>=op->preced, calcOperation,tmp,opStack,vStack);
                cPUSH(opStack, op); opPushed=TRUE;
                
            }
            else if ( readINumber(&variant,ptr,&ptr) ) { cPUSH(vStack, variant); }
            else if ( readFNumber(&variant,ptr,&ptr) ) { cPUSH(vStack, variant); }
            else if ( readName(name,sizeof(name),ptr,&ptr) ) {
                variantRef = findVariantInMap(&theNameMap, name);
                if (variantRef==NULL) {
                    assert( theVariant.type == TYPE_UNSOLVED );
                    while (*ptr!=CH_PARAM_SEP && !isendofline(*ptr)) { ++ptr; }
                    theVariant.unsolved.end = (*out_end) = ptr;
                    return &theVariant;
                }
                cPUSH(vStack, (*variantRef));
            }
            else { error(ERR_INVALID_EXPRESSION,0); return &theVariant; }
            skipblankspaces(ptr);
        }

        /* process any pending operator before return */
        cWHILE_PRECEDENCE(>=MIN_PRECEDENCE, calcOperation,tmp,opStack,vStack);
        /* while ( cPEEK(opStack)!=&OpSafeGuard ) { cEXECUTE(calcOperation,opStack,vStack); } */
        if ( cPEEK(opStack)==&OpParenthesis) { error(ERR_TOO_MANY_OPEN_PTHESES,0); return NULL; }
        if ( vStack.i!=3 || opStack.i!=2 ) { error(ERR_INVALID_EXPRESSION,0); return &theVariant; }
        theVariant = cPOP(vStack);
    }

    while (*ptr!=CH_PARAM_SEP && !isendofline(*ptr)) { ++ptr; }
    (*out_end) = ptr;
    
    return &theVariant;
}


/*=================================================================================================================*/
#pragma mark - > DEFERRED OUTPUT

/* ATTENTION!: this structure must have the same format that `VariantElement` */
typedef struct DeferredOutput { Variant variant; int userValue; } DeferredOutput;

static VariantList deferredOutputList = EmptyVariantList;

static void addDeferredOutput(int userValue, const Variant* variant) {
    addVariantToListKI(&deferredOutputList, variant, userValue);
}
static const DeferredOutput* getFirstDeferredOutput(void) {
    return (const DeferredOutput*)deferredOutputList.first;
}

static const DeferredOutput* getNextDeferredOutput(const DeferredOutput* output) {
    return (DeferredOutput*) ((const VariantElement*)output)->next;
}

/*=================================================================================================================*/
#pragma mark - > MAIN CODE

/*
 qeval addConstant(..)
 qeval evaluateExpression(..)
 qeval addDeferredOutput(..)
 qeval solveDeferredOutputs(..)
 qeval getNextDeferredOutput(..)
 */

static void init(void) {
    g.curFilePath   = NULL;
    g.curLineNumber = 0;
    g.errorCount    = 0;
}


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
        prevFilePath   = g.curFilePath   ; g.curFilePath   = filePath;
        prevLineNumber = g.curLineNumber ; g.curLineNumber = 1;
        ptr = fileBuffer; while ( *ptr!=CH_ENDFILE && success ) {
            evaluateTextLine(ptr,&ptr); ++g.curLineNumber;
        }
        g.curFilePath   = prevFilePath;
        g.curLineNumber = prevLineNumber;
    }
    /* release resources and return */
    free(fileBuffer);
    return success ? TRUE : FALSE;
}

static void printDeferredOutput() {
    utf8 str[256]; const DeferredOutput* output;
    for ( output=getFirstDeferredOutput(); output; output=getNextDeferredOutput(output) ) {
        switch (output->variant.type) {
            case TYPE_EMPTY:    printf("<empty>"); break;
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


/*=================================================================================================================*/
#pragma mark - > MAIN

#define isOption(param,name1,name2) (strcmp(param,name1)==0 || strcmp(param,name2)==0)
#define MAX_FILES 256  /* maximum number of files to evaluate */

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




