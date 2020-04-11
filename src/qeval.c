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

#define MIN_FILE_SIZE   (0)           /* < minimum size for loadable files (in bytes)   */
#define MAX_FILE_SIZE   (1024L*1024L) /* < maximum size for loadable files (in bytes)   */
#define MAX_ERROR_COUNT (32)          /* < maximum number of errors able to be reported */
#define MAX_NAME_LEN    (63)          /* < maximum length for names of labels, opcodes, symbols, etc (in bytes) */
#define MAX_ERRSTR_LEN  (63)          /* < ... */
#define MAX_ERRMSG_LEN  (63)          /* < ... */
#define CALC_STACK_SIZE (256)         /* < the calculator's stack size in number of elements */
#define MIN_PRECEDENCE  1             /* < the minimum valid operator predecence */



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

/* supported operators */
typedef enum OperatorID {
    OP_ADD, OP_SUB, OP_MUL, OP_DIV, OP_MOD, OP_SHL, OP_SHR,
    OP_EQ, OP_NE, OP_AND, OP_XOR, OP_OR, OP_LAND, OP_LOR, OP_INVALID
} OperatorID;

typedef struct Operator { OperatorID id; const utf8* str; int preced; } Operator;

/* information about each supported operators */
static const Operator theOperators[15] = {
    {OP_MUL ,"* ",11}, {OP_DIV,"/ ",11}, {OP_MOD,"% ",11},
    {OP_ADD ,"+ ",10}, {OP_SUB,"- ",10},
    {OP_SHL ,"<<", 9}, {OP_SHR,">>", 9},
    {OP_EQ  ,"==", 7}, {OP_NE ,"!=", 7},
    {OP_AND ,"& ", 6},
    {OP_XOR ,"^ ", 5},
    {OP_OR  ,"| ", 4},
    {OP_LAND,"&&", 3},
    {OP_LOR ,"||", 2},
    /* {OP_TERN,"?",1} */
    {0,NULL,0}
};
static const Operator OpParenthesis = {OP_INVALID,"",(MIN_PRECEDENCE-1)};
static const Operator OpSafeGuard   = {OP_INVALID,"",(MIN_PRECEDENCE-1)};


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
#pragma mark - > HANDLING ERRORS

static Error       theErrors[MAX_ERROR_COUNT];
static const utf8 *theFileName   = NULL;
static int         theLineNumber = 0;
static int         theErrorCount = 0;

#ifdef NDEBUG
#    define DLOG(x)
#else
#    define DLOG(x) printf x; printf("\n")
#endif

/**
 * Global var that indicates whether the program is being successfully executed.
 * It is TRUE while no error is reported.
 */
#define success (theErrorCount==0)

/**
 * Reports a error
 * @param errorID  The error identifier, ex: ERR_CANNOT_READ_FILE
 * @param str      The optional text attached to the error reported (it can be NULL)
 */
static Bool error(ErrorID errorID, const utf8 *str) {
    Error *error; if (theErrorCount<MAX_ERROR_COUNT) {
        error = &theErrors[theErrorCount++];
        error->id=errorID;
        error->filename=stralloc(theFileName,-1);
        error->line=theLineNumber;
        error->str=stralloc(str,MAX_ERRSTR_LEN);
    }
    return (errorID==ERR_NO_ERROR);
}

static Bool printErrorMessages(void) {
    const utf8 *message; Error *error=theErrors; int count=theErrorCount;
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
    return (theErrorCount>0);
}


/*=================================================================================================================*/
#pragma mark - > VARIANTS AND MATH

typedef enum   VariantType { TYPE_UNSOLVED, TYPE_EMPTY, TYPE_ASTRING, TYPE_CSTRING, TYPE_INUMBER, TYPE_FNUMBER } VariantType;
typedef union  Variant {
    VariantType type;
    struct { VariantType type;                          } empty;     /* < empty data          */
    struct { VariantType type; const utf8 *start;       } unsolved;  /* < unsolved expression */
    struct { VariantType type; const utf8 *start, *end; } astring;   /* < assembler string    */
    struct { VariantType type; const utf8 *start, *end; } cstring;   /* < C string            */
    struct { VariantType type; int   value;             } inumber;   /* < integer number      */
    struct { VariantType type; float value;             } fnumber;   /* < float point number  */
} Variant;

static const Variant EmptyVariant = { TYPE_EMPTY };

static Bool variantToInt(const Variant* variant, int* i) {
    if (variant->type==TYPE_INUMBER) { (*i)=variant->inumber.value;    return TRUE; }
    if (variant->type==TYPE_ASTRING) { (*i)=variant->astring.start[0]; return TRUE; }
    return FALSE;
}

static Bool variantToFloat(const Variant* variant, float* f) {
    if (variant->type==TYPE_FNUMBER) { (*f)=variant->fnumber.value;        return TRUE; }
    if (variant->type==TYPE_INUMBER) { (*f)=(float)variant->inumber.value; return TRUE; }
    return FALSE;
}

static Bool variantToString(const Variant* variant, utf8* buffer, int bufferSize) {
    utf8 *dest, *destend; const utf8 *ptr, *ptrend; int ch, digit;
    assert( variant!=NULL && buffer!=NULL && bufferSize>0 );
    switch (variant->type) {
            
        case TYPE_ASTRING:
            safecpy_begin(dest,destend,buffer,bufferSize);
            ptr=variant->astring.start; ptrend=variant->astring.end;
            do {
                while (ptr<ptrend && *ptr!=CH_ASMSTRING) { safecpy(dest,destend,ptr); }
                if  (ptr<ptrend) { safecpy_char(dest,destend,CH_ASMSTRING); ptr+=2; }
            } while (ptr<ptrend);
            safecpy_end(dest,destend);
            return TRUE;

        case TYPE_CSTRING:
            safecpy_begin(dest,destend,buffer,bufferSize);
            ptr=variant->cstring.start; while(ptr<variant->cstring.end) {
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

static void printVariant(const Variant* variant) {
    utf8 str[1024];
    switch (variant->type) {
        case TYPE_EMPTY:    printf("<empty>"); break;
        case TYPE_UNSOLVED: printf("<??>"); break;
        case TYPE_INUMBER:  printf("%d",variant->inumber.value); break;
        case TYPE_FNUMBER:  printf("%f",variant->fnumber.value); break;
        case TYPE_ASTRING:
        case TYPE_CSTRING:
            variantToString(variant,str,sizeof(str));
            printf("%s",str);
            break;
    }
}

static void printSeparator() { printf(" "); }

#define return_INumber(r,   int1, op,   int2) r->type=TYPE_INUMBER; r->inumber.value=   int1 op   int2; return
#define return_FNumber(r, float1, op, float2) r->type=TYPE_FNUMBER; r->fnumber.value= float1 op float2; return

/**
 * Calculates a simple mathematical operation between two variants applying the provided operator
 * @param[out] r          Pointer to the variant where the calculation result will be stored
 * @param[in]  variant1   The left operand
 * @param[in]  operator   The operation to apply
 * @param[in]  variant2   The right operand
 */
static void calcOperation(Variant* r, const Variant* variant1, const Operator *operator, const Variant* variant2) {
    int int1,int2; float float1,float2;
    assert( operator!=NULL && variant1!=NULL && variant2!=NULL );
    
    if ( variantToInt(variant1,&int1) && variantToInt(variant2,&int2) ) { switch (operator->id) {
        case OP_ADD: return_INumber(r, int1, +,int2); case OP_SUB: return_INumber(r, int1, -,int2);
        case OP_MUL: return_INumber(r, int1, *,int2); case OP_DIV: return_INumber(r, int1, /,int2);
        case OP_SHL: return_INumber(r, int1,<<,int2); case OP_SHR: return_INumber(r, int1,>>,int2);
        case OP_AND: return_INumber(r, int1, &,int2); case OP_OR:  return_INumber(r, int1, |,int2);
        case OP_XOR: return_INumber(r, int1, ^,int2); case OP_MOD: return_INumber(r, int1, %,int2);
        default: assert(FALSE); break;
    } }
    else if ( variantToFloat(variant1,&float1) && variantToFloat(variant2,&float2) ) { switch (operator->id) {
        case OP_ADD: return_FNumber(r, float1, +,float2); case OP_SUB: return_FNumber(r, float1, -,float2);
        case OP_MUL: return_FNumber(r, float1, *,float2); case OP_DIV: return_FNumber(r, float1, /,float2);
        default: assert(FALSE); break;
    } }
    (*r) = (*variant1);
    return;
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
    
    /* everything ok!, return end pointer & value */
    if (out_endptr) { (*out_endptr)=ptr; }
    out_v->inumber.type  = TYPE_INUMBER;
    out_v->inumber.value = value;
    return TRUE;
}

/**
 * Try to read the value of a float point literal
 * @param[out]  out_v       (output) Pointer to the variant where the read value will be stored
 * @param[in]   ptr         Pointer to the begin of the string containing the value to read
 * @param[out]  out_endptr  (optional output) Ref to the pointer where the end of reading will be stored, can be NULL
 * @returns
 *    FALSE when no value can be read because the string format does not match with a float point number
 */
static Bool readFNumber(Variant* out_v, const utf8* ptr, const utf8** out_endptr) {
    return FALSE;
}

/**
 * Try to read a operator
 * @param[out] out_op    (output) Ref to the pointer where returns a pointer to a structure with info about the operator that was read in
 * @param[in]  ptr       pointer to the begin of the string containing the operator to read
 * @param[out] out_endptr   (optional output) Ref to the pointer where the end of reading will be stored, can be NULL
 * @returns
 *    FALSE when no operator can be read because the string format does not match with any known operator
 */
static Bool readOperator(const Operator** out_op, const utf8* ptr, const utf8** out_endptr) {
    const Operator* op; int firstchar;
    assert( out_op!=NULL && ptr!=NULL );
    
    firstchar = ptr[0];
    if ( firstchar==CH_PARAM_SEP ) { return FALSE; }
    for ( op=theOperators ; op->str!=NULL ; ++op ) {
        if ( op->str[0]==firstchar ) {
            if ( op->str[1]==' '    ) { *out_op=op; if (out_endptr) { (*out_endptr)=ptr+1; } return TRUE; }
            if ( op->str[1]==ptr[1] ) { *out_op=op; if (out_endptr) { (*out_endptr)=ptr+2; } return TRUE; }
        }
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

static Variant theVariant;
struct { const Operator* array[CALC_STACK_SIZE]; int i; } opStack;
struct { Variant         array[CALC_STACK_SIZE]; int i; } vStack;


/** Macros to manage calculator's stacks */
#define cINITSTACK(stack,value0,value1) stack.array[0]=value0; stack.array[1]=value1; stack.i=2;
#define cPUSH(stack,value)              (stack.array[stack.i++]=value)
#define cPOP(stack)                     (stack.array[--stack.i])
#define cPEEK(stack)                    (stack.array[stack.i-1])
#define cEXECUTE(f, oStack, vStack) \
    f(&vStack.array[vStack.i-2], &vStack.array[vStack.i-2], oStack.array[--oStack.i], &vStack.array[vStack.i-1]); \
    --vStack.i;

#define cWHILE_PRECEDENCE(cond, f, oStack, vStack) \
    while (cPEEK(oStack)->preced cond) { cEXECUTE(f,oStack,vStack); }

static Variant * startEvaluation(const utf8 *start, const utf8 **out_end) {
    const utf8 *ptr = start;
    const Operator* op; Variant variant;
    utf8 name[MAX_NAME_LEN]; Bool continueScanning;
    assert( start!=NULL ); assert( out_end!=NULL );
    assert( *start!=CH_PARAM_SEP ); assert( *start!=CH_ENDFILE );

    theVariant.unsolved.type  = TYPE_UNSOLVED;
    theVariant.unsolved.start = ptr;

    skipblankspaces(ptr);
    if ( *ptr==CH_PARAM_SEP || isendofline(*ptr) ) {
        theVariant.empty.type = TYPE_EMPTY;
    }
    /*-- evaluate C string --------------------*/
    else if ( *ptr==CH_CSTRING ) {
        theVariant.cstring.type  = TYPE_CSTRING;
        theVariant.cstring.start = ++ptr;
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
        theVariant.astring.start = ++ptr;
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
            if (*ptr=='(') {
                cPUSH(opStack,&OpParenthesis); ++ptr;
            }
            else if (*ptr==')') {
                cWHILE_PRECEDENCE(>=MIN_PRECEDENCE, calcOperation,opStack,vStack);
                /* while (cPEEK(opStack)!=&OpParenthesis) { cEXECUTE(calcOperation,opStack,vStack); } */
                if (cPOP(opStack)!=&OpParenthesis) { error(ERR_UNEXPECTED_PTHESIS,0); return NULL; }
                ++ptr;
            }
            else if ( readINumber(&variant,ptr,&ptr) ) { cPUSH(vStack, variant); }
            else if ( readFNumber(&variant,ptr,&ptr) ) { cPUSH(vStack, variant); }
            else if ( readOperator(&op,ptr,&ptr) ) {
                cWHILE_PRECEDENCE(>=op->preced, calcOperation,opStack,vStack);
                cPUSH(opStack, op);
            }
            else if ( readName(name,sizeof(name),ptr,&ptr) ) {
                
                assert( FALSE );
            }
            else { error(ERR_INVALID_EXPRESSION,0); return &theVariant; }
            skipblankspaces(ptr);
        }

        /* process any pending operator before return */
        cWHILE_PRECEDENCE(>=MIN_PRECEDENCE, calcOperation,opStack,vStack);
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
#pragma mark - > QEVAL

/*
    named-map     :  name -> [Value]
    delayed-list  :  [Values]
    unsolved-list :  [Values]


    LD HL, delayed           -> exp = Solve("delayed"); AddToDelayList(exp, address, linenum)
    delayed  EQU constant*2  -> exp = Solve("constant*2"); AddToDelayList(exp,0,linenum); SetConst(delayed, exp)
    constant EQU 100+10      -> exp = Solve("100+10"); SetConst(constant, exp)
    LD HL, constant          -> exp = Solve("constant"); (*address) = ToInt16(exp)


*/

Variant * theImportantList;
Variant * theUnsolvedList;


/*
static Bool qeval_AddConstant(const utf8 *name, Value *value) {
    return TRUE;
}
*/

/*
static Bool qeval_AddVariable(const utf8 *name, Value *value) {
    assert( FALSE );
    return TRUE;
}
*/

/*
static Bool qeval_AddImportantValue(Value *value, int user1, int user2) {
    value->user1 = user1;
    value->user2 = user2;
    value->prev = last;
    last->next  = value;
    return TRUE;
}
 */


#define qeval_FirstImportantValue() theImportantList
#define qeval_NextImportantValue(value) ((value)->nextImportant)

/*=================================================================================================================*/
#pragma mark - > MAIN


static void main_InitGlobals() {
    theFileName      = NULL;
    theLineNumber    = 0;
    theErrorCount    = 0;
    theImportantList = NULL;
    theUnsolvedList  = NULL;
}

static Bool main_EvaluateTextLine(const utf8 *start, const utf8 **out_end) {

    Bool printByDefault = FALSE;
    Bool continueScanning;
    Variant* variant;
    const utf8 *ptr = start;
    utf8 name[128], *dest, *destend;

    /* skip all blank spaces at the beginning of the line */
    ptr=start; while ( isblank(*ptr) ) { ++ptr; }

    /* extract name */
    safecpy_begin( dest, destend, name, sizeof(name) );
    while ( !isendofcode(*ptr) && !isblank(*ptr) ) { safecpy_upper(dest,destend,ptr);  }
    safecpy_end( dest, destend );
    while (isblank(*ptr)) { ++ptr; }

    /* detect var assignation "=" */
    if ( *ptr=='=' ) {
        variant = startEvaluation(ptr+1,&ptr);
        /* DLOG_VARIANT(*variant); */
        /* qeval_AddConstantValue(name,value); */
    }
    /* detect output directive: "PRINT", "?" */
    else if (0==strcmp(name,"PRINT") || 0==strcmp(name,"?") || printByDefault) {
        do {
            variant = startEvaluation(ptr,&ptr);
            printVariant(variant);
            /* qeval_AddImportantValue(value,linenum,0); */
            continueScanning = (*ptr==CH_PARAM_SEP);
            if  (continueScanning) { ++ptr; printSeparator(); }
        } while (continueScanning); 
        theVariant.astring.type  = TYPE_ASTRING;
        theVariant.astring.start = "\n";
        theVariant.astring.end   = theVariant.astring.start + 1;
        printVariant(&theVariant);
    }

    /* place pointer in the first character of the next line and return it */
    while (!isendofline(*ptr)) { ++ptr; } skipendofline(ptr);
    if (out_end) { (*out_end) = ptr; }
    return TRUE;
}

static utf8 * allocFileBuffer(const utf8* filePath) {
    FILE *file=NULL; utf8 *fileBuffer=NULL; long fileSize=0;
    assert(filePath!=NULL);

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
    if (!success) { free(fileBuffer); fileBuffer=NULL; }
    if (file) { fclose(file); }
    return fileBuffer;
}


static void main_PrintImportantValues() {
    /*
    utf8 str[1024];
    Value *value = qeval_FirstImportantValue();
    while (value) {
        if ( qeval_ToString(value,str,sizeof(str)) ) { printf("%s",str); }
        else                                         { printf("<?>");  }
        value = qeval_NextImportantValue(value);
    }
    */
}

/*=================================================================================================================*/
#pragma mark - > MAIN

#define isOption(param,name1,name2) (strcmp(param,name1)==0 || strcmp(param,name2)==0)
#define MAX_FILES 256  /* maximum number of files to evaluate */


int main(int argc, char *argv[]) {
    int i; const utf8 *fileBuffer, *ptr;
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
        main_InitGlobals();
        for (i=0; i<numberOfFiles; ++i) {
            theFileName   = filePaths[i];
            theLineNumber = 0;
            fileBuffer = allocFileBuffer(theFileName);
            if (fileBuffer) {
                ptr=fileBuffer; theLineNumber=1;
                while ( *ptr!=CH_ENDFILE && success ) {
                    main_EvaluateTextLine(ptr,&ptr);
                    ++theLineNumber;
                }
            }
            free((void*)fileBuffer);
        }
        if (!printErrorMessages()) { main_PrintImportantValues(); }
    }
    
    return 0;
}




