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
#define VERSION "0.1"                 /* < QEVAL current version                        */
#define MIN_FILE_SIZE   (0L)          /* < minimum size for loadable files (in bytes)   */
#define MAX_FILE_SIZE   (1024L*1024L) /* < maximum size for loadable files (in bytes)   */
#define MAX_ERROR_COUNT (32)          /* < maximum number of errors able to be reported */
#define MAX_NAME_LEN    (63)          /* < maximum length for names of labels, opcodes, symbols, etc (in bytes) */
#define CALC_STACK_SIZE (256)         /* < the calculator's stack size in number of elements */



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
    SUCCESS=0, ERR_FILE_NOT_FOUND=-1000, ERR_FILE_TOO_BIG, ERR_NOT_ENOUGH_MEMORY, ERR_INT8_OUT_OF_RANGE,
    ERR_INT16_OUT_OF_RANGE, ERR_BITNUM_OUT_OF_RANGE, ERR_INVALID_EXPRESSION, ERR_INVALID_IMODE,
    ERR_UNEXPECTED_PARENTHESIS, ERR_INVALID_RST_ADDRESS, ERR_DISP_MUST_BE_ZERO,
    ERR_DISP_OUT_OF_RANGE, ERR_UNKNOWN_PARAM
} ErrorID;

/* supported operators */
typedef enum OperatorID {
    OP_ADD, OP_SUB, OP_MUL, OP_DIV, OP_MOD, OP_SHL, OP_SHR, OP_AND, OP_OR, OP_XOR
} OperatorID;

typedef struct Operator { OperatorID id; const utf8* str; int preced; } Operator;

/* information about each supported operators */
static const Operator theOperators[13] = {
    {OP_SHL ,"<<",  0}, {OP_SHR  ,">>", 0}, {OP_AND,"& ",0}, {OP_OR ,"| ",0}, {OP_XOR,"^ ",5},
    {OP_ADD ,"+ ",  6}, {OP_SUB  ,"- ", 6}, {OP_MUL,"* ",5}, {OP_DIV,"/ ",5}, {OP_MOD,"% ",5},
    {0,NULL,0}
};
static const Operator OpParenthesis = {-1,"",255};
static const Operator OpSafeGuard   = {-1,"",255};


typedef struct Error { ErrorID id; const utf8 *filename; int line; } Error;




typedef enum ValueType { TYPE_UNSOLVED, TYPE_EMPTY, TYPE_INTEGER, TYPE_ASTRING, TYPE_CSTRING } ValueType;

typedef struct Expression {
    ValueType type;
    union {
        struct { const utf8 *ptr;         } unsolved;
        struct { int value;               } integer;
        struct { const utf8 *start, *end; } astring;
        struct { const utf8 *start, *end; } cstring;
    } x;
} Expression;



const char * ValueTypeToString(ValueType type) {
    switch (type) {
        case TYPE_UNSOLVED: return "UNSOLVED"; 
        case TYPE_EMPTY:    return "EMPTY";
        case TYPE_ASTRING:  return "ASTRING";
        case TYPE_CSTRING:  return "CSTRING";
        case TYPE_INTEGER:  return "INTEGER";
    }
    return "Unknown";
}

/*=================================================================================================================*/
#pragma mark - > HELPER FUNCTIONS

#define INRANGE(c,min,max) (min<=c && c<=max)
#define PARAM_IS(param,name1,name2) (strcmp(param,name1)==0 || strcmp(param,name2)==0)

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

#ifdef NDEBUG
#   define DLOG_VALUE(value)
#else
    void DLOG_VALUE(const Expression value) {
        switch (value.type) {
            case TYPE_UNSOLVED: DLOG(("<Unsolved>")); break;
            case TYPE_INTEGER:  DLOG(("<Integer value=%d>", value.x.integer.value)); break;
            case TYPE_ASTRING:  DLOG(("<AString value=\"%c%c...\">", value.x.astring.start[0], value.x.astring.start[1] )); break;
            case TYPE_CSTRING:  DLOG(("<CString value=\"%c%c...\">", value.x.cstring.start[0], value.x.cstring.start[1] )); break;
            default:       DLOG(("<Unknown> ERROR!")); break;
        }
    }
#endif

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
#pragma mark - > READING ELEMENTS FROM THE TEXT buffer

/**
 * Try to read the value of a integer literal
 * @param[in]   ptr          pointer to the begin of the string with the value to read
 * @param[out]  out_end      (output) returns a pointer to the end of the read literal
 * @param[out]  out_value    (output) returns the value of the read literal
 * @return
 *    FALSE = no value can be read (may be the string is an operator or something else?)
 */
static Bool ReadLiteral(const utf8 *ptr, const utf8 **out_end, int *out_value) {
    const utf8 *last, *type; int base, value, digit;
    assert( out_value!=NULL && out_end!=NULL && ptr!=NULL && *ptr!=CH_PARAM_SEP && *ptr!='\0' );
    
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
    (*out_end)=ptr; (*out_value)=value; return TRUE;
}

/**
 * Try to read any name (vars, labels, etc) from the provided string
 * @param[in]  ptr        pointer to the string to read
 * @param[out] out_end    (output) returns a pointer to the next char to read immediately after the label
 * @param[in]  buffer     the buffer that will be filled in with the label name
 * @param[in]  bufferSize the buffer size in bytes
 */
static Bool ReadName(const utf8 *ptr, const utf8 **out_end, utf8 *buffer, int bufferSize) {
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

/**
 * Try to read a operator from the provided string
 * @param[in]  ptr       pointer to the string to read
 * @param[out] out_end   (output) returns a pointer to next char to read immediately after the operator
 * @param[out] out_op    (output) returns a pointer to a structure with info about the operator that was read in
 */
static Bool ReadOperator(const utf8 *ptr, const utf8 **out_end, const Operator **out_op) {
    const Operator* op; int firstchar;
    assert( out_op!=NULL && out_end!=NULL && ptr!=NULL );
    
    firstchar = ptr[0];
    if ( firstchar==CH_PARAM_SEP ) { return FALSE; }
    for ( op=theOperators ; op->str!=NULL ; ++op ) {
        if ( op->str[0]==firstchar ) {
            if ( op->str[1]==' '    ) { (*out_op)=op; (*out_end)=ptr+1; return TRUE; }
            if ( op->str[1]==ptr[1] ) { (*out_op)=op; (*out_end)=ptr+2; return TRUE; }
        }
    }
    /* the operator wasn't correctly identified */
    return FALSE;
}


/*=================================================================================================================*/
#pragma mark - > MATHEMATICAL EXPRESSION CALCULATION

static Expression theValue;
struct { const Operator* array[CALC_STACK_SIZE]; int i; } opStack;
struct { int             array[CALC_STACK_SIZE]; int i; } valueStack;


/** Macros to manage calculator's stacks */
#define cINITSTACK(stack,value0,value1) stack.array[0]=value0; stack.array[1]=value1; stack.i=2;
#define cPUSH(stack,value)              (stack.array[stack.i++]=value)
#define cPOP(stack)                     (stack.array[--stack.i])
#define cPEEK(stack)                    (stack.array[stack.i-1])
#define cEXECUTE(f, oStack, vStack) \
    vStack.array[vStack.i-2] = f(oStack.array[--oStack.i], vStack.array[vStack.i-1], vStack.array[vStack.i-2]); \
    --vStack.i;

/**
 * Solve a simple mathematical operation between two integer values applying the provided operator
 * @param[in] operator  the operation to apply
 * @param[in] arg1      the left value
 * @param[in] arg2      the right value
 */
static int SolveIntCalculation(const Operator *operator, int arg1, int arg2) {
    switch (operator->id) {
        case OP_ADD: return arg1 +  arg2;    case OP_SUB: return arg1 -  arg2;
        case OP_MUL: return arg1 *  arg2;    case OP_DIV: return arg1 /  arg2;
        case OP_MOD: return arg1 %  arg2;    case OP_SHL: return arg1 << arg2;
        case OP_SHR: return arg1 >> arg2;    case OP_AND: return arg1 &  arg2;
        case OP_OR:  return arg1 |  arg2;    case OP_XOR: return arg1 ^  arg2;
        default:     return arg1;
    }
}

/**
 * Solve a simple mathematical operation between two floating point values using the provided operator
 * @param[in] operator  the operation to apply
 * @param[in] arg1      the left value
 * @param[in] arg2      the right value
 */
static float SolveFloatCalculation(const Operator *operator, float arg1, float arg2) {
    assert( FALSE ); /* not implemented yet */
    return arg1;
}


static Expression * StartCalculation(const utf8 *start, const utf8 **out_end) {
    const utf8 *ptr = start;
    const Operator* op;
    int value; utf8 name[MAX_NAME_LEN]; Bool continueScanning;
    assert( start!=NULL ); assert( out_end!=NULL );
    assert( *start!=CH_PARAM_SEP ); assert( *start!=CH_ENDFILE );

    theValue.type           = TYPE_UNSOLVED;
    theValue.x.unsolved.ptr = ptr;

    skipblankspaces(ptr);
    if ( *ptr==CH_PARAM_SEP || isendofline(*ptr) ) {
        theValue.type = TYPE_EMPTY;
    }
    else if ( *ptr==CH_CSTRING ) {
        theValue.type          = TYPE_CSTRING;
        theValue.x.cstring.start = ++ptr;
        do {
            while ( !isendofcode(*ptr) && *ptr!=CH_CSTRING) { ++ptr; }
            continueScanning = ptr[0]==CH_CSTRING && *(ptr-1)==CH_STRING_ESCAPE;
            if  (continueScanning) { ptr+=2; }
        } while (continueScanning);
        theValue.x.cstring.end = ptr;
        if (*ptr==CH_CSTRING) { ++ptr; }
    }
    else if ( *ptr==CH_ASMSTRING ) {
        theValue.type          = TYPE_ASTRING;
        theValue.x.astring.start = ++ptr;
        do {
            while ( !isendofcode(*ptr) && *ptr!=CH_ASMSTRING) { ++ptr; }
            continueScanning = ptr[0]==CH_ASMSTRING && ptr[1]==CH_ASMSTRING;
            if  (continueScanning) { ptr+=2; }
        } while (continueScanning);
        theValue.x.astring.end = ptr;
        if (*ptr==CH_ASMSTRING) { ++ptr; }
    }
    else {
        cINITSTACK(opStack,    &OpSafeGuard, &OpSafeGuard);
        cINITSTACK(valueStack,            0,            0);
        while (*ptr!=CH_PARAM_SEP && !isendofcode(*ptr)) {
            if (*ptr=='(') {
                cPUSH(opStack,&OpParenthesis);
            }
            else if (*ptr==')') {
                while (cPEEK(opStack)!=&OpParenthesis) { cEXECUTE(SolveIntCalculation,opStack,valueStack); }
                if (cPOP(opStack)==&OpSafeGuard) { err(ERR_UNEXPECTED_PARENTHESIS); return NULL; }
            }
            else if ( ReadLiteral(ptr,&ptr,&value) ) {
                cPUSH(valueStack, value);
            }
            else if ( ReadName(ptr,&ptr,name,sizeof(name)) ) {
                assert( FALSE );
            }
            else if ( ReadOperator(ptr,&ptr,&op) ) {
                while (cPEEK(opStack)->preced<=op->preced) { cEXECUTE(SolveIntCalculation,opStack,valueStack) }
                cPUSH(opStack,op);
            }
            else {
                err(ERR_INVALID_EXPRESSION);
                return &theValue;
            }
            skipblankspaces(ptr);
        }

        /* process any pending operator before return */
        while ( cPEEK(opStack)!=&OpSafeGuard ) { cEXECUTE(SolveIntCalculation,opStack,valueStack); }
        if ( valueStack.i!=3 || opStack.i!=2 ) { err(ERR_INVALID_EXPRESSION); return &theValue; }

        theValue.type          = TYPE_INTEGER;
        theValue.x.integer.value = cPOP(valueStack);
    }

    while (*ptr!=CH_PARAM_SEP && !isendofline(*ptr)) { ++ptr; }
    (*out_end) = ptr;
    return &theValue;
}


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

Expression * theDelayedList;
Expression * theUnsolvedList;


static Expression * qeval_MakeExpression(const utf8 *start, const utf8 **out_end) {
    const utf8 *ptr = start;
    Expression *expression = malloc(sizeof(Expression));
    expression->type = TYPE_UNSOLVED;
    if (out_end) { (*out_end)=ptr; }
    return expression;
}


static Bool qeval_AddNamedExpression(const utf8 *name, Expression *expression) {

    return TRUE;
}

static Bool qeval_AddDelayedExpression(int user1, int user2, Expression *expression) {
    /*
    expression->user1 = user1;
    expression->user2 = user2;
    expression->prev = last;
    last->next = expression;
    */
    return TRUE;
}

static Bool qeval_ToInt16(const Expression *value, int *out_integer) {
    switch (value->type) {
        case TYPE_INTEGER:
            (*out_integer) = value->x.integer.value;
            return TRUE;
        default:
            return FALSE;
    }
}

static Bool qeval_ToString(const Expression *value, utf8 *buffer, int bufferSize) {
    utf8 *dest, *destend; const utf8 *ptr, *ptrend;
    int ch, digit;

    switch (value->type)
    {
        case TYPE_ASTRING:
            safecpy_begin(dest,destend,buffer,bufferSize);
            ptr=value->x.astring.start; ptrend=value->x.astring.end;
            do {
                while (ptr<ptrend && *ptr!=CH_ASMSTRING) { safecpy(dest,destend,ptr); }
                if  (ptr<ptrend) { safecpy_char(dest,destend,CH_ASMSTRING); ptr+=2; }
            } while (ptr<ptrend);
            safecpy_end(dest,destend);
            return TRUE;

        case TYPE_CSTRING:
            safecpy_begin(dest,destend,buffer,bufferSize);
            ptr=value->x.cstring.start; while(ptr<value->x.cstring.end) {
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

#define qeval_FirstDelayedExpression() theDelayedList
#define qeval_NextDelayedExpression(exp) ((exp)->nextDelayed)

/*=================================================================================================================*/
#pragma mark - > MAIN


static void main_InitGlobals() {
    theFileName     = NULL;
    theLineNumber   = 0;
    theErrorCount   = 0;
    theDelayedList  = NULL;
    theUnsolvedList = NULL;
}

static void main_PrintValue(const Expression *value) {
    utf8 str[1024]; int integer;
    if      (qeval_ToInt16(value,&integer))         { printf("%d ",integer); }
    else if (qeval_ToString(value,str,sizeof(str))) { printf("%s",str);      }
    else                                            { printf("??? ");        }
}

static Bool main_EvaluateTextLine(const utf8 *start, const utf8 **out_end) {

    Bool printByDefault = FALSE;
    Bool continueScanning;
    Expression *value;
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
        value = StartCalculation(ptr+1,&ptr);
        /* DLOG_VALUE(*value); */
        /* qeval_AddNamedExpression(name,expression); */
    }
    /* detect output directive: "PRINT", "?" */
    else if (0==strcmp(name,"PRINT") || 0==strcmp(name,"?") || printByDefault) {
        do {
            value = StartCalculation(ptr,&ptr);
            main_PrintValue(value);
            /* AddDelayedExpression(linenum,0,expression); */
            continueScanning = (*ptr==CH_PARAM_SEP);
            if  (continueScanning) { ++ptr; }
        } while (continueScanning); 
        theValue.type = TYPE_ASTRING;
        theValue.x.astring.start = "\n";
        theValue.x.astring.end   = theValue.x.astring.start + 1;
        main_PrintValue(&theValue);
    }

    /* place pointer in the first character of the next line and return it */
    while (!isendofline(*ptr)) { ++ptr; } skipendofline(ptr);
    if (out_end) { (*out_end) = ptr; }
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
    /*
    utf8 str[1024];
    Expression *expression = qeval_FirstDelayedExpression();
    while (expression) {
        if ( qeval_ToString(expression,str,sizeof(str)) ) { printf("%s",str); }
        else                                              { printf("<?>");  }
        expression = qeval_NextDelayedExpression(expression);
    }
    */
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



