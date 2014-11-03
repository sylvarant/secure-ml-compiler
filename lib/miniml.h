/*
 * =====================================================================================
 *
 *       Filename:  miniml.h
 *
 *    Description:  The definitions we need to compile into
 *
 *         Author:  Adriaan Larmuseau, ajhl
 *        Company:  Uppsala
 *
 * =====================================================================================
 */


#ifndef MINIML_INCLUDED
#define MINIML_INCLUDED

#include "binding.h" // adds global.h !

/*-----------------------------------------------------------------------------
 *  Preprocessing
 *-----------------------------------------------------------------------------*/
#define SCM_(TYPE) struct V(TYPE) { \
                          TAG t;

#define _SCM }; 


/*-----------------------------------------------------------------------------
 * Base Types 
 *-----------------------------------------------------------------------------*/

enum{ MFALSE = 0, MTRUE = 1 };


/*-----------------------------------------------------------------------------
 * Types
 *-----------------------------------------------------------------------------*/

typedef enum T(Tag_e){
    T(UNIT), T(INT), T(BOOLEAN), T(ARROW), T(STAR)
} T(TAG);

typedef union Type_u{
    T(TAG) t;
    struct T(Arrow) * a;
    struct T(Star)  * s;
    void * byte;         
} TYPE;

struct T(Arrow){
    T(TAG) t;
    TYPE left;
    TYPE right;
};

struct T(Star){
    T(TAG) t;
    TYPE left;
    TYPE right;
};


/*-----------------------------------------------------------------------------
 * Values
 *-----------------------------------------------------------------------------*/

SCM_(Int)
    int value;
_SCM

SCM_(Boolean)
    unsigned int value;
_SCM

SCM_(Closure)
    BINDING * env;
  //  BINDING * mod;
    union Value_u (*lam)(BINDING *,union Value_u); 
_SCM

SCM_(Pair)
   union Value_u * left;
   union Value_u * right;
_SCM

typedef union Value_u {
    struct V(Boolean) b;
    struct V(Int) i;
    struct V(Closure) c;
    struct V(Pair) p;
} VALUE;


/*-----------------------------------------------------------------------------
 * Function definitions
 *-----------------------------------------------------------------------------*/

typedef void* (* PrimOp) (void*,void*);
typedef VALUE (* Lambda)(BINDING *,VALUE);
typedef VALUE (* Gettr)(void);


/*-----------------------------------------------------------------------------
 * Modules
 *-----------------------------------------------------------------------------*/

typedef struct Structure{
    BINDING * mod;
}STRUCTURE;


BINDING * toplevel = NULL;
BINDING * exchange = NULL;
unsigned int LOADED = 0;


/*-----------------------------------------------------------------------------
 * type constructors - TODO
 *-----------------------------------------------------------------------------*/

FUNCTIONALITY TYPE makeTUnit(void);
FUNCTIONALITY TYPE makeTInt(void);
FUNCTIONALITY TYPE makeTBoolean(void);
FUNCTIONALITY TYPE makeTArrow(TYPE, TYPE);
FUNCTIONALITY TYPE makeTStar(TYPE, TYPE);

/*-----------------------------------------------------------------------------
 *  statically defined auxilary methods
 *-----------------------------------------------------------------------------*/

LOCAL DATA convertV(VALUE);
LOCAL DATA convert(void *,TAG t);
LOCAL int bootup(void);


/*-----------------------------------------------------------------------------
 *  statically inlined helper functions
 *-----------------------------------------------------------------------------*/

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    mistakeFromOutside
 *  Description:    terminate when the outside makes a mistake
 * =====================================================================================
 */
LOCAL void mistakeFromOutside(void)
{
    exit(2); 
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    check_state
 *  Description:    check the current load state
 * =====================================================================================
 */
LOCAL void check_state(void)
{
    if(!LOADED){ 
        if(bootup()) LOADED = 1;
    }
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    str_cpy
 *  Description:    inplace string copy
 * =====================================================================================
 */
LOCAL void str_cpy(char * dest,char * input,int size)
{
    for(int i = 0; i < size; i++) dest[i] = input[i];
    dest[size] = '\0';
}


/*-----------------------------------------------------------------------------
 *  statically inlined constructor methods
 *-----------------------------------------------------------------------------*/

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    makeInt
 *  Description:    create a numeric value
 * =====================================================================================
 */
LOCAL VALUE makeInt(int n)
{
    VALUE v;
    v.i.t = INT;
    v.i.value = n;
    return v;
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    makeBoolean
 *  Description:    create a boolean
 * =====================================================================================
 */
LOCAL VALUE makeBoolean(unsigned int b)
{
    VALUE v;
    v.b.t = BOOLEAN;
    v.b.value =  b;
    return v;
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    makeClosure
 *  Description:    create a closure
 * =====================================================================================
 */
LOCAL VALUE makeClosure(BINDING * env, /*BINDING * mod,*/ Lambda lambda)
{
    VALUE v;
    v.c.t = CLOSURE;
    v.c.lam = lambda;
    v.c.env = env;
  //  v.c.mod = mod;
    return v;
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    makePair
 *  Description:    create a Pair
 * =====================================================================================
 */
LOCAL VALUE makePair(VALUE left, VALUE right)
{
    VALUE v;
    v.p.t = PAIR;
    v.p.left = MALLOC(sizeof(VALUE));
    v.p.right = MALLOC(sizeof(VALUE));
    *(v.p.left) = left;
    *(v.p.right) = right;
    return v;
}


#endif
