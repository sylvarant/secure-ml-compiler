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
    T(IGNORE), T(INT), T(BOOLEAN), T(ARROW), T(STAR)
} T(TAG);

struct T(Arrow){
    struct Type_u * left;
    struct Type_u * right;
};

struct T(Star){
    struct Type_u * left;
    struct Type_u * right;
};

typedef struct Type_u{
    T(TAG) t;
    union {
        struct T(Arrow) a;
        struct T(Star) s;
    };
} TYPE;


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
 * Modules
 *-----------------------------------------------------------------------------*/

typedef struct Structure{
    BINDING * mod;
}STRUCTURE;


/*-----------------------------------------------------------------------------
 * Type tracking 
 *-----------------------------------------------------------------------------*/

typedef struct Meta_s
{
    unsigned int call;
    TYPE type;
    union {
        void * value;
        union Value_u (*gettr)();
    };
} META;


/*-----------------------------------------------------------------------------
 * Helper structs/unions
 *-----------------------------------------------------------------------------*/

union safe_cast{
    int value;
    void * bytes;
};

struct value_type{
    VALUE val;
    TYPE ty;
};


/*-----------------------------------------------------------------------------
 * Function Pointers
 *-----------------------------------------------------------------------------*/

typedef void* (* PrimOp) (void*,void*);
typedef VALUE (* Lambda)(BINDING *,VALUE);
typedef VALUE (* Gettr)(void);

/*-----------------------------------------------------------------------------
 * Global variables for the secure component
 *-----------------------------------------------------------------------------*/

BINDING * toplevel = NULL;
BINDING * exchange = NULL;
BINDING * closure_exchange = NULL;
unsigned int LOADED = 0;


/*-----------------------------------------------------------------------------
 *  statically defined auxilary methods
 *-----------------------------------------------------------------------------*/

// data marshalling functions
LOCAL int getAdressClo(void);
LOCAL int getAdress(void);
LOCAL DATA convertV(VALUE,TYPE);
LOCAL struct value_type convertD(DATA);
LOCAL DATA convert(void *,TAG t,TYPE);

// type checking
LOCAL TYPE get_type(VALUE);
LOCAL void unify_types(TYPE,TYPE);

// created by the compiler
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
    DEBUG_PRINT("Mistake From Outside !");
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
LOCAL VALUE makeClosure(BINDING * env, Lambda lambda)
{
    VALUE v;
    v.c.t = CLOSURE;
    v.c.lam = lambda;
    v.c.env = env;
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

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    makeTIGNORE
 *  Description:    create an IGNORE type
 * =====================================================================================
 */
LOCAL TYPE makeTIGNORE(void)
{
    TYPE t;
    t.t = T(IGNORE);
    return t;
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    makeTInt
 *  Description:    create an INT type
 * =====================================================================================
 */
LOCAL TYPE makeTInt(void)
{
    TYPE t;
    t.t = T(INT);
    return t;
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    makeTBoolean
 *  Description:    create a BOOLEAN type
 * =====================================================================================
 */
LOCAL TYPE makeTBoolean(void)
{
    TYPE t;
    t.t = T(BOOLEAN);
    return t;
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    makeTArrow
 *  Description:    create an Arrow type - mallocs !
 * =====================================================================================
 */
LOCAL TYPE makeTArrow(TYPE left, TYPE right)
{
    TYPE t;
    t.t = T(ARROW);
    t.a.left = MALLOC(sizeof(TYPE));
    t.a.right = MALLOC(sizeof(TYPE));
    *(t.a.left) = left; 
    *(t.a.right) = right; 
    return t;
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    makeTStar
 *  Description:    create a Star/Pair type - mallocs !
 * =====================================================================================
 */
LOCAL TYPE makeTStar(TYPE left, TYPE right)
{
    TYPE t;
    t.t = T(STAR);
    t.a.left = MALLOC(sizeof(TYPE));
    t.a.right = MALLOC(sizeof(TYPE));
    *(t.a.left) = left; 
    *(t.a.right) = right; 
    return t;
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    insertBigBinding
 *  Description:    helper function for inserting bindings
 * =====================================================================================
 */
LOCAL void insertBigBinding(BINDING ** binding, void * key, void * val,unsigned int call,TYPE ty)
{
    META * m = MALLOC(sizeof(META));
    m->call = call;
    m->type = ty;
    m->value = val;
    insertBinding(binding,key,m);
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    getValue
 *  Description:    helper function for grabbing values
 * =====================================================================================
 */
LOCAL VALUE getValue(BINDING * binding,void * key) 
{
    return *((VALUE *)getBinding(binding,key,cmp_char));

}

#endif
