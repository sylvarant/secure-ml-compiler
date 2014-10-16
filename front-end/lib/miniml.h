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

#include <stdio.h>

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
 *  Tags used by MiniML terms and types
 *-----------------------------------------------------------------------------*/

/*
 * Tags for terms
 */
typedef enum Tag_e {
    INT, BOOLEAN, CLOSURE, PAIR 
} TAG;

/*
 * Tags for types
 * TODO :: add signatures
 */
Typedef enum T(Tag_e){
    T(UNIT), T(INT), T(BOOLEAN), T(ARROW), T(STAR)
} T(TAG);


/*-----------------------------------------------------------------------------
 * Union of Types
 *-----------------------------------------------------------------------------*/

typedef union Type_u{
    T(TAG) t;
    struct T(Arrow) * a;
    struct T(Star)  * s;
    void * byte;         
} TYPE;


/*-----------------------------------------------------------------------------
 * Union of Values
 *-----------------------------------------------------------------------------*/

typedef union Value_u {
    void * byte;
    struct V(Boolean) b;
    struct V(Int) i;
    struct V(Closure) c;
    struct V(Pair) p;
} VALUE;


/*-----------------------------------------------------------------------------
 * Function definitions
 *-----------------------------------------------------------------------------*/

typedef void* (* PrimOp) (void*,void*);
typedef VALUE (* Lambda)();


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
    BINDING * mod;
    Lambda lam;
_SCM

SCM_(Pair)
   VALUE left;
   VALUE right;
_SCM


/*-----------------------------------------------------------------------------
 * Type Structure Definitions
 *-----------------------------------------------------------------------------*/

struct T(Arrow){
    T(TAG) t;
    TYPE left;
    TYPE right;
};

struct T(Star){
    T(Tag) t;
    TYPE left;
    TYPE right;
};


/*-----------------------------------------------------------------------------
 * type constructors - TODO
 *-----------------------------------------------------------------------------*/

FUNCTIONALITY TYPE makeT(Unit)(void);
FUNCTIONALITY TYPE makeT(Int)(void);
FUNCTIONALITY TYPE makeT(Boolean)(void);
FUNCTIONALITY TYPE makeT(Arrow)(TYPE, TYPE);
FUNCTIONALITY TYPE makeT(Star)(TYPE, TYPE);


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
LOCAL VALUE makeClosure(BINDING * env, BINDING * mod, Lambda lambda)
{
    VALUE v;
    v.c.t = CLOSURE;
    v.c.lam = lambda;
    v.c.env = env;
    v.c.mod = mod;
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
    v.p.left = left;
    v.p.right = right;
    return v;
}



#endif
