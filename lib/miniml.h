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
                          TERMTAG t;

#define _SCM }; 


/*-----------------------------------------------------------------------------
 * Base Types 
 *-----------------------------------------------------------------------------*/

enum{ MFALSE = 0, MTRUE = 1 };


/*-----------------------------------------------------------------------------
 * Types
 *-----------------------------------------------------------------------------*/

typedef enum T(Tag_e){
    T(IGNORE), T(INT), T(BOOLEAN), T(ARROW), T(STAR), T(MODULE), T(VALUE), 
    T(DECLARATION), T(FUNCTOR), T(ABSTRACT), T(SIGNATURE)
} T(TAG);

struct T(Arrow){
    struct Type_u * left;
    struct Type_u * right;
};

struct T(Star){
    struct Type_u * left;
    struct Type_u * right;
};

struct T(Abstract){
    char * name;
};

struct T(Value){ 
    char * name;
    struct Type_u * type;
};

struct T(Declaration){
    char * name;
    struct Type_u * type;
};

struct T(Signature) {
    struct Type_u * type;
    struct T(Signature) * next;
};

struct T(Functor){
    char * name;
    struct Type_u * left;
    struct Type_u * right;
};

struct T(Module){
    char * name;
    struct Type_u * type;
};

typedef struct Type_u{
    T(TAG) t;
    union {
        struct T(Arrow) a;
        struct T(Star) s;
        struct T(Module) m;
        struct T(Value) v;
        struct T(Declaration) d;
        struct T(Functor) f;
        struct T(Signature) ss;
        struct T(Abstract) aa;
    };
} TYPE;

// global constant types
const struct Type_u T(Ignore) = {.t = T(IGNORE), .a = 0};
const struct Type_u T(Int) = {.t = T(INT), .a = 0};
const struct Type_u T(Boolean) = {.t = T(BOOLEAN), .a = 0};


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
    BINDING * mod;
    BINDING * env;
    union Value_u (*lam)(BINDING*,BINDING *,union Value_u); 
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

typedef enum acc_e { BVAL , BMOD } ACC;

typedef struct module_s{
    int mask; 
    MODTAG type;
    int stamp; 
    BINDING * strls;
    union content {
        struct structure {
            int count;
            char ** names;
            ACC * accs; 
            union field_t {
                struct module_s * module;
                VALUE (*gettr)(BINDING *);
            } * fields;
        }s;
        struct functor {
            void (*Functor) (void);      
        }f;
    }c;
}MODULE;

typedef union field_t FIELD;


/*-----------------------------------------------------------------------------
 * Type tracking of structure - DEPRECATED
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
typedef VALUE (* Lambda)(BINDING *,BINDING *,VALUE);
typedef VALUE (* Gettr)(MODULE);
typedef MODULE (* Functor)(BINDING*,MODULE);


/*-----------------------------------------------------------------------------
 * Global variables for the secure component
 *-----------------------------------------------------------------------------*/

BINDING * toplevel = NULL;
BINDING * exchange = NULL;
BINDING * closure_exchange = NULL;
BINDING * abstract_exchange = NULL;
unsigned int LOADED = 0;


/*-----------------------------------------------------------------------------
 *  statically defined auxilary methods
 *-----------------------------------------------------------------------------*/

// data marshalling functions
LOCAL int getAdressClo(void);
LOCAL int getAdressAbs(void);
LOCAL int getAdress(void);
LOCAL DATA convertV(VALUE,TYPE);
LOCAL struct value_type convertD(DATA);
LOCAL DATA convert(void *,TERMTAG t,TYPE);
LOCAL DTYPE convertT(TYPE);
LOCAL VALUE get_value(MODULE top,MODULE m,char * str);
LOCAL MODULE get_module(MODULE m,char * str); 
LOCAL MODULE path_module(MODULE,char*,int);
LOCAL VALUE path_value(BINDING *,char*,int);
LOCAL MODULE emptyModule(void);
LOCAL DTYPE emptyType(void);

// type checking
LOCAL TYPE get_type(VALUE);
LOCAL void unify_types(TYPE,TYPE);

// created by the compiler


/*-----------------------------------------------------------------------------
 *  statically inlined helper functions
 *-----------------------------------------------------------------------------*/

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:  mistakeFromOutside
 *  Description:  terminate when the outside makes a mistake
 * =====================================================================================
 */
LOCAL void mistakeFromOutside(void)
{
    DEBUG_PRINT("Mistake From Outside !");
    exit(2); 
}


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:  str_cpy
 *  Description:  inplace string copy
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
 *         Name:  makeInt
 *  Description:  create a numeric value
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
 *         Name:  makeBoolean
 *  Description:  create a boolean
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
 *         Name:  makeClosure
 *  Description:  create a closure
 * =====================================================================================
 */
LOCAL VALUE makeClosure(BINDING * mod,BINDING * env, Lambda lambda)
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
 *         Name:  makePair
 *  Description:  create a Pair
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
 *         Name:  makeTArrow
 *  Description:  create an Arrow type - mallocs !
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
 *         Name:  makeTStar
 *  Description:  create a Star/Pair type - mallocs !
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
 *         Name:  makeTAbstract
 *  Description:  create an abstract type
 * =====================================================================================
 */
LOCAL TYPE makeTAbstract(char * name)
{
   TYPE t;
   t.t = T(ABSTRACT);
   t.aa.name = name;
   return t;
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:  makeTValue
 *  Description:  create a value type binding for a signature  
 * =====================================================================================
 */
LOCAL TYPE makeTValue(char * name, TYPE type)
{
    TYPE t; 
    t.t = T(VALUE);
    t.v.name = name;
    t.v.type = MALLOC(sizeof(TYPE));
    *(t.v.type) = type;
    return t;
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:  makeTDeclaration
 *  Description:  create a type declaration for a signature 
 * =====================================================================================
 */
LOCAL TYPE makeTDeclaration(char * name, TYPE type)
{
    TYPE t;
    t.t = T(DECLARATION);
    t.d.name = name;
    t.d.type = MALLOC(sizeof(TYPE));
    *(t.d.type) = type;
    return t;
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:  makeTModule
 *  Description:  create a type declaration for a signature 
 * =====================================================================================
 */
LOCAL TYPE makeTModule(char * name, TYPE contents)
{
    TYPE t;
    t.t = T(MODULE);
    t.m.name = name;
    t.m.type = MALLOC(sizeof(TYPE));
    *(t.m.type) = contents;
    return t;
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:  makeTFunctor
 *  Description:  create a functor type
 * =====================================================================================
 */
LOCAL TYPE makeTFunctor(char * name, TYPE left, TYPE right)
{
    TYPE t;
    t.t = T(FUNCTOR);
    t.f.name = name;
    t.f.left = MALLOC(sizeof(TYPE));
    t.f.right = MALLOC(sizeof(TYPE));
    *(t.f.left) = left;
    *(t.f.right) = right;
    return t;
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:  makeTSignature
 *  Description:  create a standalone signature object
 * =====================================================================================
 */
LOCAL TYPE makeTSignature(TYPE sign)
{
    TYPE t;
    t.t = T(SIGNATURE);
    t.ss.type = MALLOC(sizeof(TYPE));
    *(t.ss.type) = sign;
    t.ss.next = NULL;
    return t;
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:  chainTSignature
 *  Description:  add a type to an existing chain
 * =====================================================================================
 */
LOCAL TYPE chainTSignature(TYPE chain,TYPE sign)
{
    if (chain.t != T(SIGNATURE)) mistakeFromOutside();
    TYPE result = chain;
    TYPE capsule = makeTSignature(sign);
    struct T(Signature) arg = capsule.ss;
    arg.next = result.ss.next;
     
    result.ss.next = MALLOC(sizeof(struct T(Signature)));
    *(result.ss.next) = arg;
    return chain;
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:  insertBigBinding
 *  Description:  helper function for inserting bindings
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
 *         Name:  getValue
 *  Description:  helper function for grabbing values
 * =====================================================================================
 */
LOCAL VALUE getValue(BINDING * binding,void * key) 
{
    return *((VALUE *)getBinding(binding,key,cmp_char));
}


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:  emptyModule
 *  Description:  return an empty module (for testing purposes)
 * =====================================================================================
 */
LOCAL MODULE emptyModule(void)
{
   MODULE m;  
   return m;
}


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:  emptyType
 *  Description:  return an empty type (for testing purposes)
 * =====================================================================================
 */
LOCAL DTYPE emptyType(void)
{
   DTYPE d;  
   return d;
}

#endif
