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
                struct module_s (*mgettr)(BINDING *);
                VALUE (*gettr)(BINDING *);
            } * fields;
            union entry_t {
                void * byte;
                DATA (*entry_v)(void);
                DATA (*entry_v2)(MODDATA);
                MODDATA (*entry_m) (void);
                MODDATA (*entry_m2) (MODDATA);
            } * entries;
        }s;
        struct functor {
            struct module_s (*Functor) (BINDING*,struct module_s);      
            int count;
            char ** names;
            union entry_t * entries;
            int stamp;
        }f;
    }c;
}MODULE;

typedef union field_t FIELD;
typedef union entry_t ENTRY;
typedef union content CONTENT;


/*-----------------------------------------------------------------------------
 * Type tracking of structure - DEPRECATED
 *-----------------------------------------------------------------------------*/

typedef struct Meta_s
{
    unsigned int call;
    TYPE type;
    void * value;
} META;


/*-----------------------------------------------------------------------------
 * Helper structs/unions
 *-----------------------------------------------------------------------------*/

union safe_cast{
    int value;
    void * byte;
};

struct value_type{
    VALUE val;
    TYPE ty;
};

struct module_type{
    MODULE m;
    TYPE ty;
};

/*-----------------------------------------------------------------------------
 * Function Pointers
 *-----------------------------------------------------------------------------*/

typedef void* (*PrimOp) (void*,void*);
typedef VALUE (*Lambda)(BINDING *,BINDING *,VALUE);
typedef VALUE (*Gettr)(MODULE);
typedef MODULE (*Functor)(BINDING*,MODULE);
typedef DATA (*callback)(DATA);


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
LOCAL DTYPE convertT(TYPE);
LOCAL MODDATA convertM(MODULE,TYPE);
LOCAL struct module_type * convertMD(MODDATA);
LOCAL struct value_type convertD(DATA,TYPE);
LOCAL void checkModule(MODULE,int);
LOCAL VALUE get_value(MODULE,char *);
LOCAL MODULE get_module(MODULE m,char * str); 
LOCAL MODULE path_module(BINDING *,char*);
LOCAL VALUE path_value(BINDING *,char*);
LOCAL VALUE get_value_path(MODULE,char*);
LOCAL MODULE get_module_path(MODULE,char*);
LOCAL MODULE updateEntry(MODULE,BINDING*,int,int,char **,ENTRY * ls);
LOCAL VALUE foreign_lambda(BINDING *,BINDING *,VALUE);

// type checking
LOCAL int type_check(TYPE,TYPE); 
LOCAL void unify_types(TYPE,TYPE);


/*-----------------------------------------------------------------------------
 *  statically inlined helper functions
 *-----------------------------------------------------------------------------*/

/* 
 * ===  FUNCTION ======================================================================
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
 * ===  FUNCTION ======================================================================
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
 * ===  FUNCTION ======================================================================
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
 * ===  FUNCTION ======================================================================
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
 * ===  FUNCTION ======================================================================
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
 * ===  FUNCTION ======================================================================
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
 * ===  FUNCTION ======================================================================
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
 * ===  FUNCTION ======================================================================
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
 * ===  FUNCTION ======================================================================
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
 * ===  FUNCTION ======================================================================
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
 * ===  FUNCTION ======================================================================
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
 * ===  FUNCTION ======================================================================
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
 * ===  FUNCTION ======================================================================
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
 * ===  FUNCTION ======================================================================
 *         Name:  chainTSignature
 *  Description:  add a type to an existing chain
 * =====================================================================================
 */
LOCAL TYPE chainTSignature(TYPE chain,TYPE sign)
{
    if (chain.t != T(SIGNATURE)) mistakeFromOutside();
    TYPE capsule = makeTSignature(sign);
    capsule.ss.next = MALLOC(sizeof(struct T(Signature)));
    *(capsule.ss.next) = chain.ss; 
    return capsule;
}

/* 
 * ===  FUNCTION ======================================================================
 *         Name:  insertBigBinding
 *  Description:  helper function for inserting bindings
 * =====================================================================================
 */
LOCAL void insertBigBinding(BINDING ** binding, void * key, void * val,TYPE ty)
{
    META * m = MALLOC(sizeof(META));
    m->type = ty;
    m->value = val;
    insertBinding(binding,key,m);
}

/* 
 * ===  FUNCTION ======================================================================
 *         Name:  getValue
 *  Description:  helper function for grabbing values
 * =====================================================================================
 */
LOCAL VALUE getValue(BINDING * binding,void * key) 
{
    return *((VALUE *)getBinding(binding,key,cmp_char));
}


/* 
 * ===  FUNCTION ======================================================================
 *         Name:  getModule
 *  Description:  helper function for grabbing values
 * =====================================================================================
 */
LOCAL MODULE getModule(BINDING * binding,void * key)
{
    return *((MODULE *)getBinding(binding,key,cmp_char));
}

/* 
 * ===  FUNCTION ======================================================================
 *         Name:  makeContentF
 *  Description:  return a Module Content for a functor
 * =====================================================================================
 */
LOCAL CONTENT makeContentF(Functor f,int stamp)
{
    CONTENT ret;
    ret.f.Functor = f;
    ret.f.stamp = stamp;
    return ret; 
}

/* 
 * ===  FUNCTION ======================================================================
 *         Name:  makeContentS
 *  Description:  return a Module Content for a structure
 * =====================================================================================
 */
LOCAL CONTENT makeContentS(int c,char ** n,ACC * a,FIELD * fs,ENTRY * es)
{
    CONTENT ret;
    ret.s.count = c;
    ret.s.names = MALLOC(c * sizeof(char*));
    for(int i = 0; i < c; i++) ret.s.names[i] = n[i];
    ret.s.accs = MALLOC(c * sizeof(ACC));
    for(int i = 0; i < c; i++) ret.s.accs[i] = a[i]; 
    ret.s.fields = MALLOC(c * sizeof(FIELD));
    for(int i = 0; i < c; i++) ret.s.fields[i] = fs[i]; 
    ret.s.entries = MALLOC(c * sizeof(ENTRY));
    for(int i = 0; i < c; i++) ret.s.entries[i] = es[i];
    return ret;     
}

/* 
 * ===  FUNCTION ======================================================================
 *         Name:  makeModule
 *  Description:  return a Module
 * =====================================================================================
 */
LOCAL MODULE makeModule(MODTAG t, int s, BINDING * ls,CONTENT c)
{
    MODULE ret;
    ret.type = t;
    ret.stamp = s;
    ret.strls = ls;
    ret.c = c;
    return ret;
}

/* 
 * ===  FUNCTION ======================================================================
 *         Name:  checkModule
 *  Description:  check that the stamp of a module is in accord with the functor
 * =====================================================================================
 */
LOCAL void checkModule(MODULE m, int c)
{
    if(m.stamp != c) mistakeFromOutside();
}

/* 
 * ===  FUNCTION ======================================================================
 *         Name: updateFStamp
 *  Description: update the Functor Stamp if object is a functor
 * =====================================================================================
 */
LOCAL MODULE updateFStamp(MODULE m, int nfctr,BINDING *ls)
{
    MODULE ret = m;
    if(ret.type == FUNCTOR) ret.c.f.stamp = nfctr;
    ret.strls = ls;
    return ret;
}


#endif
