/*
 * =====================================================================================
 *
 *       Filename:  miniml.h
 *
 *    Description:  The definitions we need to compile into
 *
 *         Author:  MYSTERY MAN, 
 *        Company:  SOMEWHERE
 *
 * =====================================================================================
 */


#ifndef MINIMLRAGE_INCLUDED
#define MINIMLRAGE_INCLUDED

#include "binding.h" // adds global.h !

/*-----------------------------------------------------------------------------
 *  Preprocessing
 *-----------------------------------------------------------------------------*/
#define SCM_(TYPE) struct V(TYPE) { \
                         unsigned int t;

#define _SCM }; 

#define SWITCH(dec,value,call) if(dec == 0){ \
    dec = 1; \
    value = call;\
    }

/*-----------------------------------------------------------------------------
 * Base Types 
 *-----------------------------------------------------------------------------*/

enum{ MFALSE = 0, MTRUE = 1 };
enum{ CHUNK = TERMCOUNT };


/*-----------------------------------------------------------------------------
 * Types
 *-----------------------------------------------------------------------------*/

typedef enum T(Tag_e){
    TIGNORE, TINT, TBOOLEAN, TARROW, TSTAR, TMODULE, TVALUE, 
    TDECLARATION, TFUNCTOR, TABSTRACT, TSIGNATURE, TUNIT, TREF
} T(TAG);

struct T(Arrow){
    struct Type_u * left;
    struct Type_u * right;
};

struct T(Star){
    struct Type_u * left;
    struct Type_u * right;
};

struct T(Ref){
    struct Type_u * type;
};

struct T(Abstract){
    int identifier; 
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
        struct T(Ref) r;
        struct T(Module) m;
        struct T(Value) v;
        struct T(Declaration) d;
        struct T(Functor) f;
        struct T(Signature) ss;
        struct T(Abstract) aa;
    };
} TYPE;

// global constant types
extern const struct Type_u T(Ignore); 
extern const struct Type_u T(Int); 
extern const struct Type_u T(Unit);
extern const struct Type_u T(Boolean); 


/*-----------------------------------------------------------------------------
 * Values
 *-----------------------------------------------------------------------------*/

SCM_(Empty)
_SCM

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

SCM_(Chunk)
    union Value_u * val;
    //union Value_u (*fix)(union Value_u);
_SCM

SCM_(Location)
    union Value_u * content;
_SCM

SCM_(ForeignLoc)
    DATA * cell;
    TYPE ty;
_SCM

SCM_(Pair)
   union Value_u * left;
   union Value_u * right;
_SCM

typedef union Value_u {
    struct V(Location) l;
    struct V(ForeignLoc) ll;
    struct V(Empty) e;
    struct V(Boolean) b;
    struct V(Int) i;
    struct V(Closure) c;
    struct V(Chunk) cc;
    struct V(Pair) p;
} VALUE;

// global constant values
extern const VALUE V(Unit); 
extern const VALUE V(True); 
extern const VALUE V(False);

/*-----------------------------------------------------------------------------
 * Modules
 *-----------------------------------------------------------------------------*/
typedef DATA (*foreignval) (void);
typedef MODDATA (*foreignmod) (void);

struct foreign_s{
    TYPE req;
    union {
        foreignval fe;
        foreignmod me;
    };
};

typedef enum acc_e { BVAL, BMOD, BDVAL, BDMOD, BUVAL } ACC;
typedef enum isentry_e { YES, NO} ISENTRY;

typedef struct module_s{
    MODTAG type;
    BINDING * strls;
    INTLIST * keys;
    union content {
        struct structure {
            int count;
            char ** names;
            ACC * accs; 
            ISENTRY * ie; 
            union field_t {
                struct module_s * module;
                struct module_s (*mgettr)(BINDING *);
                VALUE (*gettr) (BINDING *);
                VALUE value;
                struct foreign_s * foreign;
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

extern BINDING * toplevel; 
extern BINDING * exchange; 
extern BINDING * closure_exchange; 
extern BINDING * location_exchange;
extern BINDING * abstract_exchange;
unsigned int LOADED;


/*-----------------------------------------------------------------------------
 *  statically defined auxilary methods
 *-----------------------------------------------------------------------------*/

// data marshalling functions
LOCAL int getAdress(void);
LOCAL int getAdressClo(void);
LOCAL int getAdressAbs(void);
LOCAL int getAdressLoc(void);
FUNCTIONALITY int getObjId(void);
FUNCTIONALITY VALUE doFix(VALUE*);
FUNCTIONALITY DATA convertV(VALUE,TYPE);
LOCAL DTYPE convertT(TYPE);
LOCAL MODDATA convertM(MODULE,TYPE);
LOCAL MODULE updateKeys(MODULE,INTLIST*);
LOCAL struct module_type convertMD(MODDATA,TYPE);
FUNCTIONALITY struct value_type convertD(DATA,TYPE);
LOCAL void checkModule(MODULE,ENTRY *);
LOCAL VALUE get_value(MODULE,char *);
LOCAL MODULE get_module(MODULE m,char * str); 
LOCAL MODULE path_module(BINDING *,char*);
LOCAL VALUE path_value(BINDING *,char*);
LOCAL VALUE get_value_path(MODULE,char*);
LOCAL MODULE get_module_path(MODULE,char*);
LOCAL MODULE updateEntry(MODULE,BINDING*,int,char **,ENTRY * ls);
LOCAL VALUE foreign_lambda(BINDING *,BINDING *,VALUE);
LOCAL MODULE foreign_module(foreignmod f,TYPE);
LOCAL VALUE foreign_value(foreignval f,TYPE);
FUNCTIONALITY MODULE load_struct(MODULE);

// type checking
LOCAL int type_check(TYPE,TYPE); 
FUNCTIONALITY void unify_types(TYPE,TYPE);



/*-----------------------------------------------------------------------------
 *  statically inlined helper functions
 *-----------------------------------------------------------------------------*/

/* 
 * ===  FUNCTION ======================================================================
 *         Name:  str_cpy
 *  Description:  inplace string copy
 * =====================================================================================
 */
LOCAL inline void str_cpy(char * dest,char * input,int size)
{
    for(int i = 0; i < size; i++) dest[i] = input[i];
    dest[size] = '\0';
}


/* 
 * ===  FUNCTION ======================================================================
 *         Name:  is_loaded
 *  Description:  check that the module has been loaded - for use in entry point
 * =====================================================================================
 */
LOCAL inline void is_loaded()
{
    if(!LOADED) mistakeFromOutside();
}


/*-----------------------------------------------------------------------------
 *  statically inlined term implementations
 *-----------------------------------------------------------------------------*/

/* 
 * ===  FUNCTION ======================================================================
 *         Name:  makeInt
 *  Description:  create a numeric value
 * =====================================================================================
 */
LOCAL inline VALUE makeInt(int n)
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
LOCAL inline VALUE makeBoolean(unsigned int b)
{
    if(b) return V(True);
    else return V(False);
}

/* 
 * ===  FUNCTION ======================================================================
 *         Name:  makeLocation
 *  Description:  create a location pointing to the value
 * =====================================================================================
 */
LOCAL inline VALUE makeLocation(VALUE val)
{
    VALUE v;
    v.l.t = LOCATION;
    v.l.content = MALLOC(sizeof(VALUE));
    *(v.l.content) = val;
    return v;
}

/* 
 * ===  FUNCTION ======================================================================
 *         Name:  makeForeignLoc
 *  Description:  create a location pointing to an outside memory cell
 * =====================================================================================
 */
LOCAL inline VALUE makeForeignLoc(DATA * cell, TYPE ty)
{
    VALUE v;
    v.ll.t = BYTES;
    v.ll.cell = cell;
    v.ll.ty = ty;
    return v;
}

/* 
 * ===  FUNCTION ======================================================================
 *         Name:  makeChunk
 *  Description:  chunk a fix evaluation
 * =====================================================================================
 */
LOCAL inline VALUE makeChunk(VALUE * ptr)
{
    VALUE v;
    v.cc.t = CHUNK;
    v.cc.val = ptr; 
    return v;
}

/* 
 * ===  FUNCTION ======================================================================
 *         Name:  makeAssign
 *  Description:  Assign to a location
 * =====================================================================================
 */
LOCAL inline VALUE makeAssign(VALUE left,VALUE right)
{
    if(left.b.t == BYTES){
        DATA d = convertV(right,left.ll.ty);
        *(left.ll.cell) = d;
    } else{
        *(left.l.content) = right;
    }
    return V(Unit);
}

/* 
 * ===  FUNCTION ======================================================================
 *         Name:  makeDeref
 *  Description:  Dereference a location
 * =====================================================================================
 */
LOCAL inline VALUE makeDeref(VALUE loc)
{
    if(loc.b.t == BYTES){
       struct value_type vt = convertD(*(loc.ll.cell),loc.ll.ty);
       unify_types(loc.ll.ty,vt.ty);
       return vt.val;
    }
    return *(loc.l.content);
}

/* 
 * ===  FUNCTION ======================================================================
 *         Name:  makeClosure
 *  Description:  create a closure
 * =====================================================================================
 */
LOCAL inline VALUE makeClosure(BINDING * mod,BINDING * env, Lambda lambda)
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
 *         Name:  makeFix
 *  Description:  create a fix
 * =====================================================================================
 */
LOCAL inline VALUE makeFix(VALUE val)
{
   VALUE * ptr = MALLOC(sizeof(VALUE)); 
   *ptr = val;
   return doFix(ptr);
}

/* 
 * ===  FUNCTION ======================================================================
 *         Name:  makePair
 *  Description:  create a Pair
 * =====================================================================================
 */
LOCAL inline VALUE makePair(VALUE left, VALUE right)
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
 *         Name:  doExit
 *  Description:  terminate with the argument
 * =====================================================================================
 */
LOCAL inline VALUE doExit(VALUE arg)
{
    exit(1); 
    return arg;
}


/*-----------------------------------------------------------------------------
 *  statically inlined type implementations
 *-----------------------------------------------------------------------------*/

/* 
 * ===  FUNCTION ======================================================================
 *         Name:  makeTArrow
 *  Description:  create an Arrow type - mallocs !
 * =====================================================================================
 */
LOCAL inline TYPE makeTArrow(TYPE left, TYPE right)
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
LOCAL inline TYPE makeTStar(TYPE left, TYPE right)
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
LOCAL inline TYPE makeTAbstract(char * name)
{
   TYPE t;
   t.t = T(ABSTRACT);
   t.aa.identifier = -1;
   t.aa.name = name;
   return t;
}

/* 
 * ===  FUNCTION ======================================================================
 *         Name:  makeTRef
 *  Description:  create a reference type
 * =====================================================================================
 */
LOCAL inline TYPE makeTRef(TYPE type)
{
    TYPE t;
    t.t = T(REF);
    t.r.type  = MALLOC(sizeof(TYPE));
    *(t.r.type) = type;
    return t;
}

/* 
 * ===  FUNCTION ======================================================================
 *         Name:  makeIdTAbstract
 *  Description:  create an abstract type with a dynamic identifier
 * =====================================================================================
 */
LOCAL inline TYPE makeIdTAbstract(char * name,int id)
{
   TYPE t;
   t.t = T(ABSTRACT);
   t.aa.identifier = id;
   t.aa.name = name;
   return t;
}

/* 
 * ===  FUNCTION ======================================================================
 *         Name:  makeTValue
 *  Description:  create a value type binding for a signature  
 * =====================================================================================
 */
LOCAL inline TYPE makeTValue(char * name, TYPE type)
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
LOCAL inline TYPE makeTDeclaration(char * name, TYPE type)
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
LOCAL inline TYPE makeTModule(char * name, TYPE contents)
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
LOCAL inline TYPE makeTFunctor(char * name, TYPE left, TYPE right)
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
LOCAL inline TYPE makeTSignature(TYPE sign)
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
LOCAL inline TYPE chainTSignature(TYPE chain,TYPE sign)
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
LOCAL inline void insertBigBinding(BINDING ** binding, void * key, void * val,TYPE ty)
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
LOCAL inline VALUE getValue(BINDING * binding,void * key) 
{
    VALUE v = *((VALUE *)getBinding(binding,key,cmp_char));
    if (v.b.t == CHUNK){
        return doFix(v.cc.val);
    }
    return v;
}


/* 
 * ===  FUNCTION ======================================================================
 *         Name:  getModule
 *  Description:  helper function for grabbing values
 * =====================================================================================
 */
LOCAL inline MODULE getModule(BINDING * binding,void * key)
{
    return *((MODULE *)getBinding(binding,key,cmp_char));
}

/* 
 * ===  FUNCTION ======================================================================
 *         Name:  makeContentF
 *  Description:  return a Module Content for a functor
 * =====================================================================================
 */
LOCAL inline CONTENT makeContentF(Functor f)
{
    CONTENT ret;
    ret.f.Functor = f;
    ret.f.count = -1;
    ret.f.names = NULL;
    ret.f.entries = NULL;
    return ret; 
}

/* 
 * ===  FUNCTION ======================================================================
 *         Name:  makeContentS
 *  Description:  return a Module Content for a structure
 * =====================================================================================
 */
LOCAL inline CONTENT makeContentS(int c,char ** n,ACC * a,FIELD * fs,ISENTRY * ies,ENTRY * es)
{
    CONTENT ret;
    ret.s.count = c;
    ret.s.names = MALLOC(c * sizeof(char*));
    for(int i = 0; i < c; i++) ret.s.names[i] = n[i];
    ret.s.accs = MALLOC(c * sizeof(ACC));
    for(int i = 0; i < c; i++) ret.s.accs[i] = a[i]; 
    ret.s.fields = MALLOC(c * sizeof(FIELD));
    for(int i = 0; i < c; i++) ret.s.fields[i] = fs[i]; 
    ret.s.ie = MALLOC(c*sizeof(ISENTRY));
    for(int i = 0; i < c; i++) ret.s.ie[i] = ies[i];
    ret.s.entries = es;
    return ret;     
}

/* 
 * ===  FUNCTION ======================================================================
 *         Name:  makeModule
 *  Description:  return a Module
 * =====================================================================================
 */
LOCAL inline MODULE makeModule(MODTAG t, BINDING * ls,CONTENT c)
{
    MODULE ret;
    ret.type = t;
    ret.keys = NULL;
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
LOCAL inline void checkModule(MODULE m,ENTRY * ptr)
{
    if(m.type != STRUCTURE) mistakeFromOutside();
    if(m.c.s.entries != ptr) mistakeFromOutside();
}

/* 
 * ===  FUNCTION ======================================================================
 *         Name: updateBinding
 *  Description: update the objects binding
 * =====================================================================================
 */
LOCAL inline MODULE updateBinding(MODULE m,BINDING *ls)
{
    MODULE ret = m;
    ret.strls = ls;
    return ret;
}

/* 
 * ===  FUNCTION ======================================================================
 *         Name: updateKeys
 *  Description: update the keys of a dyn module
 * =====================================================================================
 */
LOCAL inline MODULE updateKeys(MODULE m,INTLIST *keys)
{
    MODULE ret = m;
    ret.keys = pushIntlist(keys,getObjId());
    return ret;
}


#endif

