/*
 * =====================================================================================
 *
 *       Filename:  entry.h
 *
 *    Description:  global header file, defines the general entry points
 *                  for the attacker
 *
 *         Author:  Adriaan, 
 *        Company:  Uppsala IT
 *
 * =====================================================================================
 */

#ifndef ENTRY_INCLUDED
#define ENTRY_INCLUDED


/*-----------------------------------------------------------------------------
 * SANCUS SPM structure
 *-----------------------------------------------------------------------------*/

#ifdef SANCUS_SPM

    #include <sancus/sm_support.h>
    #define __MSP430_INTRINSICS_H_
    #include <msp430.h>

    #define SPM_Name "secure_module" // TODO perl insert ?

    extern struct SancusModule secure_vm;

    // Entry point is sacred
    #define ENTRYPOINT SM_ENTRY(SPM_NAME) extern

    #include <stdlib.h>

#else

#ifdef FIDES_PMA

  #include <PCBAC/spm_annotations.h>
  #define PCBAC_SPM_NAME "secure_module"
  #define ENTRYPOINT ENTRY_POINT

  #include <SPM_Core/stdlib.h>
  #include <fides_libc/types.h>
  #include <SecureKernel/spm_id.h>

#else
    
    #define ENTRYPOINT extern

    #include <stdlib.h>

#endif
#endif

/*-----------------------------------------------------------------------------
 *  Memory Sharing
 *-----------------------------------------------------------------------------*/

extern void * malloc(size_t);
#define OUTERM malloc


/*-----------------------------------------------------------------------------
 *  Data Sharing
 *-----------------------------------------------------------------------------*/

typedef enum Termtag_e {
    INT, BOOLEAN, CLOSURE, PAIR, ABSTRACT, CALLBACK, UNIT, LOCATION, BYTES, TERMCOUNT
} TERMTAG;

typedef struct Data_s{
    TERMTAG t;   
    union {
        void * byte;
        int value; 
        int identifier;
        struct Data_s (*call)(struct Data_s);
        struct {
            struct Data_s * left;
            struct Data_s * right;
        };
    };
} DATA;


/*-----------------------------------------------------------------------------
 *  Type Sharing
 *-----------------------------------------------------------------------------*/

typedef enum Type_e {
    TYINT, TYBOOLEAN, TYARROW, TYSTAR,TYUNIT,TYREF,
    TYDECLARATION, TYFUNCTOR, TYABSTRACT, TYSIGNATURE, TYMODULE, TYVALUE, 
} TYPE_INFO;

typedef struct Type_s {
    TYPE_INFO t;
    union {
        struct Type_s * reftype;
        struct {
            char * name;
            struct Type_s * type;
        };
        struct {
           struct Type_s * left; 
           struct Type_s * right;
        };
        struct {
            char * fname;
            struct Type_s * fleft; 
            struct Type_s * fright;
        };
        struct {
            int count;
            struct Type_s * list;
        };
    };
} DTYPE;


/*-----------------------------------------------------------------------------
 *  Module Sharing
 *-----------------------------------------------------------------------------*/

typedef enum Modtag_e{
    STRUCTURE, FUNCTOR, 
} MODTAG;

typedef enum Calltag_e { VAL , MOD } CALLTAG;

typedef struct Moduledata_s{
    MODTAG t;
//    DTYPE type;
    int identifier;
    union{
        struct {
            int count;
            char ** names; 
            CALLTAG * accs;
            void ** fcalls;
        };
        struct Moduledata_s (*fctr) (struct Moduledata_s);
    };
} MODDATA;

// for the attacker's convenience
typedef DATA (*func_entry) (MODDATA);
typedef MODDATA (*mod_entry) (MODDATA);


/*-----------------------------------------------------------------------------
 *  General Entrypoints 
 *-----------------------------------------------------------------------------*/

ENTRYPOINT DATA applyClosure(int,DATA);
ENTRYPOINT MODDATA applyFunctor(int,MODDATA);
ENTRYPOINT DATA readLocation(int);
ENTRYPOINT void setLocation(int,DATA);
ENTRYPOINT void load(void);

#endif

