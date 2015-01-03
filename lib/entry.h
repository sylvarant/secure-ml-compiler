/*
 * =====================================================================================
 *
 *       Filename:  entry.h
 *
 *    Description:  global header file, defines the general entry points
 *                  for the attacker
 *
 *         Author:  Adriaan Larmuseau, ajhl
 *        Company:  Uppsala
 *
 * =====================================================================================
 */


#ifndef ENTRY_INCLUDED
#define ENTRY_INCLUDED

#include <stdlib.h>

/*-----------------------------------------------------------------------------
 * SANCUS SPM structure
 *-----------------------------------------------------------------------------*/

#ifdef SANCUS_SPM

    #include <sancus/sm_support.h>
    #define __MSP430_INTRINSICS_H_
    #include <msp430.h>

    #define SPM_NAME "secure_vm" // TODO perl insert ?

    extern struct SancusModule secure_vm;

    // Entry point is sacred
    #define ENTRYPOINT SM_ENTRY(SPM_NAME) extern

#else
    
    #define ENTRYPOINT extern

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
    INT, BOOLEAN, CLOSURE, PAIR, ABSTRACT 
} TERMTAG;

typedef struct Data_s{
    TERMTAG t;   
    union {
        void * bytes;
        int value; 
        int identifier;
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
    TYINT, TYBOOLEAN, TYARROW, TYSTAR, 
    TYDECLARATION, TYFUNCTOR, TYABSTRACT, TYSIGNATURE, TYMODULE, TYVALUE, 
} TYPE_INFO;

typedef struct Type_s {
    TYPE_INFO t;
    union {
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
            struct Type_s ** list;
        };
    };
} DTYPE;


/*-----------------------------------------------------------------------------
 *  Module Sharing
 *-----------------------------------------------------------------------------*/
typedef enum Modtag_e{
    STRUCTURE, FUNCTOR 
} MODTAG;

typedef struct Moduledata_s{
    MODTAG t;
    DTYPE type;
    int identifier;
    struct {
        char ** names; 
        void ** fcalls;
    };
} MODDATA;


/*-----------------------------------------------------------------------------
 *  General Entrypoints 
 *-----------------------------------------------------------------------------*/

ENTRYPOINT DATA closure_entry(int,DATA);
ENTRYPOINT MODDATA functor_entry(int,MODDATA);

#endif

