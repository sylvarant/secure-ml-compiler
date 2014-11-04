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

typedef enum Tag_e {
    INT, BOOLEAN, CLOSURE, PAIR, MODULE, FUNCTOR,
} TAG;

typedef struct Data_s{
    TAG t;   
    union {
        char * bytes;
        int value; 
        int identifier;
        struct {
            struct Data_s * left;
            struct Data_s * right;
        };
    };
} DATA;


/*-----------------------------------------------------------------------------
 *  Secure Entrypoints 
 *-----------------------------------------------------------------------------*/

ENTRYPOINT DATA path_entry(char * path, int size);
ENTRYPOINT DATA closure_entry(int id,DATA d);


#endif
