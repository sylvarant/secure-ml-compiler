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
 *  Secure Entrypoints 
 *-----------------------------------------------------------------------------*/

ENTRYPOINT void path_entry(char * path);
ENTRYPOINT void closure_entry(int id,void * p);


#endif
