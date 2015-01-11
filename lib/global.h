/*
 * =====================================================================================
 *
 *       Filename:  global.h
 *
 *    Description:  The global definitions for the code within the SPM
 *
 *         Author:  Dr. Fritz Mehner (mn), mehner@fh-swf.de
 *        Company:  FH Südwestfalen, Iserlohn
 *
 * =====================================================================================
 */


#ifndef GLOBAL_SPM_INCLUDED
#define GLOBAL_SPM_INCLUDED

#include "entry.h"

/*-----------------------------------------------------------------------------
 *  DEBUGGING
 *-----------------------------------------------------------------------------*/

#define DEBUG

#ifdef DEBUG
    #include <stdlib.h> // TODO remove duplicates
    #include <stdio.h>
    #define DEBUG_PRINT(...) \
	do{	fprintf(stderr,"DEBUG:: "); fprintf(stderr,__VA_ARGS__); fprintf(stderr,"\n");fflush(stderr);} while(0)
#else
    #define DEBUG_PRINT(...) do {} while (0)
#endif


/*-----------------------------------------------------------------------------
 *  SPM
 *-----------------------------------------------------------------------------*/

#ifdef SANCUS_SPM

    #define LOCAL SM_FUNC(SPM_NAME) static
    #define SECRET_DATA SM_DATA(SPM_NAME) static 
    #define FUNCTIONALITY SM_FUNC(SPM_NAME) extern __attribute__ ((visibility ("hidden")))

    // TODO custom memory manager ...

#else
    // standard c - headers
    #include<stdlib.h>    

    // standard c - memory management
    #define MALLOC malloc
    #define FREE(x) if((x) != NULL) free((x))
    
    // who's who
    #define LOCAL static
    #define SECRET_DATA static
    #define FUNCTIONALITY extern __attribute__ ((visibility ("hidden")))

#endif


/*-----------------------------------------------------------------------------
 *  Naming
 *-----------------------------------------------------------------------------*/

#define T(NAME) T##NAME
#define V(NAME) V##NAME


/*-----------------------------------------------------------------------------
 *  Static Size 
 *-----------------------------------------------------------------------------*/

union Value_u;


/* 
 * ===  FUNCTION ======================================================================
 *         Name:  mistakeFromOutside
 *  Description:  terminate when the outside makes a mistake
 * =====================================================================================
 */
LOCAL void mistakeFromOutside(void)
{
    DEBUG_PRINT("Mistake From Outside !");
    exit(2); 
}

#endif

