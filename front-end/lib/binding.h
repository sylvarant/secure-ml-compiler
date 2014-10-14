/*
 * =====================================================================================
 *
 *       Filename:  binding.h
 *
 *    Description:  Implementation of the bindings used in the run-time
 *
 *         Author:  Adriaan Larmuseau, ajhl
 *        Company:  Uppsala
 *
 * =====================================================================================
 */


#ifndef BINDING_INCLUDED
#define BINDING_INCLUDED

#include "global.h"


/*-----------------------------------------------------------------------------
 *  Environment Binding
 *  Binds a key -> to a chunk of memory
 *-----------------------------------------------------------------------------*/

typedef struct Binding_t 
{
    char * key;
    char * chunk;
    struct N(Binding_t) * next;
} BINDING;


/*-----------------------------------------------------------------------------
 *  Functionality
 *-----------------------------------------------------------------------------*/

FUNCTIONALITY void insertBinding(BINDING **,char *,char *);
FUNCTIONALITY void * getBinding(BINDING *,char *);

#endif


