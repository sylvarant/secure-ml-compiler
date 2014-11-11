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
 *  Binds a key -> to either a function call or a module
 *-----------------------------------------------------------------------------*/
typedef struct Binding_s 
{
    char * key;
    void * contents;
    struct Binding_s * next;
} BINDING;


/*-----------------------------------------------------------------------------
 *  Functionality
 *-----------------------------------------------------------------------------*/

FUNCTIONALITY void insertBinding(BINDING **,char *,void *);
FUNCTIONALITY void * getBinding(BINDING *,char *);

#endif


