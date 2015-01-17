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
 *  Key comparison methods
 *-----------------------------------------------------------------------------*/
 typedef int (*compare)(void *,void*);

/*-----------------------------------------------------------------------------
 *  Environment Binding
 *  Binds a key -> to either a function call or a module
 *-----------------------------------------------------------------------------*/
typedef struct Binding_s 
{
    void * key;
    void * contents;
    struct Binding_s * next;
} BINDING;

typedef struct intlist_s
{
    int content;
    struct intlist_s * next;
}INTLIST;


/*-----------------------------------------------------------------------------
 *  Functionality
 *-----------------------------------------------------------------------------*/

FUNCTIONALITY void insertBinding(BINDING **,void *,void *);
FUNCTIONALITY BINDING* pushBinding(BINDING *,void *,void *);
FUNCTIONALITY void * getBinding(BINDING *,void *,compare);
FUNCTIONALITY INTLIST * pushIntlist(INTLIST *,int);
FUNCTIONALITY int getPosIntlist(INTLIST *,int);
FUNCTIONALITY int cmp_char(void *,void*);
FUNCTIONALITY int cmp_int(void*,void*);

#endif


