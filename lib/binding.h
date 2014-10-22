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

// trickery
typedef struct max_s{
    void * weight[3];
} MAX;


/*-----------------------------------------------------------------------------
 *  Environment Binding
 *  Binds a key -> to either a function call or a module
 *-----------------------------------------------------------------------------*/

typedef struct Meta_s
{
    unsigned int call;
    union {
        void * value;
        MAX (*gettr)(struct Binding_s *);
    };
} META;

typedef struct Binding_s 
{
    char * key;
    META * contents;
    struct Binding_s * next;
} BINDING;


/*-----------------------------------------------------------------------------
 *  Functionality
 *-----------------------------------------------------------------------------*/

FUNCTIONALITY void insertBinding(BINDING **,char *,void *,unsigned int);
FUNCTIONALITY void * getBinding(BINDING *,char *);

#endif


