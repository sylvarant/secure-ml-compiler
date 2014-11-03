/*
 * =====================================================================================
 *
 *       Filename:  binding.c
 *
 *    Description:  Implementation of the binding system
 *
 *         Author:  Adriaan Larmuseau, ajhl
 *        Company:  Uppsala
 *
 * =====================================================================================
 */

#include <string.h> // TODO sancus liability
#include "binding.h"

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    getBinding
 *  Description:    return an element from a given environment for a given key
 * =====================================================================================
 */
FUNCTIONALITY META * getBinding(BINDING * ls, char * key)
{
    BINDING * node = ls;
    while(node) {
        if(strcmp(key,node->key) == 0)
            return node->contents;
        node = node->next;
    }
    return NULL;
}


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    insertBinding
 *  Description:    add a new binding to the environment
 * =====================================================================================
 */
FUNCTIONALITY void insertBinding(BINDING ** head, char * key,void * value,unsigned int call)
{
    BINDING * node = MALLOC(sizeof(BINDING));
    META * cont = MALLOC(sizeof(META));
    cont->call = call;
    if(call) cont->value = value;
    else cont->gettr = value; // suck it

    if(*head == NULL){
        *head        = node;
        (*head)->next = NULL;
    }else{
        node->next = (*head);
        (*head)    = node;
    }
    node->key = key;
    node->contents = cont;

}


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    getAdress
 *  Description:    return adresses, may overflow
 * =====================================================================================
 */
FUNCTIONALITY int getAdress(void)
{
    static  int addr = 0;
    return ++addr;
}


