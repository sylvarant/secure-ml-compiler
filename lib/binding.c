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

#include <stdlib.h>
#include <string.h>


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    getBinding
 *  Description:    return an element from a given environment for a given key
 * =====================================================================================
 */
FUNCTIONALITY void * getBinding(BINDING * ls, char * key)
{
    BINDING * node = ls;
    while(node) {
        if(strcmp(key,node->key) == 0)
            return node->chunk;
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
FUNCTIONALITY void insertBinding(BINDING ** head, char * key, char * chunk)
{
    BINDING * node = MALLOC(sizeof(BINDING));

    if(*head == NULL){
        *head        = node;
        (*head)->next = NULL;
    }else{
        node->next = (*head);
        (*head)    = node;
    }
    node->key = key;
    node->address = chunk;

}

