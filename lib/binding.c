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
 *         Name:    mycmp
 *  Description:    taken from apple
 * =====================================================================================
 */
FUNCTIONALITY int cmp_char(void *vs1, void *vs2)
{
    char * s1 = (char *) vs1;
    char * s2 = (char *) vs2;

    for ( ; *s1 == *s2; s1++, s2++)
	if (*s1 == '\0')
	    return 0;
    return ((*(unsigned char *)s1 < *(unsigned char *)s2) ? -1 : +1);
}

FUNCTIONALITY int cmp_int(void *vs1, void *vs2)
{
    int s1 = (int) vs1;
    int s2 = (int) vs2;

    if(s1==s2) return 0;
    else return 1;
}


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    getBinding
 *  Description:    return an element from a given environment for a given key
 * =====================================================================================
 */
FUNCTIONALITY void * getBinding(BINDING * ls, void * key, compare cmp)
{
    BINDING * node = ls;
    while(node) {
        if(cmp(key,node->key) == 0)
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
FUNCTIONALITY void insertBinding(BINDING ** head,void * key,void * value)
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
    node->contents = value;
}





