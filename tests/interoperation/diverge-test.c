/*
 * =====================================================================================
 *
 *       Filename:  diverge-test.c
 *
 *         Author:  MYSTERY MAN, 
 *        Company:  SOMEWHERE
 *
 * =====================================================================================
 */

#include "unit.h"
#include "diverge.h"

int tests_run = 0;
int tests_set = 2;

// test the exit statement
/*CRASH(terminate)
    DATA temp = Inner_terminate();
RECOVER*/

TEST(recursion)
    DATA closure = Inner_recurse();  
    CHECK("Divergence is not a closure",closure.t == CLOSURE);
    DATA bln = { .t = BOOLEAN, .value = 1 }; 
    DATA result =applyClosure(closure.identifier,bln);
    CHECK("Result is not true",result.value == 1); 
DONE

TEST(recursion2)
    DATA closure = Inner_recurse();  
    DATA bln = { .t = BOOLEAN, .value = 0 }; 
    DATA result =applyClosure(closure.identifier,bln);
    CHECK("Result is not true",result.value == 1); 
DONE

LIST
//    RUN(terminate);
    RUN(recursion);
    RUN(recursion2);
DONE

INCLUDE_MAIN


