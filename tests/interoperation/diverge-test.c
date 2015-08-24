/*
 * =====================================================================================
 *
 *       Filename:  diverge-test.c
 *
 *         Author:  Adriaan, 
 *        Company:  Uppsala IT
 *
 * =====================================================================================
 */

#include "unit.h"
#include "diverge.h"

int tests_run = 0;
int tests_set = 3;


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

// test the exit statement
CRASH(terminate)
    DATA closure = Inner_terminate();  
    DATA bln = { .t = BOOLEAN, .value = 1 }; 
    DATA temp = applyClosure(closure.identifier,bln);
RECOVER

LIST
    RUN(recursion);
    RUN(recursion2);
    RUN(terminate);
DONE

INCLUDE_MAIN


