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
CRASH(terminate)
    DATA temp = Inner_terminate();
RECOVER

LIST
    RUN(terminate);
DONE

INCLUDE_MAIN


