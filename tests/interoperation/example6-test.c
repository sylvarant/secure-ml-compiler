/*
 * =====================================================================================
 *
 *       Filename:  example6-test.c
 *
 *         Author:  Adriaan Larmuseau, ajhl
 *        Company:  Uppsala
 *
 * =====================================================================================
 */

#include "unit.h"
#include "entry.h"

int tests_run = 0;
int tests_set = 2;

TEST(getaddClosure)
    DATA temp = path_entry("add",3);
    CHECK("Did not fetch a Closure from add",temp.t == CLOSURE);
DONE

TEST(getcompClosure)
    DATA temp = path_entry("comp",4);
    CHECK("Did not fetch a Closure from comp",temp.t == CLOSURE);
DONE


LIST
    RUN(getaddClosure);
    RUN(getcompClosure);
DONE

INCLUDE_MAIN


