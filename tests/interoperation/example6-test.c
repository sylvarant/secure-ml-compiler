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
int tests_set = 3;

TEST(getaddClosure)
    DATA temp = path_entry("add",3);
    CHECK("Did not fetch a Closure from add",temp.t == CLOSURE);
DONE

TEST(applyClosure)
    DATA temp = path_entry("add",3);
    CHECK("Did not fetch a Closure from add",temp.t == CLOSURE);
    DATA input = { .t = INT, .value = 5 };
    DATA res = closure_entry(temp.identifier,input);
    CHECK("Did not get a correct result",res.value == 6);
DONE

TEST(getcompClosure)
    DATA temp = path_entry("comp",4);
    CHECK("Did not fetch a Closure from comp",temp.t == CLOSURE);
DONE

CRASH(crashCloApply)
    DATA temp = path_entry("add",3);
    CHECK("Did not fetch a Closure from add",temp.t == CLOSURE);
    DATA input = { .t = BOOLEAN, .value = 5 };
    DATA res = closure_entry(temp.identifier,input);
RECOVER

LIST
    RUN(getaddClosure);
    RUN(applyClosure);
    RUN(getcompClosure);
    RUN(crashCloApply);
DONE

INCLUDE_MAIN


