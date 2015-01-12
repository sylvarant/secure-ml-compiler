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

#include "arithmetic.h"
#include "unit.h"


int tests_run = 0;
int tests_set = 4;

TEST(getaddClosure)
    DATA temp = add();
    CHECK("Did not fetch a Closure from add",temp.t == CLOSURE);
DONE

TEST(applyClosure)
    DATA temp = add();
    CHECK("Did not fetch a Closure from add",temp.t == CLOSURE);
    DATA input = { .t = INT, .value = 5 };
    DATA res = closureEntry(temp.identifier,input);
    CHECK("Did not get a correct result",res.value == 6);
DONE

TEST(getcompClosure)
    DATA temp = comp();
    CHECK("Did not fetch a Closure from comp",temp.t == CLOSURE);
DONE

CRASH(crashCloApply)
    DATA temp = add();
    CHECK("Did not fetch a Closure from add",temp.t == CLOSURE);
    DATA input = { .t = BOOLEAN, .value = 5 };
    DATA res = closureEntry(temp.identifier,input);
RECOVER

LIST
    RUN(getaddClosure);
    RUN(applyClosure);
    RUN(getcompClosure);
    RUN(crashCloApply);
DONE

INCLUDE_MAIN


