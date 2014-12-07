/*
 * =====================================================================================
 *
 *       Filename:  example8-test.c
 *
 *         Author:  Adriaan Larmuseau, ajhl
 *        Company:  Uppsala
 *
 * =====================================================================================
 */

#include "unit.h"
#include "entry.h"

int tests_run = 0;
int tests_set = 6;

TEST(getf1Closure)
    DATA temp = path_entry(PATH(function1));
    CHECK("Did not fetch a Closure from f1 ",temp.t == CLOSURE);
DONE

TEST(getf2Closure)
    DATA temp = path_entry(PATH(function2));
    CHECK("Did not fetch a Closure from f2",temp.t == CLOSURE);
DONE

TEST(getf3Closure)
    DATA temp = path_entry(PATH(function3));
    CHECK("Did not fetch a Closure from f3",temp.t == CLOSURE);
DONE

TEST(getf4Closure)
    DATA temp = path_entry(PATH(function4));
    CHECK("Did not fetch a Closure from f4",temp.t == CLOSURE);
DONE

TEST(applytof4)
    DATA clo = path_entry(PATH(function4));
    DATA arg = path_entry(PATH(function3));
    DATA res = closure_entry(clo.identifier,arg);
    CHECK("Did not recieve a Boolean",res.t == BOOLEAN);
DONE

CRASH(crashf4)
    DATA clo = path_entry(PATH(function4));
    DATA arg = path_entry(PATH(function2));
    DATA res = closure_entry(clo.identifier,arg);
RECOVER

LIST
    RUN(getf1Closure);
    RUN(getf2Closure);
    RUN(getf3Closure);
    RUN(getf4Closure);
    RUN(applytof4);
    RUN(crashf4);
DONE

INCLUDE_MAIN


