/*
 * =====================================================================================
 *
 *       Filename:  functions-test.c
 *
 *         Author:  MYSTERY MAN, 
 *        Company:  SOMEWHERE
 *
 * =====================================================================================
 */

#include "unit.h"

#include "functions.h"

int tests_run = 0;
int tests_set = 6;

TEST(getf1Closure)
    DATA temp = function1();
    CHECK("Did not fetch a Closure from f1 ",temp.t == CLOSURE);
DONE

TEST(getf2Closure)
    DATA temp = function2();
    CHECK("Did not fetch a Closure from f2",temp.t == CLOSURE);
DONE

TEST(getf3Closure)
    DATA temp = function3();
    CHECK("Did not fetch a Closure from f3",temp.t == CLOSURE);
DONE

TEST(getf4Closure)
    DATA temp = function4();
    CHECK("Did not fetch a Closure from f4",temp.t == CLOSURE);
DONE

TEST(applytof4)
    DATA clo = function4();
    DATA arg = function3();
    DATA res = closureEntry(clo.identifier,arg);
    CHECK("Did not recieve a Boolean",res.t == BOOLEAN);
DONE

CRASH(crashf4)
    DATA clo = function4();
    DATA arg = function2();
    DATA res = closureEntry(clo.identifier,arg);
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


