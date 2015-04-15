/*
 * =====================================================================================
 *
 *       Filename:  functions-calls.c
 *
 *         Author:  MYSTERY MAN, 
 *        Company:  SOMEWHERE
 *
 * =====================================================================================
 */

#include "unit.h"

#include "functions.h"

int tests_run = 0;
int tests_set = 3;


/*-----------------------------------------------------------------------------
 *  attacker functions
 *-----------------------------------------------------------------------------*/

DATA goodfunction(DATA val)
{
   int result = val.value == 0;
   DATA ret = {.t = BOOLEAN, .value = result};
   return ret;
}

DATA badfunction(DATA val)
{
    DATA ret = {.t = INT, .value = 80};
    return ret;
}


/*-----------------------------------------------------------------------------
 *  tests
 *-----------------------------------------------------------------------------*/

TEST(getf4Closure)
    DATA temp = function4();
    CHECK("Did not fetch a Closure from f4",temp.t == CLOSURE);
DONE

TEST(applytof4)
    DATA clo = function4();
    DATA arg = {.t = CALLBACK, .call = goodfunction};
    DATA res = applyClosure(clo.identifier,arg);
    CHECK("Did not recieve a Boolean",res.t == BOOLEAN);
DONE

CRASH(crashf4)
    DATA clo = function4();
    DATA arg = {.t = CALLBACK, .call = badfunction};
    DATA res = applyClosure(clo.identifier,arg);
RECOVER

LIST
    RUN(getf4Closure);
    RUN(applytof4);
    RUN(crashf4);
DONE

INCLUDE_MAIN


