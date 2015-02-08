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
int tests_set = 1;
enum { TESTCOUNT = 10000000};



/*-----------------------------------------------------------------------------
 *  attacker functions
 *-----------------------------------------------------------------------------*/

DATA goodfunction(DATA val)
{
   int result = val.value == 0;
   DATA ret = {.t = BOOLEAN, .value = result};
   return ret;
}


/*-----------------------------------------------------------------------------
 *  test
 *-----------------------------------------------------------------------------*/

TEST(applytof4)
    BASIC clo = CALL(function4);

    // marshall vs local
    #ifndef INSECURE
    BASIC arg = {.t = CALLBACK, .call = goodfunction};
    #else
    BASIC arg = CALL(function3);
    #endif

    // time
    START
    for(int i = 0; i < TESTCOUNT; i++){
        BASIC res = APPLYCL(clo,arg);
    }
    STOP(stdout);

DONE



LIST
    RUN(applytof4);
DONE

INCLUDE_MAIN


