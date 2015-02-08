/*
 * =====================================================================================
 *
 *       Filename:  functions-time.c
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


TEST(applytof4)

    // get the arguments
    BASIC clo = CALL(function4);
    BASIC arg = CALL(function3);

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


