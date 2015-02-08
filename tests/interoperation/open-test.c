/*
 * =====================================================================================
 *
 *       Filename:  open-test.c
 *
 *         Author:  MYSTERY MAN, 
 *        Company:  SOMEWHERE
 *
 * =====================================================================================
 */

#include "open.h"
#include "unit.h"

int tests_run = 0;
int tests_set = 2;

TEST(getModule)
    MODDATA temp = Functions();
    CHECK("Did not get the functions module",temp.t == STRUCTURE);
DONE

TEST(getGettr)
    DATA temp = simple_test();
    CHECK("Did not get the boolean",temp.t == BOOLEAN);
    CHECK("Result is not true",temp.value == 1);
DONE

LIST
    RUN(getModule);
    RUN(getGettr);
DONE

INCLUDE_MAIN

