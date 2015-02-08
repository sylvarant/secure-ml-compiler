/*
 * =====================================================================================
 *
 *       Filename:  example4-test.c
 *
 *         Author:  MYSTERY MAN, 
 *        Company:  SOMEWHERE
 *
 * =====================================================================================
 */

#include "unit.h"

#include "inner.h"

int tests_run = 0;
int tests_set = 2;

TEST(getModule)
    MODDATA temp = Outer();
    CHECK("Did not fetch Outer module",temp.identifier == 1);
DONE

TEST(getValue)
    DATA temp = Outer_Inner_hell();
    CHECK("Did not fetch Integer",temp.t == BOOLEAN);
    CHECK("Did not fetch value ",temp.value == 1);
DONE

LIST
    RUN(getModule);
    RUN(getValue);
DONE

INCLUDE_MAIN


