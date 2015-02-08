/*
 * =====================================================================================
 *
 *       Filename:  sequence-test.c
 *
 *         Author:  MYSTERY MAN, 
 *        Company:  SOMEWHERE
 *
 * =====================================================================================
 */

#include "unit.h"

#include "sequence.h"

int tests_run = 0;
int tests_set = 2;

TEST(getModule)
    MODDATA temp = this();
    CHECK("Did not fetch the this module",temp.identifier == 1);
DONE

TEST(getTest)
    DATA temp = test1();
    CHECK("Did not fetch Integer",temp.t == INT);
    CHECK("Did not fetch value 8",temp.value == 8);
DONE

LIST
    RUN(getModule);
    RUN(getTest);
DONE

INCLUDE_MAIN


