/*
 * =====================================================================================
 *
 *       Filename:  example4-test.c
 *
 *         Author:  Adriaan Larmuseau, ajhl
 *        Company:  Uppsala
 *
 * =====================================================================================
 */

#include "unit.h"
#include "entry.h"

int tests_run = 0;
int tests_set = 2;

TEST(getModule)
    DATA temp = path_entry("Outer",5);
    CHECK("Did not fetch Outer module",temp.t == MODULE);
DONE

TEST(getValue)
    DATA temp = path_entry("Outer.value",11);
    CHECK("Did not fetch Integer",temp.t == BOOLEAN);
    CHECK("Did not fetch value ",temp.value == 1);
DONE

LIST
    RUN(getModule);
    RUN(getValue);
DONE

INCLUDE_MAIN


