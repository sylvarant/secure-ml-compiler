/*
 * =====================================================================================
 *
 *       Filename:  example3-test.c
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
    DATA temp = path_entry(PATH(Main));
    CHECK("Did not fetch Main module",temp.t == MODULE);
DONE

TEST(getCLosure)
    DATA temp = path_entry(PATH(Main.main));
    CHECK("Did not fetch Integer",temp.t == INT);
    CHECK("Did not fetch value 5",temp.value == 5);
DONE

LIST
    RUN(getModule);
    RUN(getCLosure);
DONE

INCLUDE_MAIN


