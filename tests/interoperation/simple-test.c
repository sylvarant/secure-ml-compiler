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

#include "simple.h"

int tests_run = 0;
int tests_set = 2;

TEST(getModule)
    MODDATA temp = Main();
    CHECK("Did not fetch Main module",temp.identifier == 1);
DONE

TEST(getCLosure)
    DATA temp = Main_main();
    CHECK("Did not fetch Integer",temp.t == INT);
    CHECK("Did not fetch value 5",temp.value == 5);
DONE

LIST
    RUN(getModule);
    RUN(getCLosure);
DONE

INCLUDE_MAIN


