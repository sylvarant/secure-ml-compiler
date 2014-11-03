/*
 * =====================================================================================
 *
 *       Filename:  exa-1.c
 *
 *    Description:  global header file, defines the general entry points
 *
 *         Author:  Adriaan Larmuseau, ajhl
 *        Company:  Uppsala
 *
 * =====================================================================================
 */

#include "unit.h"
#include "entry.h"

int tests_run = 0;

TEST(getModule)
    DATA temp = path_entry("Main",4);
    CHECK("Did not fetch Main module",temp.t == MODULE);
DONE

TEST(getCLosure)
    DATA temp = path_entry("Main.main",9);
    CHECK("Did not fetch Integer",temp.t == INT);
    DEBUG_PRINT("value = %d",temp.value);
    CHECK("Did not fetch value 5",temp.value == 5);
DONE

LIST
    RUN(getModule);
    RUN(getCLosure);
DONE

INCLUDE_MAIN


