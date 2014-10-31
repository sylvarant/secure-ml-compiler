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
    DEBUG_PRINT("Attempting path_entry");
    DATA temp = path_entry("Main",4);
    CHECK("Did not fetch Main module",temp.t == MODULE);
    DEBUG_PRINT("Done");
DONE

TEST(getCLosure)
    DATA temp = path_entry("Main.main",9);
    CHECK("Did not fetch Closure Main.main",temp.t == CLOSURE);
DONE

LIST
    RUN(getModule);
    RUN(getCLosure);
DONE

INCLUDE_MAIN


