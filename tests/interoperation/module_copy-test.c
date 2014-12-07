/*
 * =====================================================================================
 *
 *       Filename:  example5-test.c
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
    DATA temp = path_entry(PATH(One));
    CHECK("Did not fetch the One module",temp.t == MODULE);
DONE

TEST(getValue)
    DATA temp = path_entry(PATH(Two.value));
    CHECK("Did not fetch Pair form Two.value",temp.t == PAIR);
    CHECK("Pair left hand side is not INT",temp.left->t == INT);
    CHECK("Pair right hand side is not BOOLEAN",temp.right->t == BOOLEAN);
    CHECK("Did not obtain left value == 1",temp.left->value == 1);
    CHECK("Did not obtain right value == 0",temp.right->value == 0);
DONE

LIST
    RUN(getModule);
    RUN(getValue);
DONE

INCLUDE_MAIN


