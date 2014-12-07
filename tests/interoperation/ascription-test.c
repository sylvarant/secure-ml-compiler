/*
 * =====================================================================================
 *
 *       Filename:  example9-test.c
 *
 *         Author:  Adriaan Larmuseau, ajhl
 *        Company:  Uppsala
 *
 * =====================================================================================
 */

#include "unit.h"
#include "entry.h"

int tests_run = 0;
int tests_set = 3;

TEST(testpublic)
    DATA temp = path_entry(PATH(Main.public));
    CHECK("Did not fetch a Closure from public",temp.t == CLOSURE);
    DATA arg = { .t = INT, .value = 3};
    DATA res = closure_entry(temp.identifier,arg);
    CHECK("Did not recieve back 80",res.value == 80);
DONE

TEST(testinnerf)
    DATA temp = path_entry(PATH(Main.Inner.innerf));
    CHECK("Did not fetch 20 from innerf",temp.value == 20);
DONE

CRASH(crashsecret)
    DATA temp = path_entry(PATH(Main.IgnoreMe.secret));
RECOVER

LIST
    RUN(testpublic);
    RUN(testinnerf);
    RUN(crashsecret);
DONE

INCLUDE_MAIN


