/*
 * =====================================================================================
 *
 *       Filename:  example9-test.c
 *
 *         Author:  MYSTERY MAN, 
 *        Company:  SOMEWHERE
 *
 * =====================================================================================
 */

#include "ascription.h"
#include "unit.h"


int tests_run = 0;
int tests_set = 2;

TEST(testpublic)
    DATA temp = Main_public();
    CHECK("Did not fetch a Closure from public",temp.t == CLOSURE);
    DATA arg = { .t = INT, .value = 3};
    DATA res = applyClosure(temp.identifier,arg);
    CHECK("Did not recieve back 80",res.value == 80);
DONE

TEST(testinnerf)
    DATA temp = Main_Inner_innerf();
    CHECK("Did not fetch 20 from innerf",temp.value == 20);
DONE

/* DEPRECATED
CRASH(crashsecret)
    DATA temp = path_entry(PATH(Main.IgnoreMe.secret));
RECOVER
*/

LIST
    RUN(testpublic);
    RUN(testinnerf);
 //   RUN(crashsecret);
DONE

INCLUDE_MAIN


