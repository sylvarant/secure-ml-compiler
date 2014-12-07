/*
 * =====================================================================================
 *
 *       Filename:  example10-test.c
 *
 *         Author:  Adriaan Larmuseau, ajhl
 *        Company:  Uppsala
 *
 * =====================================================================================
 */

#include "unit.h"
#include "entry.h"

int tests_run = 0;
int tests_set = 5;

TEST(testAlternateCreate)
    DATA temp = path_entry(PATH(Alternate.create));
    CHECK("Did not fetch an Abstract from Alternate.create",temp.t == ABSTRACT);
DONE

TEST(testAbstractCreate)
    DATA temp = path_entry(PATH(Abstract.create));
    CHECK("Did not fetch an Abstract from Abstract.create",temp.t == ABSTRACT);
DONE

TEST(testAbstractfunc) 
    DATA clo = path_entry(PATH(Abstract.func));
    CHECK("Did not fetch a Closure from Abstract.func",clo.t == CLOSURE);
    DATA arg = path_entry(PATH(Abstract.create));
    DATA res = closure_entry(clo.identifier,arg);  
    CHECK("Did not fetch an Abstract from applying Abstract.func",res.t == ABSTRACT);
DONE

CRASH(crashAbstractfunc)
    DATA clo = path_entry(PATH(Abstract.func));
    DATA arg = path_entry(PATH(Alternate.create));
    DATA res = closure_entry(clo.identifier,arg);  
RECOVER

TEST(testAbstractInnerDumb)
    DATA clo = path_entry(PATH(Abstract.Inner.dumb));
    CHECK("Did not fetch a Closure from Abstract.Inner.dumb",clo.t == CLOSURE);
    DATA clo2 = path_entry(PATH(Abstract.func));
    DATA arg2 = path_entry(PATH(Abstract.create));
    DATA arg = closure_entry(clo2.identifier,arg2);  
    DATA res = closure_entry(clo.identifier,arg);  
    CHECK("Did not retrieve the value 10 from dumb",res.value == 10); 
DONE

LIST
    RUN(testAlternateCreate);
    RUN(testAbstractCreate);
    RUN(testAbstractfunc);
    RUN(crashAbstractfunc);
    RUN(testAbstractInnerDumb);
DONE

INCLUDE_MAIN


