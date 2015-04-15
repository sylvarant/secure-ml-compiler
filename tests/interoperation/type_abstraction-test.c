/*
 * =====================================================================================
 *
 *       Filename:  example10-test.c
 *
 *         Author:  MYSTERY MAN, 
 *        Company:  SOMEWHERE
 *
 * =====================================================================================
 */

#include "unit.h"

#include "type_abstraction.h"

int tests_run = 0;
int tests_set = 5;

TEST(testAlternateCreate)
    DATA temp = Alternate_create();
    CHECK("Did not fetch an Abstract from Alternate.create",temp.t == ABSTRACT);
DONE

TEST(testAbstractCreate)
    DATA temp = Abstract_create();
    CHECK("Did not fetch an Abstract from Abstract.create",temp.t == ABSTRACT);
DONE

TEST(testAbstractfunc) 
    DATA clo = Abstract_func();
    CHECK("Did not fetch a Closure from Abstract.func",clo.t == CLOSURE);
    DATA arg = Abstract_create();
    DATA res = applyClosure(clo.identifier,arg);  
    CHECK("Did not fetch an Abstract from applying Abstract.func",res.t == ABSTRACT);
DONE

CRASH(crashAbstractfunc)
    DATA clo = Abstract_func();
    DATA arg = Alternate_create();
    DATA res = applyClosure(clo.identifier,arg);  
RECOVER

TEST(testAbstractInnerDumb)
    DATA clo = Abstract_Inner_dumb();
    CHECK("Did not fetch a Closure from Abstract.Inner.dumb",clo.t == CLOSURE);
    DATA clo2 = Abstract_func();
    DATA arg2 = Abstract_create();
    DATA arg = applyClosure(clo2.identifier,arg2);  
    DATA res = applyClosure(clo.identifier,arg);  
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


