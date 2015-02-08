/*
 * =====================================================================================
 *
 *       Filename:  functor_two-test.c
 *
 *         Author:  MYSTERY MAN, 
 *        Company:  SOMEWHERE
 *
 * =====================================================================================
 */

#include "unit.h"
#include <string.h>

#include "functor_two.h"

int tests_run = 0;
int tests_set = 8;


TEST(getModule)
    MODDATA temp = Input();
    CHECK("Did not fetch the Input module",temp.identifier == 1);
    CHECK("Does not have enough names",temp.count == 1);
DONE

TEST(getTest1)
    MODDATA functor = Test1();
    CHECK("Did not fetch the test 1 functor",functor.t == FUNCTOR);
DONE

TEST(getTest2)
    MODDATA functor = Test2();
    CHECK("Did not fetch the test 2 functor",functor.t == FUNCTOR);
DONE

TEST(applyFunctor1)
    MODDATA temp = Input();
    MODDATA functor = Test1();
    MODDATA new = functorEntry(functor.identifier,temp);
    CHECK("Did not produce a new module",new.t == STRUCTURE);
    CHECK("New module does not have enough names",new.count == 1);
    CHECK("New module does not contain testfst",(strcmp(new.names[0],"testfst") == 0));
DONE

TEST(applyFunctor2)
    MODDATA temp = Input();
    MODDATA functor = Test2();
    MODDATA new = functorEntry(functor.identifier,temp);
    CHECK("Did not produce a new module",new.t == STRUCTURE);
    CHECK("New module does not have enough names",new.count == 1);
    CHECK("New module does not contain testfst",(strcmp(new.names[0],"testfst") == 0));
DONE

TEST(dynamicEntry1)
    MODDATA temp = Input();
    MODDATA functor = Test1();
    MODDATA new = functorEntry(functor.identifier,temp);
    func_entry call = new.fcalls[0];
    DATA result = call(new);
    CHECK("Result is a closure",result.t == CLOSURE);
DONE

TEST(dynamicEntry2)
    MODDATA temp = Input();
    MODDATA functor = Test2();
    MODDATA new = functorEntry(functor.identifier,temp);
    func_entry call = new.fcalls[0];
    DATA result = call(new);
    CHECK("Result is a closure",result.t == CLOSURE);
DONE

CRASH(crashDynamicEntry)
    MODDATA temp = Input();
    MODDATA functor1 = Test1();
    MODDATA new1 = functorEntry(functor1.identifier,temp);
    MODDATA functor2 = Test2();
    MODDATA new2 = functorEntry(functor2.identifier,temp);
    func_entry call = new1.fcalls[0];
    DATA result = call(new2);
RECOVER


LIST
    RUN(getModule);
    RUN(getTest1);
    RUN(getTest2);
    RUN(applyFunctor1);
    RUN(applyFunctor2);
    RUN(dynamicEntry1);
    RUN(dynamicEntry2);
    RUN(crashDynamicEntry);
DONE

INCLUDE_MAIN


