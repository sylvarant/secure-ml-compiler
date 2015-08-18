/*
 * =====================================================================================
 *
 *       Filename:  basic_functor-test.c
 *
 *         Author:  Adriaan, 
 *        Company:  Uppsala IT
 *
 * =====================================================================================
 */

#include "basic_functor.h"
#include "unit.h"


int tests_run = 0;
int tests_set = 8;


TEST(getModule)
    MODDATA temp = IsZero();
    CHECK("Did not fetch the One module",temp.identifier == 1);
    CHECK("Does not have enough names",temp.count == 1);
DONE

TEST(getFunctor)
    MODDATA functor = PairTest();
    CHECK("Did not fetch the functor module",functor.t == FUNCTOR);
DONE

TEST(applyf)
    MODDATA temp = IsZero();
    MODDATA functor = PairTest();
    MODDATA new = applyFunctor(functor.identifier,temp);
    CHECK("Did not produce a new module",new.t == STRUCTURE);
    CHECK("New module does not have enough names",new.count == 1);
    CHECK("New module does not contain testfst",(strcmp(new.names[0],"testfst") == 0));
DONE

CRASH(crashApplyFunctor)
    MODDATA temp = PairTestZero(); 
    MODDATA functor = PairTest();
    MODDATA new = applyFunctor(functor.identifier,temp);
RECOVER

TEST(dynamicEntry)
    MODDATA temp = IsZero();
    MODDATA functor = PairTest();
    MODDATA new = applyFunctor(functor.identifier,temp);
    func_entry call = new.fcalls[0];
    DATA result = call(new);
    CHECK("Result is a closure",result.t == CLOSURE);
DONE

CRASH(crashDynamicEntry)
    MODDATA temp = PairTestZero();
    DATA result = PairTest_Functor_testfst(temp);
RECOVER

TEST(dynamicResult)
    MODDATA temp = IsZero();
    MODDATA functor = PairTest();
    MODDATA new = applyFunctor(functor.identifier,temp);
    func_entry call = new.fcalls[0];
    DATA closure = call(new);
    DATA left = { .t = INT, .value = 2 };
    DATA right = { .t = INT, .value = 0};
    DATA arg  = { .t = PAIR, .left = &left, .right = &right};
    DATA result = applyClosure(closure.identifier,arg); 
    CHECK("Result is Boolean",result.t == BOOLEAN);  
    CHECK("Result is false",result.value == 0);  
    DATA arg2 = {.t = PAIR, .left = &right, .right = &right};
    DATA result2 = applyClosure(closure.identifier,arg2); 
    CHECK("Result is true",result2.value == 1);  
DONE

CRASH(dynamicResultTypeFail)
    MODDATA temp = IsZero();
    MODDATA functor = PairTest();
    MODDATA new = applyFunctor(functor.identifier,temp);
    func_entry call = new.fcalls[0];
    DATA closure = call(new);
    DATA left = { .t = INT, .value = 2 };
    DATA result = applyClosure(closure.identifier,left); 
RECOVER


LIST
    RUN(getModule);
    RUN(getFunctor);
    RUN(applyf);
    RUN(crashApplyFunctor);
    RUN(dynamicEntry);
    RUN(crashDynamicEntry);
    RUN(dynamicResult);
    RUN(dynamicResultTypeFail);
DONE

INCLUDE_MAIN


