/*
 * =====================================================================================
 *
 *       Filename:  higher_order_functor-test.c
 *
 *         Author:  Adriaan Larmuseau, ajhl
 *        Company:  Uppsala
 *
 * =====================================================================================
 */

#include "higher_order_functor.h"
#include "unit.h"


int tests_run = 0;
int tests_set = 6;

TEST(getModule)
    MODDATA temp = IntOrder();
    CHECK("Did not fetch the IntOrder module",temp.identifier == 1);
    CHECK("Does not have enough names",temp.count == 2);
DONE

TEST(getFunctor)
    MODDATA functor = LexicographicOrder();
    CHECK("Did not fetch the functor module",functor.t == FUNCTOR);
DONE

TEST(getInnerFunctor)
    MODDATA temp = IntOrder();
    MODDATA functor = LexicographicOrder();
    MODDATA inner = functorEntry(functor.identifier,temp);
    CHECK("Did not fetch the inner functor module",inner.t == FUNCTOR);
DONE


TEST(applyFunctor)
    MODDATA temp = IntOrder();
    MODDATA functor = LexicographicOrder();
    MODDATA inner = functorEntry(functor.identifier,temp);
    MODDATA object = functorEntry(inner.identifier,temp);
    CHECK("Did not produce a new module",object.t == STRUCTURE);
    CHECK("New module does not have enough names",object.count == 2);
    CHECK("New module does not contain testfst",(strcmp(object.names[0],"equal") == 0));
DONE


TEST(dynamicEntry)
    MODDATA temp = IntOrder();
    MODDATA functor = LexicographicOrder();
    MODDATA inner = functorEntry(functor.identifier,temp);
    MODDATA object = functorEntry(inner.identifier,temp);
    func_entry call = object.fcalls[0];
    DATA result = call(object);
    CHECK("Result is a closure",result.t == CLOSURE);
DONE

TEST(dynamicResult)
    MODDATA temp = IntOrder();
    MODDATA functor = LexicographicOrder();
    MODDATA inner = functorEntry(functor.identifier,temp);
    MODDATA object = functorEntry(inner.identifier,temp);
    func_entry call = object.fcalls[0];
    DATA closure = call(object);
    DATA left = { .t = INT, .value = 2 };
    DATA right = { .t = INT, .value = 0};
    DATA arg  = { .t = PAIR, .left = &left, .right = &right};
    DATA once = closureEntry(closure.identifier,arg); 
    DEBUG_PRINT("fetching result");
    DATA result = closureEntry(once.identifier,arg);  
    CHECK("Result is Boolean",result.t == BOOLEAN);  
    CHECK("Result is true",result.value == 1);
DONE

LIST
    RUN(getModule);
    RUN(getFunctor);
    RUN(getInnerFunctor);
    RUN(applyFunctor);
    RUN(dynamicEntry);
    RUN(dynamicResult);
DONE

INCLUDE_MAIN


