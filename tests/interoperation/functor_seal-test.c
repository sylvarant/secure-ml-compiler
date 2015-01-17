/*
 * =====================================================================================
 *
 *       Filename:  functor_seal-test.c
 *
 *         Author:  Adriaan Larmuseau, ajhl
 *        Company:  Uppsala
 *
 * =====================================================================================
 */

#include "functor_seal.h"
#include "unit.h"


int tests_run = 0;
int tests_set = 7;


TEST(getModule)
    MODDATA temp = IsZero();
    CHECK("Did not fetch the One module",temp.identifier == 1);
    CHECK("Does not have enough names",temp.count == 2);
DONE

TEST(getFunctor)
    MODDATA functor = Test();
    CHECK("Did not fetch the functor module",functor.t == FUNCTOR);
DONE

TEST(applyFunctor)
    MODDATA temp = IsZero();
    MODDATA functor = Test();
    MODDATA new = functorEntry(functor.identifier,temp);
    CHECK("Did not produce a new module",new.t == STRUCTURE);
    CHECK("New module does not have enough names",new.count == 2);
    CHECK("New module does not contain testfst",(strcmp(new.names[0],"testfst") == 0));
    CHECK("New module does not contain input",(strcmp(new.names[1],"input") == 0));
DONE

TEST(dynamicEntry)
    MODDATA temp = IsZero();
    MODDATA functor = Test();
    MODDATA new = functorEntry(functor.identifier,temp);
    func_entry call = new.fcalls[0];
    func_entry call2 = new.fcalls[1];
    DATA result = call(new);
    DATA result2 = call2(new);
    CHECK("Result of fcall 0 is a closure",result.t == CLOSURE);
    CHECK("Result of fcall 1 is a closure",result2.t == CLOSURE);
DONE

TEST(dynamicResult)
    MODDATA temp = IsZero();
    MODDATA functor = Test();
    MODDATA new = functorEntry(functor.identifier,temp);
    func_entry call = new.fcalls[0];
    func_entry call2 = new.fcalls[1];
    DATA closure = call(new);
    DATA closure2 = call2(new);
    DATA arg = { .t = INT, .value = 2 };
    DATA result = closureEntry(closure.identifier,arg); 
    CHECK("Result is Abstract",result.t == ABSTRACT);  
    DATA result2 = closureEntry(closure2.identifier,result); 
    CHECK("Result is false",result2.value == 0);  
DONE

CRASH(dynamicResultTypeFail)
    MODDATA temp = IsZero();
    MODDATA functor = Test();
    MODDATA new = functorEntry(functor.identifier,temp);
    func_entry call = new.fcalls[1];
    DATA closure = call(new);
    DATA left = { .t = INT, .value = 2 };
    DATA result = closureEntry(closure.identifier,left); 
RECOVER

CRASH(dynamicResultTypeFail2)
    MODDATA temp = IsZero();
    MODDATA functor = Test();
    MODDATA new = functorEntry(functor.identifier,temp);
    MODDATA second = functorEntry(functor.identifier,temp);
    func_entry scall = second.fcalls[0];
    DATA left = { .t = INT, .value = 2 };
    DATA sclosure = scall(second);
    DATA type2 = closureEntry(sclosure.identifier,left); 
    func_entry call = new.fcalls[1];
    DATA closure = call(new);
    DATA result = closureEntry(closure.identifier,type2); 
RECOVER

LIST
    RUN(getModule);
    RUN(getFunctor);
    RUN(applyFunctor);
    RUN(dynamicEntry);
    RUN(dynamicResult);
    RUN(dynamicResultTypeFail);
    RUN(dynamicResultTypeFail2);
DONE

INCLUDE_MAIN


