/*
 * =====================================================================================
 *
 *       Filename:  functor_arg-test.c
 *
 *
 *         Author:  Adriaan Larmuseau, ajhl
 *        Company:  Uppsala
 *
 * =====================================================================================
 */

// TEST in dev

#include "functor_arg.h"
#include "unit.h"

int tests_run = 0;
int tests_set = 6;

TEST(getSetup)
    MODDATA arg = Argument();
    CHECK("Did not fetch the Argument mdoule",arg.t == STRUCTURE);
    CHECK("Does not have enough names",arg.count == 1);
    MODDATA functor = IdF();
    CHECK("Did not fetch the Functor",functor.t == FUNCTOR);
DONE

TEST(applyFunctor)
    MODDATA arg = Argument();
    MODDATA functor = IdF();
    MODDATA new = functorEntry(functor.identifier,arg);
    CHECK("Did not produce a new module",new.t == STRUCTURE);
    CHECK("New module does not have enough names",new.count == 1);
    CHECK("New module does not contain func",(strcmp(new.names[0],"func") == 0));
    CHECK("Entry Point is off ",new.fcalls[0] == IdF_Functor_func);
DONE

TEST(dynGettr)
    MODDATA arg = Argument();
    MODDATA functor = IdF();
    MODDATA new = functorEntry(functor.identifier,arg);
    func_entry func = new.fcalls[0];
    DATA closure = func(new);
    DATA input = {.t = INT , .value = 10};
    DATA result = closureEntry(closure.identifier,input);
    CHECK("Did not return 11",result.value == 11);
DONE

TEST(simpleApp)
    MODDATA farg = IdF();
    MODDATA fhigh = SimpleAppF();
    MODDATA new = functorEntry(fhigh.identifier,farg); 
DONE

TEST(higherOrder)
    MODDATA arg = Argument();
    MODDATA farg = IdF(); 
    MODDATA fhigh = IdHighF();  
    MODDATA new = functorEntry(fhigh.identifier,farg);  
    CHECK("result of higher order id is not functor",new.t == FUNCTOR);
    MODDATA str = functorEntry(new.identifier,arg);
    CHECK("Result is not a structure",str.t == STRUCTURE);
    func_entry call = str.fcalls[0];
    DATA closure = call(str); 
    CHECK("result is not a closure",closure.t == CLOSURE);
DONE

CRASH(crashDynStamp)
    MODDATA arg = Argument();
    MODDATA farg = IdF(); 
    MODDATA fhigh = IdHighF();  
    MODDATA new = functorEntry(fhigh.identifier,farg);  
    MODDATA str = functorEntry(new.identifier,arg); 
    func_entry call = str.fcalls[0];
    DATA closure = call(arg); 
RECOVER

LIST
    RUN(getSetup);
    RUN(applyFunctor);
    RUN(dynGettr);
    RUN(simpleApp);
    RUN(higherOrder);
    RUN(crashDynStamp);
DONE

INCLUDE_MAIN

