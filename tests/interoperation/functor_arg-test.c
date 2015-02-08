/*
 * =====================================================================================
 *
 *       Filename:  functor_arg-test.c
 *
 *
 *         Author:  MYSTERY MAN, 
 *        Company:  SOMEWHERE
 *
 * =====================================================================================
 */

// TEST in dev

#include "functor_arg.h"
#include "unit.h"

int tests_run = 0;
int tests_set = 11;

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
    func_entry func = IdF_Functor_func; 
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
    CHECK("Result does not containt the right entry points",str.fcalls[0] == IdHighF_Functor_Functor_func);
    func_entry call = IdHighF_Functor_Functor_func;
    DATA closure = call(str); 
    CHECK("result is not a closure",closure.t == CLOSURE);
DONE

CRASH(crashDynStamp)
    MODDATA arg = Argument();
    MODDATA farg = IdF(); 
    MODDATA fhigh = IdHighF();  
    MODDATA new = functorEntry(fhigh.identifier,farg);  
    MODDATA str = functorEntry(new.identifier,arg); 
    func_entry call = IdHighF_Functor_Functor_func;
    DATA closure = call(arg); 
RECOVER

TEST(simpleAppF)
    MODDATA farg = IdF();
    MODDATA fhigh = SimpleAppF();
    MODDATA str = functorEntry(fhigh.identifier,farg); 
    CHECK("Result does not contain the right entry points",str.fcalls[0] == SimpleAppF_Functor_func);
    func_entry call = SimpleAppF_Functor_func; 
    DATA closure = call(str); 
    CHECK("Did not recieve a closure",closure.t == CLOSURE);
DONE

CRASH(crashSimpleAppF)
    MODDATA farg = IdF();
    MODDATA fhigh = SimpleAppF();
    MODDATA str = functorEntry(fhigh.identifier,farg); 
    SimpleAppF_Functor_func(Argument());
RECOVER

TEST(innerAppStatic)
    MODDATA farg = IdF();
    MODDATA fhigh = InnerAppF();
    MODDATA str = functorEntry(fhigh.identifier,farg);
    MODDATA inner = InnerAppF_Functor_Argument(str);
    CHECK("Inner is not a structure",inner.t == STRUCTURE);
    CHECK("Inner does not point the the right entry point",inner.fcalls[0] == InnerAppF_Functor_Argument_func);
    DATA closure = InnerAppF_Functor_Argument_func(inner);
    CHECK("Inner.func does not return a closure",closure.t == CLOSURE);
DONE

TEST(innerAppResult)
    MODDATA farg = IdF();
    MODDATA fhigh = InnerAppF();
    MODDATA str = functorEntry(fhigh.identifier,farg);
    MODDATA inner = InnerAppF_Functor_Result(str);
    CHECK("Inner is not a structure",inner.t == STRUCTURE);
    CHECK("Inner does not point the the right entry point",inner.fcalls[0] == InnerAppF_Functor_Result_func);
    DATA closure = InnerAppF_Functor_Result_func(inner);
    CHECK("Inner.func does not return a closure",closure.t == CLOSURE);
DONE

CRASH(crashInnerApp)
    MODDATA inner = InnerAppF_Functor_Result(Argument());
RECOVER

LIST
    RUN(getSetup);
    RUN(applyFunctor);
    RUN(dynGettr);
    RUN(simpleApp);
    RUN(higherOrder);
    RUN(crashDynStamp);
    RUN(simpleAppF);
    RUN(crashSimpleAppF);
    RUN(innerAppStatic);
    RUN(innerAppResult);
    RUN(crashInnerApp);
DONE

INCLUDE_MAIN

