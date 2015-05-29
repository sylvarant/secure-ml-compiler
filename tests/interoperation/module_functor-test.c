/*
 * =====================================================================================
 *
 *       Filename:  module_functor-test.c
 *
 *         Author:  Adriaan, 
 *        Company:  Uppsala IT
 *
 * =====================================================================================
 */

#include "module_functor.h"
#include "unit.h"


int tests_run = 0;
int tests_set = 8;

TEST(getModule)
    MODDATA temp = IsZero();
    CHECK("Did not fetch the One module",temp.identifier == 1);
    CHECK("Does not have enough names",temp.count == 2);
DONE

TEST(getFunctor)
    MODDATA functor = Test();
    CHECK("Did not fetch the functor module",functor.t == FUNCTOR);
DONE

TEST(applyf)
    MODDATA temp = IsZero();
    MODDATA functor = Test();
    MODDATA new = applyFunctor(functor.identifier,temp);
    CHECK("Did not produce a new module",new.t == STRUCTURE);
    CHECK("New module does not have enough names",new.count == 5);
    CHECK("New module does not contain testfst",(strcmp(new.names[0],"testfst") == 0));
    CHECK("New module does not contain New",(strcmp(new.names[1],"New") == 0));
    CHECK("New module does not contain Origin",(strcmp(new.names[2],"Origin") == 0));
DONE

TEST(dynamicModule)
    MODDATA temp = IsZero();
    MODDATA functor = Test();
    MODDATA new = applyFunctor(functor.identifier,temp);
    mod_entry call = new.fcalls[1];
    MODDATA result = call(new);
    CHECK("Result is a Structure",result.t == STRUCTURE);
    CHECK("Result does not enough names",result.count == 1);
DONE

TEST(dynamicModuleEntry)
    MODDATA temp = IsZero();
    MODDATA functor = Test();
    MODDATA new = applyFunctor(functor.identifier,temp);
    mod_entry call = new.fcalls[1];
    MODDATA inner = call(new);
    func_entry call2 = inner.fcalls[0];
    DATA result = call2(inner); 
    CHECK("Result is not 5",result.value == 5);
DONE

CRASH(crashdynamicModuleEntry)
    MODDATA temp = IsZero();
    MODDATA functor = Test();
    MODDATA new = applyFunctor(functor.identifier,temp);
    mod_entry call = new.fcalls[1];
    MODDATA inner = call(new);
    func_entry call2 = inner.fcalls[0];
    DATA result = call2(new); 
RECOVER

TEST(assignedModule)
    MODDATA temp = IsZero();
    MODDATA functor = Test();
    MODDATA new = applyFunctor(functor.identifier,temp);
    mod_entry call = new.fcalls[2];
    MODDATA inner = call(new);
    CHECK("Result is a Structure",inner.t == STRUCTURE);
    CHECK("Result does not enough names",inner.count == 1);
DONE 

TEST(assignedModuleEntry)
    MODDATA temp = IsZero();
    MODDATA functor = Test();
    MODDATA new = applyFunctor(functor.identifier,temp);
    mod_entry call = Test_Functor_Origin; 
    MODDATA origin = call(new);
    func_entry test = Test_Functor_Origin_test;
    DATA arg = {.t = INT, .value = 10};
    DATA closure = test(origin); 
 /*   DATA result = applyClosure(closure.identifier,arg);    
    CHECK("Result is not a boolean",result.t == BOOLEAN);
    CHECK("Result is not false",result.value == 0);  */
DONE 

LIST
    RUN(getModule);
    RUN(getFunctor);
    RUN(applyf);
    RUN(dynamicModule);
    RUN(dynamicModuleEntry);
    RUN(assignedModule);
    RUN(assignedModuleEntry);
    RUN(crashdynamicModuleEntry);
DONE

INCLUDE_MAIN


