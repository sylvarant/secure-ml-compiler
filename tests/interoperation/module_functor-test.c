/*
 * =====================================================================================
 *
 *       Filename:  module_functor-test.c
 *
 *         Author:  Adriaan Larmuseau, ajhl
 *        Company:  Uppsala
 *
 * =====================================================================================
 */

#include "module_functor.h"
#include "unit.h"


int tests_run = 0;
int tests_set = 5;

typedef DATA (*func_entry) (MODDATA);
typedef MODDATA (*mod_entry) (MODDATA);

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
    MODDATA new = functor_entry(functor.identifier,temp);
    CHECK("Did not produce a new module",new.t == STRUCTURE);
    CHECK("New module does not have enough names",new.count == 2);
    CHECK("New module does not contain testfst",(strcmp(new.names[0],"testfst") == 0));
    CHECK("New module does not contain New",(strcmp(new.names[1],"New") == 0));
DONE

TEST(dynamicModule)
    MODDATA temp = IsZero();
    MODDATA functor = Test();
    MODDATA new = functor_entry(functor.identifier,temp);
    mod_entry call = new.fcalls[1];
    MODDATA result = call(new);
    CHECK("Result is a Structure",result.t == STRUCTURE);
    CHECK("Result does not enough names",result.count == 1);
DONE

TEST(dynamicModuleEntry)
    MODDATA temp = IsZero();
    MODDATA functor = Test();
    MODDATA new = functor_entry(functor.identifier,temp);
    mod_entry call = new.fcalls[1];
    MODDATA inner = call(new);
    func_entry call2 = inner.fcalls[0];
    DATA result = call2(new); 
    CHECK("Result is not 5",result.value == 5);
DONE


LIST
    RUN(getModule);
    RUN(getFunctor);
    RUN(applyFunctor);
    RUN(dynamicModule);
    RUN(dynamicModuleEntry);
DONE

INCLUDE_MAIN


