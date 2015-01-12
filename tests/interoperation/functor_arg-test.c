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
int tests_set = 2;

TEST(getSetup)
    MODDATA arg = Argument();
    CHECK("Did not fetch the Argument mdoule",arg.t == STRUCTURE);
    CHECK("Does not have enough names",arg.count == 1);
    MODDATA functor = SimpleFunctor();
    CHECK("Did not fetch the Functor",functor.t == FUNCTOR);
DONE

TEST(applyFunctor)
    MODDATA arg = Argument();
    MODDATA functor = SimpleFunctor();
    MODDATA new = functorEntry(functor.identifier,arg);
    CHECK("Did not produce a new module",new.t == STRUCTURE);
    CHECK("New module does not have enough names",new.count == 1);
    CHECK("New module does not contain func",(strcmp(new.names[0],"func") == 0));

DONE

LIST
    RUN(getSetup);
    RUN(applyFunctor);
DONE

INCLUDE_MAIN

