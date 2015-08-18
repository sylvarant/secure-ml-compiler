/*
 * =====================================================================================
 *
 *       Filename:  basic_functor-benchtest.c
 *
 *         Author:  Adriaan, 
 *        Company:  Uppsala IT
 *
 * =====================================================================================
 */

#include "basic_functor.h"
#include "unit.h"


int tests_run = 0;
int tests_set = 1;
enum { TESTCOUNT = 10000};

DATA foreigntest(DATA val)
{
    DATA result = {.t = BOOLEAN, .value = 1};
    return result;
}

DATA gooddata(){
    DATA arg = {.t = CALLBACK, .call = foreigntest};
    return arg;
}

TEST(functorExecution)

  #ifndef INSECURE
  STRCT argument = CALLM(IsZero);
  argument.identifier = -1;
  argument.fcalls[0] = gooddata;
  STRCT functor = PairTest();
  #else
  STRCT argument = CALLM(IsZero);
  #endif

  // time
  START
  for(int i = 0; i < TESTCOUNT; i++){
    #ifndef INSECURE
    STRCT new = applyFunctor(functor.identifier,argument);
    #else 
    STRCT new = _PairTest_Fctr1(NULL,argument);
    #endif
  }
  STOP(stdout);

DONE

LIST
    RUN(functorExecution);
DONE

INCLUDE_MAIN


