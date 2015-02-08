/*
 * =====================================================================================
 *
 *       Filename:  basic_functor-test.c
 *
 *         Author:  MYSTERY MAN, 
 *        Company:  SOMEWHERE
 *
 * =====================================================================================
 */

#include "basic_functor.h"
#include "unit.h"


int tests_run = 0;
int tests_set = 1;
enum { TESTCOUNT = 100000};

TEST(dynamicEntry)
    STRCT temp = CALLM(IsZero);
    #ifndef INSECURE
    STRCT functor = PairTest();
    STRCT new = functorEntry(functor.identifier,temp);
    #else 
    STRCT new = _PairTest_Fctr1(NULL,temp);
    #endif
    
    // time
    START
    for(int i = 0; i < TESTCOUNT; i++){
        #ifndef INSECURE
        BASIC result = PairTest_Functor_testfst(new);
        #else
        BASIC result = _PairTest_Functor_testfst(new.strls);    
        #endif
    }
    STOP(stdout);
    
DONE

LIST
    RUN(dynamicEntry);
DONE

INCLUDE_MAIN


