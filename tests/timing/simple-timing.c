/*
 * =====================================================================================
 *
 *       Filename:  simple-timing.c
 *
 *         Author:  Adriaan, 
 *        Company:  Uppsala IT
 *
 * =====================================================================================
 */

#include "unit.h"
#include "simple.h"
enum { TESTCOUNT = 10000000};

int tests_run = 0;
int tests_set = 2;

// work around for deficiency in insec comp
#ifdef INSECURE
VALUE insec_dev()
{
    static int s = 1;
    static VALUE cache;
    if(s){
        cache = _Main_main(NULL);
        s =0;
    }
    return cache;
}
#endif

TEST(timeModule)
     // time
    START
    for(int i = 0; i < TESTCOUNT; i++){
        STRCT temp = CALLM(Main);
        #ifdef INSECURE
        CHECK("id == ?",temp.c.s.entries != NULL);
        #endif
    }
    STOP(stdout);
DONE

TEST(timeValue)
    // time
    START
    for(int i = 0; i < TESTCOUNT; i++){
        #ifndef INSECURE
        BASIC temp = CALL(Main_main);
        #else
        BASIC temp = insec_dev();
        #endif
    }
    STOP(stdout);
DONE

LIST
    RUN(timeModule);
    RUN(timeValue);
DONE

INCLUDE_MAIN


