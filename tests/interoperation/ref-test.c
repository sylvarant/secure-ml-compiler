/*
 * =====================================================================================
 *
 *       Filename:  ref-test.c
 *
 *         Author:  MYSTERY MAN, 
 *        Company:  SOMEWHERE
 *
 * =====================================================================================
 */

#include "unit.h"

#include "ref.h"

int tests_run = 0;
int tests_set = 5;

TEST(getLocation)
    DATA temp = counter();
    CHECK("Did not fetch the location",temp.t == LOCATION);
    DATA result = locationEntry(temp.identifier);
    CHECK("Did not fetch the integer",result.t == INT);
DONE

TEST(breakCall)
    DATA closure = call();
    CHECK("Did not fetch a Closure",closure.t == CLOSURE);
    DATA tr = { .t = BOOLEAN, .value = 0 };
    DATA result = closureEntry(closure.identifier,tr);
    CHECK("Did not obtain a boolean from call",result.t == BOOLEAN);
    DATA temp = counter();
    DATA ivalue = locationEntry(temp.identifier);
    CHECK("Value is not 1",ivalue.value == 1); 
DONE

TEST(getValue)
    DATA temp = value();
    CHECK("Did not fetch an integer",temp.t == INT);
    CHECK("Did not fetch a value 2",temp.value == 2);
DONE

TEST(Interop)
    DATA closure = crazy();
    CHECK("Did not fetch a Closure",closure.t == CLOSURE);
    DATA bln = { .t = BOOLEAN, .value = 1 }; 
    DATA arg = { .t = BYTES, .byte = &bln};
    DATA result = closureEntry(closure.identifier,arg); 
    CHECK("Value is not 1",result.value == 0); 
DONE

CRASH(Interop2)
    DATA closure = crazy();
    DATA bln = { .t = INT, .value = 5 }; 
    DATA arg = { .t = BYTES, .byte = &bln};
    DATA result = closureEntry(closure.identifier,arg); 
RECOVER

LIST
    RUN(getLocation);
    RUN(breakCall);
    RUN(getValue);
    RUN(Interop);
    RUN(Interop2);
DONE

INCLUDE_MAIN


