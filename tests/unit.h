/*
 * =====================================================================================
 *
 *       Filename:  unit.h
 *
 *    Description: based on http://www.jera.com/techinfo/jtns/jtn002.html 
 *
 * =====================================================================================
 */

#ifndef UNIT_H
#define UNIT_H

#include <stdio.h>
#include <stdlib.h>

/*-----------------------------------------------------------------------------
 *  DEBUGGING
 *-----------------------------------------------------------------------------*/

#define DEBUG

#ifdef DEBUG
    #include <stdlib.h> // TODO remove duplicates
    #include <stdio.h>
    #define DEBUG_PRINT(...) \
	do{	fprintf(stderr,"DEBUG:: "); fprintf(stderr,__VA_ARGS__); fprintf(stderr,"\n");fflush(stderr);} while(0)
#else
    #define DEBUG_PRINT(...) do {} while (0)
#endif



/*-----------------------------------------------------------------------------
 *  UNIT 
 *-----------------------------------------------------------------------------*/

#define CHECK(message, test) do { if (!(test)) return message; } while (0)
#define RUN(test) do { char *message = test(); tests_run++; \
                                if (message) return message; } while (0)

#define INCLUDE_MAIN int main(void){\
    char *result = all_tests();\
    if (result != 0) printf("%s\n", result);\
    printf("Tests run: %d\n", tests_run);\
    return result != 0;\
}

#define TEST(func) static char * func(){
#define LIST static char * all_tests(){ 
#define DONE return 0;}

extern int tests_run;

#endif
