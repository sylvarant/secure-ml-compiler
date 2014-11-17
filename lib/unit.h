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

#include <unistd.h>
#include <sys/types.h>
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
    printf("Ran: %d/%d Tests\n", tests_run,tests_set);\
    return result != 0;\
}

// normal tests
#define TEST(func) static char * func(){ \
    DEBUG_PRINT("Running Test: %s", #func);
#define LIST static char * all_tests(){ 
#define DONE return 0;}

// crash tests
#define CRASH(func) static char * func(void){ \
    pid_t childPID; \
    int status;\
    childPID = fork(); \
    if(childPID < 0) return "Failed to fork";\
    if(childPID == 0){

#define RECOVER exit(0);}\
    while (wait(&status) != childPID){} \
    if(status > 0) return 0;\
    return "Outside did not CRASH"; \
    }

extern int tests_run;
extern int tests_set;

#endif
