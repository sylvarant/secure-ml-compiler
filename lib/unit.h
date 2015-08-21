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
#include <sys/time.h>
#if defined(__linux__)
  #include <sys/wait.h>
#endif
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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
 *  Secure vs Insecure
 *-----------------------------------------------------------------------------*/
#ifdef INSECURE
    #define BASIC VALUE
    #define STRCT MODULE
    #define APPLYCL(x,y)  x.c.lam(x.c.mod,x.c.env,y) 
    #define CALL(x) _ ## x(NULL) 
    #define CALLM(x) str_ ## x
    #define NOTE "Ran: %d/%d Insecure Tests\n"
#else

    #define BASIC DATA
    #define STRCT MODDATA
    #define APPLYCL(x,y) applyClosure(x.identifier,y)
    #define CALL(x) x()
    #define CALLM(x) CALL(x)
    #define NOTE "Ran: %d/%d Tests\n"

#endif



/*-----------------------------------------------------------------------------
 *  UNIT 
 *-----------------------------------------------------------------------------*/

#define CHECK(message, test) do { if (!(test)) return message; } while (0)
#define RUN(test) do { char *message = test(); tests_run++; \
                                if (message) return message; } while (0)

#define INCLUDE_MAIN int main(void){\
    load();\
    char *result = all_tests();\
    if (result != 0) printf("%s\n", result);\
    printf(NOTE, tests_run,tests_set);\
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

// time tests
#define START struct timeval  tv1, tv2;\
    gettimeofday(&tv1, NULL);

#define STOP(x) gettimeofday(&tv2, NULL);\
    fprintf(x,"Total time = %f seconds\n", \
         (double) (tv2.tv_usec - tv1.tv_usec) / 1000000 + \
         (double) (tv2.tv_sec - tv1.tv_sec));

// global variables
extern int tests_run;
extern int tests_set;


/*-----------------------------------------------------------------------------
 *  HELPERS
 *-----------------------------------------------------------------------------*/

#define PATH(s) (#s),strlen(#s)

#endif
