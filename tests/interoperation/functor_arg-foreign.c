/*
 * =====================================================================================
 *
 *       Filename:  functor_arg-foreign.c
 *
 *
 *         Author:  Adriaan, 
 *        Company:  Uppsala IT
 *
 * =====================================================================================
 */

// TEST in dev

#include "functor_arg.h"
#include "unit.h"

int tests_run = 0;
int tests_set = 9;

DATA goodclosure(DATA val)
{
    DATA result = val;
    result.value += 100;
    return result;
}

DATA badclosure(DATA val)
{
   int result = val.value == 0;
   DATA ret = {.t = BOOLEAN, .value = result};
   return ret;
}

DATA gooddata(){
    DATA arg = {.t = CALLBACK, .call = goodclosure};
    return arg;
}

DATA baddata(){
    DATA arg = {.t = CALLBACK, .call = badclosure};
    return arg;
}

MODDATA myarg; 

TEST(getSetup)
    MODDATA arg = Argument();
    CHECK("Did not fetch the Argument mdoule",arg.t == STRUCTURE);
    CHECK("Does not have enough names",arg.count == 1);
    myarg = arg;
    myarg.identifier = -1;
DONE

TEST(applyf)
    MODDATA functor = IdF();
    MODDATA new = applyFunctor(functor.identifier,myarg);
    CHECK("Did not produce a new module",new.t == STRUCTURE);
    CHECK("New module does not have enough names",new.count == 1);
    CHECK("New module does not contain func",(strcmp(new.names[0],"func") == 0));
    CHECK("Entry Point is off ",new.fcalls[0] == IdF_Functor_func);
DONE


TEST(dynGettr)
    MODDATA functor = IdF();
    MODDATA new = applyFunctor(functor.identifier,myarg);
    func_entry func = IdF_Functor_func; 
    DATA closure = func(new);
    DATA input = {.t = INT , .value = 10};
    DATA result = applyClosure(closure.identifier,input);
    CHECK("Did not return 11",result.value == 11);
DONE

TEST(attFun)
    myarg.fcalls[0] = gooddata;
    MODDATA functor = IdF();
    MODDATA new = applyFunctor(functor.identifier,myarg);
    func_entry func = IdF_Functor_func; 
    DATA closure = func(new);
    DATA input = {.t = INT , .value = 10};
    DATA result = applyClosure(closure.identifier,input);
    CHECK("Did not return 11",result.value == 110);
DONE

CRASH(evilFun)
    myarg.fcalls[0] = baddata;
    MODDATA functor = IdF();
    MODDATA new = applyFunctor(functor.identifier,myarg);
    func_entry func = IdF_Functor_func; 
    DATA closure = func(new);
    DATA input = {.t = INT , .value = 10};
    DATA result = applyClosure(closure.identifier,input);
RECOVER

MODDATA module(void)
{
    MODDATA m;
    return m;
}

CRASH(modinjection)
    myarg.accs[0] = MOD; 
    myarg.fcalls[0] = module;
    MODDATA functor = IdF();
    MODDATA new = applyFunctor(functor.identifier,myarg);
    func_entry func = IdF_Functor_func; 
    DATA closure = func(new);
RECOVER

/*----------------------------
 * inner module
 *---------------------------*/
DTYPE ignored;
char * innames [] =  {"test"};
CALLTAG inacc[] = {VAL};
void * ptrs[] = {gooddata};
MODDATA inner = {.t = STRUCTURE, .identifier = -1,.count = 1,.names = innames,.accs=inacc,.fcalls=ptrs};

MODDATA inner_mod(void)
{
   return inner; 
}

TEST(deepModule)
    MODDATA mod = myarg;
    mod.fcalls[0] = (void *) inner_mod;
    mod.accs[0] = MOD;
    mod.names[0] = "Inner";
    MODDATA functor = IdFDeep(); 
    MODDATA new = applyFunctor(functor.identifier,mod);
    MODDATA obj = IdFDeep_Functor_Inner(new);
    CHECK("Resulting object does not have correct entry point",obj.fcalls[0] == IdFDeep_Functor_Inner_test);
    DATA closure = IdFDeep_Functor_Inner_test(obj); 
    CHECK("Value does not produce closure",closure.t == CLOSURE);
DONE

/*----------------------------
 * foreign identity functor
 *---------------------------*/
MODDATA att_functor(MODDATA m)
{
   return m; 
}

TEST(foreignFunc)
    MODDATA farg = {.t = FUNCTOR, .identifier = -1, .fctr = att_functor};
    MODDATA funct = SimpleAppF();
    MODDATA str = applyFunctor(funct.identifier,farg);
    CHECK("Result does not contain the right entry points",str.fcalls[0] == SimpleAppF_Functor_func);
    func_entry call = SimpleAppF_Functor_func; 
    DATA closure = call(str); 
    CHECK("Did not recieve a closure",closure.t == CLOSURE);
DONE


/*----------------------------
 * foreign alternate functor
 *---------------------------*/
MODDATA alt_functor(MODDATA m)
{
   return inner; 
}

CRASH(functorinjection)
    MODDATA farg = {.t = FUNCTOR, .identifier = -1, .fctr = alt_functor};
    MODDATA funct = SimpleAppF();
    MODDATA str = applyFunctor(funct.identifier,farg);
RECOVER

// the tests
LIST
    RUN(getSetup);
    RUN(applyf);
    RUN(dynGettr);
    RUN(attFun);
    RUN(evilFun);
    RUN(modinjection);
    RUN(deepModule);
    RUN(foreignFunc);
    RUN(functorinjection);
DONE

INCLUDE_MAIN

