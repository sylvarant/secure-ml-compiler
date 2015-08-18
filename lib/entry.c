/*
 * =====================================================================================
 *
 *       Filename:  entry.c
 *
 *    Description:  The actual entry points. 
 *                  To be included at the end of the generated code
 *
 *         Author:  
 *        Company:  Uppsala IT
 *
 * =====================================================================================
 */

/* 
 * ===  FUNCTION ======================================================================
 *         Name: applyClosure
 *  Description: entry point for closure application
 * =====================================================================================
 */
ENTRYPOINT DATA applyClosure(int id, DATA d)
{
    // fetch and type check
    union safe_cast key = {.value = id};
    META * meta = (META*) getBinding(closure_exchange,key.byte,cmp_int);
    VALUE closure = *((VALUE *) meta->value);
    TYPE required = *(meta->type.a.left);

    // the argument
    struct value_type argument = convertD(d,required);
    TYPE given = argument.ty;
    unify_types(required,given); 

    // when typechecks have succeeded apply the argument to the closure
    VALUE result = closure.c.lam(closure.c.mod,closure.c.env,argument.val);
    return convertV(result,*(meta->type.a.right)); 
}


/* 
 * ===  FUNCTION ======================================================================
 *         Name: readLocation
 *  Description: entry point for fetching location values
 * =====================================================================================
 */
ENTRYPOINT DATA readLocation(int id)
{
    union safe_cast key = {.value = id};
    META * meta = (META*) getBinding(location_exchange,key.byte,cmp_int);
    VALUE location = *((VALUE *) meta->value);
    TYPE type = *((meta->type).r.type);
    return convertV(*(location.l.content),type);
}


/* 
 * ===  FUNCTION ======================================================================
 *         Name: setLocation
 *  Description: entry point for setting a value to a shared location
 * =====================================================================================
 */
ENTRYPOINT void setLocation(int id,DATA d)
{

    union safe_cast key = {.value = id};
    META * meta = (META*) getBinding(location_exchange,key.byte,cmp_int);
    VALUE location = *((VALUE *) meta->value);
    TYPE required = *((*(meta->type.a.left)).r.type);

    // the argument
    struct value_type argument = convertD(d,required);
    TYPE given = argument.ty;
    unify_types(required,given); 

    // assign
    *(location.l.content) = argument.val;
}


/* 
 * ===  FUNCTION ======================================================================
 *         Name: applyFunctor
 *  Description: entry point for functor application
 * =====================================================================================
 */
ENTRYPOINT MODDATA applyFunctor(int id,MODDATA d)
{
    // fetch and type check
    union safe_cast key = {.value = id};
    struct module_type * mt = getBinding(exchange,key.byte,cmp_int);
    MODULE functor = mt->m;
    TYPE functortype = mt->ty;
    TYPE required = *(functortype.f.left);

    struct module_type amt = convertMD(d,required);
    MODULE arg = amt.m;
    TYPE given = amt.ty;
    unify_types(required,given);

    // if everything is fine update the functor with the argument and apply
    //DEBUG_PRINT("FUNCTOR APPL");
    MODULE new = functor.c.f.Functor(functor.strls,arg); 

    // update the generate module
    MODULE updated = updateEntry(new,NULL,functor.c.f.count,functor.c.f.names,functor.c.f.entries);
    MODULE nks = updateKeys(updated,functor.keys);
    MODDATA ret = convertM(nks,*(functortype.f.right)); 
    return ret;
}

