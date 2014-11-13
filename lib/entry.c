/*
 * =====================================================================================
 *
 *       Filename:  entry.c
 *
 *    Description:  The actual entry points. 
 *                  To be included at the end of the generated code
 *
 *         Author:  Ajhl
 *        Company:  Uppsala
 *
 * =====================================================================================
 */


/*-----------------------------------------------------------------------------
 * Adress generators
 *-----------------------------------------------------------------------------*/

LOCAL int getAdress(void)
{
    static  int addr = 0;
    return ++addr;
}

LOCAL int getAdressClo(void)
{
    static  int addr = 0;
    return ++addr;
}


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    nextId
 *  Description:    get the next identifier from a path
 * =====================================================================================
 */
LOCAL char * nextId(char ** str)
{
    char * start = *str;  
    char * end = start;
    while(*end != '\0' && *end != '.') end++;


    int res_size = end - start;
    char * result = malloc((res_size)+1);
    for(int i = 0; i < res_size; i++) result[i] = start[i];
    result[res_size] = '\0';  
    
    if(*end == '.') end++;
    *str = end;

    return result; 
}


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    path_entry
 *  Description:    entry point for paths
 * =====================================================================================
 */
ENTRYPOINT DATA path_entry(char * path, int size)
{
    check_state();
  
    if(path[size] != '\0') mistakeFromOutside(); // buffer check

    // get the correct meta value by browsing through the maps
    BINDING * map = toplevel; 
    char * remainder = path;
    META * meta = NULL;
    while(*remainder != '\0')
    {
        char * x_i = nextId(&remainder);
        meta = (META *) getBinding(map,x_i,cmp_char); 
        if(meta == NULL ||(meta->call && *remainder != '\0')) mistakeFromOutside();
        struct Structure * temp =  meta->value;
        map = temp->mod;
    }

    // return call or structure
    if(meta->call)
    {
        VALUE v = ((meta->gettr)()); 
        return convertV(v,meta->type);
    }
    
    return convert(meta->value,MODULE,meta->type);
}


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    closure_entry
 *  Description:    entry point for closure application
 * =====================================================================================
 */
ENTRYPOINT DATA closure_entry(int id, DATA d)
{
    check_state();  

    // fetch and type check
    struct value_type argument = convertD(d);
    union safe_cast key = {.value = id};

    META * meta = (META*) getBinding(closure_exchange,key.bytes,cmp_int);
    VALUE closure = *((VALUE *) meta->value);
    TYPE required = *(meta->type.a.left);
    TYPE given = argument.ty;
    unify_types(required,given); 
     
    // when typechecks have succeeded apply the argument to the closure
    VALUE result = closure.c.lam(closure.c.env,argument.val);
    return convertV(result,meta->type); 
}

