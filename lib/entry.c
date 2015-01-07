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
    static int addr = 0;
    return ++addr;
}

LOCAL int getAdressClo(void)
{
    static int addr = 0;
    return ++addr;
}

LOCAL int getAdressAbs(void)
{
    static int addr = 0;
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
    char * result = MALLOC((res_size)+1);
    for(int i = 0; i < res_size; i++) result[i] = start[i];
    result[res_size] = '\0';  
    
    if(*end == '.') end++;
    *str = end;

    return result; 
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:  get_module
 *  Description:  return a module member 
 * =====================================================================================
 */
MODULE get_module(MODULE m,char * str)
{
    if(m.type != STRUCTURE) mistakeFromOutside();  

    for(int i = 0; i < m.c.s.count; i++)
    {
        if(cmp_char(m.c.s.names[i],str) == 0)
        {
            switch(m.c.s.accs[i])
            {
                case BMOD :{ 
                    return *((m.c.s.fields[i]).module);
                }
                default : mistakeFromOutside();
            }
        }
    }
    mistakeFromOutside();
    return emptyModule(); // for gcc warning purposes
}


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:  get_value
 *  Description:  return a value member 
 * =====================================================================================
 */
VALUE get_value(MODULE top,MODULE m,char * str)
{
    if(m.type != STRUCTURE) mistakeFromOutside();  

    for(int i = 0; i < m.c.s.count; i++)
    {
        if(cmp_char(m.c.s.names[i],str) == 0)
        {
            switch(m.c.s.accs[i])
            {
                case BVAL :{ 
                    return ((m.c.s.fields[i]).gettr(top.strls));
                }
                default : mistakeFromOutside();
            }
        }
    }
    mistakeFromOutside();
    return makeBoolean(0); // for gcc warning purposes
}


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:  path_module
 *  Description:  call up a module
 * =====================================================================================
 */
LOCAL MODULE path_module(MODULE s,char * path,int size)
{
    if(path[size] != '\0') mistakeFromOutside(); // buffer check
    return emptyModule();
}


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:  path_value
 *  Description:  call up a value
 * =====================================================================================
 */
LOCAL VALUE path_value(BINDING * binding,char * path,int size)
{
    if(path[size] != '\0') mistakeFromOutside(); // buffer check
    return makeBoolean(0); 
}


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    closure_entry
 *  Description:    entry point for closure application
 * =====================================================================================
 */
ENTRYPOINT DATA closure_entry(int id, DATA d)
{
    // fetch and type check
    struct value_type argument = convertD(d);
    union safe_cast key = {.value = id};

    META * meta = (META*) getBinding(closure_exchange,key.bytes,cmp_int);
    VALUE closure = *((VALUE *) meta->value);
    TYPE required = *(meta->type.a.left);
    TYPE given = argument.ty;
    unify_types(required,given); 
     
    // when typechecks have succeeded apply the argument to the closure
    VALUE result = closure.c.lam(closure.c.mod,closure.c.env,argument.val);
    return convertV(result,*(meta->type.a.right)); 

}


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    functor_entry
 *  Description:    entry point for functor application
 * =====================================================================================
 */
ENTRYPOINT MODDATA functor_entry(int id,MODDATA d){
  MODDATA ret;
  union safe_cast key = {.value = id};
  struct module_type * mt = getBinding(exchange,key.bytes,cmp_int);

  return ret;
}

