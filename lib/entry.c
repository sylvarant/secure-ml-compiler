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
 * ===  FUNCTION ======================================================================
 *         Name: nextId
 *  Description: get the next identifier from a path
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
 * ===  FUNCTION ======================================================================
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
    MODULE empty;
    mistakeFromOutside();
    return empty; // for gcc warning purposes
}


/* 
 * ===  FUNCTION ======================================================================
 *         Name:  get_value
 *  Description:  return a value member 
 * =====================================================================================
 */
VALUE get_value(BINDING * top,MODULE m,char * str)
{
    if(m.type != STRUCTURE) mistakeFromOutside();  

    for(int i = 0; i < m.c.s.count; i++)
    {
        if(cmp_char(m.c.s.names[i],str) == 0)
        {
            switch(m.c.s.accs[i])
            {
                case BVAL :{ 
                    return ((m.c.s.fields[i]).gettr(top));
                }
                default : mistakeFromOutside();
            }
        }
    }
    mistakeFromOutside();
    return makeBoolean(0); // for gcc warning purposes
}


/* 
 * ===  FUNCTION ======================================================================
 *         Name:  path_module
 *  Description:  call up a module
 * =====================================================================================
 */
LOCAL MODULE path_module(BINDING * binding,char * path,int size)
{
    if(path[size] != '\0') mistakeFromOutside(); // buffer check

    char * remainder = path;
    char * it = nextId(&remainder);
    struct module_type * m = getBinding(binding,it,cmp_char); 
    FREE(it);
    
    MODULE mod = m->m;
    while(*remainder != '\0')
    {
        char * it = nextId(&remainder);
        mod = get_module(mod,it); 
        FREE(it);
    }

    return mod; 
}


/* 
 * ===  FUNCTION ======================================================================
 *         Name:  path_value
 *  Description:  call up a value
 * =====================================================================================
 */
LOCAL VALUE path_value(BINDING * binding,char * path,int size)
{
    if(path[size] != '\0') mistakeFromOutside(); // buffer check

    char * remainder = path;
    char * it = nextId(&remainder);
    struct module_type * m = getBinding(binding,it,cmp_char); 
    FREE(it);
    
    if(*remainder == '\0') mistakeFromOutside();

    MODULE mod = m->m;
    do
    {
        it = nextId(&remainder);
        if(*remainder == '\0')
        {
            VALUE v = get_value(binding,mod,it); 
            FREE(it);
            return v; 
        }
        mod = get_module(mod,it); 
        FREE(it);
    }while(1);

    return makeBoolean(0); // gcc is whiny
}


/* 
 * ===  FUNCTION ======================================================================
 *         Name: closure_entry
 *  Description: entry point for closure application
 * =====================================================================================
 */
ENTRYPOINT DATA closure_entry(int id, DATA d)
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
 *         Name: functor_entry
 *  Description: entry point for functor application
 * =====================================================================================
 */
ENTRYPOINT MODDATA functor_entry(int id,MODDATA d)
{
    // fetch and type check
    union safe_cast key = {.value = id};
    struct module_type * mt = getBinding(exchange,key.byte,cmp_int);
    MODULE functor = mt->m;
    TYPE functortype = mt->ty;
    TYPE required = *(functortype.f.left);

    struct module_type * amt = convertMD(d);
    MODULE arg = amt->m;
    TYPE given = amt->ty;
    unify_types(required,given);
    // if everything is fine update the functor with the argument and apply
    //DEBUG_PRINT("FUNCTOR APPL");
    insertBinding(&functor.strls,functor.c.f.var,amt);
    MODULE new = functor.c.f.Functor(functor.strls,arg); 
    MODDATA ret = convertM(new,*(functortype.f.right)); // TODO update right
    return ret;
}

