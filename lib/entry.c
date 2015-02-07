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

FUNCTIONALITY int getObjId(void)
{
    static int addr = 0;
    return ++addr;
}

LOCAL int getAdressLoc(void)
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

                case BDMOD :{
                    struct foreign_s fs = *((m.c.s.fields[i]).foreign);
                    return foreign_module(fs.me,fs.req);
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
VALUE get_value(MODULE m,char * str)
{
    if(m.type != STRUCTURE) mistakeFromOutside();  

    for(int i = 0; i < m.c.s.count; i++)
    {
        if(cmp_char(m.c.s.names[i],str) == 0)
        {
            switch(m.c.s.accs[i])
            {
                case BVAL :{ 
                    return ((m.c.s.fields[i]).gettr(m.strls));
                }

                case BDVAL :{
                    struct foreign_s fs = *((m.c.s.fields[i]).foreign);
                    return foreign_value(fs.fe,fs.req);
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
 *         Name:  get_module_path
 *  Description:  return a module member pointed to by a path
 * =====================================================================================
 */
LOCAL MODULE get_module_path(MODULE m,char * path)
{
    char * remainder = path;
    MODULE mod = m;
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
 *         Name:  get_value_path
 *  Description:  return a value member pointed to by a path
 * =====================================================================================
 */
LOCAL VALUE get_value_path(MODULE m,char * path)
{
    char * remainder = path;
    MODULE mod = m;
    if(*remainder == '\0') mistakeFromOutside();
    do
    {
        char * it = nextId(&remainder);
        if(*remainder == '\0')
        {
            VALUE v = get_value(mod,it); 
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
 *         Name:  path_module
 *  Description:  call up a module
 * =====================================================================================
 */
LOCAL MODULE path_module(BINDING * binding,char * path)
{
    char * remainder = path;
    char * it = nextId(&remainder);
    MODULE * m = getBinding(binding,it,cmp_char); 
    FREE(it);
    return get_module_path(*m,remainder); 
}


/* 
 * ===  FUNCTION ======================================================================
 *         Name:  path_value
 *  Description:  call up a value
 * =====================================================================================
 */
LOCAL VALUE path_value(BINDING * binding,char * path)
{
    char * remainder = path;
    char * it = nextId(&remainder);
    MODULE * m = getBinding(binding,it,cmp_char); 
    FREE(it);
    return get_value_path(*m,remainder);
}


/* 
 * ===  FUNCTION ======================================================================
 *         Name: closureEntry
 *  Description: entry point for closure application
 * =====================================================================================
 */
ENTRYPOINT DATA closureEntry(int id, DATA d)
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
 *         Name: locationEntry
 *  Description: entry point for fetching location values
 * =====================================================================================
 */
ENTRYPOINT DATA locationEntry(int id)
{
    union safe_cast key = {.value = id};
    META * meta = (META*) getBinding(location_exchange,key.byte,cmp_int);
    VALUE location = *((VALUE *) meta->value);
    TYPE type = *((meta->type).r.type);
    return convertV(*(location.l.content),type);
}


/* 
 * ===  FUNCTION ======================================================================
 *         Name: functorEntry
 *  Description: entry point for functor application
 * =====================================================================================
 */
ENTRYPOINT MODDATA functorEntry(int id,MODDATA d)
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

