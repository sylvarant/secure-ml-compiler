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
    while(*end != '\0' || *end != '.') end++;

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

    // check that the string is null terminated
    if(path[size] != '\0') mistakeFromOutside();
    BINDING * map = toplevel; 
    char * remainder = path;
    while(remainder){
        char * x_i = nextId(&remainder);
        META * meta =  getBinding(map,x_i);
        if(meta->call)
        {
            if(remainder[0] == '\0'){ 
                VALUE v; 
                v.maxsize =  ((meta->gettr)(map)); 
                return convertV(v);
            }
            else mistakeFromOutside();
        }
        else{
           if(remainder[0] == '\0') return convert(meta->value,MODULE);
           else
           { 
                struct Structure * temp =  meta->value;
                map = temp->mod;
           }
        }
    }
    mistakeFromOutside();
}


