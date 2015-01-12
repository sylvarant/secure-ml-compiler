/*
 * =====================================================================================
 *
 *       Filename:  data.c
 *
 *    Description:  All conversion code goes here
 *
 *         Author:  Ajhl
 *        Company:  Uppsala
 *
 * =====================================================================================
 */

/* 
 * ===  FUNCTION ======================================================================
 *         Name: free_type
 *  Description: save some space on the heap
 * =====================================================================================
 */
LOCAL void free_type(TYPE ty)
{
    switch(ty.t){

        case T(IGNORE):
        case T(INT) :
        case T(ABSTRACT):
        case T(BOOLEAN): break;

        case T(ARROW):
        case T(STAR): {
            free_type(*(ty.a.left));
            free_type(*(ty.a.right));
            FREE(ty.a.left);
            FREE(ty.a.right);
            break;
        }

        case T(VALUE):
        case T(DECLARATION):
        case T(MODULE): {
            free_type(*(ty.m.type));
            FREE(ty.m.type);
            break;
        }

        case T(FUNCTOR): {
            free_type(*(ty.f.left));
            free_type(*(ty.f.right));
            FREE(ty.f.left);
            FREE(ty.f.right);
            break;
        }

        case T(SIGNATURE): {
            struct T(Signature) * it = &ty.ss;
            do {
                free_type(*(it->type));
                FREE(it->type);
                it = it->next;
            }while(it != NULL);
        }
    }
    return;
}

/* 
 * ===  FUNCTION ======================================================================
 *         Name: convertV
 *  Description: convert a miniml value into data for the attacker
 * =====================================================================================
 */
LOCAL DATA convertV(VALUE input,TYPE ty)
{
    DATA d;

    // if it's an abstract type, secure it
    if(ty.t == T(ABSTRACT))
    {
        d.t = ABSTRACT;
        d.identifier = getAdressAbs();
        VALUE * value = MALLOC(sizeof(VALUE)); 
        *value = input;
        insertBigBinding(&abstract_exchange,d.byte,value,ty);
        return d;
    }

    // general conversion
    switch(input.b.t)
    {
        case INT:{
            d.t = INT; 
            d.value = input.i.value;
            break;
        }

        case BOOLEAN:{
            d.t = BOOLEAN;
            d.value = input.b.value; 
            break;
        }

        case CLOSURE:{
            d.identifier = getAdressClo(); 
            VALUE * value = MALLOC(sizeof(VALUE)); 
            *value = input;
            insertBigBinding(&closure_exchange,d.byte,value,ty);
            d.t = CLOSURE;
            break;
        }

        case PAIR:{
            if(ty.t != T(STAR)) mistakeFromOutside();
            DATA left = convertV(*(input.p.left),*(ty.s.left));
            DATA right = convertV(*(input.p.right),*(ty.s.right)); 
            d.left = OUTERM(sizeof(DATA));
            d.right = OUTERM(sizeof(DATA));
            *(d.left) = left;
            *(d.right) = right;
            d.t = PAIR;
            break;
        }

        default:{
            DEBUG_PRINT("Wrong TAG %d observed in VALUE conversion",d.t);
            exit(3); 
            break;
        }
    }
    //free_type(ty); 
    return d;
}


/* 
 * ===  FUNCTION ======================================================================
 *         Name: foreign_lambda
 *  Description: call a foreign function as though it were a closure 
 * =====================================================================================
 */
LOCAL VALUE foreign_lambda(BINDING * fptr,BINDING * type,VALUE v)
{
    callback call = (callback) fptr;    
    TYPE * ty = (TYPE *) type;  
    struct T(Arrow) arrow = ty->a;
    DATA input = convertV(v,*(arrow.left));
    TYPE required = *(arrow.right);
    struct value_type valty = convertD(call(input),required); // going out
    TYPE given = valty.ty;
    unify_types(required,given);
    return valty.val;
}


/* 
 * ===  FUNCTION ======================================================================
 *         Name: convertD
 *  Description: convert a DATA struct of the attacker into a miniml value
 * =====================================================================================
 */
LOCAL struct value_type convertD(DATA input,TYPE req)
{
    struct value_type result;
    switch(input.t)
    {
        case INT:{ 
            result.val = makeInt(input.value); 
            result.ty = T(Int);    
            break;
        }

        case BOOLEAN:{
            result.val = makeBoolean(input.value); 
            result.ty = T(Boolean); 
            break;
        }

        case CLOSURE:{
            META * meta = (META *) getBinding(closure_exchange,input.byte,cmp_int);
            VALUE temp = *((VALUE *) meta->value);
            result.val = temp;
            result.ty = meta->type;
            break;
        }

        case CALLBACK:{
            TYPE * ty = MALLOC(sizeof(TYPE));  
            *ty = req;
            VALUE val = makeClosure((BINDING *)input.call,(BINDING *)ty,foreign_lambda);
            result.val = val;
            result.ty = req; 
            break;
        }

        case ABSTRACT:{
            META * meta = (META *) getBinding(abstract_exchange,input.byte,cmp_int);
            VALUE temp = *((VALUE *) meta->value);
            result.val = temp;
            result.ty = meta->type;
            break;
        }

        case PAIR:{
            if(req.t != T(STAR)) mistakeFromOutside();
            struct value_type l = convertD(*(input.left),*(req.s.left));
            struct value_type r = convertD(*(input.right),*(req.s.right)); 
            result.val = makePair(l.val,r.val);
            result.ty = makeTStar(l.ty,r.ty);
            break;
        }

        default:{
            DEBUG_PRINT("Wrong TAG %d observed in DATA conversion",input.t);
            exit(3); 
            break;
        }
    }
    return result;
}


/* 
 * ===  FUNCTION ======================================================================
 *         Name: outsidestring
 *  Description: pump char's into the attacker memory
 * =====================================================================================
 */
LOCAL char * outsidestring(char * s)
{
    char * input = s;
    int count = 0;
    while(*s++ != '\0'){count++;}
    char * ret = OUTERM(count);
    for(int j = 0; j < count; j++) ret[j] = input[j];
    return ret;
}


/* 
 * ===  FUNCTION ======================================================================
 *         Name: convertM
 *  Description: convert an inside Module into a MODDATA for the outside 
 * =====================================================================================
 */
LOCAL MODDATA convertM(MODULE m,TYPE ty)
{
    MODDATA ret;
    ret.t = m.type; 
    ret.type = convertT(ty);
    struct structure s = m.c.s;
    if(m.type == STRUCTURE){
        int count = 0;
        for(int i = 0; i < s.count; i++){
            if((s.entries[i]).byte != NULL) count++;
        }
        ret.count = count;
        ret.names = OUTERM(sizeof(char*)*count);
        ret.fcalls = OUTERM(sizeof(void*)*count);
        for(int i = 0; i < count; i++){
            ret.names[i] = outsidestring(s.names[i]); 
            //DEBUG_PRINT("name == %s of %s",ret.names[i],s.names[i]);
            ret.fcalls[i] = s.entries[i].byte;
        }
    }
    struct module_type * ptr = MALLOC(sizeof(struct module_type));
    ptr->m = m;
    ptr->ty = ty;
    union safe_cast key; 
    key.value = getAdress();
    ret.identifier = key.value;
    insertBinding(&exchange,key.byte,(void *)ptr);
    return ret;
}


/* 
 * ===  FUNCTION ======================================================================
 *         Name: convertM
 *  Description: convert an inside Module into a MODDATA for the outside 
 * =====================================================================================
 */
LOCAL MODULE updateEntry(MODULE m,BINDING * strls,int stamp,int count,char ** names,ENTRY * ls)
{
    MODULE ret = m;
    ret.stamp = stamp;
    ret.strls = strls;
    for(int i = 0; i < ret.c.s.count ; i++) ret.c.s.entries[i].byte = NULL;
        
    for(int j = 0; j < count; j++){
        for(int i = 0; i < ret.c.s.count ; i++){
            if(cmp_char(names[j],ret.c.s.names[i]) == 0){
                ret.c.s.entries[i] = ls[j];  
                break;
            }
        }
    }
    return ret;
}


/* 
 * ===  FUNCTION ======================================================================
 *         Name: convertMD
 *  Description: convert MODDATA into a Module 
 * =====================================================================================
 */
LOCAL struct module_type * convertMD(MODDATA d)
{
    if(d.identifier > 0){
        union safe_cast key;
        key.value = d.identifier;
        struct module_type * m = getBinding(exchange,key.byte,cmp_int);
        return m;
    }
    
    mistakeFromOutside(); 
    return NULL;
}


/* 
 * ===  FUNCTION ======================================================================
 *         Name: convertT
 *  Description: convert an internal type to an outside type
 * =====================================================================================
 */
LOCAL DTYPE convertT(TYPE ty)
{
    DTYPE typ;
    switch(ty.t)
    {
        case T(ABSTRACT) :{
            typ.t = TYABSTRACT;
            typ.name = outsidestring(ty.aa.name);
            typ.type = NULL;
            break;
        }

        case T(INT) :{
            typ.t = TYINT;
            break;
        }

        case T(BOOLEAN) :{
            typ.t = TYBOOLEAN;
            break;
        }

        case T(ARROW) :{
            typ.t = TYARROW;
            typ.left = OUTERM(sizeof(DTYPE));
            typ.right = OUTERM(sizeof(DTYPE));
            *typ.left = convertT(*(ty.a.left));
            *typ.right = convertT(*(ty.a.right));
            break;
        }

        case T(STAR) :{
            typ.t = TYSTAR;
            typ.left = OUTERM(sizeof(DTYPE));
            typ.right = OUTERM(sizeof(DTYPE));
            *typ.left = convertT(*(ty.s.left));
            *typ.right = convertT(*(ty.s.right));
            break;
        }

        case T(VALUE) :{
            typ.t = TYVALUE; 
            typ.name = outsidestring(ty.v.name);
            typ.type = OUTERM(sizeof(DTYPE));
            *typ.type = convertT(*(ty.v.type));
            break;
        }

        case T(MODULE) :{
            typ.t = TYMODULE; 
            typ.name = outsidestring(ty.m.name);
            typ.type = OUTERM(sizeof(DTYPE));
            *typ.type = convertT(*(ty.v.type));
            break;
        }

        case T(DECLARATION) :{
            typ.t = TYDECLARATION; 
            typ.name = outsidestring(ty.d.name);
            typ.type = OUTERM(sizeof(DTYPE));
            *typ.type = convertT(*(ty.v.type));
            break;
        }

        case T(FUNCTOR) :{
            typ.t = TYDECLARATION; 
            typ.fname = outsidestring(ty.f.name);
            typ.fleft = OUTERM(sizeof(DTYPE));
            typ.fright = OUTERM(sizeof(DTYPE));
            *typ.fleft = convertT(*(ty.f.left));
            *typ.fright = convertT(*(ty.f.right));
            break;
        }

        case T(SIGNATURE) :{
            typ.t = TYSIGNATURE;
            struct T(Signature) * it = &(ty.ss);
            int count = 1;
            while(it->next != NULL){
                count++;
                it = it->next;
            }
            typ.count = count;
            typ.list = OUTERM(sizeof(DTYPE) * count);
            it = &(ty.ss);
            for(int i = 0; i < count; i++){
                typ.list[i] = convertT(*(it->type));
                it = it->next;
            }
            break;
        }

        case T(IGNORE):{
            typ.t = TYABSTRACT;
            typ.name = outsidestring("ignore");
            typ.type = NULL;
            break;
        }
    }
    return typ;
}


/* 
 * ===  FUNCTION ======================================================================
 *         Name: find_type
 *  Description: find a type within a signature (subtyping)
 * =====================================================================================
 */
LOCAL int find_type(TYPE * ty,struct T(Signature) * sig)
{
    if(ty == NULL || sig == NULL) return MFALSE;

    struct T(Signature) * next = sig;
    do{
        if(next->type == NULL) return MFALSE;
        if(type_check(*(next->type),*(ty)) == MTRUE) return MTRUE;
        next = next->next;
    }while(next != NULL);
    return MFALSE;
}


/* 
 * ===  FUNCTION ======================================================================
 *         Name: type_check
 *  Description: type check given versus required type
 * =====================================================================================
 */
LOCAL int type_check(TYPE req,TYPE given)
{
    if (req.t == T(IGNORE)) return MTRUE;

    if (req.t != given.t) return MFALSE;

    switch(req.t)
    {
        case T(INT):
        case T(BOOLEAN): return MTRUE;


        case T(ABSTRACT):{
            if(cmp_char(req.aa.name,given.aa.name) != 0 ) return MFALSE;
            return MTRUE;
        }

        case T(STAR):
        case T(ARROW):{
            if(type_check(*(req.a.left),*(given.a.left))  
                && type_check(*(req.a.right),*(given.a.right))) return MTRUE;
            return MFALSE;
        }

        case T(VALUE):
        case T(MODULE):
        case T(DECLARATION): {
            if(cmp_char(given.d.name,req.d.name) != 0) return MFALSE;
            if(given.d.type != NULL && req.d.type != NULL 
                && type_check(*(req.d.type),*(given.d.type))) return MTRUE;
            else if(given.d.type != req.d.type) return MFALSE;
            return MTRUE;
        }

        // subtyping
        case T(SIGNATURE): {
            struct T(Signature) * next = &req.ss;
            do{
                if(find_type(next->type,&given.ss) == MFALSE) return MFALSE;
                next = next->next;
            }while(next != NULL);
            return MTRUE;
        }

        case T(FUNCTOR):{
            if(cmp_char(given.f.name,req.f.name)) return MFALSE;
            if(given.f.left != NULL & req.f.left != NULL 
                && type_check(*(req.f.left),*(given.f.left)))
            {
                if(given.f.right != NULL & req.f.right != NULL
                    && type_check(*(req.f.right),*(given.f.right))) return MTRUE;
                else if(given.f.right != req.f.right) return MFALSE;
            }
            else if(given.f.left != req.f.left) return MFALSE;
            return MTRUE;
        }

        default :{
            DEBUG_PRINT("Unidentified tag %d",req.t);
            mistakeFromOutside();
            return MFALSE;
        }
    }

}


/* 
 * ===  FUNCTION ======================================================================
 *         Name: unify_types
 *  Description: abort if type unification returns false
 * =====================================================================================
 */
LOCAL void unify_types(TYPE req,TYPE given)
{
    if(type_check(req,given) == MFALSE) mistakeFromOutside(); 
    return;
}


