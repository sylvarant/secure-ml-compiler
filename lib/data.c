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
 * ===  FUNCTION  ======================================================================
 *         Name:    convertV
 *  Description:    convert a miniml value into data for the attacker
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
        insertBigBinding(&abstract_exchange,d.bytes,value,0,ty);
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
            insertBigBinding(&closure_exchange,d.bytes,value,0,ty);
            d.t = CLOSURE;
            break;
        }

        case PAIR:{
            DATA left = convertV(*(input.p.left),ty);
            DATA right = convertV(*(input.p.right),ty); // TODO left and right if not ignore 
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
    return d;
}


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    convertD
 *  Description:    convert a DATA struct of the attacker into a miniml value
 * =====================================================================================
 */
LOCAL struct value_type convertD(DATA input)
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
            META * meta = (META *) getBinding(closure_exchange,input.bytes,cmp_int);
            VALUE temp = *((VALUE *) meta->value);
            result.val = temp;
            result.ty = meta->type;
            break;
        }

        case ABSTRACT:{
            META * meta = (META *) getBinding(abstract_exchange,input.bytes,cmp_int);
            VALUE temp = *((VALUE *) meta->value);
            result.val = temp;
            result.ty = meta->type;
            break;
        }

        case PAIR:{
            struct value_type l = convertD(*(input.left));
            struct value_type r = convertD(*(input.right)); 
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
 * ===  FUNCTION  ======================================================================
 *         Name:    convertT
 *  Description:    convert an internal type to an outside type
 * =====================================================================================
 */
LOCAL DTYPE convertT(TYPE ty)
{
    DTYPE typ;
    switch(ty.t)
    {
        default:{
            typ.t = TYABSTRACT;
            typ.abname = "test";
            typ.type = NULL;
            break;
        }
    }
    return typ;
}


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    unify_types
 *  Description:    type unification, C-style
 * =====================================================================================
 */
LOCAL void unify_types(TYPE req,TYPE given)
{
    if (req.t == T(IGNORE)) return;

    if (req.t != given.t) mistakeFromOutside();

    switch(req.t)
    {
        case T(INT):
        case T(BOOLEAN): break;


        case T(ABSTRACT):{
            if(cmp_char(req.aa.name,given.aa.name) != 0 ) mistakeFromOutside();
            break;
        }

        case T(STAR):
        case T(ARROW):{
            unify_types(*(req.a.left),*(given.a.left));  // CAREFULL: relies on static structure of the struct
            unify_types(*(req.a.right),*(given.a.right)); 
            break;
        }

        default :{
            DEBUG_PRINT("Unidentified tag %d",req.t);
            mistakeFromOutside();
            break;
        }
    }
}


