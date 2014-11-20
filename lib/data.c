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
            d.identifier = getAdress(); 
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
            result.ty = makeTInt();    
            break;
        }

        case BOOLEAN:{
            result.val = makeBoolean(input.value); 
            result.ty = makeTBoolean(); 
            break;
        }

        case CLOSURE:{
            META * meta = (META *) getBinding(closure_exchange,input.bytes,cmp_int);
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
 *         Name:    convert
 *  Description:    convert a pointer to an identifier for the outside world
 * =====================================================================================
 */
LOCAL DATA convert(void * p, TAG t,TYPE ty)
{
    DATA d;
    d.t = t;
    d.identifier = getAdress();
    insertBigBinding(&exchange,(char *)d.bytes,p,0,ty);    
    return d;
}


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    unify_types
 *  Description:    type unification, C-style
 * =====================================================================================
 */
LOCAL void unify_types(TYPE t1,TYPE t2)
{
    switch(t1.t)
    {
        case T(INT):
        case T(BOOLEAN):{
            if(t2.t != t1.t) mistakeFromOutside();
            break;
        }

        case T(IGNORE): break;

        case T(STAR):
        case T(ARROW):{
            if(t1.t != t2.t) mistakeFromOutside();
            unify_types(*(t1.a.left),*(t2.a.left));  // CAREFULL: relies on static structure of the struct
            unify_types(*(t1.a.right),*(t2.a.right)); 
            break;
        }

        default :{
            DEBUG_PRINT("Unidentified tag %d",t1.t);
            mistakeFromOutside();
            break;
        }
    }
}


