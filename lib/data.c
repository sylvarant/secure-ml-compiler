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
LOCAL DATA convertV(VALUE input)
{
    DATA d;
    d.t = input.b.t;
    switch(d.t)
    {
        case INT: 
        case BOOLEAN: d.value = input.i.value; break;

        case CLOSURE:{
            d.identifier = getAdress(); 
            VALUE * value = MALLOC(sizeof(VALUE)); 
            *value = input;
            insertBinding(&exchange,d.bytes,value,0);
            break;
        }

        case PAIR:{
            DATA left = convertV(*(input.p.left));
            DATA right = convertV(*(input.p.right));
            d.left = OUTERM(sizeof(DATA));
            d.right = OUTERM(sizeof(DATA));
            *(d.left) = left;
            *(d.right) = right;
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
LOCAL VALUE convertD(DATA input)
{
    VALUE v;
    switch(input.t)
    {
        case INT: makeInt(input.value); break;

        case BOOLEAN: makeBoolean(input.value); break;

        case CLOSURE: {
            META * meta = getBinding(exchange,(char *) input.bytes);
            if(meta->call) mistakeFromOutside();
            VALUE temp = *((VALUE *) meta->value);
            if(temp.c.t != CLOSURE) mistakeFromOutside();
            v = temp;
        }

        case PAIR: makePair(convertD(*(input.left)),convertD(*(input.right))); break;

        default:{
            DEBUG_PRINT("Wrong TAG %d observed in VALUE conversion",input.t);
            exit(3); 
            break;
        }
    }
    return v;
}


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    convert
 *  Description:    convert a pointer to an identifier for the outside world
 * =====================================================================================
 */
LOCAL DATA convert(void * p, TAG t)
{
    DATA d;
    d.t = t;
    d.identifier = getAdress();
    insertBinding(&exchange,(char *) d.bytes,p,0);    
    return d;
}

