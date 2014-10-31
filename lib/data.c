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
    switch(d.t){

        case INT: 
        case BOOLEAN: d.value = input.i.value; break;

        case CLOSURE:{
            unsigned int id = getAdress(); 
            VALUE * value = MALLOC(sizeof(VALUE)); 
            *value = input;
            insertBinding(&exchange,(char *) id,value,0);
            d.identifier = id;
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
            DEBUG_PRINT("Wrong TAG observed in VALUE conversion");
            exit(3); 
            break;
        }
    }
    d.value = 0;
    return d;
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
    unsigned int id = getAdress(); 
    insertBinding(&exchange,(char *) id,p,0);    
    d.identifier = id;
    return d;
}

