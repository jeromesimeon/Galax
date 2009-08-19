/***********************************************************************/
/*                                                                     */
/*                                 GALAX                               */
/*                              XQuery Engine                          */
/*                                                                     */
/*  Copyright 2001-2007.                                               */
/*  Distributed only by permission.                                    */
/*                                                                     */
/***********************************************************************/

/* $Id: xsDecimal.java,v 1.3 2007/02/01 22:08:51 simeon Exp $ */

package galapi;

public class xsDecimal extends Atomic
{
    public xsDecimal (int d)
	throws GalapiException
    {

	setItem (decimalToItem(d));
    }

    public int toInt ()
	throws GalapiException
    {

	return itemToDecimal (getNativeItem());
    }


    protected static native int decimalToItem (int i);
    protected static native int itemToDecimal (int it);


}// class xsDecimal
