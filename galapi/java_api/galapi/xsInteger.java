/***********************************************************************/
/*                                                                     */
/*                                 GALAX                               */
/*                              XQuery Engine                          */
/*                                                                     */
/*  Copyright 2001-2007.                                               */
/*  Distributed only by permission.                                    */
/*                                                                     */
/***********************************************************************/

/* $Id: xsInteger.java,v 1.3 2007/02/01 22:08:51 simeon Exp $ */

package galapi;

public class xsInteger extends Atomic
{
    public xsInteger (int d)
	throws GalapiException
    {

	setItem (integerToItem(d));
    }

    public int toInt ()
	throws GalapiException
    {

	return itemToInteger (getNativeItem());
    }


    protected static native int integerToItem (int i);
    protected static native int itemToInteger (int it);

}// class xsInteger

