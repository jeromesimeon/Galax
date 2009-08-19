/***********************************************************************/
/*                                                                     */
/*                                 GALAX                               */
/*                              XQuery Engine                          */
/*                                                                     */
/*  Copyright 2001-2007.                                               */
/*  Distributed only by permission.                                    */
/*                                                                     */
/***********************************************************************/

/* $Id: xsInt.java,v 1.3 2007/02/01 22:08:51 simeon Exp $ */

package galapi;

public class xsInt extends Atomic
{

    public xsInt (int d)
	throws GalapiException
    {

	setItem (intToItem(d));
    }

    public int toInt ()
	throws GalapiException
    {
	return itemToInt (getNativeItem());
    }


    protected static native int intToItem (int i);
    protected static native int itemToInt (int it);

}// class xsInt
