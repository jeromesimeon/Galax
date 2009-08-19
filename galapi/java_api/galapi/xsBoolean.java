/***********************************************************************/
/*                                                                     */
/*                                 GALAX                               */
/*                              XQuery Engine                          */
/*                                                                     */
/*  Copyright 2001-2007.                                               */
/*  Distributed only by permission.                                    */
/*                                                                     */
/***********************************************************************/

/* $Id: xsBoolean.java,v 1.3 2007/02/01 22:08:51 simeon Exp $ */

package galapi;

public class xsBoolean extends Atomic
{
    public xsBoolean (boolean b)
	throws GalapiException
    {

	setItem (booleanToItem(b));	
    }

    public boolean toBoolean ()
	throws GalapiException
    {

	return itemToBoolean (getNativeItem());
    }

    protected static native int booleanToItem (boolean b);
    protected static native boolean itemToBoolean (int i);

}// class xsBoolean
