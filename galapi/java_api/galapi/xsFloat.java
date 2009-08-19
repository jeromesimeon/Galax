/***********************************************************************/
/*                                                                     */
/*                                 GALAX                               */
/*                              XQuery Engine                          */
/*                                                                     */
/*  Copyright 2001-2007.                                               */
/*  Distributed only by permission.                                    */
/*                                                                     */
/***********************************************************************/

/* $Id: xsFloat.java,v 1.3 2007/02/01 22:08:51 simeon Exp $ */

package galapi;

public class xsFloat extends Atomic
{
    public xsFloat (double f)
	throws GalapiException
    {

	setItem (floatToItem(f));
    }

    public double toDouble ()
	throws GalapiException
    {

	return itemToFloat (getNativeItem());
    }

    protected static native int floatToItem (double f);
    protected static native double itemToFloat (int it);

}// class xsFloat
