/***********************************************************************/
/*                                                                     */
/*                                 GALAX                               */
/*                              XQuery Engine                          */
/*                                                                     */
/*  Copyright 2001-2007.                                               */
/*  Distributed only by permission.                                    */
/*                                                                     */
/***********************************************************************/

/* $Id: xsDouble.java,v 1.3 2007/02/01 22:08:51 simeon Exp $ */

package galapi;

public class xsDouble extends Atomic
{
    public xsDouble (double g)
	throws GalapiException
    {

	setItem (doubleToItem(g));
    }

    public double toDouble ()
	throws GalapiException
    {

	return itemToDouble (getNativeItem());
    }

    protected static native int doubleToItem (double f);
    protected static native double itemToDouble (int it);

}// class xsDouble
