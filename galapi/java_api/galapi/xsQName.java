/***********************************************************************/
/*                                                                     */
/*                                 GALAX                               */
/*                              XQuery Engine                          */
/*                                                                     */
/*  Copyright 2001-2007.                                               */
/*  Distributed only by permission.                                    */
/*                                                                     */
/***********************************************************************/

/* $Id: xsQName.java,v 1.4 2007/02/01 22:08:51 simeon Exp $ */

package galapi;

public class xsQName extends Atomic
{
    public xsQName (NamespaceEnv nsenv, String qName)
	throws GalapiException
    {
	int item;
	item = qNameToItem(nsenv.getNativeItem(), qName);
	setItem (item);
    }

    protected static native int qNameToItem (int i, String s);

}// class xsQName

