/***********************************************************************/
/*                                                                     */
/*                                 GALAX                               */
/*                              XQuery Engine                          */
/*                                                                     */
/*  Copyright 2001-2007.                                               */
/*  Distributed only by permission.                                    */
/*                                                                     */
/***********************************************************************/

/* $Id: AtomicList.java,v 1.3 2007/02/01 22:08:51 simeon Exp $ */

/**
 * @(#)AtomicList.java
 *
 */


package galapi;

/**
 * An <code>AtomicList</code> incapsulates a list of 
 * {@link galapi.Atomic Atomic} values.
 *
 */

public class AtomicList extends ItemList
{
    public AtomicList ()
	throws GalapiException
    { super(); }

    protected AtomicList (int il)
	throws GalapiException
    {
	setItemList (il);
    }

}// class AtomicList

