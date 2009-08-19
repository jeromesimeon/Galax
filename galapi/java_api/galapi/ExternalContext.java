/***********************************************************************/
/*                                                                     */
/*                                 GALAX                               */
/*                              XQuery Engine                          */
/*                                                                     */
/*  Copyright 2001-2007.                                               */
/*  Distributed only by permission.                                    */
/*                                                                     */
/***********************************************************************/

/* $Id: ExternalContext.java,v 1.7 2007/02/01 22:08:51 simeon Exp $ */

/**
 * @(#)ExternalContext.java
 *
 * 
 */

package galapi;

/**
 * The <code>ExternalContext</code> includes an optional value for the context
 * item (known as ".") and a list of variable name, <code>ItemList</code> value
 * pairs.
 * 
 */

public class ExternalContext extends Context
{
    
    /**
     * @param ctxt_item is the context item (.).
     * @param tz_item is the local time zone 
     * @param vars are bindings from variable names to their value
     *
     */
    public ExternalContext (ProcessingContext pc, ItemList ctxt_item, ItemList tz_item, NameValuePair vars[])
	throws GalapiException
    {

	String varnames[] = new String[vars.length];
	int varvals[] = new int[vars.length];
	for (int i=0; i<vars.length; i++) {
	    varnames[i] = vars[i].getName();
	    varvals[i] = vars[i].getValue().getNativeItemList();
	}

	setItem (externalContextToItem (pc.getNativeItem(), 
					ctxt_item.getNativeItemList(),
					tz_item.getNativeItemList(), 
					varnames, varvals));
    }

    public ExternalContext ()
	throws GalapiException
    {
	setItem (defaultExternalContext ());
    }

    protected static native int externalContextToItem (int pc,
						       int ci, 
						       int tz,
						       String vars[],
						       int values[]);
    protected static native int defaultExternalContext ();

}// class ExternalContext
