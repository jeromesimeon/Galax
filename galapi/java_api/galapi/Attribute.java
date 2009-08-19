/***********************************************************************/
/*                                                                     */
/*                                 GALAX                               */
/*                              XQuery Engine                          */
/*                                                                     */
/*  Copyright 2001-2007.                                               */
/*  Distributed only by permission.                                    */
/*                                                                     */
/***********************************************************************/

/* $Id: Attribute.java,v 1.4 2007/02/01 22:08:51 simeon Exp $ */

/**
 * @(#)Attribute.java
 *
 */

package galapi;

/**
 * A node attribute in the XQeury data model.
 *
 * @see Item
 * @see Node
 */

public class Attribute extends Node
{
    private xsQName q;
    private xsString xs;
    private xsQName q1;

    /**
     * @param qName the name of the attribute
     * @param s the content of the attribute
     * @param qName1 the name of the type of the attribute
     *
     */
    public Attribute (xsQName qName, xsString s, xsQName qName1)
	throws GalapiException
    {

	int i = -1;
	q = qName;
	xs = s;
	q1 = qName1;
	

	i = attributeToItem (qName.getNativeItem (),
			     s.getNativeItem (),
			     qName1.getNativeItem () );
	setItem (i);
    }


    protected void finalize ()  throws Throwable
    {
	q1.finalize ();
	xs.finalize ();	
	q.finalize  ();
    }


    protected Attribute (int native_item) {
	setItem (native_item);
    }

    
    protected static native int attributeToItem (int q, int s, int q1);


}// class Attribute
