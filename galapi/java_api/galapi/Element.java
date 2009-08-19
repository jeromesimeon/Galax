/***********************************************************************/
/*                                                                     */
/*                                 GALAX                               */
/*                              XQuery Engine                          */
/*                                                                     */
/*  Copyright 2001-2007.                                               */
/*  Distributed only by permission.                                    */
/*                                                                     */
/***********************************************************************/

/* $Id: Element.java,v 1.5 2007/02/01 22:08:51 simeon Exp $ */

/**
 * @(#)Element.java
 *
 */

package galapi;

/**
 * A node element in the XQeury data model.
 *
 * @see Item
 * @see Node
 */

public class Element extends Node
{
    protected Element (int native_item) {
	setItem (native_item);
    }
    
    private xsQName q;
    private xsQName q1;
    private AttributeList attributes;
    private ItemList nodes;

    /**
     *
     * @param qName the name of the element
     * @param attrs the attributes of the element 
     * @param nds the descendant nodes of the element
     * @param qName1 the name of the type of the element
     *
     */

    public Element (xsQName qName, AttributeList attrs, 
		    ItemList nds, xsQName qName1) 
	throws GalapiException {

	q = qName;
	attributes = attrs;
	nodes = nds;
	q1 = qName1;
	// int i = -1;

	int i = elementToItem(qName.getNativeItem (),
			      attrs.getNativeItemList (),
			      nds.getNativeItemList (),
			      qName1.getNativeItem () );
	setItem (i);
    }

    
    protected static native int elementToItem(int q, int attrs,
					      int nodes, int q1);


    /**
     * Releases all resources used by this element (attributes, nodes etc)
     *
     * <EM>Caution: If the element was constructed using variables 
     * as arguments, these variables are deallocated after
     * calling this method </EM>
     */
    public void finalize ()  throws Throwable
    {

	q1.finalize ();
	nodes.finalize  ();
	attributes.finalize ();
	q.finalize ();	
    }
}// class Element
