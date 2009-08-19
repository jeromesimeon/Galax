/***********************************************************************/
/*                                                                     */
/*                                 GALAX                               */
/*                              XQuery Engine                          */
/*                                                                     */
/*  Copyright 2001-2007.                                               */
/*  Distributed only by permission.                                    */
/*                                                                     */
/***********************************************************************/

/* $Id: Document.java,v 1.2 2007/02/01 22:08:51 simeon Exp $ */

/**
 * @(#)Document.java
 *
 */

package galapi;

/**
 * A document element in the XQeury data model.
 *
 * @see Item
 * @see Node
 */

public class Document extends Node
{
    protected Document (int native_item) {
	setItem (native_item);
    }
    
    private ItemList nodes;

    /**
     *
     * @param nds the descendant nodes of the document
     */

    public Document (String uri, ItemList nds) 
	throws GalapiException {

	nodes = nds;

	int i = documentToItem (uri, nds.getNativeItemList ());
	setItem (i);
    }

    
    protected static native int documentToItem (String uri, int nodes);


    /**
     * Releases all resources used by this document.
     *
     * <EM>Caution: If the document was constructed using variables 
     * as arguments, these variables are deallocated after
     * calling this method </EM>
     */
    public void finalize ()  throws Throwable
    {

	nodes.finalize  ();
    }
}// class Document
