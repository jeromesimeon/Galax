/***********************************************************************/
/*                                                                     */
/*                                 GALAX                               */
/*                              XQuery Engine                          */
/*                                                                     */
/*  Copyright 2001-2007.                                               */
/*  Distributed only by permission.                                    */
/*                                                                     */
/***********************************************************************/

/* $Id: Node.java,v 1.3 2007/02/01 22:08:51 simeon Exp $ */

/**
 * @(#)Node.java
 *
 */

package galapi;

/**
 * A <code>Node</code> is an <code>Item</code> representing an XML node in 
 * the XQuery data model.
 * A <code>Node</code> has a string value of type <code>xsString<code> and
 * a typed value which can be retrieved by calling the method
 * {@link #typedValue typedValue}. The <tt>name</tt> of a node is of type
 *  <code>xsQName</code>
 *
 * @author Nicola Onose
 *
 * @see Item
 * @see Element
 * @see Attribute
 *
 */

public class Node extends Item
{
    protected Node() {}
    
    protected Node (int native_item) {
	setItem (native_item);
    }


    /**
     * Get the parent of the node
     *
     */
    
    public NodeList parent ()
	throws GalapiException
    {
	// int il = -1;

	int il = nativeParent (getNativeItem());
	return new NodeList (il);
    }
    

    /**
     * Get the list of node children.
     *
     */

    public NodeList children ()
	throws GalapiException
    {
	//int il = -1;

	int  il = nativeChildren (getNativeItem());

	return new NodeList (il);
    }


    /**
     * Get the URI associated with the document if 
     * there is one.
     *
     */

    public AtomicList baseURI ()
	throws GalapiException
    {	
	// int i = -1;

	int  i = nativeBaseURI (getNativeItem());

	return new AtomicList (i);
    }


    public AtomicList nodeName ()
	throws GalapiException
    {	
	//int i = -1;

	int i = nativeNodeName (getNativeItem());

	return new AtomicList (i);
    }


    public String nodeKind ()
	throws GalapiException
    {
	return nativeNodeKind (getNativeItem());
    }

    /**
     * It returns the typed value of a <code>Node</code>. 
     * A <code>Node</code> also has a string representation of its value.
     *
     */
    public AtomicList typedValue ()
	throws GalapiException
    {
	//int il = -1;

	int il = nativeTypedValue (getNativeItem());

	return new AtomicList (il);
    }

    /**
     * Get the list of attributes of the node.
     * 
     */

    public AttributeList attributes ()
	throws GalapiException
    {
	//int il = -1;

	int  il = nativeAttributes (getNativeItem());
	    
	return new AttributeList (il);
    }

    protected static native int nativeParent (int n);
    protected static native int nativeChildren (int n);

    protected static native int nativeBaseURI (int n);

    protected static native int nativeNodeName (int n);
    protected static native String nativeNodeKind (int n);

    protected static native int nativeTypedValue (int n);
    protected static native int nativeAttributes (int n);

}// class Node
