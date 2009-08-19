/***********************************************************************/
/*                                                                     */
/*                                 GALAX                               */
/*                              XQuery Engine                          */
/*                                                                     */
/*  Copyright 2001-2007.                                               */
/*  Distributed only by permission.                                    */
/*                                                                     */
/***********************************************************************/

/* $Id: Item.java,v 1.7 2007/02/01 22:08:51 simeon Exp $ */

/**
 * @(#)Item.java
 *
 */

package galapi;

/**
 * An <tt>Item</tt> is the a primitive datatype for a value. 
 * Any XQuery value is an item or a list of items.
 * Items are either <code>Atomic</code> or <code>Node</code>.
 * 
 * @author Nicola Onose
 *
 * @see galapi.ItemList
 * @see galapi.Atomic
 * @see galapi.Node
 *
 */


public class Item
{
    protected int item;

    protected Item () {}

    protected Item (int native_item) {
	setItem (native_item);
    }

    /**
     *
     * @return a string representation of the item
     *
     * @throws RuntimeException when there is an internal Galax error
     *
     */
    public String toString ()
    {

	return nativeStringValue (item);
    }

    
    

    /**
     *
     * This method is used to release the item and the memory allocated to it.
     *
     */
    protected void finalize ()  throws Throwable {
	nativeFree (item);
    }


    /**
     * XQuery Accessor: get kind of item.
     *
     */
    public String itemKind () {
	return nativeItemKind (item);
    }

    /**
     * XQuery Accessor: cast the item to a node
     *
     */
    public Node getNode () {
	// int i = -1;

	/*	int i = nativeNode (item); */

	return new Node (item);
    }

    /**
     * XQuery Accessor: cast the item to an atomic value
     *
     */
    public Atomic getAtomic () {
	// int i = -1;

	/*	int  i = nativeAtomicValue (item); */
	return new Atomic (item);
    }

    /**
     * XQuery Accessor: cast the item to an element
     *
     */
    public Element getElement () {
	//int i = -1;

	/*	int i = nativeElement (item); */

	return new Element (item);
    }

    /**
     * XQuery Accessor: cast the item to an attribute
     *
     */
    public Attribute getAttribute () {
	//int i = -1;

	/*	int i = nativeAttribute (item); */

	return new Attribute (item);
    }

    
    /*    protected static native int nativeAtomicValue (int i);
    protected static native int nativeElement (int i);
    protected static native int nativeAttribute (int i);
    protected static native int nativeNode (int i);
    */
    protected static native String nativeStringValue (int i);
    protected static native String nativeItemKind (int i);


    protected void setItem (int i)
    {
	item = i;
    }// setItem()

    protected int getNativeItem () {
	return item;
    }// getNativeItem()
    

    protected static native void nativeFree (int i);
    
}// class Item

