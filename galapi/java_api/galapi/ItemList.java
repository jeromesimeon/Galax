/***********************************************************************/
/*                                                                     */
/*                                 GALAX                               */
/*                              XQuery Engine                          */
/*                                                                     */
/*  Copyright 2001-2007.                                               */
/*  Distributed only by permission.                                    */
/*                                                                     */
/***********************************************************************/

/* $Id: ItemList.java,v 1.5 2007/02/01 22:08:51 simeon Exp $ */

/**
 * @(#)ItemList.java
 *
 *
 */

package galapi;

/**
 * 
 * An <code>ItemList</cose> is a collection of items 
 * that can be processed or returned as a result
 * by the functions in the class {@link galapi.Galax Galax}.
 * 
 */

public class ItemList
{
    protected int ilist;
    protected void setItemList (int il) {ilist = il;}
    protected int getNativeItemList () {return ilist;}

    /**
     * creates an empty item list
     *
     */
    public ItemList () 
	throws GalapiException
    {	
	setItemList (nativeEmptyList());
    }

    /*
     * copy constructor
     */
    protected ItemList (int il) {	
	setItemList (il);
    }
    
    /**
     * Release memory resources related to the <code>ItemList</code>.
     */
    public void finalize () throws Throwable
    {
	nativeFreeItemList (ilist);
    }


    /**
     *  Creates a new list by adding an <code>Item</code> at the
     *  beginning of an <code>ItemList</code>.
     * 
     * @return the new list obtained
     */

    public static ItemList cons (Item i, ItemList cdr) 
	throws GalapiException
    {
	int j = nativeCons (i.getNativeItem (), cdr.getNativeItemList ());
	return new ItemList (j);
    }

    /**
     *
     * @return true if the list is empty, false otherwise
     */

    public boolean isEmpty () 
    {
	return nativeIsEmpty (ilist);
    }

    /**
     *
     * @return the first item in the list
     */

    public Item ItemsFirst () 
	throws GalapiException
    { // car
	int i = nativeItemsFirst (ilist); 
	return new Item (i);
    }// ItemsFirst()


    /**
     * @return the list without its first item
     */

    public ItemList ItemsNext () 
	throws GalapiException
    { // cdr
	int il = nativeItemsNext (ilist);
	return new ItemList (il);
    }// ItemsNext()


    /**
     *
     * @return a string representation of the item
     *
     * @throws RuntimeException when there is an internal Galax error
     *
     */
    
    public String toString () {
	return nativeSerialize (ilist);
    }


    protected static native int nativeEmptyList ();

    protected static native void nativeFreeItemList (int il);
    protected static native int nativeCons (int i, int il);
    protected static native boolean nativeIsEmpty (int il);
    protected static native int nativeItemsFirst (int il);
    protected static native int nativeItemsNext (int il);

    protected static native String nativeSerialize (int il);
}// class ItemList

