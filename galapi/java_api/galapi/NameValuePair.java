/***********************************************************************/
/*                                                                     */
/*                                 GALAX                               */
/*                              XQuery Engine                          */
/*                                                                     */
/*  Copyright 2001-2007.                                               */
/*  Distributed only by permission.                                    */
/*                                                                     */
/***********************************************************************/

/**
 * @(#)NameValuePair.java
 *
 */

package galapi;

public class NameValuePair
{
    String name;
    ItemList value;

    /**
     * Builds a name/value pair, usually for representing a variable.
     *
     * @param n the name of the variable
     * @param v the value of the variable
     */
    public NameValuePair (String n, ItemList v)
	throws GalapiException
    {
	name = n;
	value = v;
    }

    public String getName () { return name; }

    public ItemList getValue () { return value; }

}// class NameValuePair
