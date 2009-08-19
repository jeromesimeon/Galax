/***********************************************************************/
/*                                                                     */
/*                                 GALAX                               */
/*                              XQuery Engine                          */
/*                                                                     */
/*  Copyright 2001-2007.                                               */
/*  Distributed only by permission.                                    */
/*                                                                     */
/***********************************************************************/

/* $Id: AttributeList.java,v 1.4 2007/02/01 22:08:51 simeon Exp $ */

package galapi;

public class AttributeList extends NodeList
{
    
    public AttributeList()
	throws GalapiException
    {
	super();
    }

    protected AttributeList (int il)  {	
	super (il);
    }
    

    public static AttributeList cons (Attribute a, AttributeList cdr)
	throws GalapiException
    {
	int j = nativeCons (a.getNativeItem (), cdr.getNativeItemList ());
	return new AttributeList (j);
    }

    
}// class AttributeList

