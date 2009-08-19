/***********************************************************************/
/*                                                                     */
/*                                 GALAX                               */
/*                              XQuery Engine                          */
/*                                                                     */
/*  Copyright 2001-2007.                                               */
/*  Distributed only by permission.                                    */
/*                                                                     */
/***********************************************************************/

/* $Id: ProcessingContext.java,v 1.9 2007/02/01 22:08:51 simeon Exp $ */

/**
 * @(#)ProcessingContext.java
 *
 */

package galapi;


/**
 * A <code>ProcessingContext</code> contains flags 
 * for controlling debugging, printing, and 
 * the processing phases
 *
 */

public class ProcessingContext extends Context
{
    
    protected ProcessingContext() {}

    protected ProcessingContext (int native_item) {
	setItem (native_item);
    }
    

    /**
     * Enable ing of memory usage.
     * @param   b If true, memory ing is enabled
     * @throws  GalapiException
     */
    
    public void setMonitorMem(boolean b)
    throws GalapiException
    {
	nativeSetMonitorMem(this.getNativeItem(), b);
    }
    /**
     * Enable ing of time
     * @param   b If true, time ing is enabled
     * @throws  GalapiException
     */
    
    public void setMonitorTime(boolean b)
    throws GalapiException
    {
	nativeSetMonitorTime(this.getNativeItem(), b);
    }
    
    /**
     * @return the ing data of the last call
     * @throws  GalapiException
     */
    
    public ItemList monitorOfLastCall()
    throws GalapiException
    {
	    int il = nativeMonitorOfLastCall(this.getNativeItem());
	    return new ItemList(il);
    }
    
    /**
     * @return the monitoing data of all the calls made
     * @throws  GalapiException
     */
    
    public ItemList monitorOfAllCalls()
    throws GalapiException
    {
	    int il = nativeMonitorOfAllCalls(this.getNativeItem());
	    return new ItemList(il);
    }
    
    /* -- PROCESSING PHASE CONTROL --*/
    /**
     * Enable/disable the normalization phase
     * @param   n If true, phase is enabled
     * @throws  GalapiException
     */
    public void setNormalizationPhase(boolean n)
    throws GalapiException
    {
	    nativeSetNormalizationPhase(this.getNativeItem(), n);
    }
    
    /**
     * Enable/disable the typing phase
     * @param   t If true, phase is enabled
     * @throws  GalapiException
     */
    public void setTypingPhase(boolean t)
    throws GalapiException
    {
	    nativeSetTypingPhase(this.getNativeItem(), t);
    }
    
    /**
     * @Enable/disable the rewriting phase
     * @param   c If true, phase is enabled
     * @throws  GalapiException
     */
    public void setRewritingPhase(boolean c)
    throws GalapiException
    {
	    nativeSetRewritingPhase(this.getNativeItem(), c);
    }
    
    /**
     * Enable/disable the evaluation phase
     * @param   e If true, phase is enabled
     * @throws  GalapiException
     */
    public void setEvaluationPhase(boolean e)
    throws GalapiException
    {
	    nativeSetEvaluationPhase(this.getNativeItem(), e);
    }
    
    /* -- PROCESSING PHASE OPTIONS CONTROL --*/

    /** distinct-docorder operations are all removed */
    public static final int SBDO_Remove   = 0;
    /** distinct-docorder operations are all preserved */
    public static final int SBDO_Preserve = 1;
    /** distinct-docorder operations are removed using a sloppy approach (may be buggy) */
    public static final int SBDO_Adhoc    = 2;
    /** distinct-docorder operations are removed based on the tidy automaton optimization */
    public static final int SBDO_Tidy     = 3;
    /** distinct-docorder operations are removed based on the duptidy automaton optimization */
    public static final int SBDO_DupTidy  = 4;
    /** distinct-docorder operations are removed based on the sloppy automaton optimization */
    public static final int SBDO_Sloppy   = 5;

    /**
     * Set one of the sbdo optimization modes (default = SBDO_Adhoc)
     * @param 	kind One of SBDO_Remove, SBDO_Preserve, SBDO_Adhoc, SBDO_Tidy, SBDO_DupTidy, SBDO_Sloppy
     * @throws  GalapiException
     */
    public void setSbdoKind(int kind)
    throws GalapiException
    {
	    if ((kind < 0) || (kind > SBDO_Sloppy)) {
		    throw new GalapiException("Error in setting SBDO kind. Must be one of SBDO_Remove, SBDO_Preserve, SBDO_Adhoc, SBDO_Tidy, SBDO_DupTidy, SBDO_Sloppy");
	    }
	    else {
		    nativeSetSbdoKind(this.getNativeItem(), kind);
	    }
    }

    /** no typing performed */
    public static final int TYPING_None   = 0;
    /** only weak typing is performed */
    public static final int TYPING_Weak   = 1;
    /** Strong typing is performed (may be time consuming) */
    public static final int TYPING_Strong = 2;
    
    /**
     * Set one of the typing modes (default = TYPING_None)
     * @param 	kind One of TYPING_None, TYPING_Weak, TYPING_Strong
     * @throws  GalapiException
     */
    public void setTypingKind(int kind)
    throws GalapiException
    {
	    if ((kind < 0) || (kind > TYPING_Strong)) {
		    throw new GalapiException("Error in setting typing kind. Must be one of TYPING_None, TYPING_Weak, TYPING_Strong");
	    }
	    else {
		    nativeSetTypingKind(this.getNativeItem(), kind);
	    }
    }
    
    /** Output result as well formed XML document */
    public static final int SERIALIZE_WellFormed = 0;
    /** Output result as regular XQuery output */
    public static final int SERIALIZE_XQuery     = 1;
    /** Output canonical */
    public static final int SERIALIZE_Canonical  = 2;
    
    /**
     * Set one of the serialization modes (default = SERIALIZE_WellFormed)
     * @param 	kind One of SERIALIZE_WellFormed, ERIALIZE_XQuery, SERIALIZE_Canonical
     * @throws  GalapiException
     */
    public void setSerializationKind(int kind)
    throws GalapiException
    {
	    if ((kind < 0) || (kind > SERIALIZE_Canonical)) {
		    throw new GalapiException("Error in setting serialization kind. Must be one of SERIALIZE_WellFormed, ERIALIZE_XQuery, SERIALIZE_Canonical");
	    }
	    else {
		    nativeSetSerializationKind(this.getNativeItem(), kind);
	    }	    
    }
        
    /** Remove namespace nodes */
    public static final int SERIALIZE_NS_Strip    = 0;
    /** Preserve namespace nodes */
    public static final int SERIALIZE_NS_Preserve = 1;

    /**
     * Enable/disable namespace node serialization (default = SERIALIZE_NS_Preserve)
     * @param 	kind One of SERIALIZE_NS_Preserve, SERIALIZE_NS_Strip
     * @throws  GalapiException
     */
    public void setSerializeNsKind(int kind)
    throws GalapiException
    {
	    if ((kind < 0) || (kind > SERIALIZE_NS_Preserve)) {
		    throw new GalapiException("Error in setting serialization kind. Must be one of SERIALIZE_NS_Preserve, SERIALIZE_NS_Strip");
	    }
	    else {
		    nativeSetSerializeNsKind(this.getNativeItem(), kind);
	    }	    
    }
    
    /** Use no projection on the input document */
    public static final int PROJECTION_None      = 0;
    /** Use standard projection algorithm */
    public static final int PROJECTION_Standard  = 1;
    /** Advanced projection */
    public static final int PROJECTION_Optimized = 2;
    
    /**
     * Enable one of the projection modes (default = PROJECTION_None)
     * @param 	kind One of PROJECTION_None, PROJECTION_Standard, PROJECTION_Optimized
     * @throws  GalapiException
     */
    public void setProjectionKind(int kind)
    throws GalapiException
    {
	    if ((kind < 0) || (kind > PROJECTION_Optimized)) {
		    throw new GalapiException("Error in setting serialization kind. Must be one of PROJECTION_None, PROJECTION_Standard, PROJECTION_Optimized");
	    }
	    else {
		    nativeSetProjectionKind(this.getNativeItem(), kind);
	    }	    
    }
    
    /* -- DATA MODEL OPTIONS CONTROL --*/
    /**
     * Enable/disable XQuery whitespace preservation
     * @param 	b If true, whitespace is preserved
     * @throws  GalapiException
     */
    public void setXQueryWhiteSpace(boolean b)
    throws GalapiException
    {
	    nativeSetXQueryWhiteSpace(this.getNativeItem(), b);
    }
    
    /**
     * Enable/disable XML whitespace preservation
     * @param 	b If true, whitespace is preserved
     * @throws  GalapiException
     */
    public void setXmlWhitespace(boolean b)
    throws GalapiException
    {
	    nativeSetXmlWhiteSpace(this.getNativeItem(), b);
    }
    
    /**
     * Enable/disable preservation of comments and PI's in XML
     * @param 	b If true, PI's and comments are preserved
     * @throws  GalapiException
     */
    public void setXmlPisAndComments(boolean b)
    throws GalapiException
    {
	    nativeSetXmlPisAndComments(this.getNativeItem(), b);
    }
    
    protected native int nativeMonitorOfLastCall(int pc);
    protected native int nativeMonitorOfAllCalls(int pc);
    protected native void nativeSetMonitorMem(int pc, boolean b);
    protected native void nativeSetMonitorTime(int pc, boolean b);

    protected native void nativeSetNormalizationPhase(int pc, boolean b);
    protected native void nativeSetTypingPhase(int pc, boolean b);
    protected native void nativeSetRewritingPhase(int pc, boolean b);
    protected native void nativeSetEvaluationPhase(int pc, boolean b);
    
    protected native void nativeSetSbdoKind(int pc, int kind);
    protected native void nativeSetTypingKind(int pc, int kind);
    protected native void nativeSetSerializationKind(int pc, int kind);
    protected native void nativeSetSerializeNsKind(int pc, int kind);
    protected native void nativeSetProjectionKind(int pc, int kind);
    
    protected native void nativeSetXQueryWhiteSpace(int pc, boolean b);
    protected native void nativeSetXmlWhiteSpace(int pc, boolean b);
    protected native void nativeSetXmlPisAndComments(int pc, boolean b);
    
}// class ProcessingContext
