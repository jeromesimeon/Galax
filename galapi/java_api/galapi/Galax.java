/***********************************************************************/
/*                                                                     */
/*                                 GALAX                               */
/*                              XQuery Engine                          */
/*                                                                     */
/*  Copyright 2001-2007.                                               */
/*  Distributed only by permission.                                    */
/*                                                                     */
/***********************************************************************/


/* $Id: Galax.java,v 1.12 2007/12/06 02:19:14 mff Exp $ */

/**
 * @(#)Galax.java
 *
 */

package galapi;


/**
 * The <code>Galax</code> class contains methods needed to perform 
 * queries and to manipulate data under different representations.
 *
 */

public class Galax
{
    static ProcessingContext defPC;
    static ExternalContext defEC;

    /* Input source kind: a file, a string buffer, or an HTTP URL. */
    public static final int  File_Input  = 0;
    public static final int  Buffer_Input = 1;
    public static final int  Http_Input = 2;

    /** Module does not take an external context item */
    public static final boolean NoExternalContextItem   = false;
    /** Module does take an external context item */
    public static final boolean ExternalContextItem   = true;

    static {    
	try {
	    System.out.flush ();
	    System.loadLibrary("galaxoptj");
	} catch (Exception e) {
	    System.err.println ("cannot load native Galax library");
	    System.err.println ("trying with bytecode version");
	    System.err.flush ();
            try {
  	       System.loadLibrary("galaxj");
            } catch (Exception e2) {
	       System.err.println ("cannot load bytecode Galax library");
	       System.err.println ("Aborting!");
 	       System.exit (1);
            }
	}
    }



    /**
     *
     * Does the initialization stuff, that is it passes program arguments 
     * to the OCaml Sys module.
     *
     */
    public static void init ()
	throws GalapiException
    {

	nativeInitialize ();

	int i = nativeDefaultProcessingContext ();
	defPC = new ProcessingContext (i);
	defEC = new ExternalContext ();
	
    }// init()
    

    /*
     * initialize
     */
    private static native void nativeInitialize ();


    /**************************/
    /* Document I/O functions */
    /**************************/

    /**
     * Loads an XML document into the Galax data model.
     *
     * @param pc Processing context for loading document
     * @param input_source_kind How the input XML is provided
     * @param input The input itself (string, file, etc.)
     * @return a list of nodes giving the Galax representation of the document
     *
     */

    public static NodeList loadDocument (ProcessingContext pc, int input_source_kind, String input)
	throws GalapiException
    {
	//	int i = -1;

	int i = nativeLoadDocument (pc.getNativeItem(), input_source_kind, input);

	return new NodeList (i);
    }
    
    /**
     * 
     * Serializes an XQuery data model value to a string
     *
     * @param xml a collection of items representing an XML value
     *
     * @return a string representation of the collection of items
     *  
     */

    public static String serializeToString (ProcessingContext pc, ItemList xml)
	throws GalapiException
    {

	String s = nativeSerializeToString (pc.getNativeItem(), xml.getNativeItemList());

	return s;
    }


    /**
     * 
     * Serializes an XQuery data model value to standard output
     *
     * @param xml a collection of items representing an XML value
     *
     */

    public static void serializeToStdout (ProcessingContext pc, ItemList xml)
	throws GalapiException
    {

	nativeSerializeToStdout (pc.getNativeItem(), xml.getNativeItemList());

    }

    /**
     * 
     * Serializes an XQuery data model value to a file
     *
     * @param filename the name of the output file to be produced
     * @param xml a collection of items representing an XML value
     *
     */

    public static void serializeToFile (ProcessingContext pc, String filename, ItemList xml)
	throws GalapiException
    {

	nativeSerializeToFile (pc.getNativeItem(), filename, xml.getNativeItemList());

    }


    /**********************/
    /* Evaluation context */
    /**********************/

    /**
     *
     * Returns the default {@link galapi.ProcessingContext ProcessingContext} 
     * which just contains flags for controlling debugging, printing, and 
     * the processing phases.
     *
     */

    public static ProcessingContext defaultProcessingContext()
	throws GalapiException
    {
	return defPC;
    }


    /**
     * Load the standard Galax library, which contains the built-in
     * types, namespaces, and functions.
     *
     */

    public static CompiledProgram loadStandardLibrary (ProcessingContext pc)
	throws GalapiException
    {
	//int i = -1;
	
	int i = nativeLoadStandardLibrary (pc.getNativeItem());

	return new CompiledProgram (i);
    }
    
    /**
     * Used to import other library modules.
     * 
     * @return the {@link galapi.CompiledProgram CompiledProgram} argument extended 
     * with the module in the string filename argument.
     *
     */

    public static CompiledProgram importLibraryModule (CompiledProgram compiled_prolog, int input_source_kind, String input)
	throws GalapiException
    {
	//	int i = -1;

	int i = nativeImportLibraryModule (compiled_prolog.getNativeItem(), input_source_kind, input);
	    
	return new CompiledProgram (i);
    }

    /**
     * Used to import a main module defined in a file.
     * @param has_ext_ctxt_item  True, if the main module expects the context item to be provided by the external environment. 
     * @param input_source_kind Specifies whether main module is in
     * file, string buffer or HTTP URL.
     * @return the {@link galapi.CompiledModule CompiledModule}
     * argument contains a compiled prolog and an {@link
     * galapi.ItemList ItemList} containing statements to evaluate.
     * 
     * @see CompiledModule
     *
     */

    public static CompiledModule importMainModule (CompiledProgram compiled_prolog, boolean has_ext_ctxt_item, int input_source_kind, String input)
	throws GalapiException
    {

	//	int i = -1;

	int i = nativeImportMainModule (compiled_prolog.getNativeItem(), has_ext_ctxt_item, input_source_kind, input);

	return new CompiledModule (i);
    }
    
    /**
     * Evaluates the expressions for all - possibly
     * mutually dependent - global variables. 
     * <p>It must be called before the other evalXXX functions.
     *
     * @param compiled_prolog a context obtained with one of the methods from
     * this class
     * 
     * @return a new context in which the expressions are evaluated
     */

    public static PreparedProgram evalProgram (CompiledProgram compiled_prolog, ExternalContext exc)
	throws GalapiException
    {

	//int i = -1;

	int i = nativeEvalProgram(compiled_prolog.getNativeItem(), exc.getNativeItem());

	return new PreparedProgram (i);	
    }


    /********************/
    /* Query evaluation */
    /********************/

    /**
     * Given the <code>CompiledProgram</code>, evaluates the XQuery
     *  statement  in  the string argument.  
     * 
     * @return if the statement is an XQuery expression,
     *  returns a list of items, otherwise if the statement is an
     *  XQuery update, returns an empty list because update statements have
     *  side effects on the data model store, but do not return values.
     */

    public static ItemList evalStatement(CompiledProgram c, int input_source_kind, String input)
	throws GalapiException
    {

	//int il = -1;

	int il = nativeEvalStatement (c.getNativeItem(), input_source_kind, input);

	return new ItemList (il);
    }

    /**
     * Given the {@link galapi.PreparedProgram PreparedProgram}, 
     * evaluates a single compiled statement.
     * 
     * @return an XSquery sequence representing the result of this evaluation
     */

    public static ItemList evalCompiledStatement (PreparedProgram pp, Item cs)
	throws GalapiException
    {

	//int il = -1;
	int il = nativeEvalCompiledStatement (pp.getNativeItem(), cs.getNativeItem());

	return new ItemList (il);
    }

    /*
     * native methods
     *
     */
    protected static native int nativeLoadDocument (int pc, int input_source_kind, String filename);
    protected static native String nativeSerializeToString (int pc, int xml);
    protected static native void nativeSerializeToStdout (int pc, int xml);
    protected static native void nativeSerializeToFile (int pc, String filename, int xml);

    protected static native int nativeDefaultProcessingContext ();
    protected static native int nativeLoadStandardLibrary (int pc);
    protected static native int nativeImportLibraryModule (int compiled_prolog, int input_source_kind, String input);
    protected static native int nativeImportMainModule (int compiled_prolog, boolean ext_ctxt_bool, int input_source_kind, String input);

    protected static native int nativeEvalProgram (int compiled_prolog, int external_context);

    protected static native int nativeEvalStatement(int c, int input_source_kind, String input);
    protected static native int nativeEvalCompiledStatement (int prepared_prolog, int compiled_stmt);

}// class Galax

