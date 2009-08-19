/**
 *
 * @author Nicola Onose
 *
 */


import galapi.*;

public class Example
{

/* 
   Example 1 

   Build a query context from a file.
   Load a document.
   Add a global variable to query context, bound to the loaded document.
   Evaluate a query contained in a string.
   Serialize result to standard output

*/
    
    static void example1 (ProcessingContext pc, CompiledProgram cp, ExternalContext exc) throws GalapiException {
    
	CompiledModule cm =  Galax.importMainModule (cp, Galax.NoExternalContextItem, Galax.File_Input, "../docs/seq.xq");
	PreparedProgram pp = Galax.evalProgram(cm.getCompiledProgram(), exc);
	ItemList r = Galax.evalStatement(pp, Galax.Buffer_Input, 
					 "for $s in $seq_context:report//section[section.title = \"Procedure\"] return ($s//incision)[2]/instrument");
	Galax.serializeToStdout (pc,r);

    }// example1()

/* 
   Example 2

   Build a query context from a file.
   Load a document.
   Evaluate a query contained in a file.
   Serialize result to standard output.

*/
    static void example2 (ProcessingContext pc,CompiledProgram cp, ExternalContext exc) throws GalapiException  {
	/* Load main module */
	CompiledModule cm =  Galax.importMainModule (cp, Galax.NoExternalContextItem, Galax.File_Input, "../docs/seq.xq");

	/* Evaluate module's prolog */
	PreparedProgram pp = Galax.evalProgram(cm.getCompiledProgram(), exc);

	/* Evaluate first statement in module */
	ItemList il = cm.getCompiledStatements(); 
	Item cs = il.ItemsFirst();
	ItemList r = Galax.evalCompiledStatement (pp, cs);
	Galax.serializeToStdout (pc,r);

    }// example2()


/* 
   Example 3

   Build a query context from a file.
   Create some XML values. 
   Evaluate a query function contained in the query context by
   applying to XML values.
   Serialize result to a string & print. 

*/
    static void example3 (ProcessingContext pc,CompiledProgram cp, ExternalContext exc)
	throws GalapiException  {

	CompiledModule cm = Galax.importMainModule (cp, Galax.NoExternalContextItem, Galax.File_Input, "../docs/xq-example2.xq"); 
	PreparedProgram pp = Galax.evalProgram(cm.getCompiledProgram(), exc);

	ItemList r = Galax.evalStatement(pp, Galax.Buffer_Input, "xq-example1:test2($y,$x)"); 
	Galax.serializeToStdout (pc,r);
	
    }// example3()


    public static void main (String args[]) 
    	throws GalapiException  {
	
	Galax.init ();

	ProcessingContext pc = Galax.defaultProcessingContext ();
	NameValuePair []vars = new NameValuePair[2];

	xsInteger xi = new xsInteger (12);
	xsString xs = new xsString ("a string"); 
	ItemList xil = ItemList.cons (xi, new ItemList());
	ItemList xsl = ItemList.cons (xs, new ItemList());
	vars[0] = new NameValuePair("x", xsl);
	vars[1] = new NameValuePair("y", xil);
	ExternalContext exc = new ExternalContext(pc, new ItemList(), new ItemList(), vars);

	CompiledProgram cp = Galax.loadStandardLibrary (pc);
	example1 (pc,cp,exc);

	cp = Galax.loadStandardLibrary (pc);
	example2 (pc,cp,exc);

	ItemList xmldoc = Galax.loadDocument (pc, Galax.File_Input, "../docs/report.xml");

	vars = new NameValuePair[3];
	vars[0] = new NameValuePair("x", xsl);
	vars[1] = new NameValuePair("y", xil);
	vars[2] = new NameValuePair("v", xsl);
	exc = new ExternalContext(pc, new ItemList(), new ItemList(), vars);

	cp = Galax.loadStandardLibrary (pc);
	example3 (pc,cp,exc);

	//	example4 (pp);
	
    }// main()

}// class Example
