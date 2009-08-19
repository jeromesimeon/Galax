/**
 *
 * @author Nicola Onose
 *
    IF YOU ADD TESTS TO THIS FILE, YOU MUST ALSO ADD THEM TO: 
      examples/c_api/c-test.c
      examples/caml_api/test.ml
 */

import galapi.*;

public class Test
{
    public static void main (String sysargs[]) throws Exception {

	ItemList il, items;
	String str;
	xsInteger i; 
	xsInteger i1; 
	xsDecimal i2; 
	xsString s;
	xsBoolean b; 
	xsFloat d;
	xsDouble d2;
	xsDateTime dt;
	xsDate date;
        xsTime time; 
	xsDayTimeDuration dtd;
	xsYearMonthDuration ymd;
	
	try { 
	    Galax.init ();      
	    ExternalContext defexc = new ExternalContext ();

	    ProcessingContext pc = Galax.defaultProcessingContext ();
	    pc.setMonitorTime(true);  
	    pc.setSerializationKind(ProcessingContext.SERIALIZE_WellFormed);  

	    /* xs:int */	
	    i = new xsInteger (20);
	    if (20 != i.toInt()) 
		System.out.println ("Error! roundtripping int");
	    il = ItemList.cons (i, new ItemList());
	    str = il.toString();

	    /* We are using serializeToString here so that the output
	     * matches that of the C and Caml tests */
	    System.out.println ("Test Int:\n"+Galax.serializeToString(pc,il));

	    /* xs:integer */	
	    i1 = new xsInteger (20);
	    if (20 != i1.toInt()) 
		System.out.println ("Error! roundtripping integer");
	    System.out.println ("Test Integer:\n"+Galax.serializeToString(pc,ItemList.cons(i1, new ItemList())));

	    /* xs:decimal */	
	    i2 = new xsDecimal (20);
	    if (20 != i2.toInt()) 
		System.out.println ("Error! roundtripping decimal");
	    System.out.println ("Test Decimal:\n"+Galax.serializeToString(pc,ItemList.cons(i2, new ItemList())));

	    /* xs:string */
	    s = new xsString ("user test");
	    str = s.toString();
	    if (!s.toString().equals("user test")) 
		System.out.println ("Error! roundtripping string");	
	    System.out.println ("Test String:\n"+Galax.serializeToString(pc,ItemList.cons(s, new ItemList())));

	    /* xs:boolean */
	    b = new xsBoolean (false);
	    if (b.toBoolean()) 
		System.out.println ("Error! roundtripping boolean");	
	    System.out.println ("Test Boolean:\n"+Galax.serializeToString(pc,ItemList.cons(b, new ItemList())));
	
	    /* xs:float */
	    d = new xsFloat (98.549);
	    if (98.549 != d.toDouble()) 
		System.out.println ("Error! roundtripping float "+d.toDouble());	
	    System.out.println ("Test Float:\n"+Galax.serializeToString(pc,ItemList.cons(d, new ItemList())));

	    /* xs:double */
	    d2 = new xsDouble (98.549);
	    if (98.549 != d2.toDouble()) 
		System.out.println ("Error! roundtripping double "+d2.toDouble());	
	    System.out.println ("Test Double:\n"+Galax.serializeToString(pc,ItemList.cons(d2, new ItemList())));

	    /* xs:dateTime */
	    dt = new xsDateTime("1991-01-02T01:01:01.01Z");
	    System.out.println ("Test DateTime:\n"+Galax.serializeToString(pc,ItemList.cons(dt, new ItemList())));

	    /* xs:date */
	    date = new xsDate("2000-03-12"); 
	    System.out.println ("Test Date:\n"+Galax.serializeToString(pc,ItemList.cons(date, new ItemList())));

	    /* xs:time */
	    time = new xsTime("03:04:05.06");
	    System.out.println ("Test Time:\n"+Galax.serializeToString(pc,ItemList.cons(time, new ItemList())));

	    /* xs:dayTimeDuration */
	    dtd = new xsDayTimeDuration("P1DT2H3M1.1S");
	    System.out.println ("Test DayTimeDuration:\n"+Galax.serializeToString(pc,ItemList.cons(dtd, new ItemList())));

	    /* xs:yearMonthDuration */ 
	    ymd = new xsYearMonthDuration("P2Y3M");
	    System.out.println ("Test YearMonthDuration:\n"+Galax.serializeToString(pc,ItemList.cons(ymd, new ItemList())));

	    /* serialization */
	    NodeList docitems = Galax.loadDocument (pc,Galax.File_Input, "../docs/xml-example.xml");
	    Galax.serializeToStdout (pc,docitems);
	    str = Galax.serializeToString (pc,docitems);
	    System.out.println ("Test load_document:\n"+str);		

	    /* External context with an implicit timezone */	    
	    xsDayTimeDuration tz = new xsDayTimeDuration("-P0DT5H"); 
	    ItemList tzl = ItemList.cons(tz, new ItemList());
	    NameValuePair vars[] = new NameValuePair[3];
	    vars[0] = new NameValuePair ("v", docitems);
	    vars[1] = new NameValuePair ("x", docitems);
	    vars[2] = new NameValuePair ("y", il);
	    ExternalContext ec = new ExternalContext (pc,docitems, tzl, vars);

	    /* Import main module */
	    CompiledProgram sc = Galax.loadStandardLibrary (pc);
	    CompiledModule cm =  Galax.importMainModule (sc, Galax.ExternalContextItem, Galax.File_Input, "../docs/xq-example2.xq");
	    CompiledProgram cp = cm.getCompiledProgram(); 
	    PreparedProgram pp = Galax.evalProgram(cm.getCompiledProgram(), ec);
	    items = Galax.evalCompiledStatement(pp, (cm.getCompiledStatements()).ItemsFirst());
	    System.out.println ("Test import_main_module:\n"+items);

	    /* xs:QName */
	    xsQName q = new xsQName (cp.nsenvFromCompiledProgram(), "xq-example1:bar"); 
	    System.out.println ("Test QName:\n"+Galax.serializeToString(pc,ItemList.cons(q, new ItemList())));

	    /* ROUND TRIPPING */
	    /* comment */
	    Comment c = new Comment (new xsString ("This is a comment"));
	    System.out.println ("Test node_kind:\n"+c.nodeKind());
	    System.out.println ("Test commentNode:\n"+Galax.serializeToString(pc,ItemList.cons(c, new ItemList())));	

	    /* PI */
	    ProcessingInstruction p = 
		new ProcessingInstruction (new xsString ("target"),
					   new xsString ("This is a processing instruction"));	
	    System.out.println ("Test processingInstructionNode:\n"+Galax.serializeToString(pc,ItemList.cons(p, new ItemList())));	
	
	    if (!p.nodeKind().equals("processing-instruction")) 
		System.out.println ("Error! node kind "+(p.nodeKind())+" != processing-instruction");

	    /* element */
	    Element e = new Element (q, new AttributeList(), new ItemList(), q);
	    System.out.println ("Test elementNode:\n"+Galax.serializeToString(pc,ItemList.cons(e, new ItemList())));
	    
	    /* xs:QName */
	    xsQName q1 = new xsQName (cp.nsenvFromCompiledProgram(), "baz");
	    System.out.println ("Test QName:\n"+Galax.serializeToString(pc,ItemList.cons(q1, new ItemList())));
	    
	    /* document */ 
	    ItemList el = 
		ItemList.cons (p,
			       ItemList.cons (c,
					      ItemList.cons (e,
							     ItemList.cons (e,new ItemList()))));
	    
	    Document dn = new Document ("http://db.bell-labs.com/galax",  el);
	    System.out.println ("Test documentNode:\n"+Galax.serializeToString(pc,ItemList.cons(dn, new ItemList())));

	    /* attribute */
	    Attribute a = new Attribute (q1, new xsString ("attribute value"), q1);
	    System.out.println ("Test attributeNode:\n"+Galax.serializeToString(pc,ItemList.cons(a, new ItemList())));	
	    
	    /* element */
	    el = 
		ItemList.cons (e,
			       ItemList.cons (e,new ItemList()));
	    
	    e = new Element (q,  AttributeList.cons (a, new AttributeList()), el, q);
	    System.out.println ("Test elementNode children:\n"+Galax.serializeToString(pc,ItemList.cons(e, new ItemList())));
    
	    /* node name */
	    AtomicList q1opt = e.nodeName(); 
	    if (q1opt.isEmpty()) 
		System.out.println ("Error! node name is empty\n");
	    else	
		System.out.println ("Test node_name:\n"+Galax.serializeToString(pc,q1opt));

	    /* children */
	    NodeList k = e.children ();
	    System.out.println ("Test children:\n"+Galax.serializeToString(pc,k));
	    
	    /* parent */
	    NodeList pr = (k.ItemsFirst().getNode()).parent(); 
	    System.out.println ("Test parent:\n"+Galax.serializeToString(pc,pr));
	    
	    /* attributes */
	    AttributeList ats = e.attributes ();
	    System.out.println ("Test attributes:\n"+Galax.serializeToString(pc,ats));	
	    
	    /* text node */
	    Text t = new Text (new xsString("this is a text node"));
	    System.out.println ("Test text:\n"+Galax.serializeToString(pc,ItemList.cons(t, new ItemList())));
	    
	    /* Base uri of a document node */
	    /*
	    AtomicList bs = docitem.baseURI();
	    if (bs.isEmpty()) {
		System.out.println ("Error! base uri is empty\n"); 
	    } 
	    */

	    /* Base uri of an element node */
	    Node docitem = docitems.ItemsFirst().getNode();
	    AtomicList bs = docitem.children().ItemsFirst().getNode().baseURI();
	    if (bs.isEmpty()) {
		System.out.println ("Error! base uri is empty\n"); 
	    } else {
		System.out.println ("Test base_uri:\n"+Galax.serializeToString(pc,bs));
	    }

	    /* Eval statement from string */	    
	    items = Galax.evalStatement (pp, Galax.Buffer_Input, "xq-example1:test0()");	
	    System.out.println ("Test eval_statement 1:\n"+Galax.serializeToString(pc,items));
	    
	    items = Galax.evalStatement(pp, Galax.Buffer_Input, "20.0 * 3.3");
	    System.out.println ("Test eval_statement 2:\n"+Galax.serializeToString(pc,items));

	    items = Galax.evalStatement(cp, Galax.Buffer_Input, "$x");
	    System.out.println ("Test variable $x:\n"+items);
	    items = Galax.evalStatement(cp, Galax.Buffer_Input, "$y");
	    System.out.println ("Test variable $y:\n"+items);

	    /* Test the implicit timezone */
	    items = Galax.evalStatement (cp, Galax.Buffer_Input, "xs:dateTime('1991-12-31T23:00:00.0') = xs:dateTime('2000-01-01T04:00:00.0Z')");
	    System.out.println ("Test dateTime equality:\n"+items);

	    items = Galax.evalStatement(cp, Galax.Buffer_Input, "./html/ul/li");
	    System.out.println ("Test eval_statement 3:\n"+items);

	    items = Galax.evalStatement(cp, Galax.File_Input, "../docs/htmlpath.xq");
	    System.out.println ("Test eval_statement 4:\n"+items);

	    items = Galax.evalStatement(cp, Galax.Buffer_Input, "$v/html/ul/li");
	    System.out.println ("Test eval_statement 5:\n"+items);

	    /* Test rebinding variables */
	    i = new xsInteger(30); 
	    il = ItemList.cons (i, new ItemList());
	    /* External context */	    
	    NameValuePair vars1[] = new NameValuePair[3];
	    vars[0] = new NameValuePair ("v", il);
	    vars[1] = new NameValuePair ("x", docitems);
	    vars[2] = new NameValuePair ("y", il);
	    ec = new ExternalContext (pc,il, tzl, vars);

	    cp = Galax.evalProgram (cm.getCompiledProgram(), ec);
	    items = Galax.evalStatement(cp, Galax.Buffer_Input, "($v, $z)");
	    System.out.println ("Test rebinding of variables:\n"+items);

	    /* Import library module */
	    sc = Galax.loadStandardLibrary (pc);
	    cm = Galax.importMainModule (sc, Galax.NoExternalContextItem, Galax.File_Input, "../docs/schema.xq");
	    cp = cm.getCompiledProgram(); 
	    pp = Galax.evalProgram (cp, defexc);	
	    items = Galax.evalStatement (pp, Galax.Buffer_Input, "$xmlschema:po/HisPo:purchaseOrder/items/item/quantity");
	    System.out.println ("Test import_main_module:\n"+Galax.serializeToString(pc,items));

	    /* Typed value */
	    AtomicList al = items.ItemsFirst().getNode().typedValue();
	    System.out.println ("Test typed_value:\n"+Galax.serializeToString(pc,al));
	    System.out.println (":" + al.ItemsFirst().itemKind());

	    /* Dump the monitor output */
	    /*
	      items = pc.monitorOfLastCall(); 
	      System.out.println (Galax.serializeToString(pc,items)); 

	      items = pc.monitorOfAllCalls(); 
	      System.out.println (Galax.serializeToString(pc,items));
	    */

	    /* Raise an error intentionally -- always do this last */
	    
	    NodeList invalid_items = Galax.loadDocument (pc,Galax.File_Input, "no-such-file.xml");
	} catch (Exception ex2) {
	    System.out.println ("This is an intentional error");
	    /* System.out.println(ex2); */
	    System.out.flush ();
	    /* throw ex2; */
	}	
    }	// main()

}// class Test
