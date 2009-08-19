(: ----------------------------------------
     Use Case "Seq" : Operations on trees
   ---------------------------------------- :)
module namespace seq_context = "seq_context";

declare schema { 
  declare element report { element section* };

  declare element section { 
    element section.title, 
    element section.content 
  };
  
  declare element section.title { xs:string } ;
  declare element section.content  { 
   ( text | 
     element anesthesia | 
     element prep | 
     element incision | 
     element action | 
     element observation )*
  };
  declare element anesthesia { text };
  declare element prep { (text | element action)* };
  declare element incision { ( text | element geography | element instrument)* };
  declare element action { ( text | element instrument )* };
  declare element observation { text };
  
  declare element geography { text };
  declare element instrument { text };
};

declare variable $seq_context:report := doc("../docs/report.xml") treat as document-node() ;

