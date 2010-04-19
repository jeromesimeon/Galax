(: -----------------------------------------
     Use Case "STRING" : String search
   ----------------------------------------- :)

declare variable $company-data := doc("../docs/company-data.xml");
declare variable $input := doc("../docs/string.xml");

declare function local:partners($company as xs:string) as element()*
{
    let $c := $company-data//company[name = $company]
    return $c//partner
};

