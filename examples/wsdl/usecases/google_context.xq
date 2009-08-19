declare namespace GoogleSearch = "../apache/GoogleSearch.wsdl";
declare namespace typens = "urn:GoogleSearch";
declare namespace soap = "http://schemas.xmlsoap.org/wsdl/soap/";
declare namespace soapenc = "http://schemas.xmlsoap.org/soap/encoding/";
declare namespace wsdl = "http://schemas.xmlsoap.org/wsdl/";
declare namespace xsd = "http://www.w3.org/2001/XMLSchema";
declare function
  GoogleSearch:doGoogleSearch.1(
  $oe as item()*,
  $ie as item()*,
  $lr as item()*,
  $safeSearch as item()*,
  $restrict as item()*,
  $filter as item()*,
  $maxResults as item()*,
  $start as item()*,
  $q as item()*,
  $key as item()*,
  $u as xsd:anyURI*
) as item()* {
  let $input :=
    <soapenv:Envelope
      xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/">
      <soapenv:Body>
        <xns:doGoogleSearch
          xmlns:xns="urn:GoogleSearch"
          soapenv:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/">
          <key>{$key}</key>
          <q>{$q}</q>
          <start>{$start}</start>
          <maxResults>{$maxResults}</maxResults>
          <filter>{$filter}</filter>
          <restrict>{$restrict}</restrict>
          <safeSearch>{$safeSearch}</safeSearch>
          <lr>{$lr}</lr>
          <ie>{$ie}</ie>
          <oe>{$oe}</oe>
        </xns:doGoogleSearch>
      </soapenv:Body>
    </soapenv:Envelope>,
      $output :=
    glx:soap-call($u, "POST", "SoapAction: urn:GoogleSearchAction", $input)
  return
    if ($output//
         *:Fault
          [fn:namespace-uri(.) = "http://schemas.xmlsoap.org/soap/envelope/"]
           )
    then fn:error($output/node())
    else $output/node()/node()/node()/node()[fn:normalize-space(.) != ""]
};
declare function
  GoogleSearch:doSpellingSuggestion.1(
  $phrase as item()*,
  $key as item()*,
  $u as xsd:anyURI*
) as item()* {
  let $input :=
    <soapenv:Envelope
      xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/">
      <soapenv:Body>
        <xns:doSpellingSuggestion
          xmlns:xns="urn:GoogleSearch"
          soapenv:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/">
          <key>{$key}</key>
          <phrase>{$phrase}</phrase>
        </xns:doSpellingSuggestion>
      </soapenv:Body>
    </soapenv:Envelope>,
      $output :=
    glx:soap-call($u, "POST", "SoapAction: urn:GoogleSearchAction", $input)
  return
    if ($output//
         *:Fault
          [fn:namespace-uri(.) = "http://schemas.xmlsoap.org/soap/envelope/"]
           )
    then fn:error($output/node())
    else $output/node()/node()/node()/node()[fn:normalize-space(.) != ""]
};
declare function
  GoogleSearch:doGetCachedPage.1(
  $url as item()*,
  $key as item()*,
  $u as xsd:anyURI*
) as item()* {
  let $input :=
    <soapenv:Envelope
      xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/">
      <soapenv:Body>
        <xns:doGetCachedPage
          xmlns:xns="urn:GoogleSearch"
          soapenv:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/">
          <key>{$key}</key>
          <url>{$url}</url>
        </xns:doGetCachedPage>
      </soapenv:Body>
    </soapenv:Envelope>,
      $output :=
    glx:soap-call($u, "POST", "SoapAction: urn:GoogleSearchAction", $input)
  return
    if ($output//
         *:Fault
          [fn:namespace-uri(.) = "http://schemas.xmlsoap.org/soap/envelope/"]
           )
    then fn:error($output/node())
    else $output/node()/node()/node()/node()[fn:normalize-space(.) != ""]
};
declare function
  GoogleSearch:doGoogleSearch(
  $oe as item()*,
  $ie as item()*,
  $lr as item()*,
  $safeSearch as item()*,
  $restrict as item()*,
  $filter as item()*,
  $maxResults as item()*,
  $start as item()*,
  $q as item()*,
  $key as item()*
) as item()* {
  GoogleSearch:doGoogleSearch.1(
    $oe,
    $ie,
    $lr,
    $safeSearch,
    $restrict,
    $filter,
    $maxResults,
    $start,
    $q,
    $key,
    xsd:anyURI("http://api.google.com/search/beta2")
  )
};
declare function
  GoogleSearch:doSpellingSuggestion(
  $phrase as item()*,
  $key as item()*
) as item()* {
  GoogleSearch:doSpellingSuggestion.1(
    $phrase,
    $key,
    xsd:anyURI("http://api.google.com/search/beta2")
  )
};
declare function
  GoogleSearch:doGetCachedPage(
  $url as item()*,
  $key as item()*
) as item()* {
  GoogleSearch:doGetCachedPage.1(
    $url,
    $key,
    xsd:anyURI("http://api.google.com/search/beta2")
  )
};
