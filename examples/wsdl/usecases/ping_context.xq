declare namespace PingService = "PingService.wsdl";
declare namespace tns = "http://www.xmethods.net/sd/PingService.wsdl";
declare namespace soap = "http://schemas.xmlsoap.org/wsdl/soap/";
declare function
  PingService:pingHost.1(
  $hostname as item()*,
  $u as xs:anyURI*
) as item()* {
  let $input :=
    <soapenv:Envelope
      xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/">
      <soapenv:Body>
        <xns:pingHost
          xmlns:xns="urn:xmethodsSoapPing"
          soapenv:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/">
          <hostname>{$hostname}</hostname>
        </xns:pingHost>
      </soapenv:Body>
    </soapenv:Envelope>,
      $output :=
    glx:soap-call(
      $u,
      "POST",
      "SoapAction: urn:xmethodsSoapPing#pingHost",
      $input
    )
  return
    if ($output//
         *:Fault
          [fn:namespace-uri(.) = "http://schemas.xmlsoap.org/soap/envelope/"]
           )
    then fn:error($output/node())
    else $output/node()/node()/node()/node()[fn:normalize-space(.) != ""]
};
declare function PingService:pingHost($hostname as item()*) as item()* {
  PingService:pingHost.1(
    $hostname,
    xs:anyURI("http://services.xmethods.net:80/perl/soaplite.cgi")
  )
};
