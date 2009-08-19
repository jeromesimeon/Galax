declare namespace ForecastByICAO = "/home/nicnic/Work/services/misc/ws/ForecastByICAO.wsdl";
declare namespace http = "http://schemas.xmlsoap.org/wsdl/http/";
declare namespace soap = "http://schemas.xmlsoap.org/wsdl/soap/";
declare namespace s = "http://www.w3.org/2001/XMLSchema";
declare namespace s0 = "http://innergears.com/WebServices/ForecastByICAO";
declare namespace soapenc = "http://schemas.xmlsoap.org/soap/encoding/";
declare namespace tm = "http://microsoft.com/wsdl/mime/textMatching/";
declare namespace mime = "http://schemas.xmlsoap.org/wsdl/mime/";
declare function
  ForecastByICAO:GetForecastByICAO.1(
  $parameters as item()*,
  $u as xs:anyURI*
) as item()* {
  let $input :=
    <soapenv:Envelope
      xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/">
      <soapenv:Body>{$parameters}</soapenv:Body>
    </soapenv:Envelope>,
      $output :=
    glx:soap-call(
      $u,
      "POST",
      "SoapAction: http://innergears.com/WebServices/ForecastByICAO/GetForecastByICAO",
      $input
    )
  return
    if ($output//
         *:Fault
          [fn:namespace-uri(.) = "http://schemas.xmlsoap.org/soap/envelope/"]
           )
    then fn:error($output/node())
    else $output/node()/node()/node()[fn:normalize-space(.) != ""]
};
declare function ForecastByICAO:GetForecastByICAO($parameters as item()*) as item()* {
  ForecastByICAO:GetForecastByICAO.1(
    $parameters,
    xs:anyURI(
      "http://www.innergears.com/WebServices/ForecastByICAO/ForecastByICAO.asmx"
    )
  )
};
