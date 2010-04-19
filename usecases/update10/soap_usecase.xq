(: Q1: Produce a SOAP response to a message. :)
(: Q1: Check to see if the airports are unambiguously specified in the incoming message. Produce a SOAP response by transforming the incoming message, modifying the time and date to the current time, and replacing the body with a request to clarify which airport should be used for New York City. :)
declare namespace 
  env="http://www.w3.org/2003/05/soap-envelope";
declare namespace 
  m="http://travelcompany.example.org/reservation";
declare namespace 
  n="http://mycompany.example.com/employees";
declare namespace 
  p="http://travelcompany.example.org/reservation/travel";
(:
    Nicola: we don't use
            fn:current-dateTime()
            as in the real usecase, because it would be
            hard to do automatic regression tests
:)
declare variable $current_dateTime := xs:dateTime("2006-08-14T10:56:42-05:00");

(:  A clarification is needed only if there are no
 :  airports or more than one for a given city. If
 :  there is precisely one, there is no need to
 :  ask for information on that city.
 :)

declare function local:airportChoices($city as xs:string)
{
  let $airports := collection("../docs/airports")/airports/AIRPORT[CITY = $city]
  return
    if (count($airports) = 0)
    then 
       <error> No airports found for {$city}!</error>
    else if (count($airports) > 1) 
    then 
       <airportChoices>
        { 
          for $c in $airports/CODE
          return (string( $c ), " ")
        }
       </airportChoices>
    else ()
};

(:  Make sure that each airport is unambiguous. If there is
 :  more than one airport for a city, ask for clarification.
 :
 :  The primer only shows the error condition, so it is not
 :  clear what to do if there are no errors. Here, we simply
 :  return the airports in the itinerary.
 :)

declare function local:airports($in as element(env:Envelope))
{
    let $departureDeparting := 
      $in//p:departure/p:departing
    let $departureDepartingAirports := 
      collection("../docs/airports")/airports/AIRPORT[CITY = $departureDeparting]
    let $departureArriving := 
      $in//p:departure/p:arriving
    let $departureArrivingAirports := 
      collection("../docs/airports")/airports/AIRPORT[CITY = $departureArriving]
    let $returnDeparting := 
      $in//p:return/p:departing
    let $returnDepartingAirports := 
      collection("../docs/airports")/airports/AIRPORT[CITY = $returnDeparting]
    let $returnArriving := 
      $in//p:return/p:arriving
    let $returnArrivingAirports := 
      collection("../docs/airports")/airports/AIRPORT[CITY = $returnArriving]
    return
       if ( count($departureDepartingAirports)=0 or 
            count($departureDepartingAirports)>1 or 
            count($departureArrivingAirports)=0 or 
            count($departureArrivingAirports)>1 or 
            count($returnDepartingAirports)=0 or 
            count($returnDepartingAirports)>1 or 
            count($returnArrivingAirports)=0 or 
            count($returnArrivingAirports)>1 )
         then
          <p:itineraryClarification>
            <p:departure>
              <p:departing>
                { local:airportChoices($departureDeparting) }
              </p:departing>
              <p:arriving>
                { local:airportChoices($departureArriving) }
              </p:arriving> 
            </p:departure>
            <p:return>
              <p:departing>
                { local:airportChoices($returnDeparting) }
              </p:departing>
              <p:arriving>
                { local:airportChoices($returnArriving) }
              </p:arriving>
            </p:return>
          </p:itineraryClarification>
         else 
          <p:itinerary>
            <p:departure>
              <p:departing>{$departureDeparting}</p:departing>
              <p:arriving>{$departureArriving}</p:arriving>
            </p:departure>
            <p:return>
              <p:departing>{$returnDeparting}</p:departing>
              <p:arriving>{$returnArriving}</p:arriving>
            </p:return>
          </p:itinerary>
};

declare variable $msg := doc("../docs/soap_input.xml");

copy $out := $msg/env:Envelope
modify (
  replace value of node $out//m:dateAndTime 
     with $current_dateTime,
  replace node $out//env:Body 
  with <env:Body>
        { local:airports($out) }
       </env:Body>
)
return $out
