
let $x :=
 <GetForecastByICAO>
	<ICAOCode>1000</ICAOCode>
 </GetForecastByICAO>
return
ForecastByICAO:GetForecastByICAO ($x)