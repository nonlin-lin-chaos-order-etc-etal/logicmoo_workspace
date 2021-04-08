/*

 _________________________________________________________________________
|	Copyright (C) 1982						  |
|									  |
|	David Warren,							  |
|		SRI International, 333 Ravenswood Ave., Menlo Park,	  |
|		California 94025, USA;					  |
|									  |
|	Fernando Pereira,						  |
|		Dept. of Architecture, University of Edinburgh,		  |
|		20 Chambers St., Edinburgh EH1 1JZ, Scotland		  |
|									  |
|	This program may be used, copied, altered or included in other	  |
|	programs only for academic purposes and provided that the	  |
|	authorship of the initial program is aknowledged.		  |
|	Use for commercial purposes without the previous written 	  |
|	agreement of the authors is forbidden.				  |
|_________________________________________________________________________|

*/

% Inversion of the 'in' relation.
% ------------------------------

contains(X,Y) :- directly_contains(X,Y).
contains(X,Y) :- directly_contains(X,W), contains(W,Y).

% directly_contains(Country,CityOrRiver):- in_country(Country,CityOrRiver), ( \+ route_spatial(_,river,_,CityOrRiver,_) -> Place=city ; Place=river).
directly_contains(Continent,Region):- continent_region(Continent,Region).
directly_contains(Region,Country):- region_country(Region,Country).
directly_contains(Country,CityOrRiver):- in_country(Country,CityOrRiver).

/*
:- expects_dialect(pfc).

continent_region(Continent,Region) ==> (ti(region,Region),ti(continent,Continent)).
region_country(Region,Country) ==> (ti(region,Region),ti(country,Country)).
% Arg2 might be a river or city
in_country(Country,_) ==> ti(country,Country).
*/

continent_region(africa,central_africa).
continent_region(africa,east_africa).
continent_region(africa,north_africa).
continent_region(africa,southern_africa).
continent_region(africa,west_africa).

continent_region(america,caribbean).
continent_region(america,central_america).
continent_region(america,north_america).
continent_region(america,south_america).

continent_region(asia,far_east).
continent_region(asia,indian_subcontinent).
continent_region(asia,middle_east).
continent_region(asia,northern_asia).
continent_region(asia,southeast_east).

continent_region(australasia,oceania).

region_country(oceania,australia).
region_country(oceania,fiji).
region_country(oceania,new_zealand).
region_country(oceania,papua_new_guinea).
region_country(oceania,tonga).
region_country(oceania,western_samoa).

continent_region(europe,eastern_europe).
continent_region(europe,scandinavia).
continent_region(europe,southern_europe).
continent_region(europe,western_europe).


region_country(scandinavia,denmark).
region_country(scandinavia,finland).
region_country(scandinavia,norway).
region_country(scandinavia,sweden).

region_country(western_europe,austria).
region_country(western_europe,belgium).
region_country(western_europe,eire).
region_country(western_europe,france).
region_country(western_europe,iceland).
region_country(western_europe,liechtenstein).
region_country(western_europe,luxembourg).
region_country(western_europe,netherlands).
region_country(western_europe,switzerland).
region_country(western_europe,united_kingdom).
region_country(western_europe,west_germany).

region_country(eastern_europe,bulgaria).
region_country(eastern_europe,czechoslovakia).
region_country(eastern_europe,east_germany).
region_country(eastern_europe,hungary).
region_country(eastern_europe,poland).
region_country(eastern_europe,romania).

region_country(southern_europe,albania).
region_country(southern_europe,andorra).
region_country(southern_europe,cyprus).
region_country(southern_europe,greece).
region_country(southern_europe,italy).
region_country(southern_europe,malta).
region_country(southern_europe,monaco).
region_country(southern_europe,portugal).
region_country(southern_europe,san_marino).
region_country(southern_europe,spain).
region_country(southern_europe,yugoslavia).

region_country(north_america,canada).
region_country(north_america,united_states).

region_country(central_america,belize).
region_country(central_america,costa_rica).
region_country(central_america,el_salvador).
region_country(central_america,guatemala).
region_country(central_america,honduras).
region_country(central_america,mexico).
region_country(central_america,nicaragua).
region_country(central_america,panama).

region_country(caribbean,bahamas).
region_country(caribbean,barbados).
region_country(caribbean,cuba).
region_country(caribbean,dominican_republic).
region_country(caribbean,grenada).
region_country(caribbean,haiti).
region_country(caribbean,jamaica).
region_country(caribbean,trinidad_and_tobago).

region_country(south_america,argentina).
region_country(south_america,bolivia).
region_country(south_america,brazil).
region_country(south_america,chile).
region_country(south_america,colombia).
region_country(south_america,ecuador).
region_country(south_america,french_guiana).
region_country(south_america,guyana).
region_country(south_america,paraguay).
region_country(south_america,peru).
region_country(south_america,surinam).
region_country(south_america,uruguay).
region_country(south_america,venezuela).

region_country(north_africa,algeria).
region_country(north_africa,egypt).
region_country(north_africa,libya).
region_country(north_africa,morocco).
region_country(north_africa,tunisia).

region_country(west_africa,cameroon).
region_country(west_africa,dahomey).
region_country(west_africa,equatorial_guinea).
region_country(west_africa,gambia).
region_country(west_africa,ghana).
region_country(west_africa,guinea).
region_country(west_africa,guinea_bissau).
region_country(west_africa,ivory_coast).
region_country(west_africa,liberia).
region_country(west_africa,mali).
region_country(west_africa,mauritania).
region_country(west_africa,niger).
region_country(west_africa,nigeria).
region_country(west_africa,senegal).
region_country(west_africa,sierra_leone).
region_country(west_africa,togo).
region_country(west_africa,upper_volta).

region_country(central_africa,burundi).
region_country(central_africa,central_african_republic).
region_country(central_africa,chad).
region_country(central_africa,congo).
region_country(central_africa,gabon).
region_country(central_africa,rwanda).
region_country(central_africa,sudan).
region_country(central_africa,zaire).

region_country(east_africa,djibouti).
region_country(east_africa,ethiopia).
region_country(east_africa,kenya).
region_country(east_africa,seychelles).
region_country(east_africa,somalia).
region_country(east_africa,tanzania).
region_country(east_africa,uganda).

region_country(southern_africa,angola).
region_country(southern_africa,botswana).
region_country(southern_africa,lesotho).
region_country(southern_africa,malagasy).
region_country(southern_africa,malawi).
region_country(southern_africa,mauritius).
region_country(southern_africa,mozambique).
region_country(southern_africa,south_africa).
region_country(southern_africa,swaziland).
region_country(southern_africa,zambia).
region_country(southern_africa,zimbabwe).

region_country(middle_east,bahrain).
region_country(middle_east,iran).
region_country(middle_east,iraq).
region_country(middle_east,israel).
region_country(middle_east,jordan).
region_country(middle_east,kuwait).
region_country(middle_east,lebanon).
region_country(middle_east,oman).
region_country(middle_east,qatar).
region_country(middle_east,saudi_arabia).
region_country(middle_east,south_yemen).
region_country(middle_east,syria).
region_country(middle_east,turkey).
region_country(middle_east,united_arab_emirates).
region_country(middle_east,yemen).

region_country(indian_subcontinent,afghanistan).
region_country(indian_subcontinent,bangladesh).
region_country(indian_subcontinent,bhutan).
region_country(indian_subcontinent,india).
region_country(indian_subcontinent,maldives).
region_country(indian_subcontinent,nepal).
region_country(indian_subcontinent,pakistan).
region_country(indian_subcontinent,sri_lanka).

region_country(southeast_east,burma).
region_country(southeast_east,cambodia).
region_country(southeast_east,indonesia).
region_country(southeast_east,laos).
region_country(southeast_east,malaysia).
region_country(southeast_east,philippines).
region_country(southeast_east,singapore).
region_country(southeast_east,thailand).
region_country(southeast_east,vietnam).

region_country(far_east,china).
region_country(far_east,japan).
region_country(far_east,north_korea).
region_country(far_east,south_korea).
region_country(far_east,taiwan).

region_country(northern_asia,mongolia).
region_country(northern_asia,soviet_union).




in_country(afghanistan,amu_darya).

in_country(angola,cubango).
in_country(angola,zambesi).

in_country(argentina,buenos_aires).
in_country(argentina,parana).

in_country(australia,melbourne).
in_country(australia,murray).
in_country(australia,sydney).

in_country(austria,danube).
in_country(austria,vienna).

in_country(bangladesh,brahmaputra).

in_country(belgium,brussels).

in_country(brazil,amazon).
in_country(brazil,parana).
in_country(brazil,rio_de_janeiro).
in_country(brazil,sao_paulo).

in_country(burma,irrawaddy).
in_country(burma,salween).

in_country(cambodia,mekong).

in_country(canada,mackenzie).
in_country(canada,montreal).
in_country(canada,toronto).
in_country(canada,yukon).

in_country(chile,santiago).

in_country(china,amur).
in_country(china,brahmaputra).
in_country(china,canton).
in_country(china,chungking).
in_country(china,dairen).
in_country(china,ganges).
in_country(china,harbin).
in_country(china,hwang_ho).
in_country(china,indus).
in_country(china,kowloon).
in_country(china,mekong).
in_country(china,mukden).
in_country(china,peking).
in_country(china,salween).
in_country(china,shanghai).
in_country(china,sian).
in_country(china,tientsin).
in_country(china,yangtze).

in_country(colombia,orinoco).

in_country(czechoslovakia,danube).
in_country(czechoslovakia,elbe).
in_country(czechoslovakia,oder).

in_country(east_germany,berlin).
in_country(east_germany,elbe).

in_country(egypt,cairo).
in_country(egypt,nile).

in_country(france,paris).
in_country(france,rhone).

in_country(ghana,volta).

in_country(greece,athens).

in_country(guinea,niger_river).
in_country(guinea,senegal_river).

in_country(hungary,budapest).
in_country(hungary,danube).

in_country(india,bombay).
in_country(india,calcutta).
in_country(india,delhi).
in_country(india,ganges).
in_country(india,hyderabad).
in_country(india,indus).
in_country(india,madras).

in_country(indonesia,jakarta).

in_country(iran,tehran).

in_country(iraq,euphrates).

in_country(italy,milan).
in_country(italy,naples).
in_country(italy,rome).

in_country(japan,kobe).
in_country(japan,kyoto).
in_country(japan,nagoya).
in_country(japan,nanking).
in_country(japan,osaka).
in_country(japan,tokyo).
in_country(japan,yokohama).

in_country(laos,mekong).

in_country(lesotho,orange).

in_country(mali,niger_river).
in_country(mali,senegal_river).

in_country(mexico,colorado).
in_country(mexico,mexico_city).
in_country(mexico,rio_grande).

in_country(mongolia,amur).
in_country(mongolia,yenisei).

in_country(mozambique,limpopo).
in_country(mozambique,zambesi).

in_country(netherlands,rhine).

in_country(niger,niger_river).

in_country(nigeria,niger_river).

in_country(pakistan,indus).
in_country(pakistan,karachi).

in_country(paraguay,parana).

in_country(peru,amazon).
in_country(peru,lima).

in_country(philippines,manila).

in_country(poland,oder).
in_country(poland,vistula).
in_country(poland,warsaw).

in_country(portugal,tagus).

in_country(romania,bucharest).
in_country(romania,danube).

in_country(senegal,senegal_river).

in_country(singapore,singapore_city).

in_country(south_africa,cubango).
in_country(south_africa,johannesburg).
in_country(south_africa,limpopo).
in_country(south_africa,orange).

in_country(south_korea,pusan).
in_country(south_korea,seoul).

in_country(soviet_union,amu_darya).
in_country(soviet_union,amur).
in_country(soviet_union,don).
in_country(soviet_union,kiev).
in_country(soviet_union,lena).
in_country(soviet_union,leningrad).
in_country(soviet_union,moscow).
in_country(soviet_union,ob).
in_country(soviet_union,volga).
in_country(soviet_union,yenisei).

in_country(spain,barcelona).
in_country(spain,madrid).
in_country(spain,tagus).

in_country(sudan,nile).

in_country(switzerland,rhine).
in_country(switzerland,rhone).

in_country(syria,euphrates).

in_country(thailand,bangkok).

in_country(turkey,euphrates).
in_country(turkey,istanbul).

in_country(uganda,nile).

in_country(united_kingdom,birmingham).
in_country(united_kingdom,glasgow).
in_country(united_kingdom,london).

in_country(united_states,chicago).
in_country(united_states,colorado).
in_country(united_states,detroit).
in_country(united_states,los_angeles).
in_country(united_states,mississippi).
in_country(united_states,new_york).
in_country(united_states,philadelphia).
in_country(united_states,rio_grande).
in_country(united_states,yukon).

in_country(upper_volta,volta).

in_country(venezuela,caracas).
in_country(venezuela,orinoco).

in_country(vietnam,mekong).
in_country(vietnam,saigon).

in_country(west_germany,danube).
in_country(west_germany,elbe).
in_country(west_germany,hamburg).
in_country(west_germany,rhine).

in_country(yugoslavia,danube).

in_country(zaire,congo_river).

in_country(zambia,congo_river).
in_country(zambia,zambesi).

:- fixup_exports.
