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
borders(X,C):- nonvar(X),nonvar(C),!,borders1(X,C),!.
borders(X,C):- borders1(X,C).

:- dynamic(direct_border/2).

borders1(X,C) :- direct_border(X,C).
borders1(X,C) :- direct_border(C,X).

add_borders(X,C):- X @> C, !, add_borders(C,X).
add_borders(X,C):- direct_border(X,C), !.
add_borders(X,C):- assertz(direct_border(X,C)), !.

% Facts about Europe.
% ------------------

:-add_borders(albania,greece).
:-add_borders(albania,yugoslavia).
:-add_borders(albania,mediterranean).

:-add_borders(andorra,france).
:-add_borders(andorra,spain).

:-add_borders(austria,czechoslovakia).
:-add_borders(austria,hungary).
:-add_borders(austria,italy).
:-add_borders(austria,liechtenstein).
:-add_borders(austria,switzerland).
:-add_borders(austria,west_germany).
:-add_borders(austria,yugoslavia).

:-add_borders(belgium,france).
:-add_borders(belgium,luxembourg).
:-add_borders(belgium,netherlands).
:-add_borders(belgium,west_germany).
:-add_borders(belgium,atlantic).

:-add_borders(bulgaria,greece).
:-add_borders(bulgaria,romania).
:-add_borders(bulgaria,turkey).
:-add_borders(bulgaria,yugoslavia).
:-add_borders(bulgaria,black_sea).

:-add_borders(cyprus,mediterranean).

:-add_borders(czechoslovakia,austria).
:-add_borders(czechoslovakia,east_germany).
:-add_borders(czechoslovakia,hungary).
:-add_borders(czechoslovakia,poland).
:-add_borders(czechoslovakia,soviet_union).
:-add_borders(czechoslovakia,west_germany).

:-add_borders(denmark,west_germany).
:-add_borders(denmark,atlantic).
:-add_borders(denmark,baltic).

:-add_borders(eire,united_kingdom).
:-add_borders(eire,atlantic).

:-add_borders(finland,norway).
:-add_borders(finland,soviet_union).
:-add_borders(finland,sweden).
:-add_borders(finland,baltic).

:-add_borders(france,andorra).
:-add_borders(france,belgium).
:-add_borders(france,italy).
:-add_borders(france,luxembourg).
:-add_borders(france,monaco).
:-add_borders(france,spain).
:-add_borders(france,switzerland).
:-add_borders(france,west_germany).
:-add_borders(france,atlantic).
:-add_borders(france,mediterranean).

:-add_borders(east_germany,czechoslovakia).
:-add_borders(east_germany,poland).
:-add_borders(east_germany,west_germany).
:-add_borders(east_germany,baltic).

:-add_borders(greece,albania).
:-add_borders(greece,bulgaria).
:-add_borders(greece,turkey).
:-add_borders(greece,yugoslavia).
:-add_borders(greece,mediterranean).

:-add_borders(hungary,austria).
:-add_borders(hungary,czechoslovakia).
:-add_borders(hungary,romania).
:-add_borders(hungary,soviet_union).
:-add_borders(hungary,yugoslavia).

:-add_borders(iceland,atlantic).

:-add_borders(italy,austria).
:-add_borders(italy,france).
:-add_borders(italy,san_marino).
:-add_borders(italy,switzerland).
:-add_borders(italy,yugoslavia).
:-add_borders(italy,mediterranean).

:-add_borders(liechtenstein,austria).
:-add_borders(liechtenstein,switzerland).

:-add_borders(luxembourg,belgium).
:-add_borders(luxembourg,france).
:-add_borders(luxembourg,west_germany).

:-add_borders(malta,mediterranean).

:-add_borders(monaco,france).
:-add_borders(monaco,mediterranean).

:-add_borders(netherlands,belgium).
:-add_borders(netherlands,west_germany).
:-add_borders(netherlands,atlantic).

:-add_borders(norway,finland).
:-add_borders(norway,sweden).
:-add_borders(norway,soviet_union).
:-add_borders(norway,arctic_ocean).
:-add_borders(norway,atlantic).

:-add_borders(poland,czechoslovakia).
:-add_borders(poland,east_germany).
:-add_borders(poland,soviet_union).
:-add_borders(poland,baltic).

:-add_borders(portugal,spain).
:-add_borders(portugal,atlantic).

:-add_borders(romania,bulgaria).
:-add_borders(romania,hungary).
:-add_borders(romania,soviet_union).
:-add_borders(romania,yugoslavia).
:-add_borders(romania,black_sea).

:-add_borders(san_marino,italy).
:-add_borders(san_marino,mediterranean).

:-add_borders(spain,andorra).
:-add_borders(spain,france).
:-add_borders(spain,portugal).
:-add_borders(spain,atlantic).
:-add_borders(spain,mediterranean).

:-add_borders(sweden,finland).
:-add_borders(sweden,norway).
:-add_borders(sweden,atlantic).
:-add_borders(sweden,baltic).

:-add_borders(switzerland,austria).
:-add_borders(switzerland,france).
:-add_borders(switzerland,italy).
:-add_borders(switzerland,liechtenstein).
:-add_borders(switzerland,west_germany).

:-add_borders(west_germany,austria).
:-add_borders(west_germany,belgium).
:-add_borders(west_germany,czechoslovakia).
:-add_borders(west_germany,denmark).
:-add_borders(west_germany,east_germany).
:-add_borders(west_germany,france).
:-add_borders(west_germany,luxembourg).
:-add_borders(west_germany,netherlands).
:-add_borders(west_germany,switzerland).
:-add_borders(west_germany,atlantic).
:-add_borders(west_germany,baltic).

:-add_borders(united_kingdom,eire).
:-add_borders(united_kingdom,atlantic).

:-add_borders(yugoslavia,albania).
:-add_borders(yugoslavia,austria).
:-add_borders(yugoslavia,bulgaria).
:-add_borders(yugoslavia,greece).
:-add_borders(yugoslavia,hungary).
:-add_borders(yugoslavia,italy).
:-add_borders(yugoslavia,romania).
:-add_borders(yugoslavia,mediterranean).

% Facts about Asia.
% ----------------

:-add_borders(afghanistan,china).
:-add_borders(afghanistan,iran).
:-add_borders(afghanistan,pakistan).
:-add_borders(afghanistan,soviet_union).

:-add_borders(bahrain,persian_gulf).

:-add_borders(bangladesh,burma).
:-add_borders(bangladesh,india).
:-add_borders(bangladesh,indian_ocean).

:-add_borders(bhutan,china).
:-add_borders(bhutan,india).

:-add_borders(burma,bangladesh).
:-add_borders(burma,china).
:-add_borders(burma,india).
:-add_borders(burma,laos).
:-add_borders(burma,thailand).
:-add_borders(burma,indian_ocean).

:-add_borders(cambodia,laos).
:-add_borders(cambodia,thailand).
:-add_borders(cambodia,vietnam).
:-add_borders(cambodia,pacific).

:-add_borders(china,afghanistan).
:-add_borders(china,bhutan).
:-add_borders(china,burma).
:-add_borders(china,india).
:-add_borders(china,laos).
:-add_borders(china,mongolia).
:-add_borders(china,nepal).
:-add_borders(china,north_korea).
:-add_borders(china,pakistan).
:-add_borders(china,soviet_union).
:-add_borders(china,vietnam).
:-add_borders(china,pacific).

:-add_borders(india,bangladesh).
:-add_borders(india,bhutan).
:-add_borders(india,burma).
:-add_borders(india,china).
:-add_borders(india,nepal).
:-add_borders(india,pakistan).
:-add_borders(india,indian_ocean).

:-add_borders(indonesia,malaysia).
:-add_borders(indonesia,papua_new_guinea).
:-add_borders(indonesia,indian_ocean).
:-add_borders(indonesia,pacific).

:-add_borders(iran,afghanistan).
:-add_borders(iran,iraq).
:-add_borders(iran,pakistan).
:-add_borders(iran,soviet_union).
:-add_borders(iran,turkey).
:-add_borders(iran,caspian).
:-add_borders(iran,persian_gulf).
:-add_borders(iran,indian_ocean).

:-add_borders(iraq,iran).
:-add_borders(iraq,jordan).
:-add_borders(iraq,kuwait).
:-add_borders(iraq,saudi_arabia).
:-add_borders(iraq,syria).
:-add_borders(iraq,turkey).
:-add_borders(iraq,persian_gulf).

:-add_borders(israel,egypt).
:-add_borders(israel,jordan).
:-add_borders(laos,burma).
:-add_borders(laos,cambodia).
:-add_borders(laos,china).
:-add_borders(laos,thailand).
:-add_borders(laos,vietnam).

:-add_borders(israel,lebanon).
:-add_borders(israel,syria).
:-add_borders(israel,mediterranean).
:-add_borders(israel,red_sea).

:-add_borders(japan,pacific).

:-add_borders(jordan,iraq).
:-add_borders(jordan,israel).
:-add_borders(jordan,saudi_arabia).
:-add_borders(jordan,syria).
:-add_borders(jordan,red_sea).

:-add_borders(kuwait,iraq).
:-add_borders(kuwait,saudi_arabia).
:-add_borders(kuwait,persian_gulf).

:-add_borders(lebanon,israel).
:-add_borders(lebanon,syria).
:-add_borders(lebanon,mediterranean).

:-add_borders(malaysia,indonesia).
:-add_borders(malaysia,singapore).
:-add_borders(malaysia,thailand).
:-add_borders(malaysia,indian_ocean).
:-add_borders(malaysia,pacific).

:-add_borders(maldives,indian_ocean).

:-add_borders(mongolia,china).
:-add_borders(mongolia,soviet_union).

:-add_borders(nepal,china).
:-add_borders(nepal,india).

:-add_borders(north_korea,china).
:-add_borders(north_korea,south_korea).
:-add_borders(north_korea,soviet_union).
:-add_borders(north_korea,pacific).

:-add_borders(oman,saudi_arabia).
:-add_borders(oman,united_arab_emirates).
:-add_borders(oman,south_yemen).
:-add_borders(oman,indian_ocean).

:-add_borders(pakistan,afghanistan).
:-add_borders(pakistan,china).
:-add_borders(pakistan,india).
:-add_borders(pakistan,iran).
:-add_borders(pakistan,indian_ocean).

:-add_borders(philippines,pacific).

:-add_borders(qatar,saudi_arabia).
:-add_borders(qatar,united_arab_emirates).
:-add_borders(qatar,persian_gulf).

:-add_borders(saudi_arabia,iraq).
:-add_borders(saudi_arabia,jordan).
:-add_borders(saudi_arabia,kuwait).
:-add_borders(saudi_arabia,oman).
:-add_borders(saudi_arabia,qatar).
:-add_borders(saudi_arabia,south_yemen).
:-add_borders(saudi_arabia,united_arab_emirates).
:-add_borders(saudi_arabia,yemen).
:-add_borders(saudi_arabia,persian_gulf).
:-add_borders(saudi_arabia,red_sea).

:-add_borders(singapore,malaysia).
:-add_borders(singapore,pacific).

:-add_borders(south_korea,north_korea).
:-add_borders(south_korea,pacific).

:-add_borders(south_yemen,oman).
:-add_borders(south_yemen,saudi_arabia).
:-add_borders(south_yemen,yemen).
:-add_borders(south_yemen,indian_ocean).

:-add_borders(soviet_union,afghanistan).
:-add_borders(soviet_union,china).
:-add_borders(soviet_union,czechoslovakia).
:-add_borders(soviet_union,finland).
:-add_borders(soviet_union,hungary).
:-add_borders(soviet_union,iran).
:-add_borders(soviet_union,mongolia).
:-add_borders(soviet_union,north_korea).
:-add_borders(soviet_union,norway).
:-add_borders(soviet_union,poland).
:-add_borders(soviet_union,romania).
:-add_borders(soviet_union,turkey).
:-add_borders(soviet_union,arctic_ocean).
:-add_borders(soviet_union,baltic).
:-add_borders(soviet_union,black_sea).
:-add_borders(soviet_union,caspian).
:-add_borders(soviet_union,pacific).

:-add_borders(sri_lanka,indian_ocean).

:-add_borders(syria,iraq).
:-add_borders(syria,israel).
:-add_borders(syria,jordan).
:-add_borders(syria,lebanon).
:-add_borders(syria,turkey).
:-add_borders(syria,mediterranean).

:-add_borders(taiwan,pacific).

:-add_borders(thailand,burma).
:-add_borders(thailand,cambodia).
:-add_borders(thailand,laos).
:-add_borders(thailand,malaysia).
:-add_borders(thailand,indian_ocean).
:-add_borders(thailand,pacific).

:-add_borders(turkey,bulgaria).
:-add_borders(turkey,greece).
:-add_borders(turkey,iran).
:-add_borders(turkey,iraq).
:-add_borders(turkey,soviet_union).
:-add_borders(turkey,syria).
:-add_borders(turkey,black_sea).
:-add_borders(turkey,mediterranean).

:-add_borders(united_arab_emirates,oman).
:-add_borders(united_arab_emirates,qatar).
:-add_borders(united_arab_emirates,saudi_arabia).
:-add_borders(united_arab_emirates,persian_gulf).

:-add_borders(vietnam,cambodia).
:-add_borders(vietnam,china).
:-add_borders(vietnam,laos).
:-add_borders(vietnam,pacific).

:-add_borders(yemen,south_yemen).
:-add_borders(yemen,saudi_arabia).
:-add_borders(yemen,red_sea).

% Facts about Africa.
% ------------------

:-add_borders(algeria,libya).
:-add_borders(algeria,mali).
:-add_borders(algeria,mauritania).
:-add_borders(algeria,morocco).
:-add_borders(algeria,niger).
:-add_borders(algeria,tunisia).
:-add_borders(algeria,mediterranean).

:-add_borders(angola,congo).
:-add_borders(angola,south_africa).
:-add_borders(angola,zaire).
:-add_borders(angola,zambia).
:-add_borders(angola,atlantic).

:-add_borders(botswana,south_africa).
:-add_borders(botswana,zimbabwe).

:-add_borders(burundi,rwanda).
:-add_borders(burundi,tanzania).
:-add_borders(burundi,zaire).

:-add_borders(cameroon,central_african_republic).
:-add_borders(cameroon,chad).
:-add_borders(cameroon,congo).
:-add_borders(cameroon,equatorial_guinea).
:-add_borders(cameroon,gabon).
:-add_borders(cameroon,nigeria).
:-add_borders(cameroon,atlantic).

:-add_borders(central_african_republic,cameroon).
:-add_borders(central_african_republic,chad).
:-add_borders(central_african_republic,congo).
:-add_borders(central_african_republic,sudan).
:-add_borders(central_african_republic,zaire).

:-add_borders(chad,cameroon).
:-add_borders(chad,central_african_republic).
:-add_borders(chad,libya).
:-add_borders(chad,niger).
:-add_borders(chad,nigeria).
:-add_borders(chad,sudan).

:-add_borders(congo,angola).
:-add_borders(congo,cameroon).
:-add_borders(congo,central_african_republic).
:-add_borders(congo,gabon).
:-add_borders(congo,zaire).
:-add_borders(congo,atlantic).

:-add_borders(dahomey,niger).
:-add_borders(dahomey,nigeria).
:-add_borders(dahomey,togo).
:-add_borders(dahomey,upper_volta).
:-add_borders(dahomey,atlantic).

:-add_borders(djibouti,ethiopia).
:-add_borders(djibouti,somalia).
:-add_borders(djibouti,indian_ocean).

:-add_borders(egypt,israel).
:-add_borders(egypt,libya).
:-add_borders(egypt,sudan).
:-add_borders(egypt,mediterranean).
:-add_borders(egypt,red_sea).

:-add_borders(equatorial_guinea,cameroon).
:-add_borders(equatorial_guinea,gabon).
:-add_borders(equatorial_guinea,atlantic).

:-add_borders(ethiopia,djibouti).
:-add_borders(ethiopia,kenya).
:-add_borders(ethiopia,somalia).
:-add_borders(ethiopia,sudan).
:-add_borders(ethiopia,red_sea).

:-add_borders(gabon,cameroon).
:-add_borders(gabon,congo).
:-add_borders(gabon,equatorial_guinea).
:-add_borders(gabon,atlantic).

:-add_borders(gambia,senegal).
:-add_borders(gambia,atlantic).

:-add_borders(ghana,ivory_coast).
:-add_borders(ghana,togo).
:-add_borders(ghana,upper_volta).
:-add_borders(ghana,atlantic).

:-add_borders(guinea,guinea_bissau).
:-add_borders(guinea,ivory_coast).
:-add_borders(guinea,liberia).
:-add_borders(guinea,mali).
:-add_borders(guinea,senegal).
:-add_borders(guinea,sierra_leone).
:-add_borders(guinea,atlantic).

:-add_borders(guinea_bissau,guinea).
:-add_borders(guinea_bissau,senegal).
:-add_borders(guinea_bissau,atlantic).

:-add_borders(ivory_coast,ghana).
:-add_borders(ivory_coast,guinea).
:-add_borders(ivory_coast,liberia).
:-add_borders(ivory_coast,mali).
:-add_borders(ivory_coast,upper_volta).
:-add_borders(ivory_coast,atlantic).

:-add_borders(kenya,ethiopia).
:-add_borders(kenya,somalia).
:-add_borders(kenya,sudan).
:-add_borders(kenya,tanzania).
:-add_borders(kenya,uganda).
:-add_borders(kenya,indian_ocean).

:-add_borders(lesotho,south_africa).

:-add_borders(liberia,ivory_coast).
:-add_borders(liberia,guinea).
:-add_borders(liberia,sierra_leone).
:-add_borders(liberia,atlantic).

:-add_borders(libya,algeria).
:-add_borders(libya,chad).
:-add_borders(libya,egypt).
:-add_borders(libya,niger).
:-add_borders(libya,sudan).
:-add_borders(libya,tunisia).
:-add_borders(libya,mediterranean).

:-add_borders(malagasy,indian_ocean).

:-add_borders(malawi,mozambique).
:-add_borders(malawi,tanzania).
:-add_borders(malawi,zambia).

:-add_borders(mali,algeria).
:-add_borders(mali,guinea).
:-add_borders(mali,ivory_coast).
:-add_borders(mali,mauritania).
:-add_borders(mali,niger).
:-add_borders(mali,senegal).
:-add_borders(mali,upper_volta).

:-add_borders(mauritania,algeria).
:-add_borders(mauritania,mali).
:-add_borders(mauritania,morocco).
:-add_borders(mauritania,senegal).
:-add_borders(mauritania,atlantic).

:-add_borders(mauritius,indian_ocean).

:-add_borders(morocco,algeria).
:-add_borders(morocco,mauritania).
:-add_borders(morocco,atlantic).
:-add_borders(morocco,mediterranean).

:-add_borders(mozambique,malawi).
:-add_borders(mozambique,south_africa).
:-add_borders(mozambique,swaziland).
:-add_borders(mozambique,tanzania).
:-add_borders(mozambique,zambia).
:-add_borders(mozambique,zimbabwe).
:-add_borders(mozambique,indian_ocean).

:-add_borders(niger,algeria).
:-add_borders(niger,chad).
:-add_borders(niger,dahomey).
:-add_borders(niger,libya).
:-add_borders(niger,mali).
:-add_borders(niger,nigeria).
:-add_borders(niger,upper_volta).

:-add_borders(nigeria,cameroon).
:-add_borders(nigeria,chad).
:-add_borders(nigeria,dahomey).
:-add_borders(nigeria,niger).
:-add_borders(nigeria,atlantic).

:-add_borders(rwanda,burundi).
:-add_borders(rwanda,tanzania).
:-add_borders(rwanda,uganda).
:-add_borders(rwanda,zaire).

:-add_borders(senegal,gambia).
:-add_borders(senegal,guinea).
:-add_borders(senegal,guinea_bissau).
:-add_borders(senegal,mali).
:-add_borders(senegal,mauritania).
:-add_borders(senegal,atlantic).

:-add_borders(seychelles,indian_ocean).

:-add_borders(sierra_leone,guinea).
:-add_borders(sierra_leone,liberia).
:-add_borders(sierra_leone,atlantic).

:-add_borders(somalia,djibouti).
:-add_borders(somalia,ethiopia).
:-add_borders(somalia,kenya).
:-add_borders(somalia,indian_ocean).

:-add_borders(south_africa,angola).
:-add_borders(south_africa,botswana).
:-add_borders(south_africa,lesotho).
:-add_borders(south_africa,mozambique).
:-add_borders(south_africa,swaziland).
:-add_borders(south_africa,zambia).
:-add_borders(south_africa,zimbabwe).
:-add_borders(south_africa,atlantic).
:-add_borders(south_africa,indian_ocean).

:-add_borders(sudan,chad).
:-add_borders(sudan,central_african_republic).
:-add_borders(sudan,egypt).
:-add_borders(sudan,ethiopia).
:-add_borders(sudan,kenya).
:-add_borders(sudan,libya).
:-add_borders(sudan,uganda).
:-add_borders(sudan,zaire).
:-add_borders(sudan,red_sea).

:-add_borders(swaziland,mozambique).
:-add_borders(swaziland,south_africa).

:-add_borders(tanzania,burundi).
:-add_borders(tanzania,kenya).
:-add_borders(tanzania,malawi).
:-add_borders(tanzania,mozambique).
:-add_borders(tanzania,rwanda).
:-add_borders(tanzania,uganda).
:-add_borders(tanzania,zaire).
:-add_borders(tanzania,zambia).
:-add_borders(tanzania,indian_ocean).

:-add_borders(togo,dahomey).
:-add_borders(togo,ghana).
:-add_borders(togo,upper_volta).
:-add_borders(togo,atlantic).

:-add_borders(tunisia,algeria).
:-add_borders(tunisia,libya).
:-add_borders(tunisia,mediterranean).

:-add_borders(uganda,kenya).
:-add_borders(uganda,rwanda).
:-add_borders(uganda,sudan).
:-add_borders(uganda,tanzania).
:-add_borders(uganda,zaire).

:-add_borders(upper_volta,dahomey).
:-add_borders(upper_volta,ghana).
:-add_borders(upper_volta,ivory_coast).
:-add_borders(upper_volta,mali).
:-add_borders(upper_volta,niger).
:-add_borders(upper_volta,togo).

:-add_borders(zaire,angola).
:-add_borders(zaire,burundi).
:-add_borders(zaire,central_african_republic).
:-add_borders(zaire,congo).
:-add_borders(zaire,rwanda).
:-add_borders(zaire,sudan).
:-add_borders(zaire,tanzania).
:-add_borders(zaire,uganda).
:-add_borders(zaire,zambia).
:-add_borders(zaire,atlantic).

:-add_borders(zambia,angola).
:-add_borders(zambia,malawi).
:-add_borders(zambia,mozambique).
:-add_borders(zambia,south_africa).
:-add_borders(zambia,tanzania).
:-add_borders(zambia,zaire).
:-add_borders(zambia,zimbabwe).

:-add_borders(zimbabwe,botswana).
:-add_borders(zimbabwe,mozambique).
:-add_borders(zimbabwe,south_africa).
:-add_borders(zimbabwe,zambia).


% Facts about America.
% -------------------

:-add_borders(argentina,bolivia).
:-add_borders(argentina,brazil).
:-add_borders(argentina,chile).
:-add_borders(argentina,paraguay).
:-add_borders(argentina,uruguay).
:-add_borders(argentina,atlantic).

:-add_borders(bahamas,atlantic).

:-add_borders(barbados,atlantic).

:-add_borders(belize,guatemala).
:-add_borders(belize,mexico).
:-add_borders(belize,atlantic).

:-add_borders(bolivia,argentina).
:-add_borders(bolivia,brazil).
:-add_borders(bolivia,chile).
:-add_borders(bolivia,paraguay).
:-add_borders(bolivia,peru).

:-add_borders(brazil,argentina).
:-add_borders(brazil,bolivia).
:-add_borders(brazil,colombia).
:-add_borders(brazil,french_guiana).
:-add_borders(brazil,guyana).
:-add_borders(brazil,paraguay).
:-add_borders(brazil,peru).
:-add_borders(brazil,surinam).
:-add_borders(brazil,uruguay).
:-add_borders(brazil,venezuela).
:-add_borders(brazil,atlantic).

:-add_borders(canada,united_states).
:-add_borders(canada,arctic_ocean).
:-add_borders(canada,atlantic).
:-add_borders(canada,pacific).

:-add_borders(chile,argentina).
:-add_borders(chile,bolivia).
:-add_borders(chile,peru).
:-add_borders(chile,pacific).

:-add_borders(colombia,brazil).
:-add_borders(colombia,ecuador).
:-add_borders(colombia,panama).
:-add_borders(colombia,peru).
:-add_borders(colombia,venezuela).
:-add_borders(colombia,atlantic).
:-add_borders(colombia,pacific).

:-add_borders(costa_rica,nicaragua).
:-add_borders(costa_rica,panama).
:-add_borders(costa_rica,atlantic).
:-add_borders(costa_rica,pacific).

:-add_borders(cuba,atlantic).

:-add_borders(dominican_republic,haiti).
:-add_borders(dominican_republic,atlantic).

:-add_borders(ecuador,colombia).
:-add_borders(ecuador,peru).
:-add_borders(ecuador,pacific).

:-add_borders(el_salvador,guatemala).
:-add_borders(el_salvador,honduras).
:-add_borders(el_salvador,pacific).

:-add_borders(french_guiana,brazil).
:-add_borders(french_guiana,surinam).

:-add_borders(greenland,arctic_ocean).
:-add_borders(greenland,atlantic).

:-add_borders(grenada,atlantic).

:-add_borders(guatemala,belize).
:-add_borders(guatemala,el_salvador).
:-add_borders(guatemala,honduras).
:-add_borders(guatemala,mexico).
:-add_borders(guatemala,atlantic).
:-add_borders(guatemala,pacific).

:-add_borders(guyana,brazil).
:-add_borders(guyana,surinam).
:-add_borders(guyana,venezuela).
:-add_borders(guyana,atlantic).

:-add_borders(haiti,dominican_republic).
:-add_borders(haiti,atlantic).

:-add_borders(honduras,el_salvador).
:-add_borders(honduras,guatemala).
:-add_borders(honduras,nicaragua).
:-add_borders(honduras,atlantic).
:-add_borders(honduras,pacific).

:-add_borders(jamaica,atlantic).

:-add_borders(mexico,belize).
:-add_borders(mexico,guatemala).
:-add_borders(mexico,united_states).
:-add_borders(mexico,atlantic).
:-add_borders(mexico,pacific).

:-add_borders(nicaragua,costa_rica).
:-add_borders(nicaragua,honduras).
:-add_borders(nicaragua,atlantic).
:-add_borders(nicaragua,pacific).

:-add_borders(panama,colombia).
:-add_borders(panama,costa_rica).
:-add_borders(panama,atlantic).
:-add_borders(panama,pacific).

:-add_borders(paraguay,argentina).
:-add_borders(paraguay,bolivia).
:-add_borders(paraguay,brazil).

:-add_borders(peru,bolivia).
:-add_borders(peru,brazil).
:-add_borders(peru,chile).
:-add_borders(peru,colombia).
:-add_borders(peru,ecuador).
:-add_borders(peru,pacific).

:-add_borders(surinam,brazil).
:-add_borders(surinam,french_guiana).
:-add_borders(surinam,guyana).

:-add_borders(trinidad_and_tobago,atlantic).

:-add_borders(united_states,canada).
:-add_borders(united_states,mexico).
:-add_borders(united_states,arctic_ocean).
:-add_borders(united_states,atlantic).
:-add_borders(united_states,pacific).

:-add_borders(uruguay,argentina).
:-add_borders(uruguay,brazil).
:-add_borders(uruguay,atlantic).

:-add_borders(venezuela,brazil).
:-add_borders(venezuela,colombia).
:-add_borders(venezuela,guyana).
:-add_borders(venezuela,atlantic).

% Facts about Australasia.
% -----------------------

:-add_borders(australia,indian_ocean).
:-add_borders(australia,pacific).

:-add_borders(fiji,pacific).

:-add_borders(new_zealand,pacific).

:-add_borders(papua_new_guinea,indonesia).
:-add_borders(papua_new_guinea,pacific).

:-add_borders(tonga,pacific).

:-add_borders(western_samoa,pacific).

:-add_borders(antarctica,southern_ocean).

% Facts about oceans and seas.
% ---------------------------

:-add_borders(arctic_ocean,atlantic).
:-add_borders(arctic_ocean,pacific).

:-add_borders(atlantic,arctic_ocean).
:-add_borders(atlantic,indian_ocean).
:-add_borders(atlantic,pacific).
:-add_borders(atlantic,southern_ocean).
:-add_borders(atlantic,baltic).
:-add_borders(atlantic,mediterranean).

:-add_borders(indian_ocean,atlantic).
:-add_borders(indian_ocean,pacific).
:-add_borders(indian_ocean,southern_ocean).
:-add_borders(indian_ocean,persian_gulf).
:-add_borders(indian_ocean,red_sea).

:-add_borders(pacific,arctic_ocean).
:-add_borders(pacific,atlantic).
:-add_borders(pacific,indian_ocean).
:-add_borders(pacific,southern_ocean).

:-add_borders(southern_ocean,atlantic).
:-add_borders(southern_ocean,indian_ocean).
:-add_borders(southern_ocean,pacific).

:-add_borders(baltic,atlantic).

:-add_borders(black_sea,mediterranean).

:-add_borders(mediterranean,atlantic).
:-add_borders(mediterranean,black_sea).

:-add_borders(persian_gulf,indian_ocean).

:-add_borders(red_sea,indian_ocean).

% Countries bordering each ocean and sea.
% --------------------------------------

:-add_borders(arctic_ocean,norway).
:-add_borders(arctic_ocean,soviet_union).
:-add_borders(arctic_ocean,canada).
:-add_borders(arctic_ocean,greenland).
:-add_borders(arctic_ocean,united_states).

:-add_borders(atlantic,belgium).
:-add_borders(atlantic,denmark).
:-add_borders(atlantic,eire).
:-add_borders(atlantic,france).
:-add_borders(atlantic,iceland).
:-add_borders(atlantic,netherlands).
:-add_borders(atlantic,norway).
:-add_borders(atlantic,portugal).
:-add_borders(atlantic,spain).
:-add_borders(atlantic,sweden).
:-add_borders(atlantic,west_germany).
:-add_borders(atlantic,united_kingdom).
:-add_borders(atlantic,angola).
:-add_borders(atlantic,cameroon).
:-add_borders(atlantic,congo).
:-add_borders(atlantic,dahomey).
:-add_borders(atlantic,equatorial_guinea).
:-add_borders(atlantic,gabon).
:-add_borders(atlantic,gambia).
:-add_borders(atlantic,ghana).
:-add_borders(atlantic,guinea).
:-add_borders(atlantic,guinea_bissau).
:-add_borders(atlantic,ivory_coast).
:-add_borders(atlantic,liberia).
:-add_borders(atlantic,mauritania).
:-add_borders(atlantic,morocco).
:-add_borders(atlantic,nigeria).
:-add_borders(atlantic,senegal).
:-add_borders(atlantic,sierra_leone).
:-add_borders(atlantic,south_africa).
:-add_borders(atlantic,togo).
:-add_borders(atlantic,zaire).
:-add_borders(atlantic,argentina).
:-add_borders(atlantic,bahamas).
:-add_borders(atlantic,barbados).
:-add_borders(atlantic,belize).
:-add_borders(atlantic,brazil).
:-add_borders(atlantic,canada).
:-add_borders(atlantic,colombia).
:-add_borders(atlantic,costa_rica).
:-add_borders(atlantic,cuba).
:-add_borders(atlantic,dominican_republic).
:-add_borders(atlantic,french_guiana).
:-add_borders(atlantic,greenland).
:-add_borders(atlantic,grenada).
:-add_borders(atlantic,guatemala).
:-add_borders(atlantic,guyana).
:-add_borders(atlantic,haiti).
:-add_borders(atlantic,honduras).
:-add_borders(atlantic,jamaica).
:-add_borders(atlantic,mexico).
:-add_borders(atlantic,nicaragua).
:-add_borders(atlantic,panama).
:-add_borders(atlantic,surinam).
:-add_borders(atlantic,trinidad_and_tobago).
:-add_borders(atlantic,united_states).
:-add_borders(atlantic,uruguay).
:-add_borders(atlantic,venezuela).

:-add_borders(indian_ocean,bangladesh).
:-add_borders(indian_ocean,burma).
:-add_borders(indian_ocean,india).
:-add_borders(indian_ocean,indonesia).
:-add_borders(indian_ocean,iran).
:-add_borders(indian_ocean,malaysia).
:-add_borders(indian_ocean,maldives).
:-add_borders(indian_ocean,oman).
:-add_borders(indian_ocean,pakistan).
:-add_borders(indian_ocean,south_yemen).
:-add_borders(indian_ocean,sri_lanka).
:-add_borders(indian_ocean,thailand).
:-add_borders(indian_ocean,djibouti).
:-add_borders(indian_ocean,kenya).
:-add_borders(indian_ocean,malagasy).
:-add_borders(indian_ocean,mauritius).
:-add_borders(indian_ocean,mozambique).
:-add_borders(indian_ocean,seychelles).
:-add_borders(indian_ocean,somalia).
:-add_borders(indian_ocean,south_africa).
:-add_borders(indian_ocean,tanzania).
:-add_borders(indian_ocean,australia).

:-add_borders(pacific,cambodia).
:-add_borders(pacific,china).
:-add_borders(pacific,indonesia).
:-add_borders(pacific,japan).
:-add_borders(pacific,malaysia).
:-add_borders(pacific,north_korea).
:-add_borders(pacific,philippines).
:-add_borders(pacific,singapore).
:-add_borders(pacific,south_korea).
:-add_borders(pacific,soviet_union).
:-add_borders(pacific,taiwan).
:-add_borders(pacific,thailand).
:-add_borders(pacific,vietnam).
:-add_borders(pacific,canada).
:-add_borders(pacific,chile).
:-add_borders(pacific,colombia).
:-add_borders(pacific,costa_rica).
:-add_borders(pacific,ecuador).
:-add_borders(pacific,el_salvador).
:-add_borders(pacific,guatemala).
:-add_borders(pacific,honduras).
:-add_borders(pacific,mexico).
:-add_borders(pacific,nicaragua).
:-add_borders(pacific,panama).
:-add_borders(pacific,peru).
:-add_borders(pacific,united_states).
:-add_borders(pacific,australia).
:-add_borders(pacific,fiji).
:-add_borders(pacific,new_zealand).
:-add_borders(pacific,papua_new_guinea).
:-add_borders(pacific,tonga).
:-add_borders(pacific,western_samoa).

:-add_borders(southern_ocean,antarctica).

:-add_borders(baltic,denmark).
:-add_borders(baltic,finland).
:-add_borders(baltic,east_germany).
:-add_borders(baltic,poland).
:-add_borders(baltic,sweden).
:-add_borders(baltic,west_germany).
:-add_borders(baltic,soviet_union).

:-add_borders(black_sea,bulgaria).
:-add_borders(black_sea,romania).
:-add_borders(black_sea,soviet_union).
:-add_borders(black_sea,turkey).

:-add_borders(caspian,iran).
:-add_borders(caspian,soviet_union).

:-add_borders(mediterranean,albania).
:-add_borders(mediterranean,cyprus).
:-add_borders(mediterranean,france).
:-add_borders(mediterranean,greece).
:-add_borders(mediterranean,italy).
:-add_borders(mediterranean,malta).
:-add_borders(mediterranean,monaco).
:-add_borders(mediterranean,san_marino).
:-add_borders(mediterranean,spain).
:-add_borders(mediterranean,yugoslavia).
:-add_borders(mediterranean,israel).
:-add_borders(mediterranean,lebanon).
:-add_borders(mediterranean,syria).
:-add_borders(mediterranean,turkey).
:-add_borders(mediterranean,algeria).
:-add_borders(mediterranean,egypt).
:-add_borders(mediterranean,libya).
:-add_borders(mediterranean,morocco).
:-add_borders(mediterranean,tunisia).

:-add_borders(persian_gulf,bahrain).
:-add_borders(persian_gulf,iran).
:-add_borders(persian_gulf,iraq).
:-add_borders(persian_gulf,kuwait).
:-add_borders(persian_gulf,qatar).
:-add_borders(persian_gulf,saudi_arabia).
:-add_borders(persian_gulf,united_arab_emirates).

:-add_borders(red_sea,israel).
:-add_borders(red_sea,jordan).
:-add_borders(red_sea,saudi_arabia).
:-add_borders(red_sea,yemen).
:-add_borders(red_sea,egypt).
:-add_borders(red_sea,ethiopia).
:-add_borders(red_sea,sudan).

:- fixup_exports.
