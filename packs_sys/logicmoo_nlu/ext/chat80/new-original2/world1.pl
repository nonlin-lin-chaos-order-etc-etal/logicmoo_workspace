/*

_________________________________________________________________________
|       Copyright (C) 1982                                                |
|                                                                         |
|       David Warren,                                                     |
|               SRI International, 333 Ravenswood Ave., Menlo Park,       |
|               California 94025, USA;                                    |
|                                                                         |
|       Fernando Pereira,                                                 |
|               Dept. of Architecture, University of Edinburgh,           |
|               20 Chambers St., Edinburgh EH1 1JZ, Scotland              |
|                                                                         |
|       This program may be used, copied, altered or included in other    |
|       programs only for academic purposes and provided that the         |
|       authorship of the initial program is aknowledged.                 |
|       Use for commercial purposes without the previous written          |
|       agreement of the authors is forbidden.                            |
|_________________________________________________________________________|


*/

% Inversion of the 'in' relation.
% ------------------------------
:- dynamic(tmp80:trans_rel_cache_create/2).
:- dynamic(tmp80:trans_rel_cache_insts/3).
:- dynamic(tmp80:trans_rel_cache/4).
trans_rel(P1,P2,X,Y) :- tmp80:trans_rel_cache_create(P1,P2),!, tmp80:trans_rel_cache(P1,P2,X,Y).
trans_rel(P1,P2,X,Y):- trans_rel_nc(P1,P2,X,Y).

trans_rel_nc(P1,P2,X,Y) :- var(X),!, no_repeats(X, trans_rel_rl(P1,P2,X,Y)).
trans_rel_nc(P1,P2,X,Y) :- nonvar(Y), !, trans_rel_lr(P1,P2,X,Y), !.
trans_rel_nc(P1,P2,X,Y) :- no_repeats(Y, trans_rel_lr(P1,P2,X,Y)).

trans_rel_lr(P1,P2,X,Y) :- call(P2,X,W), ( call(P1,W,Y) ; trans_rel_lr(P1,P2,W,Y) ).
trans_rel_rl(P1,P2,X,Y) :- call(P2,W,Y), ( call(P1,W,X) ; trans_rel_rl(P1,P2,X,W) ).

trans_rel_cache_create(P1,P2):-
  must_be(ground,(P1,P2)),
  forall(call(P2,XX,YY),
     (assert_if_new(tmp80:trans_rel_cache_insts(P1,P2,XX)),
      assert_if_new(tmp80:trans_rel_cache_insts(P1,P2,YY)))),
  forall(tmp80:trans_rel_cache_insts(P1,P2,E),
        (forall(trans_rel_nc(P1,P2,E,Y),assert_if_new(tmp80:trans_rel_cache(P1,P2,E,Y))),
         forall(trans_rel_nc(P1,P2,Y,E),assert_if_new(tmp80:trans_rel_cache(P1,P2,Y,E))))),
  asserta((trans_rel_cache_create(P1,P2):-!)),!,
  %listing(tmp80:trans_rel_cache_insts(P1,P2,_Instances)),
  %listing(tmp80:trans_rel_cache(P1,P2,_,_)),
  !.
  

%contains(X,Y) :- trans_direct(contains,X,Y).
%contains(X,Y) :- trans_direct(contains,X,W), contains(W,Y).


trans_pred(spatial,contains,X,Y) :- trans_rel(=,trans_direct(contains),X,Y).
%contains(X,X).

trans_direct(contains,Continent,Region):- continent_contains_region(Continent,Region).
trans_direct(contains,Region,Country):- region_contains_country(Region,Country).
trans_direct(contains,Country,CityOrRiver):- country_contains_thing(Country,CityOrRiver).
trans_direct(contains,Country,River):- path_pred(thru,river,River,Country).




continent_contains_region(africa,central_africa).
continent_contains_region(africa,east_africa).
continent_contains_region(africa,north_africa).
continent_contains_region(africa,southern_africa).
continent_contains_region(africa,west_africa).

continent_contains_region(america,caribbean).
continent_contains_region(america,central_america).
continent_contains_region(america,north_america).
continent_contains_region(america,south_america).

continent_contains_region(asia,far_east).
continent_contains_region(asia,indian_subcontinent).
continent_contains_region(asia,middle_east).
continent_contains_region(asia,northern_asia).
continent_contains_region(asia,southeast_east).

continent_contains_region(australasia,oceania).

continent_contains_region(europe,eastern_europe).
continent_contains_region(europe,scandinavia).
continent_contains_region(europe,southern_europe).
continent_contains_region(europe,western_europe).

region_contains_country(R,C) :- c_r_l_l_s_cap_m(C,R,_,_,_,_,_,_).
/*region_contains_country(oceania,australia).
region_contains_country(oceania,fiji).
region_contains_country(oceania,new_zealand).
region_contains_country(oceania,papua_new_guinea).
region_contains_country(oceania,tonga).
region_contains_country(oceania,western_samoa).

region_contains_country(scandinavia,denmark).
region_contains_country(scandinavia,finland).
region_contains_country(scandinavia,norway).
region_contains_country(scandinavia,sweden).

region_contains_country(western_europe,austria).
region_contains_country(western_europe,belgium).
region_contains_country(western_europe,eire).
region_contains_country(western_europe,france).
region_contains_country(western_europe,iceland).
region_contains_country(western_europe,liechtenstein).
region_contains_country(western_europe,luxembourg).
region_contains_country(western_europe,netherlands).
region_contains_country(western_europe,switzerland).
region_contains_country(western_europe,united_kingdom).
region_contains_country(western_europe,west_germany).

region_contains_country(eastern_europe,bulgaria).
region_contains_country(eastern_europe,czechoslovakia).
region_contains_country(eastern_europe,east_germany).
region_contains_country(eastern_europe,hungary).
region_contains_country(eastern_europe,poland).
region_contains_country(eastern_europe,romania).

region_contains_country(southern_europe,albania).
region_contains_country(southern_europe,andorra).
region_contains_country(southern_europe,cyprus).
region_contains_country(southern_europe,greece).
region_contains_country(southern_europe,italy).
region_contains_country(southern_europe,malta).
region_contains_country(southern_europe,monaco).
region_contains_country(southern_europe,portugal).
region_contains_country(southern_europe,san_marino).
region_contains_country(southern_europe,spain).
region_contains_country(southern_europe,yugoslavia).

region_contains_country(north_america,canada).
region_contains_country(north_america,united_states).

region_contains_country(central_america,belize).
region_contains_country(central_america,costa_rica).
region_contains_country(central_america,el_salvador).
region_contains_country(central_america,guatemala).
region_contains_country(central_america,honduras).
region_contains_country(central_america,mexico).
region_contains_country(central_america,nicaragua).
region_contains_country(central_america,panama).

region_contains_country(caribbean,bahamas).
region_contains_country(caribbean,barbados).
region_contains_country(caribbean,cuba).
region_contains_country(caribbean,dominican_republic).
region_contains_country(caribbean,grenada).
region_contains_country(caribbean,haiti).
region_contains_country(caribbean,jamaica).
region_contains_country(caribbean,trinidad_and_tobago).

region_contains_country(south_america,argentina).
region_contains_country(south_america,bolivia).
region_contains_country(south_america,brazil).
region_contains_country(south_america,chile).
region_contains_country(south_america,colombia).
region_contains_country(south_america,ecuador).
region_contains_country(south_america,french_guiana).
region_contains_country(south_america,guyana).
region_contains_country(south_america,paraguay).
region_contains_country(south_america,peru).
region_contains_country(south_america,surinam).
region_contains_country(south_america,uruguay).
region_contains_country(south_america,venezuela).

region_contains_country(north_africa,algeria).
region_contains_country(north_africa,egypt).
region_contains_country(north_africa,libya).
region_contains_country(north_africa,morocco).
region_contains_country(north_africa,tunisia).

region_contains_country(west_africa,cameroon).
region_contains_country(west_africa,dahomey).
region_contains_country(west_africa,equatorial_guinea).
region_contains_country(west_africa,gambia).
region_contains_country(west_africa,ghana).
region_contains_country(west_africa,guinea).
region_contains_country(west_africa,guinea_bissau).
region_contains_country(west_africa,ivory_coast).
region_contains_country(west_africa,liberia).
region_contains_country(west_africa,mali).
region_contains_country(west_africa,mauritania).
region_contains_country(west_africa,niger).
region_contains_country(west_africa,nigeria).
region_contains_country(west_africa,senegal).
region_contains_country(west_africa,sierra_leone).
region_contains_country(west_africa,togo).
region_contains_country(west_africa,upper_volta).

region_contains_country(central_africa,burundi).
region_contains_country(central_africa,central_african_republic).
region_contains_country(central_africa,chad).
region_contains_country(central_africa,congo).
region_contains_country(central_africa,gabon).
region_contains_country(central_africa,rwanda).
region_contains_country(central_africa,sudan).
region_contains_country(central_africa,zaire).

region_contains_country(east_africa,djibouti).
region_contains_country(east_africa,ethiopia).
region_contains_country(east_africa,kenya).
region_contains_country(east_africa,seychelles).
region_contains_country(east_africa,somalia).
region_contains_country(east_africa,tanzania).
region_contains_country(east_africa,uganda).

region_contains_country(southern_africa,angola).
region_contains_country(southern_africa,botswana).
region_contains_country(southern_africa,lesotho).
region_contains_country(southern_africa,malagasy).
region_contains_country(southern_africa,malawi).
region_contains_country(southern_africa,mauritius).
region_contains_country(southern_africa,mozambique).
region_contains_country(southern_africa,south_africa).
region_contains_country(southern_africa,swaziland).
region_contains_country(southern_africa,zambia).
region_contains_country(southern_africa,zimbabwe).

region_contains_country(middle_east,bahrain).
region_contains_country(middle_east,iran).
region_contains_country(middle_east,iraq).
region_contains_country(middle_east,israel).
region_contains_country(middle_east,jordan).
region_contains_country(middle_east,kuwait).
region_contains_country(middle_east,lebanon).
region_contains_country(middle_east,oman).
region_contains_country(middle_east,qatar).
region_contains_country(middle_east,saudi_arabia).
region_contains_country(middle_east,south_yemen).
region_contains_country(middle_east,syria).
region_contains_country(middle_east,turkey).
region_contains_country(middle_east,united_arab_emirates).
region_contains_country(middle_east,yemen).

region_contains_country(indian_subcontinent,afghanistan).
region_contains_country(indian_subcontinent,bangladesh).
region_contains_country(indian_subcontinent,bhutan).
region_contains_country(indian_subcontinent,india).
region_contains_country(indian_subcontinent,maldives).
region_contains_country(indian_subcontinent,nepal).
region_contains_country(indian_subcontinent,pakistan).
region_contains_country(indian_subcontinent,sri_lanka).

region_contains_country(southeast_east,burma).
region_contains_country(southeast_east,cambodia).
region_contains_country(southeast_east,indonesia).
region_contains_country(southeast_east,laos).
region_contains_country(southeast_east,malaysia).
region_contains_country(southeast_east,philippines).
region_contains_country(southeast_east,singapore).
region_contains_country(southeast_east,thailand).
region_contains_country(southeast_east,vietnam).

region_contains_country(far_east,china).
region_contains_country(far_east,japan).
region_contains_country(far_east,north_korea).
region_contains_country(far_east,south_korea).
region_contains_country(far_east,taiwan).

region_contains_country(northern_asia,mongolia).
region_contains_country(northern_asia,soviet_union).
*/
country_contains_thing(afghanistan,amu_darya).

country_contains_thing(angola,cubango).
country_contains_thing(angola,zambesi).

country_contains_thing(argentina,buenos_aires).
country_contains_thing(argentina,parana).

country_contains_thing(australia,melbourne).
country_contains_thing(australia,murray).
country_contains_thing(australia,sydney).

country_contains_thing(austria,danube).
country_contains_thing(austria,vienna).

country_contains_thing(bangladesh,brahmaputra).

country_contains_thing(belgium,brussels).

country_contains_thing(brazil,amazon).
country_contains_thing(brazil,parana).
country_contains_thing(brazil,rio_de_janeiro).
country_contains_thing(brazil,sao_paulo).

country_contains_thing(burma,irrawaddy).
country_contains_thing(burma,salween).

country_contains_thing(cambodia,mekong).

country_contains_thing(canada,mackenzie).
country_contains_thing(canada,montreal).
country_contains_thing(canada,toronto).
country_contains_thing(canada,yukon).

country_contains_thing(chile,santiago).

country_contains_thing(china,amur).
country_contains_thing(china,brahmaputra).
country_contains_thing(china,canton).
country_contains_thing(china,chungking).
country_contains_thing(china,dairen).
country_contains_thing(china,ganges).
country_contains_thing(china,harbin).
country_contains_thing(china,hwang_ho).
country_contains_thing(china,indus).
country_contains_thing(china,kowloon).
country_contains_thing(china,mekong).
country_contains_thing(china,mukden).
country_contains_thing(china,peking).
country_contains_thing(china,salween).
country_contains_thing(china,shanghai).
country_contains_thing(china,sian).
country_contains_thing(china,tientsin).
country_contains_thing(china,yangtze).

country_contains_thing(colombia,orinoco).

country_contains_thing(czechoslovakia,danube).
country_contains_thing(czechoslovakia,elbe).
country_contains_thing(czechoslovakia,oder).

country_contains_thing(east_germany,berlin).
country_contains_thing(east_germany,elbe).

country_contains_thing(egypt,cairo).
country_contains_thing(egypt,nile).

country_contains_thing(france,paris).
country_contains_thing(france,rhone).
country_contains_thing(france,seine).

country_contains_thing(ghana,volta).

country_contains_thing(greece,athens).

country_contains_thing(guinea,niger_river).
country_contains_thing(guinea,senegal_river).

country_contains_thing(hungary,budapest).
country_contains_thing(hungary,danube).

country_contains_thing(india,bombay).
country_contains_thing(india,calcutta).
country_contains_thing(india,delhi).
country_contains_thing(india,ganges).
country_contains_thing(india,hyderabad).
country_contains_thing(india,indus).
country_contains_thing(india,madras).

country_contains_thing(indonesia,jakarta).

country_contains_thing(iran,tehran).

country_contains_thing(iraq,euphrates).

country_contains_thing(italy,milan).
country_contains_thing(italy,naples).
country_contains_thing(italy,rome).

country_contains_thing(japan,kobe).
country_contains_thing(japan,kyoto).
country_contains_thing(japan,nagoya).
country_contains_thing(japan,nanking).
country_contains_thing(japan,osaka).
country_contains_thing(japan,tokyo).
country_contains_thing(japan,yokohama).

country_contains_thing(laos,mekong).

country_contains_thing(lesotho,orange).

country_contains_thing(mali,niger_river).
country_contains_thing(mali,senegal_river).

country_contains_thing(mexico,colorado).
country_contains_thing(mexico,mexico_city).
country_contains_thing(mexico,rio_grande).

country_contains_thing(mongolia,amur).
country_contains_thing(mongolia,yenisei).

country_contains_thing(mozambique,limpopo).
country_contains_thing(mozambique,zambesi).

country_contains_thing(netherlands,rhine).
country_contains_thing(netherlands,amsterdam).
country_contains_thing(netherlands,amersfoort).

country_contains_thing(niger,niger_river).

country_contains_thing(nigeria,niger_river).

country_contains_thing(pakistan,indus).
country_contains_thing(pakistan,karachi).

country_contains_thing(paraguay,parana).

country_contains_thing(peru,amazon).
country_contains_thing(peru,lima).

country_contains_thing(philippines,manila).

country_contains_thing(poland,oder).
country_contains_thing(poland,vistula).
country_contains_thing(poland,warsaw).

country_contains_thing(portugal,tagus).

country_contains_thing(romania,bucharest).
country_contains_thing(romania,danube).

country_contains_thing(senegal,senegal_river).

country_contains_thing(singapore,singapore_city).

country_contains_thing(south_africa,cubango).
country_contains_thing(south_africa,johannesburg).
country_contains_thing(south_africa,limpopo).
country_contains_thing(south_africa,orange).

country_contains_thing(south_korea,pusan).
country_contains_thing(south_korea,seoul).

country_contains_thing(soviet_union,amu_darya).
country_contains_thing(soviet_union,amur).
country_contains_thing(soviet_union,don).
country_contains_thing(soviet_union,kiev).
country_contains_thing(soviet_union,lena).
country_contains_thing(soviet_union,leningrad).
country_contains_thing(soviet_union,moscow).
country_contains_thing(soviet_union,ob).
country_contains_thing(soviet_union,volga).
country_contains_thing(soviet_union,yenisei).

country_contains_thing(spain,barcelona).
country_contains_thing(spain,madrid).
country_contains_thing(spain,tagus).

country_contains_thing(sudan,nile).

country_contains_thing(switzerland,rhine).
country_contains_thing(switzerland,rhone).

country_contains_thing(syria,euphrates).

country_contains_thing(thailand,bangkok).

country_contains_thing(turkey,euphrates).
country_contains_thing(turkey,istanbul).

country_contains_thing(uganda,nile).

country_contains_thing(united_kingdom,birmingham).
country_contains_thing(united_kingdom,glasgow).
country_contains_thing(united_kingdom,london).

country_contains_thing(united_states,chicago).
country_contains_thing(united_states,colorado).
country_contains_thing(united_states,detroit).
country_contains_thing(united_states,los_angeles).
country_contains_thing(united_states,mississippi).
country_contains_thing(united_states,new_york).
country_contains_thing(united_states,philadelphia).
country_contains_thing(united_states,rio_grande).
country_contains_thing(united_states,yukon).

country_contains_thing(upper_volta,volta).

country_contains_thing(venezuela,caracas).
country_contains_thing(venezuela,orinoco).

country_contains_thing(vietnam,mekong).
country_contains_thing(vietnam,saigon).

country_contains_thing(west_germany,danube).
country_contains_thing(west_germany,elbe).
country_contains_thing(west_germany,hamburg).
country_contains_thing(west_germany,rhine).

country_contains_thing(yugoslavia,danube).

country_contains_thing(zaire,congo_river).

country_contains_thing(zambia,congo_river).
country_contains_thing(zambia,zambesi).

country_contains_thing(Country,City) :- clause(city_country_popu(City,Country,_),true).
country_contains_thing(Country,City) :- specific_pred(spatial,nation_capital,Country,City).

:- fixup_exports.
/*

 _________________________________________________________________________
|       Copyright (C) 1982                                                |
|                                                                         |
|       David Warren,                                                     |
|               SRI International, 333 Ravenswood Ave., Menlo Park,       |
|               California 94025, USA;                                    |
|                                                                         |
|       Fernando Pereira,                                                 |
|               Dept. of Architecture, University of Edinburgh,           |
|               20 Chambers St., Edinburgh EH1 1JZ, Scotland              |
|                                                                         |
|       This program may be used, copied, altered or included in other    |
|       programs only for academic purposes and provided that the         |
|       authorship of the initial program is aknowledged.                 |
|       Use for commercial purposes without the previous written          |
|       agreement of the authors is forbidden.                            |
|_________________________________________________________________________|

*/

% Facts about countries.
% ---------------------

% c_r_l_l_s_cap_m(Country,Region,Latitude,Longitude,
%         Area/1000,Area mod 1000,
%         Population/1000000,Population mod 1000000 / 1000,
%         Capital,Currency)


c_r_l_l_s_cap_m(afghanistan,indian_subcontinent,33,-65,254861,18290000,kabul,afghani).
c_r_l_l_s_cap_m(albania,southern_europe,41,-20,11100,2350000,tirana,lek).
c_r_l_l_s_cap_m(algeria,north_africa,35,-11,919951,15770000,algiers,dinar).
c_r_l_l_s_cap_m(andorra,southern_europe,42,-1,179,25000,andorra_la_villa,franc_peseta).
c_r_l_l_s_cap_m(angola,southern_africa,-12,-18,481351,5810000,luanda,?).
c_r_l_l_s_cap_m(argentina,south_america,-35,66,1072067,23920000,buenos_aires,peso).
c_r_l_l_s_cap_m(australia,oceania,-23,-135,2967909,13268000,canberra,australian_dollar).
c_r_l_l_s_cap_m(austria,western_europe,47,-14,32374,7520000,vienna,schilling).
c_r_l_l_s_cap_m(bahamas,caribbean,24,74,4404,190000,nassau,bahamian_dollar).
c_r_l_l_s_cap_m(bahrain,middle_east,26,-50,231,230000,manama,dinar).
c_r_l_l_s_cap_m(bangladesh,indian_subcontinent,24,-90,55126,71317000,dacca,taka).
c_r_l_l_s_cap_m(barbados,caribbean,13,59,166,240000,bridgetown,east_carribean_dollar).
c_r_l_l_s_cap_m(belgium,western_europe,51,-5,11779,9711000,brussels,franc).
c_r_l_l_s_cap_m(belize,central_america,17,88,8866,82000,belize_town,?).
c_r_l_l_s_cap_m(bhutan,indian_subcontinent,27,-90,19305,1150000,thimphu,indian_rupee).
c_r_l_l_s_cap_m(bolivia,south_america,-17,64,424162,5330000,sucre,peso).
c_r_l_l_s_cap_m(botswana,southern_africa,-22,-24,219815,650000,gaborone,south_african_rand).
c_r_l_l_s_cap_m(brazil,south_america,-13,53,3286470,105137000,brasilia,cruzeiro).
c_r_l_l_s_cap_m(bulgaria,eastern_europe,43,-25,42829,8620000,sofia,lev).
c_r_l_l_s_cap_m(burma,southeast_east,21,-96,261789,29560000,rangoon,kyat).
c_r_l_l_s_cap_m(burundi,central_africa,-3,-30,10739,3600000,bujumbura,franc).
c_r_l_l_s_cap_m(cambodia,southeast_east,12,-105,69898,7640000,phnom_penh,riel).
c_r_l_l_s_cap_m(cameroon,west_africa,3,-12,183568,6170000,yaounde,cfa_franc).
c_r_l_l_s_cap_m(canada,north_america,60,100,3851809,22047000,ottawa,canadian_dollar).
c_r_l_l_s_cap_m(central_african_republic,central_africa,7,-20,241313,1720000,bangui,cfa_franc).
c_r_l_l_s_cap_m(chad,central_africa,12,-17,495752,3870000,n_djamena,cfa_franc).
c_r_l_l_s_cap_m(chile,south_america,-35,71,286396,10230000,santiago,escudo).
c_r_l_l_s_cap_m(china,far_east,30,-110,3691502,840000000.000000,peking,yuan).
c_r_l_l_s_cap_m(colombia,south_america,4,73,455335,23210000,bogota,peso).
c_r_l_l_s_cap_m(congo,central_africa,-1,-16,132046,1001000,brazzaville,cfa_franc).
c_r_l_l_s_cap_m(costa_rica,central_america,10,84,19653,1890000,san_jose,colon).
c_r_l_l_s_cap_m(cuba,caribbean,22,79,44218,8870000,havana,peso).
c_r_l_l_s_cap_m(cyprus,southern_europe,35,-33,3572,660000,nicosia,pound).
c_r_l_l_s_cap_m(czechoslovakia,eastern_europe,49,-17,49371,14580000,prague,koruna).
c_r_l_l_s_cap_m(dahomey,west_africa,8,-2,43483,2910000,porto_novo,cfa_franc).
c_r_l_l_s_cap_m(denmark,scandinavia,55,-9,16615,5130000,copenhagen,krone).
c_r_l_l_s_cap_m(djibouti,east_africa,12,-42,9071,45000,djibouti_city,?).
c_r_l_l_s_cap_m(dominican_republic,caribbean,19,70,18704,4430000,santa_domingo,peso).
c_r_l_l_s_cap_m(east_germany,eastern_europe,52,-12,40646,16980000,east_berlin,ddr_mark).
c_r_l_l_s_cap_m(ecuador,south_america,-2,78,105685,6730000,quito,sucre).
c_r_l_l_s_cap_m(egypt,north_africa,28,-31,386872,35620000,cairo,egyptian_pound).
c_r_l_l_s_cap_m(eire,western_europe,53,8,26600,3030000,dublin,irish_pound).
c_r_l_l_s_cap_m(el_salvador,central_america,14,89,8260,3860000,san_salvador,colon).
c_r_l_l_s_cap_m(equatorial_guinea,west_africa,1,-10,10832,300000,santa_isabel,peveta).
c_r_l_l_s_cap_m(ethiopia,east_africa,8,-40,457142,26080000,addis_ababa,ethiopean_dollar).
c_r_l_l_s_cap_m(fiji,oceania,-17,-179,7055,550000,suva,fiji_dollar).
c_r_l_l_s_cap_m(finland,scandinavia,65,-27,130119,4660000,helsinki,markka).
c_r_l_l_s_cap_m(france,western_europe,47,-3,212973,52350000,paris,franc).
c_r_l_l_s_cap_m(french_guiana,south_america,4,53,34740,27000,cayenne,?).
c_r_l_l_s_cap_m(gabon,central_africa,0,-10,102317,520000,libreville,cfa_franc).
c_r_l_l_s_cap_m(gambia,west_africa,13,16,4003,490000,banjul,dalasi).
c_r_l_l_s_cap_m(ghana,west_africa,6,1,92100,9360000,accra,cedi).
c_r_l_l_s_cap_m(greece,southern_europe,40,-23,50547,9030000,athens,drachma).
c_r_l_l_s_cap_m(grenada,caribbean,12,61,133,100000,st_georges,east_caribbean_dollar).
c_r_l_l_s_cap_m(guatemala,central_america,15,90,42042,5540000,guatamala_city,quetzal).
c_r_l_l_s_cap_m(guinea,west_africa,10,10,94925,4210000,conakry,syli).
c_r_l_l_s_cap_m(guinea_bissau,west_africa,12,15,13948,510000,bissau,pataca).
c_r_l_l_s_cap_m(guyana,south_america,5,59,83000 ,760000,georgetown,guyana_dollar).
c_r_l_l_s_cap_m(haiti,caribbean,19,72,10714,5200000,port_au_prince,gourde).
c_r_l_l_s_cap_m(honduras,central_america,14,86,43277,2780000,tegucigalpa,lempira).
c_r_l_l_s_cap_m(hungary,eastern_europe,47,-19,35919,10410000,budapest,forint).
c_r_l_l_s_cap_m(iceland,western_europe,65,19,39702,210000,reykjavik,krona).
c_r_l_l_s_cap_m(india,indian_subcontinent,20,-80,1229919,574219776.000000,new_delhi,rupee).
c_r_l_l_s_cap_m(indonesia,southeast_east,-5,-115,735268,124600000,jakarta,rupiah).
c_r_l_l_s_cap_m(iran,middle_east,33,-53,636363,32001000,tehran,rial).
c_r_l_l_s_cap_m(iraq,middle_east,33,-44,167567,10410000,baghdad,dinar).
c_r_l_l_s_cap_m(israel,middle_east,32,-35,34493,3228000,jerusalem,israeli_pound).
c_r_l_l_s_cap_m(italy,southern_europe,42,-13,116303,55262000,rome,lira).
c_r_l_l_s_cap_m(ivory_coast,west_africa,7,5,124503,4640000,abidjan,cfa_franc).
c_r_l_l_s_cap_m(jamaica,caribbean,18,77,4411,1980000,kingston,jamaican_dollar).
c_r_l_l_s_cap_m(japan,far_east,36,-136,143574,108710000,tokyo,yen).
c_r_l_l_s_cap_m(jordan,middle_east,31,-36,32297,2560000,amman,dinar).
c_r_l_l_s_cap_m(kenya,east_africa,1,-38,224960,12480000,nairobi,kenya_shilling).
c_r_l_l_s_cap_m(kuwait,middle_east,29,-47,7780,880000,kuwait_city,kuwaiti_dinar).
c_r_l_l_s_cap_m(laos,southeast_east,18,-105,3180,3180000,vientiane,kip).
c_r_l_l_s_cap_m(lebanon,middle_east,34,-36,4015,3213000,beirut,lebanese_pound).
c_r_l_l_s_cap_m(lesotho,southern_africa,-30,-28,11716,1200000,masero,rand).
c_r_l_l_s_cap_m(liberia,west_africa,6,9,43000 ,1660000,monrovia,us_dollar).
c_r_l_l_s_cap_m(libya,north_africa,28,-17,679536,2257000,tripoli,libyan_dinar).
c_r_l_l_s_cap_m(liechtenstein,western_europe,47,-9,62,23000,vaduz,swiss_franc).
c_r_l_l_s_cap_m(luxembourg,western_europe,50,-6,999,350000,luxembourg_city,luxembourg_franc).
c_r_l_l_s_cap_m(malagasy,southern_africa,-20,-47,203035,7655000,tananarive,ariary).
c_r_l_l_s_cap_m(malawi,southern_africa,-13,-34,45747,4790000,zomba,kwacha).
c_r_l_l_s_cap_m(malaysia,southeast_east,5,-110,128328,10920000,kuala_lumpa,malaysian_dollar).
c_r_l_l_s_cap_m(maldives,indian_subcontinent,2,-73,115,123000,male,rupee).
c_r_l_l_s_cap_m(mali,west_africa,15,10,464873,5380000,bamako,mali_franc).
c_r_l_l_s_cap_m(malta,southern_europe,36,-14,122,319000,valetta,pound).
c_r_l_l_s_cap_m(mauritania,west_africa,21,10,419229,1260000,nouakchott,ouguiya).
c_r_l_l_s_cap_m(mauritius,southern_africa,-20,-57,787,870000,port_louis,rupee).
c_r_l_l_s_cap_m(mexico,central_america,20,100,761601,54300000,mexico_city,peso).
c_r_l_l_s_cap_m(monaco,southern_europe,44,-7,1,30000,monaco_city,french_franc).
c_r_l_l_s_cap_m(mongolia,northern_asia,47,-103,604247,1360000,ulan_bator,tighrik).
c_r_l_l_s_cap_m(morocco,north_africa,32,6,171953,16310000,rabat,dirham).
c_r_l_l_s_cap_m(mozambique,southern_africa,-19,-35,303373,8820000,maputo,?).
c_r_l_l_s_cap_m(nepal,indian_subcontinent,28,-84,54362,12020000,katmandu,nepalese_rupee).
c_r_l_l_s_cap_m(netherlands,western_europe,52,-5,14192,13500000,amsterdam,guilder).
c_r_l_l_s_cap_m(new_zealand,oceania,-40,-176,103736,2962000,wellington,new_zealand_dollar).
c_r_l_l_s_cap_m(nicaragua,central_america,12,85,57143,2010000,managua,cordoba).
c_r_l_l_s_cap_m(niger,west_africa,13,-10,489206,4300000,niamey,cfa_franc).
c_r_l_l_s_cap_m(nigeria,west_africa,8,-8,356669,79759000,lagos,naira).
c_r_l_l_s_cap_m(north_korea,far_east,40,-127,46768,15090000,pvongvang,won).
c_r_l_l_s_cap_m(norway,scandinavia,64,-11,125181,3960000,oslo,krone).
c_r_l_l_s_cap_m(oman,middle_east,23,-58,82000 ,720000,muscat,riyal_omani).
c_r_l_l_s_cap_m(pakistan,indian_subcontinent,30,-70,342750,66750000,islamad,rupee).
c_r_l_l_s_cap_m(panama,central_america,9,80,28753,1570000,panama_city,balboa).
c_r_l_l_s_cap_m(papua_new_guinea,oceania,-8,-145,183540,2580000,port_harcourt,australian_dollar).
c_r_l_l_s_cap_m(paraguay,south_america,-23,57,157047,2670000,asuncion,guarani).
c_r_l_l_s_cap_m(peru,south_america,-8,75,496222,14910000,lima,sol).
c_r_l_l_s_cap_m(philippines,southeast_east,12,-123,115707,40220000,quezon_city,piso).
c_r_l_l_s_cap_m(poland,eastern_europe,52,-20,120359,33360000,warsaw,zloty).
c_r_l_l_s_cap_m(portugal,southern_europe,40,7,35340,8560000,lisbon,escudo).
c_r_l_l_s_cap_m(qatar,middle_east,25,-51,4000 ,115000,doha,riyal).
c_r_l_l_s_cap_m(romania,eastern_europe,46,-25,91699,5690000,bucharest,leu).
c_r_l_l_s_cap_m(rwanda,central_africa,-2,-30,10169,3980000,kigali,rwanda_franc).
c_r_l_l_s_cap_m(san_marino,southern_europe,44,-12,24,20000,san_marino_city,italian_lira).
c_r_l_l_s_cap_m(saudi_arabia,middle_east,26,-44,873000 , 8100000,riyadh,riyal).
c_r_l_l_s_cap_m(senegal,west_africa,14,14,76124,4230000,dakar,cfa_franc).
c_r_l_l_s_cap_m(seychelles,east_africa,-4,-55,40,156000,victoria,rupee).
c_r_l_l_s_cap_m(sierra_leone,west_africa,9,12,27925,2860000,freetown,leone).
c_r_l_l_s_cap_m(singapore,southeast_east,1,-104,226,2190000,singapore_city,singapore_dollar).
c_r_l_l_s_cap_m(somalia,east_africa,7,-47,246155,3100000,mogadishu,somali_shilling).
c_r_l_l_s_cap_m(south_africa,southern_africa,-30,-25,471819,23720000,pretoria,rand).
c_r_l_l_s_cap_m(south_korea,far_east,36,-128,38031,33333000,seoul,won).
c_r_l_l_s_cap_m(south_yemen,middle_east,15,-48,111000,1600000,aden,dinar).
c_r_l_l_s_cap_m(soviet_union,northern_asia,57,-80,8347250,250900000,moscow,ruble).
c_r_l_l_s_cap_m(spain,southern_europe,40,5,194883,34860000,madrid,peseta).
c_r_l_l_s_cap_m(sri_lanka,indian_subcontinent,7,-81,25332,13250000,colombo,rupee).
c_r_l_l_s_cap_m(sudan,central_africa,15,-30,967491,16900000,khartoum,pound).
c_r_l_l_s_cap_m(surinam,south_america,4,56,55000 ,208000,paramaribo,?).
c_r_l_l_s_cap_m(swaziland,southern_africa,-26,-31,6705,460000,mbabane,lilageru).
c_r_l_l_s_cap_m(sweden,scandinavia,63,-15,173665,8144000,stockholm,krona).
c_r_l_l_s_cap_m(switzerland,western_europe,46,-8,15941,6440000,bern,franc).
c_r_l_l_s_cap_m(syria,middle_east,35,-38,71498,6895000,damascus,syrian_pound).
c_r_l_l_s_cap_m(taiwan,far_east,23,-121,13592,15737000,taipei,taiwan_dollar).
c_r_l_l_s_cap_m(tanzania,east_africa,-7,-34,363708,14000000,dar_es_salaam,tanzanian_shilling).
c_r_l_l_s_cap_m(thailand,southeast_east,16,-102,198455,39950000,bangkok,baht).
c_r_l_l_s_cap_m(togo,west_africa,8,-1,21853,2120000,lome,cfa_franc).
c_r_l_l_s_cap_m(tonga,oceania,-20,173,269,90000,nukualofa,pa_anga).
c_r_l_l_s_cap_m(trinidad_and_tobago,caribbean,10,61,1979,5510000,port_of_spain,trinidad_and_tobago_dollar).
c_r_l_l_s_cap_m(tunisia,north_africa,33,-9,63378,5510000,tunis,dinar).
c_r_l_l_s_cap_m(turkey,middle_east,39,-36,301380,37930000,ankara,lira).
c_r_l_l_s_cap_m(uganda,east_africa,2,-32,91134,10810000,kampala,uganda_shilling).
c_r_l_l_s_cap_m(united_arab_emirates,middle_east,24,-54,32278,210000,abu_dhabi,dirham).
c_r_l_l_s_cap_m(united_kingdom,western_europe,54,2,94209,55930000,london,pound).
c_r_l_l_s_cap_m(united_states,north_america,37,96,3615122,211210000,washington_dc,dollar).
c_r_l_l_s_cap_m(upper_volta,west_africa,12,2,105869,5740000,ouagadougou,cfa_franc).
c_r_l_l_s_cap_m(uruguay,south_america,-32,55,68548,2990000,montevideo,peso).
c_r_l_l_s_cap_m(venezuela,south_america,8,65,352143,11520000,caracas,bolivar).
c_r_l_l_s_cap_m(vietnam,southeast_east,17,-107,126436,41850000,hanoi,dong).
c_r_l_l_s_cap_m(west_germany,western_europe,52,-9,95815,61970000,bonn,deutsche_mark).
c_r_l_l_s_cap_m(western_samoa,oceania,-14,172,1133,150000,apia,tala).
c_r_l_l_s_cap_m(yemen,middle_east,15,-44,75289,1600000,sana,rial).
c_r_l_l_s_cap_m(yugoslavia,southern_europe,44,-20,98766,21126000,belgrade,dinar).
c_r_l_l_s_cap_m(zaire,central_africa,-3,-23,905063,23560000,kinshasa,zaire).
c_r_l_l_s_cap_m(zambia,southern_africa,-15,-28,290724,4640000,lusaka,kwacha).
c_r_l_l_s_cap_m(zimbabwe,southern_africa,-20,-30,150333,5690000,salisbury,rhodesian_dollar).

:- fixup_exports.
/*

 _________________________________________________________________________
|       Copyright (C) 1982                                                |
|                                                                         |
|       David Warren,                                                     |
|               SRI International, 333 Ravenswood Ave., Menlo Park,       |
|               California 94025, USA;                                    |
|                                                                         |
|       Fernando Pereira,                                                 |
|               Dept. of Architecture, University of Edinburgh,           |
|               20 Chambers St., Edinburgh EH1 1JZ, Scotland              |
|                                                                         |
|       This program may be used, copied, altered or included in other    |
|       programs only for academic purposes and provided that the         |
|       authorship of the initial program is aknowledged.                 |
|       Use for commercial purposes without the previous written          |
|       agreement of the authors is forbidden.                            |
|_________________________________________________________________________|

*/

% Facts about cities.
% ------------------
madeup_city_country_popu(C,Nat,PopOut):- fail,
  ti(city,C), \+ clause(city_country_popu(C,_,_), true),
  once((trans_direct(contains,Nat,C), 
  c_r_l_l_s_cap_m(Nat,_,_,_,_,Pop,_,_))),  
  % estimate at least a quarter of country population
  A is integer(Pop/4000000), 
  % add a magic number
  PopOut is (A*1000) + 666.
city_country_popu(C,Nat,Pop):- madeup_city_country_popu(C,Nat,Pop).

city_country_popu(belle_mead,united_states,8).
city_country_popu(amsterdam,netherlands,800).
city_country_popu(amersfoort,netherlands,120).
city_country_popu(athens,greece,1368).
city_country_popu(bangkok,thailand,1178).
city_country_popu(barcelona,spain,1280).
city_country_popu(berlin,east_germany,3481).
city_country_popu(birmingham,united_kingdom,1112).
city_country_popu(bombay,india,2839).
city_country_popu(brussels,belgium,986).
city_country_popu(bucharest,romania,1237).
city_country_popu(budapest,hungary,1757).
city_country_popu(buenos_aires,argentina,3404).
city_country_popu(cairo,egypt,2373).
city_country_popu(calcutta,india,2549).
city_country_popu(canton,china,1496).
city_country_popu(caracas,venezuela,488).
city_country_popu(chicago,united_states,3621).
city_country_popu(chungking,china,1100).
city_country_popu(dairen,china,544).
city_country_popu(delhi,india,1744).
city_country_popu(detroit,united_states,1850).
city_country_popu(glasgow,united_kingdom,1090).
city_country_popu(hamburg,west_germany,1700).
city_country_popu(harbin,china,760).
city_country_popu(hongkong_city,hongkong,2440).
city_country_popu(hyderabad,india,1086).
city_country_popu(istanbul,turkey,1215).
city_country_popu(jakarta,indonesia,533).
city_country_popu(johannesburg,south_africa,880).
city_country_popu(karachi,pakistan,1126).
city_country_popu(kiev,soviet_union,991).
city_country_popu(kobe,japan,765).
city_country_popu(kowloon,china,547).
city_country_popu(kyoto,japan,1204).
city_country_popu(leningrad,soviet_union,2800).
city_country_popu(lima,peru,835).
city_country_popu(london,united_kingdom,8346).
city_country_popu(los_angeles,united_states,1970).
city_country_popu(madras,india,1416).
city_country_popu(madrid,spain,1700).
city_country_popu(manila,philippines,1025).
city_country_popu(melbourne,australia,1595).
city_country_popu(mexico_city,mexico,3796).
city_country_popu(milan,italy,1269).
city_country_popu(montreal,canada,1109).
city_country_popu(moscow,soviet_union,4800).
city_country_popu(mukden,china,1551).
city_country_popu(nagoya,japan,1337).
city_country_popu(nanking,japan,1020).
city_country_popu(naples,italy,1012).
city_country_popu(new_york,united_states,7795).
city_country_popu(osaka,japan,2547).
city_country_popu(paris,france,2850).
city_country_popu(peking,china,2031).
city_country_popu(philadelphia,united_states,2072).
city_country_popu(pusan,south_korea,474).
city_country_popu(rio_de_janeiro,brazil,2413).
city_country_popu(rome,italy,1760).
city_country_popu(saigon,vietnam,695).
city_country_popu(santiago,chile,1350).
city_country_popu(sao_paulo,brazil,2228).
city_country_popu(seoul,south_korea,1446).
city_country_popu(shanghai,china,5407).
city_country_popu(sian,china,629).
city_country_popu(singapore_city,singapore,1264).
city_country_popu(sydney,australia,1898).
city_country_popu(tehran,iran,1010).
city_country_popu(tientsin,china,1795).
city_country_popu(tokyo,japan,8535).
city_country_popu(toronto,canada,668).
city_country_popu(vienna,austria,1766).
city_country_popu(warsaw,poland,965).
city_country_popu(yokohama,japan,1143).
:- fixup_exports.
/*  Copyright 1986-2020 David H. D. Warren, Fernando C. N. Pereira and
    Jan Wielemaker.

    Permission is hereby granted, free of charge, to any person obtaining a
    copy of this software and associated documentation files (the
    "Software"), to deal in the Software without restriction, including
    without limitation the rights to use, copy, modify, merge, publish,
    distribute, sublicense, and/or sell copies of the Software, and to
    permit persons to whom the Software is furnished to do so, subject to
    the following conditions:

    The above copyright notice and this permission notice shall be included
    in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
    OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
    MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
    IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
    CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
    TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
    SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

:- dynamic(direct_ss/3).

symmetric_pred(spatial,B,X,C):- nonvar(X),nonvar(C),!,symmetric_direct(B,X,C),!.
symmetric_pred(spatial,B,X,C):- symmetric_direct(B,X,C).

symmetric_direct(B,X,C) :- direct_ss(B,X,C).
symmetric_direct(B,X,C) :- direct_ss(B,C,X).

add_ss(B,X,C):- X @> C, !, add_ss(B,C,X).
add_ss(B,X,C):- direct_ss(B, X,C), !.
add_ss(B,X,C):- assertz(direct_ss(B, X,C)), !.

% Facts about Europe.
% ------------------

:-add_ss(borders,albania,greece).
:-add_ss(borders,albania,yugoslavia).
:-add_ss(borders,albania,mediterranean).

:-add_ss(borders,andorra,france).
:-add_ss(borders,andorra,spain).

:-add_ss(borders,austria,czechoslovakia).
:-add_ss(borders,austria,hungary).
:-add_ss(borders,austria,italy).
:-add_ss(borders,austria,liechtenstein).
:-add_ss(borders,austria,switzerland).
:-add_ss(borders,austria,west_germany).
:-add_ss(borders,austria,yugoslavia).

:-add_ss(borders,belgium,france).
:-add_ss(borders,belgium,luxembourg).
:-add_ss(borders,belgium,netherlands).
:-add_ss(borders,belgium,west_germany).
:-add_ss(borders,belgium,atlantic).

:-add_ss(borders,bulgaria,greece).
:-add_ss(borders,bulgaria,romania).
:-add_ss(borders,bulgaria,turkey).
:-add_ss(borders,bulgaria,yugoslavia).
:-add_ss(borders,bulgaria,black_sea).

:-add_ss(borders,cyprus,mediterranean).

:-add_ss(borders,czechoslovakia,austria).
:-add_ss(borders,czechoslovakia,east_germany).
:-add_ss(borders,czechoslovakia,hungary).
:-add_ss(borders,czechoslovakia,poland).
:-add_ss(borders,czechoslovakia,soviet_union).
:-add_ss(borders,czechoslovakia,west_germany).

:-add_ss(borders,denmark,west_germany).
:-add_ss(borders,denmark,atlantic).
:-add_ss(borders,denmark,baltic).

:-add_ss(borders,eire,united_kingdom).
:-add_ss(borders,eire,atlantic).

:-add_ss(borders,finland,norway).
:-add_ss(borders,finland,soviet_union).
:-add_ss(borders,finland,sweden).
:-add_ss(borders,finland,baltic).

:-add_ss(borders,france,andorra).
:-add_ss(borders,france,belgium).
:-add_ss(borders,france,italy).
:-add_ss(borders,france,luxembourg).
:-add_ss(borders,france,monaco).
:-add_ss(borders,france,spain).
:-add_ss(borders,france,switzerland).
:-add_ss(borders,france,west_germany).
:-add_ss(borders,france,atlantic).
:-add_ss(borders,france,mediterranean).

:-add_ss(borders,east_germany,czechoslovakia).
:-add_ss(borders,east_germany,poland).
:-add_ss(borders,east_germany,west_germany).
:-add_ss(borders,east_germany,baltic).

:-add_ss(borders,greece,albania).
:-add_ss(borders,greece,bulgaria).
:-add_ss(borders,greece,turkey).
:-add_ss(borders,greece,yugoslavia).
:-add_ss(borders,greece,mediterranean).

:-add_ss(borders,hungary,austria).
:-add_ss(borders,hungary,czechoslovakia).
:-add_ss(borders,hungary,romania).
:-add_ss(borders,hungary,soviet_union).
:-add_ss(borders,hungary,yugoslavia).

:-add_ss(borders,iceland,atlantic).

:-add_ss(borders,italy,austria).
:-add_ss(borders,italy,france).
:-add_ss(borders,italy,san_marino).
:-add_ss(borders,italy,switzerland).
:-add_ss(borders,italy,yugoslavia).
:-add_ss(borders,italy,mediterranean).

:-add_ss(borders,liechtenstein,austria).
:-add_ss(borders,liechtenstein,switzerland).

:-add_ss(borders,luxembourg,belgium).
:-add_ss(borders,luxembourg,france).
:-add_ss(borders,luxembourg,west_germany).

:-add_ss(borders,malta,mediterranean).

:-add_ss(borders,monaco,france).
:-add_ss(borders,monaco,mediterranean).

:-add_ss(borders,netherlands,belgium).
:-add_ss(borders,netherlands,west_germany).
:-add_ss(borders,netherlands,atlantic).

:-add_ss(borders,norway,finland).
:-add_ss(borders,norway,sweden).
:-add_ss(borders,norway,soviet_union).
:-add_ss(borders,norway,arctic_ocean).
:-add_ss(borders,norway,atlantic).

:-add_ss(borders,poland,czechoslovakia).
:-add_ss(borders,poland,east_germany).
:-add_ss(borders,poland,soviet_union).
:-add_ss(borders,poland,baltic).

:-add_ss(borders,portugal,spain).
:-add_ss(borders,portugal,atlantic).

:-add_ss(borders,romania,bulgaria).
:-add_ss(borders,romania,hungary).
:-add_ss(borders,romania,soviet_union).
:-add_ss(borders,romania,yugoslavia).
:-add_ss(borders,romania,black_sea).

:-add_ss(borders,san_marino,italy).
:-add_ss(borders,san_marino,mediterranean).

:-add_ss(borders,spain,andorra).
:-add_ss(borders,spain,france).
:-add_ss(borders,spain,portugal).
:-add_ss(borders,spain,atlantic).
:-add_ss(borders,spain,mediterranean).

:-add_ss(borders,sweden,finland).
:-add_ss(borders,sweden,norway).
:-add_ss(borders,sweden,atlantic).
:-add_ss(borders,sweden,baltic).

:-add_ss(borders,switzerland,austria).
:-add_ss(borders,switzerland,france).
:-add_ss(borders,switzerland,italy).
:-add_ss(borders,switzerland,liechtenstein).
:-add_ss(borders,switzerland,west_germany).

:-add_ss(borders,west_germany,austria).
:-add_ss(borders,west_germany,belgium).
:-add_ss(borders,west_germany,czechoslovakia).
:-add_ss(borders,west_germany,denmark).
:-add_ss(borders,west_germany,east_germany).
:-add_ss(borders,west_germany,france).
:-add_ss(borders,west_germany,luxembourg).
:-add_ss(borders,west_germany,netherlands).
:-add_ss(borders,west_germany,switzerland).
:-add_ss(borders,west_germany,atlantic).
:-add_ss(borders,west_germany,baltic).

:-add_ss(borders,united_kingdom,eire).
:-add_ss(borders,united_kingdom,atlantic).

:-add_ss(borders,yugoslavia,albania).
:-add_ss(borders,yugoslavia,austria).
:-add_ss(borders,yugoslavia,bulgaria).
:-add_ss(borders,yugoslavia,greece).
:-add_ss(borders,yugoslavia,hungary).
:-add_ss(borders,yugoslavia,italy).
:-add_ss(borders,yugoslavia,romania).
:-add_ss(borders,yugoslavia,mediterranean).

% Facts about Asia.
% ----------------

:-add_ss(borders,afghanistan,china).
:-add_ss(borders,afghanistan,iran).
:-add_ss(borders,afghanistan,pakistan).
:-add_ss(borders,afghanistan,soviet_union).

:-add_ss(borders,bahrain,persian_gulf).

:-add_ss(borders,bangladesh,burma).
:-add_ss(borders,bangladesh,india).
:-add_ss(borders,bangladesh,indian_ocean).

:-add_ss(borders,bhutan,china).
:-add_ss(borders,bhutan,india).

:-add_ss(borders,burma,bangladesh).
:-add_ss(borders,burma,china).
:-add_ss(borders,burma,india).
:-add_ss(borders,burma,laos).
:-add_ss(borders,burma,thailand).
:-add_ss(borders,burma,indian_ocean).

:-add_ss(borders,cambodia,laos).
:-add_ss(borders,cambodia,thailand).
:-add_ss(borders,cambodia,vietnam).
:-add_ss(borders,cambodia,pacific).

:-add_ss(borders,china,afghanistan).
:-add_ss(borders,china,bhutan).
:-add_ss(borders,china,burma).
:-add_ss(borders,china,india).
:-add_ss(borders,china,laos).
:-add_ss(borders,china,mongolia).
:-add_ss(borders,china,nepal).
:-add_ss(borders,china,north_korea).
:-add_ss(borders,china,pakistan).
:-add_ss(borders,china,soviet_union).
:-add_ss(borders,china,vietnam).
:-add_ss(borders,china,pacific).

:-add_ss(borders,india,bangladesh).
:-add_ss(borders,india,bhutan).
:-add_ss(borders,india,burma).
:-add_ss(borders,india,china).
:-add_ss(borders,india,nepal).
:-add_ss(borders,india,pakistan).
:-add_ss(borders,india,indian_ocean).

:-add_ss(borders,indonesia,malaysia).
:-add_ss(borders,indonesia,papua_new_guinea).
:-add_ss(borders,indonesia,indian_ocean).
:-add_ss(borders,indonesia,pacific).

:-add_ss(borders,iran,afghanistan).
:-add_ss(borders,iran,iraq).
:-add_ss(borders,iran,pakistan).
:-add_ss(borders,iran,soviet_union).
:-add_ss(borders,iran,turkey).
:-add_ss(borders,iran,caspian_sea).
:-add_ss(borders,iran,persian_gulf).
:-add_ss(borders,iran,indian_ocean).

:-add_ss(borders,iraq,iran).
:-add_ss(borders,iraq,jordan).
:-add_ss(borders,iraq,kuwait).
:-add_ss(borders,iraq,saudi_arabia).
:-add_ss(borders,iraq,syria).
:-add_ss(borders,iraq,turkey).
:-add_ss(borders,iraq,persian_gulf).

:-add_ss(borders,israel,egypt).
:-add_ss(borders,israel,jordan).
:-add_ss(borders,laos,burma).
:-add_ss(borders,laos,cambodia).
:-add_ss(borders,laos,china).
:-add_ss(borders,laos,thailand).
:-add_ss(borders,laos,vietnam).

:-add_ss(borders,israel,lebanon).
:-add_ss(borders,israel,syria).
:-add_ss(borders,israel,mediterranean).
:-add_ss(borders,israel,red_sea).

:-add_ss(borders,japan,pacific).

:-add_ss(borders,jordan,iraq).
:-add_ss(borders,jordan,israel).
:-add_ss(borders,jordan,saudi_arabia).
:-add_ss(borders,jordan,syria).
:-add_ss(borders,jordan,red_sea).

:-add_ss(borders,kuwait,iraq).
:-add_ss(borders,kuwait,saudi_arabia).
:-add_ss(borders,kuwait,persian_gulf).

:-add_ss(borders,lebanon,israel).
:-add_ss(borders,lebanon,syria).
:-add_ss(borders,lebanon,mediterranean).

:-add_ss(borders,malaysia,indonesia).
:-add_ss(borders,malaysia,singapore).
:-add_ss(borders,malaysia,thailand).
:-add_ss(borders,malaysia,indian_ocean).
:-add_ss(borders,malaysia,pacific).

:-add_ss(borders,maldives,indian_ocean).

:-add_ss(borders,mongolia,china).
:-add_ss(borders,mongolia,soviet_union).

:-add_ss(borders,nepal,china).
:-add_ss(borders,nepal,india).

:-add_ss(borders,north_korea,china).
:-add_ss(borders,north_korea,south_korea).
:-add_ss(borders,north_korea,soviet_union).
:-add_ss(borders,north_korea,pacific).

:-add_ss(borders,oman,saudi_arabia).
:-add_ss(borders,oman,united_arab_emirates).
:-add_ss(borders,oman,south_yemen).
:-add_ss(borders,oman,indian_ocean).

:-add_ss(borders,pakistan,afghanistan).
:-add_ss(borders,pakistan,china).
:-add_ss(borders,pakistan,india).
:-add_ss(borders,pakistan,iran).
:-add_ss(borders,pakistan,indian_ocean).

:-add_ss(borders,philippines,pacific).

:-add_ss(borders,qatar,saudi_arabia).
:-add_ss(borders,qatar,united_arab_emirates).
:-add_ss(borders,qatar,persian_gulf).

:-add_ss(borders,saudi_arabia,iraq).
:-add_ss(borders,saudi_arabia,jordan).
:-add_ss(borders,saudi_arabia,kuwait).
:-add_ss(borders,saudi_arabia,oman).
:-add_ss(borders,saudi_arabia,qatar).
:-add_ss(borders,saudi_arabia,south_yemen).
:-add_ss(borders,saudi_arabia,united_arab_emirates).
:-add_ss(borders,saudi_arabia,yemen).
:-add_ss(borders,saudi_arabia,persian_gulf).
:-add_ss(borders,saudi_arabia,red_sea).

:-add_ss(borders,singapore,malaysia).
:-add_ss(borders,singapore,pacific).

:-add_ss(borders,south_korea,north_korea).
:-add_ss(borders,south_korea,pacific).

:-add_ss(borders,south_yemen,oman).
:-add_ss(borders,south_yemen,saudi_arabia).
:-add_ss(borders,south_yemen,yemen).
:-add_ss(borders,south_yemen,indian_ocean).

:-add_ss(borders,soviet_union,afghanistan).
:-add_ss(borders,soviet_union,china).
:-add_ss(borders,soviet_union,czechoslovakia).
:-add_ss(borders,soviet_union,finland).
:-add_ss(borders,soviet_union,hungary).
:-add_ss(borders,soviet_union,iran).
:-add_ss(borders,soviet_union,mongolia).
:-add_ss(borders,soviet_union,north_korea).
:-add_ss(borders,soviet_union,norway).
:-add_ss(borders,soviet_union,poland).
:-add_ss(borders,soviet_union,romania).
:-add_ss(borders,soviet_union,turkey).
:-add_ss(borders,soviet_union,arctic_ocean).
:-add_ss(borders,soviet_union,baltic).
:-add_ss(borders,soviet_union,black_sea).
:-add_ss(borders,soviet_union,caspian_sea).
:-add_ss(borders,soviet_union,pacific).

:-add_ss(borders,sri_lanka,indian_ocean).

:-add_ss(borders,syria,iraq).
:-add_ss(borders,syria,israel).
:-add_ss(borders,syria,jordan).
:-add_ss(borders,syria,lebanon).
:-add_ss(borders,syria,turkey).
:-add_ss(borders,syria,mediterranean).

:-add_ss(borders,taiwan,pacific).

:-add_ss(borders,thailand,burma).
:-add_ss(borders,thailand,cambodia).
:-add_ss(borders,thailand,laos).
:-add_ss(borders,thailand,malaysia).
:-add_ss(borders,thailand,indian_ocean).
:-add_ss(borders,thailand,pacific).

:-add_ss(borders,turkey,bulgaria).
:-add_ss(borders,turkey,greece).
:-add_ss(borders,turkey,iran).
:-add_ss(borders,turkey,iraq).
:-add_ss(borders,turkey,soviet_union).
:-add_ss(borders,turkey,syria).
:-add_ss(borders,turkey,black_sea).
:-add_ss(borders,turkey,mediterranean).

:-add_ss(borders,united_arab_emirates,oman).
:-add_ss(borders,united_arab_emirates,qatar).
:-add_ss(borders,united_arab_emirates,saudi_arabia).
:-add_ss(borders,united_arab_emirates,persian_gulf).

:-add_ss(borders,vietnam,cambodia).
:-add_ss(borders,vietnam,china).
:-add_ss(borders,vietnam,laos).
:-add_ss(borders,vietnam,pacific).

:-add_ss(borders,yemen,south_yemen).
:-add_ss(borders,yemen,saudi_arabia).
:-add_ss(borders,yemen,red_sea).

% Facts about Africa.
% ------------------

:-add_ss(borders,algeria,libya).
:-add_ss(borders,algeria,mali).
:-add_ss(borders,algeria,mauritania).
:-add_ss(borders,algeria,morocco).
:-add_ss(borders,algeria,niger).
:-add_ss(borders,algeria,tunisia).
:-add_ss(borders,algeria,mediterranean).

:-add_ss(borders,angola,congo).
:-add_ss(borders,angola,south_africa).
:-add_ss(borders,angola,zaire).
:-add_ss(borders,angola,zambia).
:-add_ss(borders,angola,atlantic).

:-add_ss(borders,botswana,south_africa).
:-add_ss(borders,botswana,zimbabwe).

:-add_ss(borders,burundi,rwanda).
:-add_ss(borders,burundi,tanzania).
:-add_ss(borders,burundi,zaire).

:-add_ss(borders,cameroon,central_african_republic).
:-add_ss(borders,cameroon,chad).
:-add_ss(borders,cameroon,congo).
:-add_ss(borders,cameroon,equatorial_guinea).
:-add_ss(borders,cameroon,gabon).
:-add_ss(borders,cameroon,nigeria).
:-add_ss(borders,cameroon,atlantic).

:-add_ss(borders,central_african_republic,cameroon).
:-add_ss(borders,central_african_republic,chad).
:-add_ss(borders,central_african_republic,congo).
:-add_ss(borders,central_african_republic,sudan).
:-add_ss(borders,central_african_republic,zaire).

:-add_ss(borders,chad,cameroon).
:-add_ss(borders,chad,central_african_republic).
:-add_ss(borders,chad,libya).
:-add_ss(borders,chad,niger).
:-add_ss(borders,chad,nigeria).
:-add_ss(borders,chad,sudan).

:-add_ss(borders,congo,angola).
:-add_ss(borders,congo,cameroon).
:-add_ss(borders,congo,central_african_republic).
:-add_ss(borders,congo,gabon).
:-add_ss(borders,congo,zaire).
:-add_ss(borders,congo,atlantic).

:-add_ss(borders,dahomey,niger).
:-add_ss(borders,dahomey,nigeria).
:-add_ss(borders,dahomey,togo).
:-add_ss(borders,dahomey,upper_volta).
:-add_ss(borders,dahomey,atlantic).

:-add_ss(borders,djibouti,ethiopia).
:-add_ss(borders,djibouti,somalia).
:-add_ss(borders,djibouti,indian_ocean).

:-add_ss(borders,egypt,israel).
:-add_ss(borders,egypt,libya).
:-add_ss(borders,egypt,sudan).
:-add_ss(borders,egypt,mediterranean).
:-add_ss(borders,egypt,red_sea).

:-add_ss(borders,equatorial_guinea,cameroon).
:-add_ss(borders,equatorial_guinea,gabon).
:-add_ss(borders,equatorial_guinea,atlantic).

:-add_ss(borders,ethiopia,djibouti).
:-add_ss(borders,ethiopia,kenya).
:-add_ss(borders,ethiopia,somalia).
:-add_ss(borders,ethiopia,sudan).
:-add_ss(borders,ethiopia,red_sea).

:-add_ss(borders,gabon,cameroon).
:-add_ss(borders,gabon,congo).
:-add_ss(borders,gabon,equatorial_guinea).
:-add_ss(borders,gabon,atlantic).

:-add_ss(borders,gambia,senegal).
:-add_ss(borders,gambia,atlantic).

:-add_ss(borders,ghana,ivory_coast).
:-add_ss(borders,ghana,togo).
:-add_ss(borders,ghana,upper_volta).
:-add_ss(borders,ghana,atlantic).

:-add_ss(borders,guinea,guinea_bissau).
:-add_ss(borders,guinea,ivory_coast).
:-add_ss(borders,guinea,liberia).
:-add_ss(borders,guinea,mali).
:-add_ss(borders,guinea,senegal).
:-add_ss(borders,guinea,sierra_leone).
:-add_ss(borders,guinea,atlantic).

:-add_ss(borders,guinea_bissau,guinea).
:-add_ss(borders,guinea_bissau,senegal).
:-add_ss(borders,guinea_bissau,atlantic).

:-add_ss(borders,ivory_coast,ghana).
:-add_ss(borders,ivory_coast,guinea).
:-add_ss(borders,ivory_coast,liberia).
:-add_ss(borders,ivory_coast,mali).
:-add_ss(borders,ivory_coast,upper_volta).
:-add_ss(borders,ivory_coast,atlantic).

:-add_ss(borders,kenya,ethiopia).
:-add_ss(borders,kenya,somalia).
:-add_ss(borders,kenya,sudan).
:-add_ss(borders,kenya,tanzania).
:-add_ss(borders,kenya,uganda).
:-add_ss(borders,kenya,indian_ocean).

:-add_ss(borders,lesotho,south_africa).

:-add_ss(borders,liberia,ivory_coast).
:-add_ss(borders,liberia,guinea).
:-add_ss(borders,liberia,sierra_leone).
:-add_ss(borders,liberia,atlantic).

:-add_ss(borders,libya,algeria).
:-add_ss(borders,libya,chad).
:-add_ss(borders,libya,egypt).
:-add_ss(borders,libya,niger).
:-add_ss(borders,libya,sudan).
:-add_ss(borders,libya,tunisia).
:-add_ss(borders,libya,mediterranean).

:-add_ss(borders,malagasy,indian_ocean).

:-add_ss(borders,malawi,mozambique).
:-add_ss(borders,malawi,tanzania).
:-add_ss(borders,malawi,zambia).

:-add_ss(borders,mali,algeria).
:-add_ss(borders,mali,guinea).
:-add_ss(borders,mali,ivory_coast).
:-add_ss(borders,mali,mauritania).
:-add_ss(borders,mali,niger).
:-add_ss(borders,mali,senegal).
:-add_ss(borders,mali,upper_volta).

:-add_ss(borders,mauritania,algeria).
:-add_ss(borders,mauritania,mali).
:-add_ss(borders,mauritania,morocco).
:-add_ss(borders,mauritania,senegal).
:-add_ss(borders,mauritania,atlantic).

:-add_ss(borders,mauritius,indian_ocean).

:-add_ss(borders,morocco,algeria).
:-add_ss(borders,morocco,mauritania).
:-add_ss(borders,morocco,atlantic).
:-add_ss(borders,morocco,mediterranean).

:-add_ss(borders,mozambique,malawi).
:-add_ss(borders,mozambique,south_africa).
:-add_ss(borders,mozambique,swaziland).
:-add_ss(borders,mozambique,tanzania).
:-add_ss(borders,mozambique,zambia).
:-add_ss(borders,mozambique,zimbabwe).
:-add_ss(borders,mozambique,indian_ocean).

:-add_ss(borders,niger,algeria).
:-add_ss(borders,niger,chad).
:-add_ss(borders,niger,dahomey).
:-add_ss(borders,niger,libya).
:-add_ss(borders,niger,mali).
:-add_ss(borders,niger,nigeria).
:-add_ss(borders,niger,upper_volta).

:-add_ss(borders,nigeria,cameroon).
:-add_ss(borders,nigeria,chad).
:-add_ss(borders,nigeria,dahomey).
:-add_ss(borders,nigeria,niger).
:-add_ss(borders,nigeria,atlantic).

:-add_ss(borders,rwanda,burundi).
:-add_ss(borders,rwanda,tanzania).
:-add_ss(borders,rwanda,uganda).
:-add_ss(borders,rwanda,zaire).

:-add_ss(borders,senegal,gambia).
:-add_ss(borders,senegal,guinea).
:-add_ss(borders,senegal,guinea_bissau).
:-add_ss(borders,senegal,mali).
:-add_ss(borders,senegal,mauritania).
:-add_ss(borders,senegal,atlantic).

:-add_ss(borders,seychelles,indian_ocean).

:-add_ss(borders,sierra_leone,guinea).
:-add_ss(borders,sierra_leone,liberia).
:-add_ss(borders,sierra_leone,atlantic).

:-add_ss(borders,somalia,djibouti).
:-add_ss(borders,somalia,ethiopia).
:-add_ss(borders,somalia,kenya).
:-add_ss(borders,somalia,indian_ocean).

:-add_ss(borders,south_africa,angola).
:-add_ss(borders,south_africa,botswana).
:-add_ss(borders,south_africa,lesotho).
:-add_ss(borders,south_africa,mozambique).
:-add_ss(borders,south_africa,swaziland).
:-add_ss(borders,south_africa,zambia).
:-add_ss(borders,south_africa,zimbabwe).
:-add_ss(borders,south_africa,atlantic).
:-add_ss(borders,south_africa,indian_ocean).

:-add_ss(borders,sudan,chad).
:-add_ss(borders,sudan,central_african_republic).
:-add_ss(borders,sudan,egypt).
:-add_ss(borders,sudan,ethiopia).
:-add_ss(borders,sudan,kenya).
:-add_ss(borders,sudan,libya).
:-add_ss(borders,sudan,uganda).
:-add_ss(borders,sudan,zaire).
:-add_ss(borders,sudan,red_sea).

:-add_ss(borders,swaziland,mozambique).
:-add_ss(borders,swaziland,south_africa).

:-add_ss(borders,tanzania,burundi).
:-add_ss(borders,tanzania,kenya).
:-add_ss(borders,tanzania,malawi).
:-add_ss(borders,tanzania,mozambique).
:-add_ss(borders,tanzania,rwanda).
:-add_ss(borders,tanzania,uganda).
:-add_ss(borders,tanzania,zaire).
:-add_ss(borders,tanzania,zambia).
:-add_ss(borders,tanzania,indian_ocean).

:-add_ss(borders,togo,dahomey).
:-add_ss(borders,togo,ghana).
:-add_ss(borders,togo,upper_volta).
:-add_ss(borders,togo,atlantic).

:-add_ss(borders,tunisia,algeria).
:-add_ss(borders,tunisia,libya).
:-add_ss(borders,tunisia,mediterranean).

:-add_ss(borders,uganda,kenya).
:-add_ss(borders,uganda,rwanda).
:-add_ss(borders,uganda,sudan).
:-add_ss(borders,uganda,tanzania).
:-add_ss(borders,uganda,zaire).

:-add_ss(borders,upper_volta,dahomey).
:-add_ss(borders,upper_volta,ghana).
:-add_ss(borders,upper_volta,ivory_coast).
:-add_ss(borders,upper_volta,mali).
:-add_ss(borders,upper_volta,niger).
:-add_ss(borders,upper_volta,togo).

:-add_ss(borders,zaire,angola).
:-add_ss(borders,zaire,burundi).
:-add_ss(borders,zaire,central_african_republic).
:-add_ss(borders,zaire,congo).
:-add_ss(borders,zaire,rwanda).
:-add_ss(borders,zaire,sudan).
:-add_ss(borders,zaire,tanzania).
:-add_ss(borders,zaire,uganda).
:-add_ss(borders,zaire,zambia).
:-add_ss(borders,zaire,atlantic).

:-add_ss(borders,zambia,angola).
:-add_ss(borders,zambia,malawi).
:-add_ss(borders,zambia,mozambique).
:-add_ss(borders,zambia,south_africa).
:-add_ss(borders,zambia,tanzania).
:-add_ss(borders,zambia,zaire).
:-add_ss(borders,zambia,zimbabwe).

:-add_ss(borders,zimbabwe,botswana).
:-add_ss(borders,zimbabwe,mozambique).
:-add_ss(borders,zimbabwe,south_africa).
:-add_ss(borders,zimbabwe,zambia).


% Facts about America.
% -------------------

:-add_ss(borders,argentina,bolivia).
:-add_ss(borders,argentina,brazil).
:-add_ss(borders,argentina,chile).
:-add_ss(borders,argentina,paraguay).
:-add_ss(borders,argentina,uruguay).
:-add_ss(borders,argentina,atlantic).

:-add_ss(borders,bahamas,atlantic).

:-add_ss(borders,barbados,atlantic).

:-add_ss(borders,belize,guatemala).
:-add_ss(borders,belize,mexico).
:-add_ss(borders,belize,atlantic).

:-add_ss(borders,bolivia,argentina).
:-add_ss(borders,bolivia,brazil).
:-add_ss(borders,bolivia,chile).
:-add_ss(borders,bolivia,paraguay).
:-add_ss(borders,bolivia,peru).

:-add_ss(borders,brazil,argentina).
:-add_ss(borders,brazil,bolivia).
:-add_ss(borders,brazil,colombia).
:-add_ss(borders,brazil,french_guiana).
:-add_ss(borders,brazil,guyana).
:-add_ss(borders,brazil,paraguay).
:-add_ss(borders,brazil,peru).
:-add_ss(borders,brazil,surinam).
:-add_ss(borders,brazil,uruguay).
:-add_ss(borders,brazil,venezuela).
:-add_ss(borders,brazil,atlantic).

:-add_ss(borders,canada,united_states).
:-add_ss(borders,canada,arctic_ocean).
:-add_ss(borders,canada,atlantic).
:-add_ss(borders,canada,pacific).

:-add_ss(borders,chile,argentina).
:-add_ss(borders,chile,bolivia).
:-add_ss(borders,chile,peru).
:-add_ss(borders,chile,pacific).

:-add_ss(borders,colombia,brazil).
:-add_ss(borders,colombia,ecuador).
:-add_ss(borders,colombia,panama).
:-add_ss(borders,colombia,peru).
:-add_ss(borders,colombia,venezuela).
:-add_ss(borders,colombia,atlantic).
:-add_ss(borders,colombia,pacific).

:-add_ss(borders,costa_rica,nicaragua).
:-add_ss(borders,costa_rica,panama).
:-add_ss(borders,costa_rica,atlantic).
:-add_ss(borders,costa_rica,pacific).

:-add_ss(borders,cuba,atlantic).

:-add_ss(borders,dominican_republic,haiti).
:-add_ss(borders,dominican_republic,atlantic).

:-add_ss(borders,ecuador,colombia).
:-add_ss(borders,ecuador,peru).
:-add_ss(borders,ecuador,pacific).

:-add_ss(borders,el_salvador,guatemala).
:-add_ss(borders,el_salvador,honduras).
:-add_ss(borders,el_salvador,pacific).

:-add_ss(borders,french_guiana,brazil).
:-add_ss(borders,french_guiana,surinam).

:-add_ss(borders,greenland,arctic_ocean).
:-add_ss(borders,greenland,atlantic).

:-add_ss(borders,grenada,atlantic).

:-add_ss(borders,guatemala,belize).
:-add_ss(borders,guatemala,el_salvador).
:-add_ss(borders,guatemala,honduras).
:-add_ss(borders,guatemala,mexico).
:-add_ss(borders,guatemala,atlantic).
:-add_ss(borders,guatemala,pacific).

:-add_ss(borders,guyana,brazil).
:-add_ss(borders,guyana,surinam).
:-add_ss(borders,guyana,venezuela).
:-add_ss(borders,guyana,atlantic).

:-add_ss(borders,haiti,dominican_republic).
:-add_ss(borders,haiti,atlantic).

:-add_ss(borders,honduras,el_salvador).
:-add_ss(borders,honduras,guatemala).
:-add_ss(borders,honduras,nicaragua).
:-add_ss(borders,honduras,atlantic).
:-add_ss(borders,honduras,pacific).

:-add_ss(borders,jamaica,atlantic).

:-add_ss(borders,mexico,belize).
:-add_ss(borders,mexico,guatemala).
:-add_ss(borders,mexico,united_states).
:-add_ss(borders,mexico,atlantic).
:-add_ss(borders,mexico,pacific).

:-add_ss(borders,nicaragua,costa_rica).
:-add_ss(borders,nicaragua,honduras).
:-add_ss(borders,nicaragua,atlantic).
:-add_ss(borders,nicaragua,pacific).

:-add_ss(borders,panama,colombia).
:-add_ss(borders,panama,costa_rica).
:-add_ss(borders,panama,atlantic).
:-add_ss(borders,panama,pacific).

:-add_ss(borders,paraguay,argentina).
:-add_ss(borders,paraguay,bolivia).
:-add_ss(borders,paraguay,brazil).

:-add_ss(borders,peru,bolivia).
:-add_ss(borders,peru,brazil).
:-add_ss(borders,peru,chile).
:-add_ss(borders,peru,colombia).
:-add_ss(borders,peru,ecuador).
:-add_ss(borders,peru,pacific).

:-add_ss(borders,surinam,brazil).
:-add_ss(borders,surinam,french_guiana).
:-add_ss(borders,surinam,guyana).

:-add_ss(borders,trinidad_and_tobago,atlantic).

:-add_ss(borders,united_states,canada).
:-add_ss(borders,united_states,mexico).
:-add_ss(borders,united_states,arctic_ocean).
:-add_ss(borders,united_states,atlantic).
:-add_ss(borders,united_states,pacific).

:-add_ss(borders,uruguay,argentina).
:-add_ss(borders,uruguay,brazil).
:-add_ss(borders,uruguay,atlantic).

:-add_ss(borders,venezuela,brazil).
:-add_ss(borders,venezuela,colombia).
:-add_ss(borders,venezuela,guyana).
:-add_ss(borders,venezuela,atlantic).

% Facts about Australasia.
% -----------------------

:-add_ss(borders,australia,indian_ocean).
:-add_ss(borders,australia,pacific).

:-add_ss(borders,fiji,pacific).

:-add_ss(borders,new_zealand,pacific).

:-add_ss(borders,papua_new_guinea,indonesia).
:-add_ss(borders,papua_new_guinea,pacific).

:-add_ss(borders,tonga,pacific).

:-add_ss(borders,western_samoa,pacific).

:-add_ss(borders,antarctica,southern_ocean).

% Facts about oceans and seas.
% ---------------------------

:-add_ss(borders,arctic_ocean,atlantic).
:-add_ss(borders,arctic_ocean,pacific).

:-add_ss(borders,atlantic,arctic_ocean).
:-add_ss(borders,atlantic,indian_ocean).
:-add_ss(borders,atlantic,pacific).
:-add_ss(borders,atlantic,southern_ocean).
:-add_ss(borders,atlantic,baltic).
:-add_ss(borders,atlantic,mediterranean).

:-add_ss(borders,indian_ocean,atlantic).
:-add_ss(borders,indian_ocean,pacific).
:-add_ss(borders,indian_ocean,southern_ocean).
:-add_ss(borders,indian_ocean,persian_gulf).
:-add_ss(borders,indian_ocean,red_sea).

:-add_ss(borders,pacific,arctic_ocean).
:-add_ss(borders,pacific,atlantic).
:-add_ss(borders,pacific,indian_ocean).
:-add_ss(borders,pacific,southern_ocean).

:-add_ss(borders,southern_ocean,atlantic).
:-add_ss(borders,southern_ocean,indian_ocean).
:-add_ss(borders,southern_ocean,pacific).

:-add_ss(borders,baltic,atlantic).

:-add_ss(borders,black_sea,mediterranean).

:-add_ss(borders,mediterranean,atlantic).
:-add_ss(borders,mediterranean,black_sea).

:-add_ss(borders,persian_gulf,indian_ocean).

:-add_ss(borders,red_sea,indian_ocean).

% Countries bordering each ocean and sea.
% --------------------------------------

:-add_ss(borders,arctic_ocean,norway).
:-add_ss(borders,arctic_ocean,soviet_union).
:-add_ss(borders,arctic_ocean,canada).
:-add_ss(borders,arctic_ocean,greenland).
:-add_ss(borders,arctic_ocean,united_states).

:-add_ss(borders,atlantic,belgium).
:-add_ss(borders,atlantic,denmark).
:-add_ss(borders,atlantic,eire).
:-add_ss(borders,atlantic,france).
:-add_ss(borders,atlantic,iceland).
:-add_ss(borders,atlantic,netherlands).
:-add_ss(borders,atlantic,norway).
:-add_ss(borders,atlantic,portugal).
:-add_ss(borders,atlantic,spain).
:-add_ss(borders,atlantic,sweden).
:-add_ss(borders,atlantic,west_germany).
:-add_ss(borders,atlantic,united_kingdom).
:-add_ss(borders,atlantic,angola).
:-add_ss(borders,atlantic,cameroon).
:-add_ss(borders,atlantic,congo).
:-add_ss(borders,atlantic,dahomey).
:-add_ss(borders,atlantic,equatorial_guinea).
:-add_ss(borders,atlantic,gabon).
:-add_ss(borders,atlantic,gambia).
:-add_ss(borders,atlantic,ghana).
:-add_ss(borders,atlantic,guinea).
:-add_ss(borders,atlantic,guinea_bissau).
:-add_ss(borders,atlantic,ivory_coast).
:-add_ss(borders,atlantic,liberia).
:-add_ss(borders,atlantic,mauritania).
:-add_ss(borders,atlantic,morocco).
:-add_ss(borders,atlantic,nigeria).
:-add_ss(borders,atlantic,senegal).
:-add_ss(borders,atlantic,sierra_leone).
:-add_ss(borders,atlantic,south_africa).
:-add_ss(borders,atlantic,togo).
:-add_ss(borders,atlantic,zaire).
:-add_ss(borders,atlantic,argentina).
:-add_ss(borders,atlantic,bahamas).
:-add_ss(borders,atlantic,barbados).
:-add_ss(borders,atlantic,belize).
:-add_ss(borders,atlantic,brazil).
:-add_ss(borders,atlantic,canada).
:-add_ss(borders,atlantic,colombia).
:-add_ss(borders,atlantic,costa_rica).
:-add_ss(borders,atlantic,cuba).
:-add_ss(borders,atlantic,dominican_republic).
:-add_ss(borders,atlantic,french_guiana).
:-add_ss(borders,atlantic,greenland).
:-add_ss(borders,atlantic,grenada).
:-add_ss(borders,atlantic,guatemala).
:-add_ss(borders,atlantic,guyana).
:-add_ss(borders,atlantic,haiti).
:-add_ss(borders,atlantic,honduras).
:-add_ss(borders,atlantic,jamaica).
:-add_ss(borders,atlantic,mexico).
:-add_ss(borders,atlantic,nicaragua).
:-add_ss(borders,atlantic,panama).
:-add_ss(borders,atlantic,surinam).
:-add_ss(borders,atlantic,trinidad_and_tobago).
:-add_ss(borders,atlantic,united_states).
:-add_ss(borders,atlantic,uruguay).
:-add_ss(borders,atlantic,venezuela).

:-add_ss(borders,indian_ocean,bangladesh).
:-add_ss(borders,indian_ocean,burma).
:-add_ss(borders,indian_ocean,india).
:-add_ss(borders,indian_ocean,indonesia).
:-add_ss(borders,indian_ocean,iran).
:-add_ss(borders,indian_ocean,malaysia).
:-add_ss(borders,indian_ocean,maldives).
:-add_ss(borders,indian_ocean,oman).
:-add_ss(borders,indian_ocean,pakistan).
:-add_ss(borders,indian_ocean,south_yemen).
:-add_ss(borders,indian_ocean,sri_lanka).
:-add_ss(borders,indian_ocean,thailand).
:-add_ss(borders,indian_ocean,djibouti).
:-add_ss(borders,indian_ocean,kenya).
:-add_ss(borders,indian_ocean,malagasy).
:-add_ss(borders,indian_ocean,mauritius).
:-add_ss(borders,indian_ocean,mozambique).
:-add_ss(borders,indian_ocean,seychelles).
:-add_ss(borders,indian_ocean,somalia).
:-add_ss(borders,indian_ocean,south_africa).
:-add_ss(borders,indian_ocean,tanzania).
:-add_ss(borders,indian_ocean,australia).

:-add_ss(borders,pacific,cambodia).
:-add_ss(borders,pacific,china).
:-add_ss(borders,pacific,indonesia).
:-add_ss(borders,pacific,japan).
:-add_ss(borders,pacific,malaysia).
:-add_ss(borders,pacific,north_korea).
:-add_ss(borders,pacific,philippines).
:-add_ss(borders,pacific,singapore).
:-add_ss(borders,pacific,south_korea).
:-add_ss(borders,pacific,soviet_union).
:-add_ss(borders,pacific,taiwan).
:-add_ss(borders,pacific,thailand).
:-add_ss(borders,pacific,vietnam).
:-add_ss(borders,pacific,canada).
:-add_ss(borders,pacific,chile).
:-add_ss(borders,pacific,colombia).
:-add_ss(borders,pacific,costa_rica).
:-add_ss(borders,pacific,ecuador).
:-add_ss(borders,pacific,el_salvador).
:-add_ss(borders,pacific,guatemala).
:-add_ss(borders,pacific,honduras).
:-add_ss(borders,pacific,mexico).
:-add_ss(borders,pacific,nicaragua).
:-add_ss(borders,pacific,panama).
:-add_ss(borders,pacific,peru).
:-add_ss(borders,pacific,united_states).
:-add_ss(borders,pacific,australia).
:-add_ss(borders,pacific,fiji).
:-add_ss(borders,pacific,new_zealand).
:-add_ss(borders,pacific,papua_new_guinea).
:-add_ss(borders,pacific,tonga).
:-add_ss(borders,pacific,western_samoa).

:-add_ss(borders,southern_ocean,antarctica).

:-add_ss(borders,baltic,denmark).
:-add_ss(borders,baltic,finland).
:-add_ss(borders,baltic,east_germany).
:-add_ss(borders,baltic,poland).
:-add_ss(borders,baltic,sweden).
:-add_ss(borders,baltic,west_germany).
:-add_ss(borders,baltic,soviet_union).

:-add_ss(borders,black_sea,bulgaria).
:-add_ss(borders,black_sea,romania).
:-add_ss(borders,black_sea,soviet_union).
:-add_ss(borders,black_sea,turkey).

:-add_ss(borders,caspian_sea,iran).
:-add_ss(borders,caspian_sea,soviet_union).

:-add_ss(borders,mediterranean,albania).
:-add_ss(borders,mediterranean,cyprus).
:-add_ss(borders,mediterranean,france).
:-add_ss(borders,mediterranean,greece).
:-add_ss(borders,mediterranean,italy).
:-add_ss(borders,mediterranean,malta).
:-add_ss(borders,mediterranean,monaco).
:-add_ss(borders,mediterranean,san_marino).
:-add_ss(borders,mediterranean,spain).
:-add_ss(borders,mediterranean,yugoslavia).
:-add_ss(borders,mediterranean,israel).
:-add_ss(borders,mediterranean,lebanon).
:-add_ss(borders,mediterranean,syria).
:-add_ss(borders,mediterranean,turkey).
:-add_ss(borders,mediterranean,algeria).
:-add_ss(borders,mediterranean,egypt).
:-add_ss(borders,mediterranean,libya).
:-add_ss(borders,mediterranean,morocco).
:-add_ss(borders,mediterranean,tunisia).

:-add_ss(borders,persian_gulf,bahrain).
:-add_ss(borders,persian_gulf,iran).
:-add_ss(borders,persian_gulf,iraq).
:-add_ss(borders,persian_gulf,kuwait).
:-add_ss(borders,persian_gulf,qatar).
:-add_ss(borders,persian_gulf,saudi_arabia).
:-add_ss(borders,persian_gulf,united_arab_emirates).

:-add_ss(borders,red_sea,israel).
:-add_ss(borders,red_sea,jordan).
:-add_ss(borders,red_sea,saudi_arabia).
:-add_ss(borders,red_sea,yemen).
:-add_ss(borders,red_sea,egypt).
:-add_ss(borders,red_sea,ethiopia).
:-add_ss(borders,red_sea,sudan).
/*

 _________________________________________________________________________
|       Copyright (C) 1982                                                |
|                                                                         |
|       David Warren,                                                     |
|               SRI International, 333 Ravenswood Ave., Menlo Park,       |
|               California 94025, USA;                                    |
|                                                                         |
|       Fernando Pereira,                                                 |
|               Dept. of Architecture, University of Edinburgh,           |
|               20 Chambers St., Edinburgh EH1 1JZ, Scotland              |
|                                                                         |
|       This program may be used, copied, altered or included in other    |
|       programs only for academic purposes and provided that the         |
|       authorship of the initial program is aknowledged.                 |
|       Use for commercial purposes without the previous written          |
|       agreement of the authors is forbidden.                            |
|_________________________________________________________________________|

*/

% Facts about rivers.
% ------------------

path_linkages(river,R,L):-river_flows(R,L).

river_flows(amazon,[atlantic,brazil,peru]).
river_flows(amu_darya,[aral_sea,soviet_union,afghanistan]).
river_flows(amur,[pacific,soviet_union,china,mongolia]).
river_flows(brahmaputra,[indian_ocean,bangladesh,china]).
river_flows(colorado,[pacific,mexico,united_states]).
river_flows(congo_river,[atlantic,zaire,zambia]).
river_flows(cubango,[botswana,south_africa,angola]).
river_flows(danube,[black_sea,romania,yugoslavia,hungary,czechoslovakia,austria,
              west_germany]).
river_flows(don,[black_sea,soviet_union]).
river_flows(elbe,[atlantic,west_germany,east_germany,czechoslovakia]).
river_flows(euphrates,[persian_gulf,iraq,syria,turkey]).
river_flows(ganges,[indian_ocean,india,china]).
river_flows(hwang_ho,[pacific,china]).
river_flows(indus,[indian_ocean,pakistan,india,china]).
river_flows(irrawaddy,[indian_ocean,burma]).
river_flows(lena,[arctic_ocean,soviet_union]).
river_flows(limpopo,[indian_ocean,mozambique,south_africa]).
river_flows(mackenzie,[arctic_ocean,canada]).
river_flows(mekong,[pacific,vietnam,cambodia,laos,china]).
river_flows(mississippi,[atlantic,united_states]).
river_flows(murray,[indian_ocean,australia]).
river_flows(niger_river,[atlantic,nigeria,niger,mali,guinea]).
river_flows(nile,[mediterranean,egypt,sudan,uganda]).
river_flows(ob,[arctic_ocean,soviet_union]).
river_flows(oder,[baltic,poland,czechoslovakia]).
river_flows(orange,[atlantic,south_africa,lesotho]).
river_flows(orinoco,[atlantic,venezuela,colombia]).
river_flows(parana,[atlantic,argentina,paraguay,brazil]).
river_flows(rhine,[atlantic,netherlands,west_germany,switzerland]).
river_flows(rhone,[mediterranean,france,switzerland]).
river_flows(rio_grande,[atlantic,mexico,united_states]).
river_flows(salween,[indian_ocean,burma,china]).
river_flows(senegal_river,[atlantic,senegal,mali,guinea]).
river_flows(seine,[atlantic,france]).
river_flows(tagus,[atlantic,portugal,spain]).
river_flows(vistula,[baltic,poland]).
river_flows(volga,[black_sea,soviet_union]).
river_flows(volta,[atlantic,ghana,upper_volta]).
river_flows(yangtze,[pacific,china]).
river_flows(yenisei,[arctic_ocean,soviet_union,mongolia]).
river_flows(yukon,[pacific,united_states,canada]).
river_flows(zambesi,[indian_ocean,mozambique,zambia,angola]).

/*
 _________________________________________________________________________
|       Copyright (C) 1982                                                |
|                                                                         |
|       David Warren,                                                     |
|               SRI International, 333 Ravenswood Ave., Menlo Park,       |
|               California 94025, USA;                                    |
|                                                                         |
|       Fernando Pereira,                                                 |
|               Dept. of Architecture, University of Edinburgh,           |
|               20 Chambers St., Edinburgh EH1 1JZ, Scotland              |
|                                                                         |
|       This program may be used, copied, altered or included in other    |
|       programs only for academic purposes and provided that the         |
|       authorship of the initial program is aknowledged.                 |
|       Use for commercial purposes without the previous written          |
|       agreement of the authors is forbidden.                            |
|_________________________________________________________________________|

*/

% NDTABL - Meta-information about database relations.

:- discontiguous nd_costs/3.
:- discontiguous nd_costs/4.
:- discontiguous nd_costs/5.
:- discontiguous nd_costs/6.
:- discontiguous nd_costs/7.

nd_costs(<,99,51,51).
nd_costs(=<,99,51,51).
nd_costs(>,99,51,51).
nd_costs(>=,99,51,51).
nd_costs(african,19,26).
nd_costs(aggregate,103,3,100,51).
nd_costs(american,19,26).
nd_costs(measure_value,7,7,22,22,51).
nd_costs(measure_unit,8,51,51).
nd_costs(asian,21,26).
nd_costs(borders,29,2,2).
nd_costs(capital,22,22).
nd_costs(capital,22,22,23).
nd_costs(card,99,100,3).
nd_costs(city,18,18).
nd_costs(continent,8,8).
nd_costs(country,22,22).
nd_costs(drains,16,16,10).
nd_costs(european,19,26).
nd_costs(exceeds,99,61,61).
nd_costs(flows,19,16,22).
nd_costs(flows,19,16,22,22).
nd_costs(in,29,26,15).
nd_costs(latitude,22,22,23).
nd_costs(latitude,23,23).
nd_costs(position_value,7,22,22,26).
nd_costs(ocean,7,7).
nd_costs(one_of,99,200,-99).
nd_costs(place,23,23).
nd_costs(count_value,7,23,23,51).
nd_costs(unit_format,7,51,51).
nd_costs(ratio,99,51,51,3).
nd_costs(region,12,12).
nd_costs(rises,16,16,22).
nd_costs(river,16,16).
nd_costs(sea,8,8).
nd_costs(seamass,10,10).
nd_costs(rel_spatial,7,40,22,22).

/*
nd_costs(area,51,51).
nd_costs(asian,21,26).
nd_costs(african,19,26).
nd_costs(american,19,26).
nd_costs(capital,22,22).
nd_costs(city,18,18).
nd_costs(continent,8,8).
nd_costs(country,22,22).
nd_costs(european,19,26).
nd_costs(latitude,23,23).
nd_costs(longitude,26,26).
nd_costs(ocean,7,7).
nd_costs(place,23,23).
nd_costs(population,51,51).
nd_costs(region,12,12).
nd_costs(river,16,16).
nd_costs(sea,8,8).
nd_costs(seamass,10,10).

nd_costs(<,99,51,51).
nd_costs(=<,99,51,51).
nd_costs(>,99,51,51).
nd_costs(>=,99,51,51).
nd_costs(area,22,22,51).
nd_costs(borders,29,22,22).
nd_costs(capital,22,22,23).
nd_costs(card,99,100,3).
nd_costs(drains,16,16,10).
nd_costs(cp(east,of),40,22,22).
nd_costs(exceeds,99,51,51).
nd_costs(flows,19,16,22).
nd_costs(in,29,26,15).
nd_costs(latitude,22,22,23).
nd_costs(longitude,22,22,26).
nd_costs(cp(north,of),40,22,22).
nd_costs(one_of,99,200,-99).
nd_costs(population,23,23,51).
nd_costs(rises,16,16,22).
nd_costs(cp(south,of),40,22,22).
nd_costs(cp(west,of),40,22,22).

nd_costs(aggregate,103,3,100,51).
nd_costs(flows,19,16,22,22).
nd_costs(ratio,99,51,51,3).
*/
