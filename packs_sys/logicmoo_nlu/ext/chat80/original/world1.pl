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
  

%contain(X,Y) :- trans_direct(contain,X,Y).
%contain(X,Y) :- trans_direct(contain,X,W), contain(W,Y).


trans_pred(spatial,contain,X,Y) :- trans_rel(=,trans_direct(contain),X,Y).
%contain(X,X).

trans_direct(contain,Continent,Region):- continent_contains_region(Continent,Region).
trans_direct(contain,Region,Country):- region_contains_country(Region,Country).
trans_direct(contain,Country,CityOrRiver):- country_contains_thing(Country,CityOrRiver).
trans_direct(contain,Country,River):- path_pred(thru,flow,river,River,Country).




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
  once((trans_direct(contain,Nat,C), 
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

:-add_ss(border,albania,greece).
:-add_ss(border,albania,yugoslavia).
:-add_ss(border,albania,mediterranean).

:-add_ss(border,andorra,france).
:-add_ss(border,andorra,spain).

:-add_ss(border,austria,czechoslovakia).
:-add_ss(border,austria,hungary).
:-add_ss(border,austria,italy).
:-add_ss(border,austria,liechtenstein).
:-add_ss(border,austria,switzerland).
:-add_ss(border,austria,west_germany).
:-add_ss(border,austria,yugoslavia).

:-add_ss(border,belgium,france).
:-add_ss(border,belgium,luxembourg).
:-add_ss(border,belgium,netherlands).
:-add_ss(border,belgium,west_germany).
:-add_ss(border,belgium,atlantic).

:-add_ss(border,bulgaria,greece).
:-add_ss(border,bulgaria,romania).
:-add_ss(border,bulgaria,turkey).
:-add_ss(border,bulgaria,yugoslavia).
:-add_ss(border,bulgaria,black_sea).

:-add_ss(border,cyprus,mediterranean).

:-add_ss(border,czechoslovakia,austria).
:-add_ss(border,czechoslovakia,east_germany).
:-add_ss(border,czechoslovakia,hungary).
:-add_ss(border,czechoslovakia,poland).
:-add_ss(border,czechoslovakia,soviet_union).
:-add_ss(border,czechoslovakia,west_germany).

:-add_ss(border,denmark,west_germany).
:-add_ss(border,denmark,atlantic).
:-add_ss(border,denmark,baltic).

:-add_ss(border,eire,united_kingdom).
:-add_ss(border,eire,atlantic).

:-add_ss(border,finland,norway).
:-add_ss(border,finland,soviet_union).
:-add_ss(border,finland,sweden).
:-add_ss(border,finland,baltic).

:-add_ss(border,france,andorra).
:-add_ss(border,france,belgium).
:-add_ss(border,france,italy).
:-add_ss(border,france,luxembourg).
:-add_ss(border,france,monaco).
:-add_ss(border,france,spain).
:-add_ss(border,france,switzerland).
:-add_ss(border,france,west_germany).
:-add_ss(border,france,atlantic).
:-add_ss(border,france,mediterranean).

:-add_ss(border,east_germany,czechoslovakia).
:-add_ss(border,east_germany,poland).
:-add_ss(border,east_germany,west_germany).
:-add_ss(border,east_germany,baltic).

:-add_ss(border,greece,albania).
:-add_ss(border,greece,bulgaria).
:-add_ss(border,greece,turkey).
:-add_ss(border,greece,yugoslavia).
:-add_ss(border,greece,mediterranean).

:-add_ss(border,hungary,austria).
:-add_ss(border,hungary,czechoslovakia).
:-add_ss(border,hungary,romania).
:-add_ss(border,hungary,soviet_union).
:-add_ss(border,hungary,yugoslavia).

:-add_ss(border,iceland,atlantic).

:-add_ss(border,italy,austria).
:-add_ss(border,italy,france).
:-add_ss(border,italy,san_marino).
:-add_ss(border,italy,switzerland).
:-add_ss(border,italy,yugoslavia).
:-add_ss(border,italy,mediterranean).

:-add_ss(border,liechtenstein,austria).
:-add_ss(border,liechtenstein,switzerland).

:-add_ss(border,luxembourg,belgium).
:-add_ss(border,luxembourg,france).
:-add_ss(border,luxembourg,west_germany).

:-add_ss(border,malta,mediterranean).

:-add_ss(border,monaco,france).
:-add_ss(border,monaco,mediterranean).

:-add_ss(border,netherlands,belgium).
:-add_ss(border,netherlands,west_germany).
:-add_ss(border,netherlands,atlantic).

:-add_ss(border,norway,finland).
:-add_ss(border,norway,sweden).
:-add_ss(border,norway,soviet_union).
:-add_ss(border,norway,arctic_ocean).
:-add_ss(border,norway,atlantic).

:-add_ss(border,poland,czechoslovakia).
:-add_ss(border,poland,east_germany).
:-add_ss(border,poland,soviet_union).
:-add_ss(border,poland,baltic).

:-add_ss(border,portugal,spain).
:-add_ss(border,portugal,atlantic).

:-add_ss(border,romania,bulgaria).
:-add_ss(border,romania,hungary).
:-add_ss(border,romania,soviet_union).
:-add_ss(border,romania,yugoslavia).
:-add_ss(border,romania,black_sea).

:-add_ss(border,san_marino,italy).
:-add_ss(border,san_marino,mediterranean).

:-add_ss(border,spain,andorra).
:-add_ss(border,spain,france).
:-add_ss(border,spain,portugal).
:-add_ss(border,spain,atlantic).
:-add_ss(border,spain,mediterranean).

:-add_ss(border,sweden,finland).
:-add_ss(border,sweden,norway).
:-add_ss(border,sweden,atlantic).
:-add_ss(border,sweden,baltic).

:-add_ss(border,switzerland,austria).
:-add_ss(border,switzerland,france).
:-add_ss(border,switzerland,italy).
:-add_ss(border,switzerland,liechtenstein).
:-add_ss(border,switzerland,west_germany).

:-add_ss(border,west_germany,austria).
:-add_ss(border,west_germany,belgium).
:-add_ss(border,west_germany,czechoslovakia).
:-add_ss(border,west_germany,denmark).
:-add_ss(border,west_germany,east_germany).
:-add_ss(border,west_germany,france).
:-add_ss(border,west_germany,luxembourg).
:-add_ss(border,west_germany,netherlands).
:-add_ss(border,west_germany,switzerland).
:-add_ss(border,west_germany,atlantic).
:-add_ss(border,west_germany,baltic).

:-add_ss(border,united_kingdom,eire).
:-add_ss(border,united_kingdom,atlantic).

:-add_ss(border,yugoslavia,albania).
:-add_ss(border,yugoslavia,austria).
:-add_ss(border,yugoslavia,bulgaria).
:-add_ss(border,yugoslavia,greece).
:-add_ss(border,yugoslavia,hungary).
:-add_ss(border,yugoslavia,italy).
:-add_ss(border,yugoslavia,romania).
:-add_ss(border,yugoslavia,mediterranean).

% Facts about Asia.
% ----------------

:-add_ss(border,afghanistan,china).
:-add_ss(border,afghanistan,iran).
:-add_ss(border,afghanistan,pakistan).
:-add_ss(border,afghanistan,soviet_union).

:-add_ss(border,bahrain,persian_gulf).

:-add_ss(border,bangladesh,burma).
:-add_ss(border,bangladesh,india).
:-add_ss(border,bangladesh,indian_ocean).

:-add_ss(border,bhutan,china).
:-add_ss(border,bhutan,india).

:-add_ss(border,burma,bangladesh).
:-add_ss(border,burma,china).
:-add_ss(border,burma,india).
:-add_ss(border,burma,laos).
:-add_ss(border,burma,thailand).
:-add_ss(border,burma,indian_ocean).

:-add_ss(border,cambodia,laos).
:-add_ss(border,cambodia,thailand).
:-add_ss(border,cambodia,vietnam).
:-add_ss(border,cambodia,pacific).

:-add_ss(border,china,afghanistan).
:-add_ss(border,china,bhutan).
:-add_ss(border,china,burma).
:-add_ss(border,china,india).
:-add_ss(border,china,laos).
:-add_ss(border,china,mongolia).
:-add_ss(border,china,nepal).
:-add_ss(border,china,north_korea).
:-add_ss(border,china,pakistan).
:-add_ss(border,china,soviet_union).
:-add_ss(border,china,vietnam).
:-add_ss(border,china,pacific).

:-add_ss(border,india,bangladesh).
:-add_ss(border,india,bhutan).
:-add_ss(border,india,burma).
:-add_ss(border,india,china).
:-add_ss(border,india,nepal).
:-add_ss(border,india,pakistan).
:-add_ss(border,india,indian_ocean).

:-add_ss(border,indonesia,malaysia).
:-add_ss(border,indonesia,papua_new_guinea).
:-add_ss(border,indonesia,indian_ocean).
:-add_ss(border,indonesia,pacific).

:-add_ss(border,iran,afghanistan).
:-add_ss(border,iran,iraq).
:-add_ss(border,iran,pakistan).
:-add_ss(border,iran,soviet_union).
:-add_ss(border,iran,turkey).
:-add_ss(border,iran,caspian_sea).
:-add_ss(border,iran,persian_gulf).
:-add_ss(border,iran,indian_ocean).

:-add_ss(border,iraq,iran).
:-add_ss(border,iraq,jordan).
:-add_ss(border,iraq,kuwait).
:-add_ss(border,iraq,saudi_arabia).
:-add_ss(border,iraq,syria).
:-add_ss(border,iraq,turkey).
:-add_ss(border,iraq,persian_gulf).

:-add_ss(border,israel,egypt).
:-add_ss(border,israel,jordan).
:-add_ss(border,laos,burma).
:-add_ss(border,laos,cambodia).
:-add_ss(border,laos,china).
:-add_ss(border,laos,thailand).
:-add_ss(border,laos,vietnam).

:-add_ss(border,israel,lebanon).
:-add_ss(border,israel,syria).
:-add_ss(border,israel,mediterranean).
:-add_ss(border,israel,red_sea).

:-add_ss(border,japan,pacific).

:-add_ss(border,jordan,iraq).
:-add_ss(border,jordan,israel).
:-add_ss(border,jordan,saudi_arabia).
:-add_ss(border,jordan,syria).
:-add_ss(border,jordan,red_sea).

:-add_ss(border,kuwait,iraq).
:-add_ss(border,kuwait,saudi_arabia).
:-add_ss(border,kuwait,persian_gulf).

:-add_ss(border,lebanon,israel).
:-add_ss(border,lebanon,syria).
:-add_ss(border,lebanon,mediterranean).

:-add_ss(border,malaysia,indonesia).
:-add_ss(border,malaysia,singapore).
:-add_ss(border,malaysia,thailand).
:-add_ss(border,malaysia,indian_ocean).
:-add_ss(border,malaysia,pacific).

:-add_ss(border,maldives,indian_ocean).

:-add_ss(border,mongolia,china).
:-add_ss(border,mongolia,soviet_union).

:-add_ss(border,nepal,china).
:-add_ss(border,nepal,india).

:-add_ss(border,north_korea,china).
:-add_ss(border,north_korea,south_korea).
:-add_ss(border,north_korea,soviet_union).
:-add_ss(border,north_korea,pacific).

:-add_ss(border,oman,saudi_arabia).
:-add_ss(border,oman,united_arab_emirates).
:-add_ss(border,oman,south_yemen).
:-add_ss(border,oman,indian_ocean).

:-add_ss(border,pakistan,afghanistan).
:-add_ss(border,pakistan,china).
:-add_ss(border,pakistan,india).
:-add_ss(border,pakistan,iran).
:-add_ss(border,pakistan,indian_ocean).

:-add_ss(border,philippines,pacific).

:-add_ss(border,qatar,saudi_arabia).
:-add_ss(border,qatar,united_arab_emirates).
:-add_ss(border,qatar,persian_gulf).

:-add_ss(border,saudi_arabia,iraq).
:-add_ss(border,saudi_arabia,jordan).
:-add_ss(border,saudi_arabia,kuwait).
:-add_ss(border,saudi_arabia,oman).
:-add_ss(border,saudi_arabia,qatar).
:-add_ss(border,saudi_arabia,south_yemen).
:-add_ss(border,saudi_arabia,united_arab_emirates).
:-add_ss(border,saudi_arabia,yemen).
:-add_ss(border,saudi_arabia,persian_gulf).
:-add_ss(border,saudi_arabia,red_sea).

:-add_ss(border,singapore,malaysia).
:-add_ss(border,singapore,pacific).

:-add_ss(border,south_korea,north_korea).
:-add_ss(border,south_korea,pacific).

:-add_ss(border,south_yemen,oman).
:-add_ss(border,south_yemen,saudi_arabia).
:-add_ss(border,south_yemen,yemen).
:-add_ss(border,south_yemen,indian_ocean).

:-add_ss(border,soviet_union,afghanistan).
:-add_ss(border,soviet_union,china).
:-add_ss(border,soviet_union,czechoslovakia).
:-add_ss(border,soviet_union,finland).
:-add_ss(border,soviet_union,hungary).
:-add_ss(border,soviet_union,iran).
:-add_ss(border,soviet_union,mongolia).
:-add_ss(border,soviet_union,north_korea).
:-add_ss(border,soviet_union,norway).
:-add_ss(border,soviet_union,poland).
:-add_ss(border,soviet_union,romania).
:-add_ss(border,soviet_union,turkey).
:-add_ss(border,soviet_union,arctic_ocean).
:-add_ss(border,soviet_union,baltic).
:-add_ss(border,soviet_union,black_sea).
:-add_ss(border,soviet_union,caspian_sea).
:-add_ss(border,soviet_union,pacific).

:-add_ss(border,sri_lanka,indian_ocean).

:-add_ss(border,syria,iraq).
:-add_ss(border,syria,israel).
:-add_ss(border,syria,jordan).
:-add_ss(border,syria,lebanon).
:-add_ss(border,syria,turkey).
:-add_ss(border,syria,mediterranean).

:-add_ss(border,taiwan,pacific).

:-add_ss(border,thailand,burma).
:-add_ss(border,thailand,cambodia).
:-add_ss(border,thailand,laos).
:-add_ss(border,thailand,malaysia).
:-add_ss(border,thailand,indian_ocean).
:-add_ss(border,thailand,pacific).

:-add_ss(border,turkey,bulgaria).
:-add_ss(border,turkey,greece).
:-add_ss(border,turkey,iran).
:-add_ss(border,turkey,iraq).
:-add_ss(border,turkey,soviet_union).
:-add_ss(border,turkey,syria).
:-add_ss(border,turkey,black_sea).
:-add_ss(border,turkey,mediterranean).

:-add_ss(border,united_arab_emirates,oman).
:-add_ss(border,united_arab_emirates,qatar).
:-add_ss(border,united_arab_emirates,saudi_arabia).
:-add_ss(border,united_arab_emirates,persian_gulf).

:-add_ss(border,vietnam,cambodia).
:-add_ss(border,vietnam,china).
:-add_ss(border,vietnam,laos).
:-add_ss(border,vietnam,pacific).

:-add_ss(border,yemen,south_yemen).
:-add_ss(border,yemen,saudi_arabia).
:-add_ss(border,yemen,red_sea).

% Facts about Africa.
% ------------------

:-add_ss(border,algeria,libya).
:-add_ss(border,algeria,mali).
:-add_ss(border,algeria,mauritania).
:-add_ss(border,algeria,morocco).
:-add_ss(border,algeria,niger).
:-add_ss(border,algeria,tunisia).
:-add_ss(border,algeria,mediterranean).

:-add_ss(border,angola,congo).
:-add_ss(border,angola,south_africa).
:-add_ss(border,angola,zaire).
:-add_ss(border,angola,zambia).
:-add_ss(border,angola,atlantic).

:-add_ss(border,botswana,south_africa).
:-add_ss(border,botswana,zimbabwe).

:-add_ss(border,burundi,rwanda).
:-add_ss(border,burundi,tanzania).
:-add_ss(border,burundi,zaire).

:-add_ss(border,cameroon,central_african_republic).
:-add_ss(border,cameroon,chad).
:-add_ss(border,cameroon,congo).
:-add_ss(border,cameroon,equatorial_guinea).
:-add_ss(border,cameroon,gabon).
:-add_ss(border,cameroon,nigeria).
:-add_ss(border,cameroon,atlantic).

:-add_ss(border,central_african_republic,cameroon).
:-add_ss(border,central_african_republic,chad).
:-add_ss(border,central_african_republic,congo).
:-add_ss(border,central_african_republic,sudan).
:-add_ss(border,central_african_republic,zaire).

:-add_ss(border,chad,cameroon).
:-add_ss(border,chad,central_african_republic).
:-add_ss(border,chad,libya).
:-add_ss(border,chad,niger).
:-add_ss(border,chad,nigeria).
:-add_ss(border,chad,sudan).

:-add_ss(border,congo,angola).
:-add_ss(border,congo,cameroon).
:-add_ss(border,congo,central_african_republic).
:-add_ss(border,congo,gabon).
:-add_ss(border,congo,zaire).
:-add_ss(border,congo,atlantic).

:-add_ss(border,dahomey,niger).
:-add_ss(border,dahomey,nigeria).
:-add_ss(border,dahomey,togo).
:-add_ss(border,dahomey,upper_volta).
:-add_ss(border,dahomey,atlantic).

:-add_ss(border,djibouti,ethiopia).
:-add_ss(border,djibouti,somalia).
:-add_ss(border,djibouti,indian_ocean).

:-add_ss(border,egypt,israel).
:-add_ss(border,egypt,libya).
:-add_ss(border,egypt,sudan).
:-add_ss(border,egypt,mediterranean).
:-add_ss(border,egypt,red_sea).

:-add_ss(border,equatorial_guinea,cameroon).
:-add_ss(border,equatorial_guinea,gabon).
:-add_ss(border,equatorial_guinea,atlantic).

:-add_ss(border,ethiopia,djibouti).
:-add_ss(border,ethiopia,kenya).
:-add_ss(border,ethiopia,somalia).
:-add_ss(border,ethiopia,sudan).
:-add_ss(border,ethiopia,red_sea).

:-add_ss(border,gabon,cameroon).
:-add_ss(border,gabon,congo).
:-add_ss(border,gabon,equatorial_guinea).
:-add_ss(border,gabon,atlantic).

:-add_ss(border,gambia,senegal).
:-add_ss(border,gambia,atlantic).

:-add_ss(border,ghana,ivory_coast).
:-add_ss(border,ghana,togo).
:-add_ss(border,ghana,upper_volta).
:-add_ss(border,ghana,atlantic).

:-add_ss(border,guinea,guinea_bissau).
:-add_ss(border,guinea,ivory_coast).
:-add_ss(border,guinea,liberia).
:-add_ss(border,guinea,mali).
:-add_ss(border,guinea,senegal).
:-add_ss(border,guinea,sierra_leone).
:-add_ss(border,guinea,atlantic).

:-add_ss(border,guinea_bissau,guinea).
:-add_ss(border,guinea_bissau,senegal).
:-add_ss(border,guinea_bissau,atlantic).

:-add_ss(border,ivory_coast,ghana).
:-add_ss(border,ivory_coast,guinea).
:-add_ss(border,ivory_coast,liberia).
:-add_ss(border,ivory_coast,mali).
:-add_ss(border,ivory_coast,upper_volta).
:-add_ss(border,ivory_coast,atlantic).

:-add_ss(border,kenya,ethiopia).
:-add_ss(border,kenya,somalia).
:-add_ss(border,kenya,sudan).
:-add_ss(border,kenya,tanzania).
:-add_ss(border,kenya,uganda).
:-add_ss(border,kenya,indian_ocean).

:-add_ss(border,lesotho,south_africa).

:-add_ss(border,liberia,ivory_coast).
:-add_ss(border,liberia,guinea).
:-add_ss(border,liberia,sierra_leone).
:-add_ss(border,liberia,atlantic).

:-add_ss(border,libya,algeria).
:-add_ss(border,libya,chad).
:-add_ss(border,libya,egypt).
:-add_ss(border,libya,niger).
:-add_ss(border,libya,sudan).
:-add_ss(border,libya,tunisia).
:-add_ss(border,libya,mediterranean).

:-add_ss(border,malagasy,indian_ocean).

:-add_ss(border,malawi,mozambique).
:-add_ss(border,malawi,tanzania).
:-add_ss(border,malawi,zambia).

:-add_ss(border,mali,algeria).
:-add_ss(border,mali,guinea).
:-add_ss(border,mali,ivory_coast).
:-add_ss(border,mali,mauritania).
:-add_ss(border,mali,niger).
:-add_ss(border,mali,senegal).
:-add_ss(border,mali,upper_volta).

:-add_ss(border,mauritania,algeria).
:-add_ss(border,mauritania,mali).
:-add_ss(border,mauritania,morocco).
:-add_ss(border,mauritania,senegal).
:-add_ss(border,mauritania,atlantic).

:-add_ss(border,mauritius,indian_ocean).

:-add_ss(border,morocco,algeria).
:-add_ss(border,morocco,mauritania).
:-add_ss(border,morocco,atlantic).
:-add_ss(border,morocco,mediterranean).

:-add_ss(border,mozambique,malawi).
:-add_ss(border,mozambique,south_africa).
:-add_ss(border,mozambique,swaziland).
:-add_ss(border,mozambique,tanzania).
:-add_ss(border,mozambique,zambia).
:-add_ss(border,mozambique,zimbabwe).
:-add_ss(border,mozambique,indian_ocean).

:-add_ss(border,niger,algeria).
:-add_ss(border,niger,chad).
:-add_ss(border,niger,dahomey).
:-add_ss(border,niger,libya).
:-add_ss(border,niger,mali).
:-add_ss(border,niger,nigeria).
:-add_ss(border,niger,upper_volta).

:-add_ss(border,nigeria,cameroon).
:-add_ss(border,nigeria,chad).
:-add_ss(border,nigeria,dahomey).
:-add_ss(border,nigeria,niger).
:-add_ss(border,nigeria,atlantic).

:-add_ss(border,rwanda,burundi).
:-add_ss(border,rwanda,tanzania).
:-add_ss(border,rwanda,uganda).
:-add_ss(border,rwanda,zaire).

:-add_ss(border,senegal,gambia).
:-add_ss(border,senegal,guinea).
:-add_ss(border,senegal,guinea_bissau).
:-add_ss(border,senegal,mali).
:-add_ss(border,senegal,mauritania).
:-add_ss(border,senegal,atlantic).

:-add_ss(border,seychelles,indian_ocean).

:-add_ss(border,sierra_leone,guinea).
:-add_ss(border,sierra_leone,liberia).
:-add_ss(border,sierra_leone,atlantic).

:-add_ss(border,somalia,djibouti).
:-add_ss(border,somalia,ethiopia).
:-add_ss(border,somalia,kenya).
:-add_ss(border,somalia,indian_ocean).

:-add_ss(border,south_africa,angola).
:-add_ss(border,south_africa,botswana).
:-add_ss(border,south_africa,lesotho).
:-add_ss(border,south_africa,mozambique).
:-add_ss(border,south_africa,swaziland).
:-add_ss(border,south_africa,zambia).
:-add_ss(border,south_africa,zimbabwe).
:-add_ss(border,south_africa,atlantic).
:-add_ss(border,south_africa,indian_ocean).

:-add_ss(border,sudan,chad).
:-add_ss(border,sudan,central_african_republic).
:-add_ss(border,sudan,egypt).
:-add_ss(border,sudan,ethiopia).
:-add_ss(border,sudan,kenya).
:-add_ss(border,sudan,libya).
:-add_ss(border,sudan,uganda).
:-add_ss(border,sudan,zaire).
:-add_ss(border,sudan,red_sea).

:-add_ss(border,swaziland,mozambique).
:-add_ss(border,swaziland,south_africa).

:-add_ss(border,tanzania,burundi).
:-add_ss(border,tanzania,kenya).
:-add_ss(border,tanzania,malawi).
:-add_ss(border,tanzania,mozambique).
:-add_ss(border,tanzania,rwanda).
:-add_ss(border,tanzania,uganda).
:-add_ss(border,tanzania,zaire).
:-add_ss(border,tanzania,zambia).
:-add_ss(border,tanzania,indian_ocean).

:-add_ss(border,togo,dahomey).
:-add_ss(border,togo,ghana).
:-add_ss(border,togo,upper_volta).
:-add_ss(border,togo,atlantic).

:-add_ss(border,tunisia,algeria).
:-add_ss(border,tunisia,libya).
:-add_ss(border,tunisia,mediterranean).

:-add_ss(border,uganda,kenya).
:-add_ss(border,uganda,rwanda).
:-add_ss(border,uganda,sudan).
:-add_ss(border,uganda,tanzania).
:-add_ss(border,uganda,zaire).

:-add_ss(border,upper_volta,dahomey).
:-add_ss(border,upper_volta,ghana).
:-add_ss(border,upper_volta,ivory_coast).
:-add_ss(border,upper_volta,mali).
:-add_ss(border,upper_volta,niger).
:-add_ss(border,upper_volta,togo).

:-add_ss(border,zaire,angola).
:-add_ss(border,zaire,burundi).
:-add_ss(border,zaire,central_african_republic).
:-add_ss(border,zaire,congo).
:-add_ss(border,zaire,rwanda).
:-add_ss(border,zaire,sudan).
:-add_ss(border,zaire,tanzania).
:-add_ss(border,zaire,uganda).
:-add_ss(border,zaire,zambia).
:-add_ss(border,zaire,atlantic).

:-add_ss(border,zambia,angola).
:-add_ss(border,zambia,malawi).
:-add_ss(border,zambia,mozambique).
:-add_ss(border,zambia,south_africa).
:-add_ss(border,zambia,tanzania).
:-add_ss(border,zambia,zaire).
:-add_ss(border,zambia,zimbabwe).

:-add_ss(border,zimbabwe,botswana).
:-add_ss(border,zimbabwe,mozambique).
:-add_ss(border,zimbabwe,south_africa).
:-add_ss(border,zimbabwe,zambia).


% Facts about America.
% -------------------

:-add_ss(border,argentina,bolivia).
:-add_ss(border,argentina,brazil).
:-add_ss(border,argentina,chile).
:-add_ss(border,argentina,paraguay).
:-add_ss(border,argentina,uruguay).
:-add_ss(border,argentina,atlantic).

:-add_ss(border,bahamas,atlantic).

:-add_ss(border,barbados,atlantic).

:-add_ss(border,belize,guatemala).
:-add_ss(border,belize,mexico).
:-add_ss(border,belize,atlantic).

:-add_ss(border,bolivia,argentina).
:-add_ss(border,bolivia,brazil).
:-add_ss(border,bolivia,chile).
:-add_ss(border,bolivia,paraguay).
:-add_ss(border,bolivia,peru).

:-add_ss(border,brazil,argentina).
:-add_ss(border,brazil,bolivia).
:-add_ss(border,brazil,colombia).
:-add_ss(border,brazil,french_guiana).
:-add_ss(border,brazil,guyana).
:-add_ss(border,brazil,paraguay).
:-add_ss(border,brazil,peru).
:-add_ss(border,brazil,surinam).
:-add_ss(border,brazil,uruguay).
:-add_ss(border,brazil,venezuela).
:-add_ss(border,brazil,atlantic).

:-add_ss(border,canada,united_states).
:-add_ss(border,canada,arctic_ocean).
:-add_ss(border,canada,atlantic).
:-add_ss(border,canada,pacific).

:-add_ss(border,chile,argentina).
:-add_ss(border,chile,bolivia).
:-add_ss(border,chile,peru).
:-add_ss(border,chile,pacific).

:-add_ss(border,colombia,brazil).
:-add_ss(border,colombia,ecuador).
:-add_ss(border,colombia,panama).
:-add_ss(border,colombia,peru).
:-add_ss(border,colombia,venezuela).
:-add_ss(border,colombia,atlantic).
:-add_ss(border,colombia,pacific).

:-add_ss(border,costa_rica,nicaragua).
:-add_ss(border,costa_rica,panama).
:-add_ss(border,costa_rica,atlantic).
:-add_ss(border,costa_rica,pacific).

:-add_ss(border,cuba,atlantic).

:-add_ss(border,dominican_republic,haiti).
:-add_ss(border,dominican_republic,atlantic).

:-add_ss(border,ecuador,colombia).
:-add_ss(border,ecuador,peru).
:-add_ss(border,ecuador,pacific).

:-add_ss(border,el_salvador,guatemala).
:-add_ss(border,el_salvador,honduras).
:-add_ss(border,el_salvador,pacific).

:-add_ss(border,french_guiana,brazil).
:-add_ss(border,french_guiana,surinam).

:-add_ss(border,greenland,arctic_ocean).
:-add_ss(border,greenland,atlantic).

:-add_ss(border,grenada,atlantic).

:-add_ss(border,guatemala,belize).
:-add_ss(border,guatemala,el_salvador).
:-add_ss(border,guatemala,honduras).
:-add_ss(border,guatemala,mexico).
:-add_ss(border,guatemala,atlantic).
:-add_ss(border,guatemala,pacific).

:-add_ss(border,guyana,brazil).
:-add_ss(border,guyana,surinam).
:-add_ss(border,guyana,venezuela).
:-add_ss(border,guyana,atlantic).

:-add_ss(border,haiti,dominican_republic).
:-add_ss(border,haiti,atlantic).

:-add_ss(border,honduras,el_salvador).
:-add_ss(border,honduras,guatemala).
:-add_ss(border,honduras,nicaragua).
:-add_ss(border,honduras,atlantic).
:-add_ss(border,honduras,pacific).

:-add_ss(border,jamaica,atlantic).

:-add_ss(border,mexico,belize).
:-add_ss(border,mexico,guatemala).
:-add_ss(border,mexico,united_states).
:-add_ss(border,mexico,atlantic).
:-add_ss(border,mexico,pacific).

:-add_ss(border,nicaragua,costa_rica).
:-add_ss(border,nicaragua,honduras).
:-add_ss(border,nicaragua,atlantic).
:-add_ss(border,nicaragua,pacific).

:-add_ss(border,panama,colombia).
:-add_ss(border,panama,costa_rica).
:-add_ss(border,panama,atlantic).
:-add_ss(border,panama,pacific).

:-add_ss(border,paraguay,argentina).
:-add_ss(border,paraguay,bolivia).
:-add_ss(border,paraguay,brazil).

:-add_ss(border,peru,bolivia).
:-add_ss(border,peru,brazil).
:-add_ss(border,peru,chile).
:-add_ss(border,peru,colombia).
:-add_ss(border,peru,ecuador).
:-add_ss(border,peru,pacific).

:-add_ss(border,surinam,brazil).
:-add_ss(border,surinam,french_guiana).
:-add_ss(border,surinam,guyana).

:-add_ss(border,trinidad_and_tobago,atlantic).

:-add_ss(border,united_states,canada).
:-add_ss(border,united_states,mexico).
:-add_ss(border,united_states,arctic_ocean).
:-add_ss(border,united_states,atlantic).
:-add_ss(border,united_states,pacific).

:-add_ss(border,uruguay,argentina).
:-add_ss(border,uruguay,brazil).
:-add_ss(border,uruguay,atlantic).

:-add_ss(border,venezuela,brazil).
:-add_ss(border,venezuela,colombia).
:-add_ss(border,venezuela,guyana).
:-add_ss(border,venezuela,atlantic).

% Facts about Australasia.
% -----------------------

:-add_ss(border,australia,indian_ocean).
:-add_ss(border,australia,pacific).

:-add_ss(border,fiji,pacific).

:-add_ss(border,new_zealand,pacific).

:-add_ss(border,papua_new_guinea,indonesia).
:-add_ss(border,papua_new_guinea,pacific).

:-add_ss(border,tonga,pacific).

:-add_ss(border,western_samoa,pacific).

:-add_ss(border,antarctica,southern_ocean).

% Facts about oceans and seas.
% ---------------------------

:-add_ss(border,arctic_ocean,atlantic).
:-add_ss(border,arctic_ocean,pacific).

:-add_ss(border,atlantic,arctic_ocean).
:-add_ss(border,atlantic,indian_ocean).
:-add_ss(border,atlantic,pacific).
:-add_ss(border,atlantic,southern_ocean).
:-add_ss(border,atlantic,baltic).
:-add_ss(border,atlantic,mediterranean).

:-add_ss(border,indian_ocean,atlantic).
:-add_ss(border,indian_ocean,pacific).
:-add_ss(border,indian_ocean,southern_ocean).
:-add_ss(border,indian_ocean,persian_gulf).
:-add_ss(border,indian_ocean,red_sea).

:-add_ss(border,pacific,arctic_ocean).
:-add_ss(border,pacific,atlantic).
:-add_ss(border,pacific,indian_ocean).
:-add_ss(border,pacific,southern_ocean).

:-add_ss(border,southern_ocean,atlantic).
:-add_ss(border,southern_ocean,indian_ocean).
:-add_ss(border,southern_ocean,pacific).

:-add_ss(border,baltic,atlantic).

:-add_ss(border,black_sea,mediterranean).

:-add_ss(border,mediterranean,atlantic).
:-add_ss(border,mediterranean,black_sea).

:-add_ss(border,persian_gulf,indian_ocean).

:-add_ss(border,red_sea,indian_ocean).

% Countries bordering each ocean and sea.
% --------------------------------------

:-add_ss(border,arctic_ocean,norway).
:-add_ss(border,arctic_ocean,soviet_union).
:-add_ss(border,arctic_ocean,canada).
:-add_ss(border,arctic_ocean,greenland).
:-add_ss(border,arctic_ocean,united_states).

:-add_ss(border,atlantic,belgium).
:-add_ss(border,atlantic,denmark).
:-add_ss(border,atlantic,eire).
:-add_ss(border,atlantic,france).
:-add_ss(border,atlantic,iceland).
:-add_ss(border,atlantic,netherlands).
:-add_ss(border,atlantic,norway).
:-add_ss(border,atlantic,portugal).
:-add_ss(border,atlantic,spain).
:-add_ss(border,atlantic,sweden).
:-add_ss(border,atlantic,west_germany).
:-add_ss(border,atlantic,united_kingdom).
:-add_ss(border,atlantic,angola).
:-add_ss(border,atlantic,cameroon).
:-add_ss(border,atlantic,congo).
:-add_ss(border,atlantic,dahomey).
:-add_ss(border,atlantic,equatorial_guinea).
:-add_ss(border,atlantic,gabon).
:-add_ss(border,atlantic,gambia).
:-add_ss(border,atlantic,ghana).
:-add_ss(border,atlantic,guinea).
:-add_ss(border,atlantic,guinea_bissau).
:-add_ss(border,atlantic,ivory_coast).
:-add_ss(border,atlantic,liberia).
:-add_ss(border,atlantic,mauritania).
:-add_ss(border,atlantic,morocco).
:-add_ss(border,atlantic,nigeria).
:-add_ss(border,atlantic,senegal).
:-add_ss(border,atlantic,sierra_leone).
:-add_ss(border,atlantic,south_africa).
:-add_ss(border,atlantic,togo).
:-add_ss(border,atlantic,zaire).
:-add_ss(border,atlantic,argentina).
:-add_ss(border,atlantic,bahamas).
:-add_ss(border,atlantic,barbados).
:-add_ss(border,atlantic,belize).
:-add_ss(border,atlantic,brazil).
:-add_ss(border,atlantic,canada).
:-add_ss(border,atlantic,colombia).
:-add_ss(border,atlantic,costa_rica).
:-add_ss(border,atlantic,cuba).
:-add_ss(border,atlantic,dominican_republic).
:-add_ss(border,atlantic,french_guiana).
:-add_ss(border,atlantic,greenland).
:-add_ss(border,atlantic,grenada).
:-add_ss(border,atlantic,guatemala).
:-add_ss(border,atlantic,guyana).
:-add_ss(border,atlantic,haiti).
:-add_ss(border,atlantic,honduras).
:-add_ss(border,atlantic,jamaica).
:-add_ss(border,atlantic,mexico).
:-add_ss(border,atlantic,nicaragua).
:-add_ss(border,atlantic,panama).
:-add_ss(border,atlantic,surinam).
:-add_ss(border,atlantic,trinidad_and_tobago).
:-add_ss(border,atlantic,united_states).
:-add_ss(border,atlantic,uruguay).
:-add_ss(border,atlantic,venezuela).

:-add_ss(border,indian_ocean,bangladesh).
:-add_ss(border,indian_ocean,burma).
:-add_ss(border,indian_ocean,india).
:-add_ss(border,indian_ocean,indonesia).
:-add_ss(border,indian_ocean,iran).
:-add_ss(border,indian_ocean,malaysia).
:-add_ss(border,indian_ocean,maldives).
:-add_ss(border,indian_ocean,oman).
:-add_ss(border,indian_ocean,pakistan).
:-add_ss(border,indian_ocean,south_yemen).
:-add_ss(border,indian_ocean,sri_lanka).
:-add_ss(border,indian_ocean,thailand).
:-add_ss(border,indian_ocean,djibouti).
:-add_ss(border,indian_ocean,kenya).
:-add_ss(border,indian_ocean,malagasy).
:-add_ss(border,indian_ocean,mauritius).
:-add_ss(border,indian_ocean,mozambique).
:-add_ss(border,indian_ocean,seychelles).
:-add_ss(border,indian_ocean,somalia).
:-add_ss(border,indian_ocean,south_africa).
:-add_ss(border,indian_ocean,tanzania).
:-add_ss(border,indian_ocean,australia).

:-add_ss(border,pacific,cambodia).
:-add_ss(border,pacific,china).
:-add_ss(border,pacific,indonesia).
:-add_ss(border,pacific,japan).
:-add_ss(border,pacific,malaysia).
:-add_ss(border,pacific,north_korea).
:-add_ss(border,pacific,philippines).
:-add_ss(border,pacific,singapore).
:-add_ss(border,pacific,south_korea).
:-add_ss(border,pacific,soviet_union).
:-add_ss(border,pacific,taiwan).
:-add_ss(border,pacific,thailand).
:-add_ss(border,pacific,vietnam).
:-add_ss(border,pacific,canada).
:-add_ss(border,pacific,chile).
:-add_ss(border,pacific,colombia).
:-add_ss(border,pacific,costa_rica).
:-add_ss(border,pacific,ecuador).
:-add_ss(border,pacific,el_salvador).
:-add_ss(border,pacific,guatemala).
:-add_ss(border,pacific,honduras).
:-add_ss(border,pacific,mexico).
:-add_ss(border,pacific,nicaragua).
:-add_ss(border,pacific,panama).
:-add_ss(border,pacific,peru).
:-add_ss(border,pacific,united_states).
:-add_ss(border,pacific,australia).
:-add_ss(border,pacific,fiji).
:-add_ss(border,pacific,new_zealand).
:-add_ss(border,pacific,papua_new_guinea).
:-add_ss(border,pacific,tonga).
:-add_ss(border,pacific,western_samoa).

:-add_ss(border,southern_ocean,antarctica).

:-add_ss(border,baltic,denmark).
:-add_ss(border,baltic,finland).
:-add_ss(border,baltic,east_germany).
:-add_ss(border,baltic,poland).
:-add_ss(border,baltic,sweden).
:-add_ss(border,baltic,west_germany).
:-add_ss(border,baltic,soviet_union).

:-add_ss(border,black_sea,bulgaria).
:-add_ss(border,black_sea,romania).
:-add_ss(border,black_sea,soviet_union).
:-add_ss(border,black_sea,turkey).

:-add_ss(border,caspian_sea,iran).
:-add_ss(border,caspian_sea,soviet_union).

:-add_ss(border,mediterranean,albania).
:-add_ss(border,mediterranean,cyprus).
:-add_ss(border,mediterranean,france).
:-add_ss(border,mediterranean,greece).
:-add_ss(border,mediterranean,italy).
:-add_ss(border,mediterranean,malta).
:-add_ss(border,mediterranean,monaco).
:-add_ss(border,mediterranean,san_marino).
:-add_ss(border,mediterranean,spain).
:-add_ss(border,mediterranean,yugoslavia).
:-add_ss(border,mediterranean,israel).
:-add_ss(border,mediterranean,lebanon).
:-add_ss(border,mediterranean,syria).
:-add_ss(border,mediterranean,turkey).
:-add_ss(border,mediterranean,algeria).
:-add_ss(border,mediterranean,egypt).
:-add_ss(border,mediterranean,libya).
:-add_ss(border,mediterranean,morocco).
:-add_ss(border,mediterranean,tunisia).

:-add_ss(border,persian_gulf,bahrain).
:-add_ss(border,persian_gulf,iran).
:-add_ss(border,persian_gulf,iraq).
:-add_ss(border,persian_gulf,kuwait).
:-add_ss(border,persian_gulf,qatar).
:-add_ss(border,persian_gulf,saudi_arabia).
:-add_ss(border,persian_gulf,united_arab_emirates).

:-add_ss(border,red_sea,israel).
:-add_ss(border,red_sea,jordan).
:-add_ss(border,red_sea,saudi_arabia).
:-add_ss(border,red_sea,yemen).
:-add_ss(border,red_sea,egypt).
:-add_ss(border,red_sea,ethiopia).
:-add_ss(border,red_sea,sudan).
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

intrans_LF(Drain,SpatialRiver,X,path_pred(ends(Flow),Drain,River,X,Y), 
   [slot(prep(into),Spatial&_,Y,_,free)],_):- 
 type_begins_thru_ends(River, _Rise, Flow, Drain),
 bfeature_path(Spatial,River,SpatialRiver).

intrans_LF(Flow,SpatialRiver,X,path_pred(thru,Flow,River,X,Y),
   [slot(prep(through),Spatial&_,Y,_,free)],_):- 
 type_begins_thru_ends(River,_Rise, Flow, _Drain),
 bfeature_path(Spatial,River,SpatialRiver).

intrans_LF(Flow,SpatialRiver,X,path_pred_links(Flow,River,X,Y,Z),
   [slot(prep(into),Spatial&_,Z,_,free),
    slot(prep(from),Spatial&_,Y,_,free)],_):- 
 type_begins_thru_ends(River,_Rise, Flow, _Drain),
 bfeature_path(Spatial,River,SpatialRiver).

intrans_LF(Rise,SpatialRiver,X,path_pred(begins(Flow),Rise,River,X,Y),
   [slot(prep(in),Spatial&_,Y,_,free)],_):- 
 type_begins_thru_ends(River, Rise, Flow, _Drain),
 bfeature_path(Spatial,River,SpatialRiver).

ti(Type,R) :- 
   type_begins_thru_ends(Type,_Rise, Flow, _Drain),
   path_linkages(Flow,Type,R,_L).

path_pred(begins(Flow),Rise,Type,R,C) :-
  type_begins_thru_ends(Type,Rise,Flow,_Drain),
  path_linkages(Flow,Type,R,L), last_link(L,C).

path_pred(ends(Flow),Drain,Type,R,S) :- 
  type_begins_thru_ends(Type,_Rise, Flow, Drain),
  path_linkages(Flow,Type,R,L), first_link(L,S).

path_pred(thru,Flow,Type,R,C) :-
 type_begins_thru_ends(Type,_Rise, Flow, _Drain),
 path_pred_links(Flow,Type,R,C,_).

path_pred_links(Flow,Type,R,C1,C2) :- 
 type_begins_thru_ends(Type,_Rise, Flow, _Drain),
 path_linkages(Flow,Type,R,L), link_pairs(L,C2,C1).

first_link([X|_],X).

last_link([X],X).
last_link([_|L],X) :- last_link(L,X).

link_pairs([X1,X2|_],X1,X2).
link_pairs([_|L],X1,X2) :- link_pairs(L,X1,X2).




% Facts about rivers.
% ------------------

type_begins_thru_ends(river,rise,flow,drain).

verb_type_lex_80(rise,main+iv).
verb_type_lex_80(flow,main+iv).
verb_type_lex_80(drain,main+iv).

verb_root_lex_80(rise).
verb_root_lex_80(flow).
verb_root_lex_80(drain).

regular_pres_lex_80(rise).
regular_pres_lex_80(flow).
regular_pres_lex_80(drain).

regular_past_lex_80(flowed,flow).
regular_past_lex_80(drained,drain).

verb_form_lex_80(rose,rise,past+fin,_).
verb_form_lex_80(rises,rise,pres+fin,3+sg).
verb_form_lex_80(risen,rise,past+part,_).
verb_form_lex_80(flows,flow,pres+fin,3+sg).
verb_form_lex_80(flowing,flow,pres+part,_).
verb_form_lex_80(drains,drain,pres+fin,3+sg).
verb_form_lex_80(draining,drain,pres+part,_).


path_linkages(flow,river,R,L):-river_flows(R,L).

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
nd_costs(aggregate80,103,3,100,51).
nd_costs(american,19,26).
nd_costs(measure_value,7,7,22,22,51).
nd_costs(measure_unit,8,51,51).
nd_costs(asian,21,26).
nd_costs(border,29,2,2).
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
nd_costs(border,29,22,22).
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

nd_costs(aggregate80,103,3,100,51).
nd_costs(flows,19,16,22,22).
nd_costs(ratio,99,51,51,3).
*/
