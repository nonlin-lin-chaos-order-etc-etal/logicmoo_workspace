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

% Data for the World Database.
% ---------------------------

:- discontiguous unit_format/2. 


% Interface.
% ---------

database80(aggregate(X,Y,Z)) :- aggregate(X,Y,Z).
database80(one_of(X,Y)) :- one_of(X,Y).
database80(ratio(X,Y,Z)) :- ratio(X,Y,Z).
database80(card(X,Y)) :- card(X,Y).
database80(borders(X,Y)) :- borders(X,Y).
%database80(capital(X)) :- capital(X).
database80(country_capital_city(X,Y)) :- country_capital_city(X,Y).
database80(circle_of_latitude(X)) :- circle_of_latitude(X).
%database80(city(X)) :- city(X).
database80(continent(X)) :- continent(X).
database80(country(X)) :- country(X).
database80(exceeds(X,Y)) :- exceeds(X,Y).
database80(loc_in(X,Y)) :- loc_in(X,Y).
database80(ocean(X)) :- ocean(X).
database80(ti(Place,X)) :- ti(Place,X).
%database80(person(X)) :- person(X).	% JW: person is not defined
database80(measure_value(count,population,X)) :- measure_value(count,population,X).
%database80(region(X)) :- region(X).

database80(river(X)) :- river(X).
database80(sea(X)) :- sea(X).
database80(seamass(X)) :- seamass(X).
database80(rel_spatial(Of,X,Y)) :- rel_spatial(Of,X,Y).
database80(unit_format(U,X)) :- unit_format(U,X).
database80(position_value(U,X,Y)) :- position_value(U,X,Y).
database80(measure_value(float,U,X,Y)) :- measure_value(float,U,X,Y).
database80(measure_value(count,population,X,Y)) :- measure_value(count,population,X,Y).
database80(asian(X)) :- asian(X).
database80(european(X)) :- european(X).
database80(african(X)) :- african(X).
database80(american(X)) :- american(X).

database80(flow_begins(river,X,Y)) :- flow_begins(river,X,Y).
database80(flow_ends(river,X,Y)) :- flow_ends(river,X,Y).
database80(flow_thru(river,X,Y)) :- flow_thru(river,X,Y).
database80(flow_from_to(river,X,Y,Z)) :- flow_from_to(river,X,Y,Z).


:-op(500,xfy,--).

%exceeds(X--U,Y--U) :- !, X > Y.
%exceeds(X1--U1,X2--U2) :- ratio(U1,U2,M1,M2), X1*M1 > X2*M2.
exceeds(X,Y):- term_variables(X-Y,Vars),freeze_until(Vars,exceeds0(X,Y)).

freeze_until([],Goal):-!, term_variables(Goal, Vars),(Vars==[] -> Goal ; freeze_until(Vars,Goal)).
freeze_until([V|Vars],Goal):- freeze(V,freeze_until(Vars,Goal)).

exceeds0(X--U,Y--U) :- !, X > Y.
exceeds0(X1--U1,X2--U2) :- ratio(U1,U2,M1,M2), X1*M1 > X2*M2.



ratio(thousand,million,1,1000).
ratio(million,thousand,1000,1).
ratio(ksqmiles,sqmiles,1000,1).
ratio(sqmiles,ksqmiles,1,1000).

unit_format(area,_X--ksqmiles).
ti(capital_city,C) :- country_capital_city(_X,C).
%ti(city,C) :- ti(capital_city,C).
%ti(city,C) :- clause(city_country_popu(C,_,_), true).
ti(city,C) :- country_contains_thing(_,C), \+ ti(river,C).
ti(country,C) :- c_r_l_l_s_cap_m(C,_,_,_,_,_,_,_).
unit_format(latitude,_X--degrees).
unit_format(longitude,_X--degrees).
ti(place,X) :- ti(continent,X); ti(region,X); ti(seamass,X); ti(country,X).
unit_format(population,_X--million).
unit_format(population,_X--thousand).
ti(region,R) :- continent_contains_region(_,R).

% if X is located in africa then X is african.
ti(african,X) :- loc_in(X,africa).
ti(american,X) :- loc_in(X,america).
ti(asian,X) :- loc_in(X,asia).
ti(european,X) :- loc_in(X,europe).

/*
loc_in(X,Y) :- var(X), nonvar(Y), !, contains(Y,X).
loc_in(X,Y) :- in0(X,W), ( W=Y ; loc_in(W,Y) ).

in0(X,Y) :- in_continent(X,Y).
in0(X,Y) :- city_country_popu(X,Y,_).
in0(X,Y) :- c_r_l_l_s_cap_m(X,Y,_,_,_,_,_,_).
in0(X,Y) :- flow_thru(_,X,Y).
*/

rel_spatial(cp(east,of),X1,X2) :- position_value(longitude,X1,L1), position_value(longitude,X2,L2), exceeds(L2,L1).
rel_spatial(cp(north,of),X1,X2) :- position_value(latitude,X1,L1), position_value(latitude,X2,L2), exceeds(L1,L2).
rel_spatial(cp(south,of),X1,X2) :- position_value(latitude,X1,L1), position_value(latitude,X2,L2), exceeds(L2,L1).
rel_spatial(cp(west,of),X1,X2) :- position_value(longitude,X1,L1), position_value(longitude,X2,L2), exceeds(L1,L2).


circle_of_latitude(equator).
circle_of_latitude(tropic_of_cancer).
circle_of_latitude(tropic_of_capricorn).
circle_of_latitude(arctic_circle).
circle_of_latitude(antarctic_circle).

position_value(latitude,equator,0--degrees).
position_value(latitude,tropic_of_cancer,23--degrees).
position_value(latitude,tropic_of_capricorn,(-23)--degrees).
position_value(latitude,arctic_circle,67--degrees).
position_value(latitude,antarctic_circle,(-67)--degrees).

position_value(latitude,C,L--degrees) :- c_r_l_l_s_cap_m(C,_,L,_,_,_,_,_).
position_value(longitude,C,L--degrees) :- c_r_l_l_s_cap_m(C,_,_,L,_,_,_,_).

measure_value(float,area,C,A--ksqmiles) :- c_r_l_l_s_cap_m(C,_,_,_,A0,_,_,_), A is A0/1000.

measure_value(count,population,C,P--thousand) :- city_country_popu(C,_,P).
measure_value(count,population,C,P--million) :- c_r_l_l_s_cap_m(C,_,_,_,_,P0,_,_), P is integer(P0/1.0E6).

country_capital_city(C,Cap) :- c_r_l_l_s_cap_m(C,_,_,_,_,_,Cap,_).

ti(continent,X):- continent(X).
continent(africa).
continent(america).
continent(antarctica).
continent(asia).
continent(australasia).
continent(europe).

/*
in_continent(scandinavia, europe).
in_continent(western_europe, europe).
in_continent(eastern_europe, europe).
in_continent(southern_europe, europe).
in_continent(north_america, america).
in_continent(central_america, america).
in_continent(caribbean, america).
in_continent(south_america, america).
in_continent(north_africa, africa).
in_continent(west_africa, africa).
in_continent(central_africa, africa).
in_continent(east_africa, africa).
in_continent(southern_africa, africa).
in_continent(middle_east,  asia).
in_continent(indian_subcontinent, asia).
in_continent(southeast_east, asia).
in_continent(far_east, asia).
in_continent(northern_asia, asia).
in_continent(oceania,australasia).
*/
in_continent(R,C):- continent_contains_region(C,R).

ti(seamass,X):- seamass(X).
seamass(X) :- ocean(X).
seamass(X) :- sea(X).

ti(ocean,arctic_ocean).
ti(ocean,atlantic).
ti(ocean,indian_ocean).
ti(ocean,pacific).
ti(ocean,southern_ocean).

ocean(X):- ti(ocean,X).

ti(sea,baltic).
ti(sea,black_sea).
ti(sea,caspian).
ti(sea,mediterranean).
ti(sea,persian_gulf).
ti(sea,red_sea).

sea(X):- ti(sea,X).

river(X):- ti(river,X).
ti(river,R) :- flow_link(river,R,_L).


flow_begins(river,R,C) :- flow_link(river,R,L), last_link(L,C).

flow_ends(river,R,S) :- flow_link(river,R,L), first_link(L,S).

flow_thru(river,R,C) :- flow_from_to(river,R,C,_).

flow_from_to(river,R,C1,C2) :- flow_link(river,R,L), link_pairs(L,C2,C1).

first_link([X|_],X).

last_link([X],X).
last_link([_|L],X) :- last_link(L,X).

link_pairs([X1,X2|_],X1,X2).
link_pairs([_|L],X1,X2) :- link_pairs(L,X1,X2).
