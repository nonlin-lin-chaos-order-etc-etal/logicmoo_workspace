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


% Interface.
% ---------

% Interface.
% ---------

database80(aggregate(X,Y,Z)) :- aggregate(X,Y,Z).
database80(one_of(X,Y)) :- one_of(X,Y).
database80(ratio(X,Y,Z)) :- ratio(X,Y,Z).
database80(card(X,Y)) :- card(X,Y).
database80(african(X)) :- african(X).
database80(american(X)) :- american(X).
database80(area(X)) :- area(X).
database80(area(X,Y)) :- area(X,Y).
database80(asian(X)) :- asian(X).
database80(borders(X,Y)) :- borders(X,Y).
database80(capital(X)) :- capital(X).
database80(capital(X,Y)) :- capital(X,Y).
database80(circle_of_latitude(X)) :- circle_of_latitude(X).
database80(city(X)) :- city(X).
database80(continent(X)) :- continent(X).
database80(country(X)) :- country(X).
database80(drains(X,Y)) :- drains(X,Y).
database80(eastof(X,Y)) :- eastof(X,Y).
database80(european(X)) :- european(X).
database80(exceeds(X,Y)) :- exceeds(X,Y).
database80(flows(X,Y)) :- flows(X,Y).
database80(flows(X,Y,Z)) :- flows(X,Y,Z).
database80(loc_in(X,Y)) :- loc_in(X,Y).
database80(latitude(X)) :- latitude(X).
database80(latitude(X,Y)) :- latitude(X,Y).
database80(longitude(X)) :- longitude(X).
database80(longitude(X,Y)) :- longitude(X,Y).
database80(northof(X,Y)) :- northof(X,Y).
database80(ocean(X)) :- ocean(X).
database80(place(X)) :- place(X).
%database80(person(X)) :- person(X).	% JW: person is not defined
database80(population(X)) :- population(X).
database80(population(X,Y)) :- population(X,Y).
database80(region(X)) :- region(X).
database80(rises(X,Y)) :- rises(X,Y).
database80(river(X)) :- river(X).
database80(sea(X)) :- sea(X).
database80(seamass(X)) :- seamass(X).
database80(southof(X,Y)) :- southof(X,Y).
database80(westof(X,Y)) :- westof(X,Y).

:-op(500,xfy,--).

exceeds(X--U,Y--U) :- !, X > Y.
exceeds(X1--U1,X2--U2) :- ratio(U1,U2,M1,M2), X1*M1 > X2*M2.

ratio(thousand,million,1,1000).
ratio(million,thousand,1000,1).
ratio(ksqmiles,sqmiles,1000,1).
ratio(sqmiles,ksqmiles,1,1000).

area(_X--ksqmiles).
capital(C) :- capital(_X,C).
city(C) :- city_country_popu(C,_,_).
country(C) :- c_r_l_l_s_cap_m(C,_,_,_,_,_,_,_).
latitude(_X--degrees).
longitude(_X--degrees).
place(X) :- continent(X); region(X); seamass(X); country(X).
population(_X--million).
population(_X--thousand).
region(R) :- in_continent(R,_).

african(X) :- loc_in(X,africa).
american(X) :- loc_in(X,america).
asian(X) :- loc_in(X,asia).
european(X) :- loc_in(X,europe).

loc_in(X,Y) :- var(X), nonvar(Y), !, contains(Y,X).
loc_in(X,Y) :- in0(X,W), ( W=Y ; loc_in(W,Y) ).

in0(X,Y) :- in_continent(X,Y).
in0(X,Y) :- city_country_popu(X,Y,_).
in0(X,Y) :- c_r_l_l_s_cap_m(X,Y,_,_,_,_,_,_).
in0(X,Y) :- flows(X,Y).

eastof(X1,X2) :- longitude(X1,L1), longitude(X2,L2), exceeds(L2,L1).
northof(X1,X2) :- latitude(X1,L1), latitude(X2,L2), exceeds(L1,L2).
southof(X1,X2) :- latitude(X1,L1), latitude(X2,L2), exceeds(L2,L1).
westof(X1,X2) :- longitude(X1,L1), longitude(X2,L2), exceeds(L1,L2).

circle_of_latitude(equator).
circle_of_latitude(tropic_of_cancer).
circle_of_latitude(tropic_of_capricorn).
circle_of_latitude(arctic_circle).
circle_of_latitude(antarctic_circle).

latitude(equator,0--degrees).
latitude(tropic_of_cancer,23--degrees).
latitude(tropic_of_capricorn,-23--degrees).
latitude(arctic_circle,67--degrees).
latitude(antarctic_circle,-67--degrees).

latitude(C,L--degrees) :- c_r_l_l_s_cap_m(C,_,L,_,_,_,_,_).
longitude(C,L--degrees) :- c_r_l_l_s_cap_m(C,_,_,L,_,_,_,_).
area(C,A--ksqmiles) :- c_r_l_l_s_cap_m(C,_,_,_,A0,_,_,_), A is A0/1000.
population(C,P--thousand) :- city_country_popu(C,_,P).
population(C,P--million) :-  c_r_l_l_s_cap_m(C,_,_,_,_,P0,_,_), P is floor(P0/1000000).
capital(C,Cap) :- c_r_l_l_s_cap_m(C,_,_,_,_,_,Cap,_).

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

seamass(X) :- ocean(X).
seamass(X) :- sea(X).

ocean(arctic_ocean).
ocean(atlantic).
ocean(indian_ocean).
ocean(pacific).
ocean(southern_ocean).

sea(baltic).
sea(black_sea).
sea(caspian).
sea(mediterranean).
sea(persian_gulf).
sea(red_sea).

river(R) :- river_flows(R,_L).

rises(R,C) :- river_flows(R,L), last(L,C).

drains(R,S) :- river_flows(R,L), first(L,S).

flows(R,C) :- flows(R,C,_).

flows(R,C1,C2) :- river_flows(R,L), links(L,C2,C1).

first([X|_],X).

last([X],X).
last([_|L],X) :- last(L,X).

links([X1,X2|_],X1,X2).
links([_|L],X1,X2) :- links(L,X1,X2).

