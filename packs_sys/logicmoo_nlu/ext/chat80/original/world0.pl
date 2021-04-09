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

% Data for the World Database.
% ---------------------------

:- if(use_pfc80).
:- expects_dialect(pfc).
:- endif.


:-op(600,xfy,--).

exceeds(X,Y):- term_variables(X-Y,Vars),freeze_until(Vars,exceeds0(X,Y)).

freeze_until([],Goal):-!, Goal.
freeze_until([V|Vars],Goal):- freeze(V,freeze_until(Vars,Goal)).

exceeds0(X--U,Y--U) :- !, X > Y.
exceeds0(X1--U1,X2--U2) :- ratio80(U1,U2,M1,M2), X1*M1 > X2*M2.

ratio80(thousand,million,1,1000).
ratio80(million,thousand,1000,1).
ratio80(ksqmiles,sqmiles,1000,1).
ratio80(sqmiles,ksqmiles,1,1000).

area(_X--ksqmiles).
capital_city(C) :- country_capital_city(_X,C).
city(C) :- city_country_popu(C,_,_).
country(C) :- c_r_l_l_s_cap_m(C,_,_,_,_,_,_,_).
latitude(_X--degrees).
longitude(_X--degrees).
%place(X) :- continent(X); region(X); seamass(X); country(X).
:- if(use_pfc80).
ti(place,X) ==> place(X).
:- else.
place(X) :- ti(place,X).
:- endif.

==> sub_ti(seamass,place).
==> sub_ti(continent,place).
==> sub_ti(region,place).
==> sub_ti(country,place).

population(_X--million).
population(_X--thousand).

african(X) :- loc_in(X,africa).
american(X) :- loc_in(X,america).
asian(X) :- loc_in(X,asia).
european(X) :- loc_in(X,europe).


rel_spatial(east_of,X1,X2) :- coordinate_spatial(longitude,X1,L1), coordinate_spatial(longitude,X2,L2), exceeds(L2,L1).
rel_spatial(north_of,X1,X2) :- coordinate_spatial(latitude,X1,L1), coordinate_spatial(latitude,X2,L2), exceeds(L1,L2).
rel_spatial(south_of,X1,X2) :- coordinate_spatial(latitude,X1,L1), coordinate_spatial(latitude,X2,L2), exceeds(L2,L1).
rel_spatial(west_of,X1,X2) :- coordinate_spatial(longitude,X1,L1), coordinate_spatial(longitude,X2,L2), exceeds(L1,L2).


ti(circle_of_latitude,equator).
ti(circle_of_latitude,tropic_of_cancer).
ti(circle_of_latitude,tropic_of_capricorn).
ti(circle_of_latitude,arctic_circle).
ti(circle_of_latitude,antarctic_circle).

coordinate_spatial(latitude,equator,0--degrees).
coordinate_spatial(latitude,tropic_of_cancer,23--degrees).
coordinate_spatial(latitude,tropic_of_capricorn,(-23)--degrees).
coordinate_spatial(latitude,arctic_circle,67--degrees).
coordinate_spatial(latitude,antarctic_circle,(-67)--degrees).

coordinate_spatial(latitude,C,L--degrees) :- c_r_l_l_s_cap_m(C,_,L,_,_,_,_,_).
coordinate_spatial(longitude,C,L--degrees) :- c_r_l_l_s_cap_m(C,_,_,L,_,_,_,_).
area(C,A--ksqmiles) :-
   c_r_l_l_s_cap_m(C,_,_,_,A0,_,_,_), A is integer(A0/1000).
population(C,P--thousand) :- city_country_popu(C,_,P).
population(C,P--million) :-
   c_r_l_l_s_cap_m(C,_,_,_,_,P0,_,_), P is integer(P0/1.0E6).
country_capital_city(C,Cap) :- c_r_l_l_s_cap_m(C,_,_,_,_,_,Cap,_).

ti(continent,africa).
ti(continent,america).
ti(continent,antarctica).
ti(continent,asia).
ti(continent,australasia).
ti(continent,europe).

continent(X):- ti(continent,X).

seamass(X):- ti(seamass,X).

==> sub_ti(ocean,seamass).
==> sub_ti(sea,seamass).

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
ti(river,R) :- river_flows(R,_L).

rises(R,C) :- river_flows(R,L), last_link(L,C).

drains(R,S) :- river_flows(R,L), first_link(L,S).

flows_thru(R,C) :- river_links(R,C,_).

river_links(R,C1,C2) :- river_flows(R,L), link_pairs(L,C2,C1).

first_link([X|_],X).

last_link([X],X).
last_link([_|L],X) :- last_link(L,X).

link_pairs([X1,X2|_],X1,X2).
link_pairs([_|L],X1,X2) :- link_pairs(L,X1,X2).

:- if(use_pfc80).
==> (( (sub_ti(Child,Parent), ti(Child,X)) ==> ti(Parent,X) )).
:- else.
ti(Parent,X) :- sub_ti(Child,Parent), ti(Child,X).
:- endif.


:- listing(seamass/1).

:- fixup_exports.
