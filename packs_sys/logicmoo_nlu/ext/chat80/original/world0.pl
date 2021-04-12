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

:- style_check(-discontiguous).

:- discontiguous unit_format/2. 


% Interface.
% ---------

database80(aggregate(X,Y,Z)) :- aggregate(X,Y,Z).
database80(one_of(X,Y)) :- one_of(X,Y).
database80(ratio(X,Y,Z)) :- ratio(X,Y,Z).
database80(card(X,Y)) :- card(X,Y).
database80(circle_of_latitude(X)) :- circle_of_latitude(X).
%database80(continent(X)) :- continent(X).
database80(exceeds(X,Y)) :- exceeds(X,Y).
database80(ti(Place,X)) :- ti(Place,X).
database80(X=Y) :- X=Y.
%database80(person(X)) :- person(X).	% JW: person is not defined


database80(unit_format(P,X)) :- unit_format(P,X).  % square miles
database80(measure_pred(Type,P,X,Y)) :- measure_pred(Type,P,X,Y). % area of
database80(count_pred(Type,P,X,Y)) :- count_pred(Type,P,X,Y). % population of 
database80(position_pred(Type,P,X,Y)) :- position_pred(Type,P,X,Y). % latitude of
database80(ordering_pred(Type,P,X,Y)) :- ordering_pred(Type,P,X,Y). % south of
database80(symmetric_pred(Type,P,X,Y)) :- symmetric_pred(Type,P,X,Y). % borders
database80(specific_pred(Type,P,X,Y)) :- specific_pred(Type,P,X,Y). % capital 
database80(trans_pred(Type,P,X,Y)) :- trans_pred(Type,P,X,Y). % contains 


%database80(path_pred(begins,river,X,Y)) :- path_pred(begins,river,X,Y).
%database80(path_pred(ends,river,X,Y)) :- path_pred(ends,river,X,Y).
database80(path_pred(Part,ObjType,X,Y)) :- path_pred(Part,ObjType,X,Y).
database80(path_pred_s2(links,ObjType,X,Y,Z)) :- path_pred_s2(links,ObjType,X,Y,Z).


:-op(500,xfy,--).

%exceeds(X--U,Y--U) :- !, X > Y.
%exceeds(X1--U1,X2--U2) :- ratio(U1,U2,M1,M2), X1*M1 > X2*M2.
exceeds(X,Y):- term_variables(X-Y,Vars),freeze_until(Vars,exceeds0(X,Y)),!.

freeze_until([],Goal):-!, term_variables(Goal, Vars),(Vars==[] -> Goal ; freeze_until(Vars,Goal)).
freeze_until([V|Vars],Goal):- freeze(V,freeze_until(Vars,Goal)),!.

exceeds0(X--U,Y--U) :- !, X > Y.
exceeds0(X1--U1,X2--U2) :- once((ratio(U1,U2,M1,M2), X1*M1 > X2*M2)).



ratio(thousand,million,1,1000).
ratio(million,thousand,1000,1).
ratio(ksqmiles,sqmiles,1000,1).
ratio(sqmiles,ksqmiles,1,1000).

unit_format(area,_X--ksqmiles).
ti(capital_city,Cap) :- c_r_l_l_s_cap_m(_,_,_,_,_,_,Cap,_). % specific_pred(spatial,nation_capital,_X,C).
%ti(city,C) :- ti(capital_city,C).
%ti(city,C) :- clause(city_country_popu(C,_,_), true).
ti(city,C) :- country_contains_thing(_,C), \+ ti(river,C).
ti(country,C) :- c_r_l_l_s_cap_m(C,_,_,_,_,_,_,_).
unit_format(latitude,_X--degrees).
unit_format(longitude,_X--degrees).

ti(SC,X) :- ti_subclass(C,SC),ti(C,X).

ti_subclass(continent,place).
ti_subclass(region,place).
ti_subclass(seamass,place).
ti_subclass(country,place).

unit_format(population,_X--million).
unit_format(population,_X--thousand).
ti(region,R) :- continent_contains_region(_,R).

% if X is contained in africa then X is african.
ti(An,X) :- agentitive_trans(Contains,Af,An), (trans_pred(spatial,Contains,Af,X);Af=X).
agentitive_trans(contains,africa,african).
agentitive_trans(contains,america,american).
agentitive_trans(contains,asia,asian).
agentitive_trans(contains,europe,european).



ordering_pred(spatial,cp(east,of),X1,X2) :- position_pred(spatial,longitude,X1,L1), position_pred(spatial,longitude,X2,L2), exceeds(L2,L1).
ordering_pred(spatial,cp(north,of),X1,X2) :- position_pred(spatial,latitude,X1,L1), position_pred(spatial,latitude,X2,L2), exceeds(L1,L2).
ordering_pred(spatial,cp(south,of),X1,X2) :- position_pred(spatial,latitude,X1,L1), position_pred(spatial,latitude,X2,L2), exceeds(L2,L1).
ordering_pred(spatial,cp(west,of),X1,X2) :- position_pred(spatial,longitude,X1,L1), position_pred(spatial,longitude,X2,L2), exceeds(L1,L2).


circle_of_latitude(equator).
circle_of_latitude(tropic_of_cancer).
circle_of_latitude(tropic_of_capricorn).
circle_of_latitude(arctic_circle).
circle_of_latitude(antarctic_circle).

position_pred(spatial,latitude,equator,0--degrees).
position_pred(spatial,latitude,tropic_of_cancer,23--degrees).
position_pred(spatial,latitude,tropic_of_capricorn,(-23)--degrees).
position_pred(spatial,latitude,arctic_circle,67--degrees).
position_pred(spatial,latitude,antarctic_circle,(-67)--degrees).

position_pred(spatial,latitude,C,L--degrees) :- c_r_l_l_s_cap_m(C,_,L,_,_,_,_,_).
position_pred(spatial,longitude,C,L--degrees) :- c_r_l_l_s_cap_m(C,_,_,L,_,_,_,_).


measure_pred(Spatial,Heads,C,Total):- is_list(C),maplist(measure_pred(Spatial,Heads),C,Setof), u_total(Setof, Total).

measure_pred(spatial,area,C,A--ksqmiles) :- c_r_l_l_s_cap_m(C,_,_,_,A0,_,_,_), A is A0/1000.

measure_pred(Spatial,Area,Where,Total) :- \+ c_r_l_l_s_cap_m(Where,_,_,_,_,_,_,_), 
 % ti(continent,Where),
 setof(Value:[Country],
               []^(database80(measure_pred(Spatial, Area, Country, Value)), 
               %database80(ti(country, Country)), 
               database80(trans_pred(Spatial,contains,Where,Country))),
               Setof),
         database80(aggregate(total, Setof, Total)).


count_pred(Spatial,Heads,C,Total):- is_list(C),maplist(count_pred(Spatial,Heads),C,Setof), u_total(Setof, Total).
count_pred(spatial,heads,C,P--thousand) :- city_country_popu(C,_,P).
count_pred(spatial,heads,C,P--million) :- c_r_l_l_s_cap_m(C,_,_,_,_,P0,_,_), P is integer(P0/1.0E6).

specific_pred(spatial,nation_capital,C,Cap) :- c_r_l_l_s_cap_m(C,_,_,_,_,_,Cap,_).

ti(continent,X):- continent(X).
continent(africa).
continent(america).
continent(antarctica).
continent(asia).
continent(australasia).
continent(europe).

ti(seamass,X):- ti(ocean,X).
ti(seamass,X):- ti(sea,X).

ti(ocean,arctic_ocean).
ti(ocean,atlantic).
ti(ocean,indian_ocean).
ti(ocean,pacific).
ti(ocean,southern_ocean).


ti(sea,baltic).
ti(sea,black_sea).
ti(sea,caspian_sea).
ti(sea,mediterranean).
ti(sea,persian_gulf).
ti(sea,red_sea).
% @TODO ti(sea,caribian).

%ti(Sea,X) :- Sea\==seamass,Sea\==ocean,Sea\==sea, agentitive_symmetric_type(Borders,Sea), (symmetric_pred(spatial,Borders,Sea,X)).
%agentitive_symmetric_type(borders,Baltic):- ti(seamass,Baltic).
% allows "baltic country" "pacific countries"   
agentitive_symmetric_type(borders,seamass).
ti(NewType,X) :- agentitive_symmetric_type(Pred,SuperType), fail,
  % dont loop
  NewType\==SuperType, NewType\==SuperType, 
  % get the type names
  ti(SuperType,NewType), 
  % find the instances 
  symmetric_pred(spatial,Pred,NewType,X),
  % dont find instances already of the super type
  \+ ti(SuperType,X).

ti(Type,R) :- path_linkages(Type,R,_L).


path_pred(begins,Type,R,C) :- path_linkages(Type,R,L), last_link(L,C).

path_pred(ends,Type,R,S) :- path_linkages(Type,R,L), first_link(L,S).

path_pred(thru,Type,R,C) :- path_pred_s2(links,Type,R,C,_).

path_pred_s2(links,Type,R,C1,C2) :- path_linkages(Type,R,L), link_pairs(L,C2,C1).

first_link([X|_],X).

last_link([X],X).
last_link([_|L],X) :- last_link(L,X).

link_pairs([X1,X2|_],X1,X2).
link_pairs([_|L],X1,X2) :- link_pairs(L,X1,X2).
