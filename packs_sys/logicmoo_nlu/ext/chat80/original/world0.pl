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

database80(aggregate80(X,Y,Z)) :- aggregate80(X,Y,Z).
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
database80(symmetric_pred(Type,P,X,Y)) :- symmetric_pred(Type,P,X,Y). % border
database80(specific_pred(Type,P,X,Y)) :- specific_pred(Type,P,X,Y). % capital 
database80(trans_pred(Type,P,X,Y)) :- trans_pred(Type,P,X,Y). % contain 


%database80(path_pred(begins(Flow),rises,river,X,Y)) :- path_pred(begins(Flow),rises,river,X,Y).
%database80(path_pred(ends(Flow),drains,river,X,Y)) :- path_pred(ends(Flow),drains,river,X,Y).
database80(path_pred(Part,Verb,ObjType,X,Y)) :- path_pred(Part,Verb,ObjType,X,Y).
database80(path_pred_links(Flow,ObjType,X,Y,Z)) :- path_pred_links(Flow,ObjType,X,Y,Z).


:- style_check(+singleton).

ti(NewType,X) :- agentitive_symmetric_type(Pred,SuperType), fail,
  % dont loop
  NewType\==SuperType, NewType\==SuperType, 
  % get the type names
  ti(SuperType,NewType), 
  % find the instances 
  symmetric_pred(spatial,Pred,NewType,X),
  % dont find instances already of the super type
  \+ ti(SuperType,X).

%ti(Sea,X) :- Sea\==seamass,Sea\==ocean,Sea\==sea, agentitive_symmetric_type(Borders,Sea), (symmetric_pred(spatial,Borders,Sea,X)).
%agentitive_symmetric_type(border,Baltic):- ti(seamass,Baltic).
% allows "baltic country" "pacific countries"   
agentitive_symmetric_type(border,seamass).

ti(SC,X) :- ti_subclass(C,SC),ti(C,X).

place_lex(place).

ti_subclass(continent,place).
ti_subclass(region,place).
ti_subclass(seamass,place).
ti_subclass(country,place).

ti(region,R) :- continent_contains_region(_,R).

% if X is contained in africa then X is african.
ti(An,X) :- agentitive_trans(Contains,Af,An), (trans_pred(spatial,Contains,Af,X);Af=X).

agentitive_trans(Contains,Af,An):- agentitive_trans_80(Contains,Af,An).

agentitive_trans_80(contain,africa,african).
agentitive_trans_80(contain,america,american).
agentitive_trans_80(contain,asia,asian).
agentitive_trans_80(contain,europe,european).



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



