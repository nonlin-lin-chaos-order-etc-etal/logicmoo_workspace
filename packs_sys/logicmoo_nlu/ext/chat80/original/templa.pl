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

:- op(400,xfy,&).

/* Nouns */

spatial(spatial).
feature_path(spatial,city,spatial&city).
feature_path(spatial,river,spatial&river).

property_LF(capital,  SpatialCityPath,    X,Spatial&place&country,Y,  
 specific_pred(Spatial,capital_city,Y,X),[],_,_):- feature_path(Spatial,city,SpatialCityPath).

property_LF(area,     measure&area,    X,Spatial&place&_,Y,  measure_pred(Spatial,area,Y,X),[],_,_).
property_LF(latitude, measure&position,X,Spatial&_,Y,       position_pred(Spatial,latitude,Y,X),[],_,_).
property_LF(longitude,measure&position,X,Spatial&_,Y,       position_pred(Spatial,longitude,Y,X),[],_,_).
property_LF(population, measure&heads, X,Spatial&_,Y,          count_pred(Spatial,heads,Y,X),[],_,_).

thing_LF(area,measure&area,X,unit_format(area,X),[],_).
thing_LF(latitude,measure&position,X,unit_format(latitude,X),[],_).
thing_LF(longitude,measure&position,X,unit_format(longitude,X),[],_).
thing_LF(population,measure&heads,X,unit_format(population,X),[],_).

thing_LF(place,Spatial&place&_,X,ti(place,X),[],_):- spatial(Spatial).
thing_LF(region,Spatial&place&_,X,ti(region,X),[],_):- spatial(Spatial).
thing_LF(continent,Spatial&place&continent,X,ti(continent,X),[],_):- spatial(Spatial).
thing_LF(country,Spatial&place&country,X,ti(country,X),[],_):- spatial(Spatial).
thing_LF(nation,Spatial&place&country,X,ti(country,X),[],_):- spatial(Spatial).
thing_LF(ocean,Spatial&place&seamass,X,ti(ocean,X),[],_):- spatial(Spatial).
thing_LF(sea,Spatial&place&seamass,X,ti(sea,X),[],_):- spatial(Spatial).
thing_LF(seamass,Spatial&place&seamass,X,ti(seamass,X),[],_):- spatial(Spatial).

thing_LF(person,_,X,ti(person,X),[],_).

thing_LF(capital,SpatialCityPath,X,ti(capital_city,X),[],_):- spatial(Spatial), feature_path(Spatial,city,SpatialCityPath).
thing_LF(city,SpatialCityPath,X,ti(city,X),[],_):- spatial(Spatial), feature_path(Spatial,city,SpatialCityPath).
thing_LF(river,SpatialRiver,X,ti(river,X),[],_):- spatial(Spatial), feature_path(Spatial,river,SpatialRiver).


aggr_noun(average,_,_,average).
aggr_noun(sum,_,_,total).
aggr_noun(total,_,_,total).

meta_noun_LF(number,of,_,V,Spatial&_,X,P,numberof(X,P,V)):- spatial(Spatial).

/* Proper nouns */

name_template_LF(X,Type):- name_template(X,Type).
name_template(X,Spatial&circle) :-  circle_of_latitude(X), spatial(Spatial).
name_template(X,SpatialCityPath) :- ti(city,X), spatial(Spatial), feature_path(Spatial,city,SpatialCityPath).
name_template(X,Spatial&place&continent) :- spatial(Spatial), ti(continent,X).
name_template(X,Spatial&place&country) :- spatial(Spatial), ti(country,X).
name_template(X,Spatial&place&_) :- spatial(Spatial), ti(region,X).
name_template(X,SpatialRiver) :- ti(river,X), spatial(Spatial), feature_path(Spatial,river,SpatialRiver).
name_template(X,Spatial&place&seamass) :- spatial(Spatial), ti(seamass,X).

/* Verbs */

trans(border,Spatial&place&_,X,Spatial&place&_,Y,symmetric_pred(Spatial,borders,X,Y),[],_,_).
trans(contain,Spatial&place&_,X,Spatial&_,Y, trans_pred(Spatial,contains,X,Y),[],_,_).
trans(govern,Spatial&_,X,Spatial&place&country,Y,specific_pred(Spatial,capital_city,Y,X),[],_,_).
trans(exceed,measure&Type,X,measure&Type,Y,exceeds(X,Y),[],_,_).

intrans(drain,SpatialRiver,X,path_pred(ends,river,X,Y), 
   [slot(prep(into),Spatial&place&_,Y,_,free)],_):- feature_path(Spatial,river,SpatialRiver).
intrans(flow,SpatialRiver,X,path_pred(thru,river,X,Y),
   [slot(prep(through),Spatial&place&_,Y,_,free)],_):- feature_path(Spatial,river,SpatialRiver).
intrans(flow,SpatialRiver,X,path_pred_s2(links,river,X,Y,Z),
   [slot(prep(into),Spatial&place&_,Z,_,free),
    slot(prep(from),Spatial&place&_,Y,_,free)],_):- feature_path(Spatial,river,SpatialRiver).
intrans(rise,SpatialRiver,X,path_pred(begins,river,X,Y),
   [slot(prep(in),Spatial&place&_,Y,_,free)],_):- feature_path(Spatial,river,SpatialRiver).

/* Adjectives */

restriction_LF(African,Spatial&_,X,ti(African,X)):- adj(African,restr), spatial(Spatial).
%restriction_LF(american,Spatial&_,X,ti(american,X)).
%restriction_LF(asian,Spatial&_,X,ti(asian,X)).
%restriction_LF(european,Spatial&_,X,ti(european,X)).

attribute_LF(large,Spatial&place&_,X,measure&area,Y,measure_pred(Spatial,area,X,Y)).
attribute_LF(small,Spatial&place&_,X,measure&area,Y,measure_pred(Spatial,area,X,Y)).
attribute_LF(great,measure&Type,X,measure&Type,Y,exceeds(X,Y)).
attribute_LF(populous,Spatial&_,X,measure&heads,Y,count_pred(Spatial,heads,Y,X)).

aggr_adj(average,_,_,average).
aggr_adj(total,_,_,total).
aggr_adj(minimum,_,_,minimum).
aggr_adj(maximum,_,_,maximum).

/* Prepositions */

adjunction_LF(in,Spatial&_-X,Spatial&place&_-Y,trans_pred(Spatial,contains,Y,X)).
adjunction_LF(cp(east,of),Spatial&_-X,Spatial&_-Y,rel_pred(Spatial,cp(east,of),X,Y)).
adjunction_LF(cp(west,of),Spatial&_-X,Spatial&_-Y,rel_pred(Spatial,cp(west,of),X,Y)).
adjunction_LF(cp(north,of),Spatial&_-X,Spatial&_-Y,rel_pred(Spatial,cp(north,of),X,Y)).
adjunction_LF(cp(south,of),Spatial&_-X,Spatial&_-Y,rel_pred(Spatial,cp(south,of),X,Y)).

/* Measure */

measure_LF(ksqmile,measure&area,[],ksqmiles).
measure_LF(sqmile,measure&area,[],sqmiles).
measure_LF(degree,measure&position,[],degrees).
measure_LF(thousand,measure&heads,[],thousand).
measure_LF(million,measure&heads,[],million).

units(large,measure&_).
units(small,measure&_).

chat_sign(large,+).
chat_sign(small,-).
chat_sign(great,+).

/* Proportions and the like */

comparator(proportion,_,V,[],proportion(V)).
comparator(percentage,_,V,[],proportion(V)).
