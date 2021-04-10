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


property_LF(capital,feature&city,X,feature&place&country,Y, country_capital_city(Y,X),[],_,_).
property_LF(area,measure&area,X,feature&place&_,Y,measure_value(area,Y,X),[],_,_).
property_LF(latitude,  measure&position,X,feature&_,Y,position_value(latitude,Y,X),[],_,_).
property_LF(longitude,measure&position,X,feature&_,Y, position_value(longitude,Y,X),[],_,_).
property_LF(population, measure&heads,X,feature&_,Y,count_value(population,Y,X),[],_,_).

thing_LF(place,feature&place&_,X,place(X),[],_).
thing_LF(area,measure&area,X,unit_format(area,X),[],_).
thing_LF(capital,feature&city,X,capital(X),[],_).
thing_LF(city,feature&city,X,city(X),[],_).
thing_LF(continent,feature&place&continent,X,continent(X),[],_).
thing_LF(country,feature&place&country,X,country(X),[],_).
thing_LF(latitude,measure&position,X,unit_format(latitude,X),[],_).
thing_LF(longitude,measure&position,X,unit_format(longitude,X),[],_).
thing_LF(ocean,feature&place&seamass,X,ocean(X),[],_).
thing_LF(person,_,X,person(X),[],_).
thing_LF(population,measure&heads,X,unit_format(population,X),[],_).
thing_LF(region,feature&place&_,X,region(X),[],_).
thing_LF(river,feature&river,X,river(X),[],_).
thing_LF(sea,feature&place&seamass,X,sea(X),[],_).
thing_LF(seamass,feature&place&seamass,X,seamass(X),[],_).

aggr_noun(average,_,_,average).
aggr_noun(sum,_,_,total).
aggr_noun(total,_,_,total).

meta_noun_LF(number,of,_,V,feature&_,X,P,numberof(X,P,V)).

/* Proper nouns */

name_template(X,feature&circle) :- circle_of_latitude(X).
name_template(X,feature&city) :- city(X).
name_template(X,feature&place&continent) :- continent(X).
name_template(X,feature&place&country) :- country(X).
name_template(X,feature&place&_) :- region(X).
name_template(X,feature&river) :- river(X).
name_template(X,feature&place&seamass) :- seamass(X).

/* Verbs */

trans(border,feature&place&_,X,feature&place&_,Y,borders(X,Y),[],_,_).
trans(contain,feature&place&_,X,feature&_,Y,loc_in(Y,X),[],_,_).
trans(govern,feature&_,X,feature&place&country,Y,capital(Y,X),[],_,_).
trans(exceed,measure&Type,X,measure&Type,Y,exceeds(X,Y),[],_,_).

intrans(drain,feature&river,X,flow_ends(river,X,Y), 
   [slot(prep(into),feature&place&_,Y,_,free)],_).
intrans(flow,feature&river,X,flow_thru(river,X,Y),
   [slot(prep(through),feature&place&_,Y,_,free)],_).
intrans(flow,feature&river,X,flow_from_to(river,X,Y,Z),
   [slot(prep(into),feature&place&_,Z,_,free),
    slot(prep(from),feature&place&_,Y,_,free)],_).
intrans(rise,feature&river,X,flow_begins(river,X,Y),
   [slot(prep(in),feature&place&_,Y,_,free)],_).

/* Adjectives */

restriction_LF(african,feature&_,X,african(X)).
restriction_LF(american,feature&_,X,american(X)).
restriction_LF(asian,feature&_,X,asian(X)).
restriction_LF(european,feature&_,X,european(X)).

attribute_LF(large,feature&place&_,X,measure&area,Y,measure_value(area,X,Y)).
attribute_LF(small,feature&place&_,X,measure&area,Y,measure_value(area,X,Y)).
attribute_LF(great,measure&Type,X,measure&Type,Y,exceeds(X,Y)).
attribute_LF(populous,feature&_,X,measure&heads,Y,count_value(population,Y,X)).

aggr_adj(average,_,_,average).
aggr_adj(total,_,_,total).
aggr_adj(minimum,_,_,minimum).
aggr_adj(maximum,_,_,maximum).

/* Prepositions */

adjunction_LF(in,feature&_-X,feature&place&_-Y,loc_in(X,Y)).
adjunction_LF(cp(east,of),feature&_-X,feature&_-Y,rel_spatial(cp(east,of),X,Y)).
adjunction_LF(cp(west,of),feature&_-X,feature&_-Y,rel_spatial(cp(west,of),X,Y)).
adjunction_LF(cp(north,of),feature&_-X,feature&_-Y,rel_spatial(cp(north,of),X,Y)).
adjunction_LF(cp(south,of),feature&_-X,feature&_-Y,rel_spatial(cp(south,of),X,Y)).

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
