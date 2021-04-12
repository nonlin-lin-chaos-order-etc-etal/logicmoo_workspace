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
:- [chatops].
% =================================================================
% General Dictionary

ag_number(1,sg).
ag_number(N,plu) :- N>1.

word(Word) :- '`' (Word).
word(Word) :- conj(Word).
word(Word) :- adverb(Word).
word(Word) :- sup_adj(Word,_).
word(Word) :- rel_adj(Word,_).
word(Word) :- adj(Word,_).
word(Word) :- name_LF(Word).
word(Word) :- terminator(Word,_).
word(Word) :- pers_pron(Word,_,_,_,_).
word(Word) :- poss_pron(Word,_,_,_).
word(Word) :- rel_pron(Word,_).
word(Word) :- verb_form(Word,_,_,_).
word(Word) :- noun_form(Word,_,_).
word(Word) :- prep(Word).
word(Word) :- quantifier_pron(Word,_,_).
word(Word) :- number(Word,_,_).
word(Word) :- det(Word,_,_,_).
word(Word) :- int_art(Word,_,_,_).
word(Word) :- int_pron(Word,_).
word(Word) :- loc_pred(_,Word,_).

'`' how.
'`' whose.
'`' there.
'`' of.
'`' ('''').
'`' (',').
'`' s.
'`' than.
'`' at.
'`' the.
'`' not.
'`' (as).
'`' that.
'`' less.
'`' more.
'`' least.
'`' most.
'`' many.
'`' where.
'`' when.
conj(and).
conj(or).

det(a,sg,a,indef).
det(all,plu,all,indef).
det(an,sg,a,indef).
det(any,_,any,indef).
det(each,sg,each,indef).
det(every,sg,every,indef).
det(no,_,no,indef).
det(some,_,some,indef).
det(the,No,the(No),def).

int_art(what,X,_,int_det(X)).
int_art(which,X,_,int_det(X)).
int_pron(what,undef).
int_pron(which,undef).
int_pron(who,subj).
int_pron(whom,compl).

name_LF(Name) :- name_template_LF(Name,_), !.

noun_form(Plu,Sin,plu) :- noun_plu(Plu,Sin).
noun_form(Sin,Sin,sg) :- noun_sin(Sin).

noun_form(proportion,proportion,_).
noun_form(percentage,percentage,_).

number(W,I,Nb) :- 
        tr_number(W,I),
        ag_number(I,Nb).

pers_pron(he,masc,3,sg,subj).
pers_pron(her,fem,3,sg,compl(_)).
pers_pron(him,masc,3,sg,compl(_)).
pers_pron(i,_,1,sg,subj).
pers_pron(it,neut,3,sg,_).
pers_pron(me,_,1,sg,compl(_)).
pers_pron(she,fem,3,sg,subj).
pers_pron(them,_,3,plu,compl(_)).
pers_pron(them,_,3,plu,subj).
pers_pron(us,_,1,plu,compl(_)).
pers_pron(we,_,1,plu,subj).
pers_pron(you,_,2,_,_).

poss_pron(her,fem,3,sg).
poss_pron(his,masc,3,sg).
poss_pron(its,neut,3,sg).
poss_pron(my,_,1,sg).
poss_pron(our,_,1,plu).
poss_pron(their,_,3,plu).
poss_pron(your,_,2,_).

prep(as).
prep(at).
prep(by).
prep(from).
prep(in).
prep(into).
prep(of).
prep(on).
prep(through).
prep(to).
prep(with).

quantifier_pron(anybody,any,person).
quantifier_pron(anyone,any,person).
quantifier_pron(anything,any,thing).
quantifier_pron(everybody,every,person).
quantifier_pron(everyone,every,person).
quantifier_pron(everything,every,thing).
quantifier_pron(nobody,no,person).
quantifier_pron(nothing,no,thing).
quantifier_pron(somebody,some,person).
quantifier_pron(someone,some,person).
quantifier_pron(something,some,thing).

regular_past(had,have).

regular_past(bordered,border).
regular_past(contained,contain).
regular_past(drained,drain).
regular_past(exceeded,exceed).
regular_past(flowed,flow).
regular_past(governed,govern).
regular_pres(border).
regular_pres(contain).
regular_pres(drain).
regular_pres(exceed).
regular_pres(flow).
regular_pres(govern).
regular_pres(rise).

regular_pres(do).
regular_pres(have).

rel_pron(which,undef).
rel_pron(who,subj).
rel_pron(whom,compl).

% wordt niet gebruikt:
root_form(1+plu).
root_form(1+sg).
root_form(2+_).
root_form(3+plu).

terminator(!,!).
terminator(.,_).
terminator(?,?).

tr_number(eight,8).
tr_number(five,5).
tr_number(four,4).
tr_number(nb(I),I).
tr_number(nine,9).
tr_number(one,1).
tr_number(seven,7).
tr_number(six,6).
tr_number(ten,10).
tr_number(three,3).
tr_number(two,2).

verb_form(am,be,pres+fin,1+sg).
verb_form(are,be,pres+fin,2+sg).
verb_form(are,be,pres+fin,_+plu).
verb_form(been,be,past+part,_).
verb_form(being,be,pres+part,_).
verb_form(did,do,past+fin,_).
verb_form(does,do,pres+fin,3+sg).
verb_form(doing,do,pres+part,_).
verb_form(done,do,past+part,_).
verb_form(has,have,pres+fin,3+sg).
verb_form(having,have,pres+part,_).
verb_form(is,be,pres+fin,3+sg).
verb_form(was,be,past+fin,1+sg).
verb_form(was,be,past+fin,3+sg).
verb_form(were,be,past+fin,2+sg).
verb_form(were,be,past+fin,_+plu).

verb_form(bordering,border,pres+part,_).
verb_form(borders,border,pres+fin,3+sg).
verb_form(containing,contain,pres+part,_).
verb_form(contains,contain,pres+fin,3+sg).
verb_form(draining,drain,pres+part,_).
verb_form(drains,drain,pres+fin,3+sg).
verb_form(exceeding,exceed,pres+part,_).
verb_form(exceeds,exceed,pres+fin,3+sg).
verb_form(flowing,flow,pres+part,_).
verb_form(flows,flow,pres+fin,3+sg).
verb_form(governing,govern,pres+part,_).
verb_form(governs,govern,pres+fin,3+sg).
verb_form(risen,rise,past+part,_).
verb_form(rises,rise,pres+fin,3+sg).
verb_form(rose,rise,past+fin,_).

verb_form(Verb,Verb,pres+fin,_+plu) :- Verb = V, verb_root(V).
% ... because [which,countries,border,france,?] was not properly parsed (the singular form was)
verb_form(Verb,Verb,inf,_) :-  Verb = V,  verb_root(V).
% ... because [does,france,border,belgium,?] was not properly parsed
verb_form(Verb,Inf,past+part,_) :- regular_past(Verb,Inf).
% ... because [is,france,bordered,by,belgium,?] was not properly parsed. Deduced from verb_form(done,do,past+part,_) above.

%verb_form(A,A,C,D) :-
%  writef("********************************** verb_form {0} failed", [[A,A,C,D]]).
%  !,
%  fail.

verb_root(be).
verb_root(do).
verb_root(have).

verb_root(border).
verb_root(contain).
verb_root(drain).
verb_root(exceed).
verb_root(flow).
verb_root(govern).
verb_root(rise).

verb_type(be,aux+be).
verb_type(do,aux+ditrans).
verb_type(have,aux+have).

verb_type(border,main+trans).
verb_type(contain,main+trans).
verb_type(drain,main+intrans).
verb_type(exceed,main+trans).
verb_type(flow,main+intrans).
verb_type(govern,main+trans).
verb_type(rise,main+intrans).


% =================================================================
% Specialised Dictionary

adj(African,restr):- agentitive_trans(_,_,African).
adj(Baltic,restr):- agentitive_symmetric_type(_,Baltic).
%adj(american,restr).
%adj(asian,restr).
%adj(european,restr).
adj(average,restr).
adj(big,quant).
adj(great,quant).
adj(great,quant).
adj(large,quant).
adj(maximum,restr).
adj(minimum,restr).
adj(new,quant).
adj(old,quant).
adj(small,quant).
adj(total,restr).
adverb(tomorrow).
adverb(yesterday).

loc_pred(of,east,prep(cp(east,of))).
loc_pred(of,north,prep(cp(north,of))).
loc_pred(of,south,prep(cp(south,of))).
loc_pred(of,west,prep(cp(west,of))).

noun_plu(areas,area).
noun_plu(averages,average).
noun_plu(capitals,capital).
noun_plu(cities,city).
noun_plu(continents,continent).
noun_plu(countries,country).
noun_plu(nations,nation).
noun_plu(states,state).
noun_plu(degrees,degree).
noun_plu(ksqmiles,ksqmile).
noun_plu(latitudes,latitude).
noun_plu(longitudes,longitude).
noun_plu(million,million).
noun_plu(numbers,number).
noun_plu(oceans,ocean).
noun_plu(persons,person).
noun_plu(people,person).
noun_plu(places,place).
noun_plu(populations,population).
noun_plu(regions,region).
noun_plu(rivers,river).
noun_plu(seamasses,seamass).
noun_plu(seas,sea).
noun_plu(sqmiles,sqmile).
noun_plu(sums,sum).
noun_plu(thousand,thousand).
noun_plu(times,time).
noun_plu(totals,total).

noun_sin(area).
noun_sin(average).
noun_sin(capital).
noun_sin(city).
noun_sin(continent).
noun_sin(country).
noun_sin(degree).
noun_sin(ksqmile).
noun_sin(latitude).
noun_sin(longitude).
noun_sin(million).
noun_sin(number).
noun_sin(ocean).
noun_sin(person).
noun_sin(place).
noun_sin(population).
noun_sin(region).
noun_sin(river).
noun_sin(sea).
noun_sin(seamass).
noun_sin(sqmile).
noun_sin(sum).
noun_sin(thousand).
noun_sin(time).
noun_sin(total).

rel_adj(bigger,big).
rel_adj(greater,great).
rel_adj(larger,large).
rel_adj(less,small).
rel_adj(newer,new).
rel_adj(older,old).
rel_adj(smaller,small).

sup_adj(biggest,big).
sup_adj(largest,large).
sup_adj(newest,new).
sup_adj(oldest,old).
sup_adj(smallest,small).

