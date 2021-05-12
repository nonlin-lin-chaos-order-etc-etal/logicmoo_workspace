/*
 _____________________________________
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
|_____________________________________|

*/
:- [chatops].
% =================================================================
% General Dictionary

ag_number(1,sg).
ag_number(N,pl) :- N>1.

chk_word(Word):- word(Word).
word(Word) :- '`' (Word).
word(Word) :- conj_lex(Word).
word(Word) :- adverb_lex(Word).
word(Word) :- sup_adj_lex(Word,_).
word(Word) :- rel_adj_lex(Word,_).
word(Word) :- adj_lex(Word,_).
word(Word) :- name_LF(Word).
word(Word) :- terminator_lex(Word,_).
word(Word) :- pers_pron_lex(Word,_,_,_,_).
word(Word) :- poss_pron_lex(Word,_,_,_).
word(Word) :- rel_pron(Word,_).
word(Word) :- verb_form_lex(Word,_,_,_).
word(Word) :- noun_form_lex(Word,_,_).
word(Word) :- prep_lex(Word).
word(Word) :- quantifier_pron_lex(Word,_,_).
word(Word) :- number_lex(Word,_,_).
word(Word) :- det_lex(Word,_,_,_).
word(Word) :- int_art_lex(Word,_,_,_).
word(Word) :- int_pron_lex(Word,_).
word(Word) :- loc_pred_lex(_,Word,_).

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
conj_lex(and).
conj_lex(or).

det_lex(a,sg,a,indef).
det_lex(all,pl,all,indef).
det_lex(an,sg,a,indef).
det_lex(any,_,any,indef).
det_lex(each,sg,each,indef).
det_lex(every,sg,every,indef).
det_lex(no,_,no,indef).
det_lex(some,_,some,indef).
det_lex(the,No,the(No),def).

int_art_lex(what,X,_,int_det(X)).
int_art_lex(which,X,_,int_det(X)).
int_pron_lex(what,undef).
int_pron_lex(which,undef).
int_pron_lex(who,subj).
int_pron_lex(whom,compl).

name_LF(Name) :- name_template_LF(Name,_).

noun_form_lex(Plu,Sin,pl) :- noun_plu_lex(Plu,Sin).
noun_form_lex(Sin,Sin,sg) :- noun_sin_lex(Sin).

noun_form_lex(proportion,proportion,_).
noun_form_lex(percentage,percentage,_).

number_lex(W,I,Nb) :- 
        tr_number(W,I),
        ag_number(I,Nb).

pers_pron_lex(he,masc,3,sg,subj).
pers_pron_lex(her,fem,3,sg,compl(_)).
pers_pron_lex(him,masc,3,sg,compl(_)).
pers_pron_lex(i,_,1,sg,subj).
pers_pron_lex(it,neut,3,sg,_).
pers_pron_lex(me,_,1,sg,compl(_)).
pers_pron_lex(she,fem,3,sg,subj).
pers_pron_lex(them,_,3,pl,compl(_)).
pers_pron_lex(them,_,3,pl,subj).
pers_pron_lex(us,_,1,pl,compl(_)).
pers_pron_lex(we,_,1,pl,subj).
pers_pron_lex(you,_,2,_,_).

poss_pron_lex(her,fem,3,sg).
poss_pron_lex(his,masc,3,sg).
poss_pron_lex(its,neut,3,sg).
poss_pron_lex(my,_,1,sg).
poss_pron_lex(our,_,1,pl).
poss_pron_lex(their,_,3,pl).
poss_pron_lex(your,_,2,_).

prep_lex(X):- prep_lex_80(X).

prep_lex_80(as).
prep_lex_80(at).
prep_lex_80(by).
prep_lex_80(from).
prep_lex_80(in).
prep_lex_80(into).
prep_lex_80(of).
prep_lex_80(on).
prep_lex_80(through).
prep_lex_80(to).
prep_lex_80(with).

quantifier_pron_lex(anybody,any,person).
quantifier_pron_lex(anyone,any,person).
quantifier_pron_lex(anything,any,thing).
quantifier_pron_lex(everybody,every,person).
quantifier_pron_lex(everyone,every,person).
quantifier_pron_lex(everything,every,thing).
quantifier_pron_lex(nobody,no,person).
quantifier_pron_lex(nothing,no,thing).
quantifier_pron_lex(somebody,some,person).
quantifier_pron_lex(someone,some,person).
quantifier_pron_lex(something,some,thing).

regular_past_lex(Had,Have):- regular_past_lex_80(Had,Have).

regular_past_lex_80(had,have).

regular_past_lex_80(contained,contain).
regular_past_lex_80(exceeded,exceed).

regular_past_lex_80(governed,govern).

regular_pres_lex(V):- regular_pres_lex_80(V).

regular_pres_lex_80(contain).
regular_pres_lex_80(exceed).


regular_pres_lex_80(govern).


regular_pres_lex_80(do(_Does)).
regular_pres_lex_80(have).

rel_pron_lex(which,undef).
rel_pron_lex(who,subj).
rel_pron_lex(whom,compl).

% wordt niet gebruikt:
root_form(1+pl).
root_form(1+sg).
root_form(2+_).
root_form(3+pl).

terminator_lex(!,!).
terminator_lex(.,_).
terminator_lex(?,?).

tr_number(eight,8).
tr_number(five,5).
tr_number(four,4).
tr_number(I,N):- atomic(I), atom_number(I,N),!.
tr_number(nine,9).
tr_number(one,1).
tr_number(seven,7).
tr_number(six,6).
tr_number(ten,10).
tr_number(three,3).
tr_number(two,2).



verb_form_lex_80(am,be,pres+fin,1+sg).
verb_form_lex_80(are,be,pres+fin,2+sg).
verb_form_lex_80(are,be,pres+fin,_+pl).
verb_form_lex_80(been,be,past+part,_).
verb_form_lex_80(being,be,pres+part,_).
verb_form_lex_80(is,be,pres+fin,3+sg).
verb_form_lex_80(was,be,past+fin,1+sg).
verb_form_lex_80(was,be,past+fin,3+sg).
verb_form_lex_80(were,be,past+fin,2+sg).
verb_form_lex_80(were,be,past+fin,_+pl).


verb_form_lex_80(do,do(_Does),pres+fin,_+pl).
verb_form_lex_80(did,do(_Does),past+fin,_).
verb_form_lex_80(does,do(_Does),pres+fin,3+sg).
verb_form_lex_80(doing,do(_Does),pres+part,_).
verb_form_lex_80(done,do(_Does),past+part,_).

verb_form_lex_80(will,do(_Does),pres+fin,3+sg).


verb_form_lex_80(has,have,pres+fin,3+sg).
verb_form_lex_80(having,have,pres+part,_).


verb_form_lex_80(containing,contain,pres+part,_).
verb_form_lex_80(contains,contain,pres+fin,3+sg).
verb_form_lex_80(exceeding,exceed,pres+part,_).
verb_form_lex_80(exceeds,exceed,pres+fin,3+sg).
verb_form_lex_80(governing,govern,pres+part,_).
verb_form_lex_80(governs,govern,pres+fin,3+sg).


verb_form_lex(Are,Be,PresFin,NthPlOrSing):- verb_form_lex_80(Are,Be,PresFin,NthPlOrSing).
verb_form_lex(Verb,Verb,pres+fin,_+pl) :- Verb = V, verb_root_lex(V).
% ... because [which,countries,border,france,?] was not properly parsed (the singular form was)
verb_form_lex(Verb,Verb,inf,_) :-  Verb = V,  verb_root_lex(V).
% ... because [does,france,border,belgium,?] was not properly parsed
verb_form_lex(Verb,Inf,past+part,_) :- regular_past_lex(Verb,Inf).
% ... because [is,france,bordered,by,belgium,?] was not properly parsed. Deduced from verb_form_lex_80(done,do(_Does),past+part,_) bellow.
%verb_form_lex(A,A,C,D) :-
%  writef("********************************** verb_form_db {0} failed", [[A,A,C,D]]).
%  !,
%  fail.

verb_root_lex(Root):- verb_root_lex_80(Root).

verb_root_lex_80(be).
verb_root_lex_80(do(_Does)).
verb_root_lex_80(have).


verb_root_lex_80(contain).
verb_root_lex_80(exceed).


verb_root_lex_80(govern).

verb_type_lex(V,MainTv):- verb_type_lex_80(V,MainTv).

verb_type_lex_80(be,aux+be).
verb_type_lex_80(do(_Does),aux+dv(_Prep)).
verb_type_lex_80(have,aux+have).



verb_type_lex_80(contain,main+tv).
verb_type_lex_80(exceed,main+tv).
verb_type_lex_80(govern,main+tv).





% =================================================================
% Specialised Dictionary

adj_lex(African,restr):- agentitive_trans(_,_,African).
adj_lex(Baltic,restr):- agentitive_symmetric_type(_,Baltic).
adj_lex(African,Restr):- adj_lex_80(African,Restr).

%adj_lex_80(american,restr).
%adj_lex_80(asian,restr).
%adj_lex_80(european,restr).
adj_lex_80(average,restr).
adj_lex_80(big,quantV).
adj_lex_80(great,quantV).
adj_lex_80(great,quantV).
adj_lex_80(large,quantV).
adj_lex_80(maximum,restr).
adj_lex_80(minimum,restr).
adj_lex_80(new,quantV).
adj_lex_80(old,quantV).
adj_lex_80(small,quantV).
adj_lex_80(total,restr).

adverb_lex(tomorrow).
adverb_lex(yesterday).

loc_pred_lex(of,east,prep(cp(east,of))).
loc_pred_lex(of,north,prep(cp(north,of))).
loc_pred_lex(of,south,prep(cp(south,of))).
loc_pred_lex(of,west,prep(cp(west,of))).

noun_plu_lex(Averages,Average):- noun_plu_lex_80(Averages,Average).

noun_plu_lex_80(areas,area).
noun_plu_lex_80(averages,average).
noun_plu_lex_80(capitals,capital).
noun_plu_lex_80(cities,city).
noun_plu_lex_80(continents,continent).
noun_plu_lex_80(countries,country).
noun_plu_lex_80(nations,nation).
noun_plu_lex_80(states,state).
noun_plu_lex_80(degrees,degree).
noun_plu_lex_80(ksqmiles,ksqmile).
noun_plu_lex_80(latitudes,latitude).
noun_plu_lex_80(longitudes,longitude).
noun_plu_lex_80(million,million).
noun_plu_lex_80(numbers,number).
noun_plu_lex_80(oceans,ocean).
noun_plu_lex_80(persons,person).
noun_plu_lex_80(people,person).
noun_plu_lex_80(places,place).
noun_plu_lex_80(populations,population).
noun_plu_lex_80(regions,region).
noun_plu_lex_80(rivers,river).
noun_plu_lex_80(seamasses,seamass).
noun_plu_lex_80(seas,sea).
noun_plu_lex_80(sqmiles,sqmile).
noun_plu_lex_80(sums,sum).
noun_plu_lex_80(thousand,thousand).
noun_plu_lex_80(times,time).
noun_plu_lex_80(totals,total).

noun_sin_lex(Area):- noun_sin_lex_80(Area).

noun_sin_lex_80(area).
noun_sin_lex_80(average).
noun_sin_lex_80(capital).
noun_sin_lex_80(city).
noun_sin_lex_80(continent).
noun_sin_lex_80(country).
noun_sin_lex_80(degree).
noun_sin_lex_80(ksqmile).
noun_sin_lex_80(latitude).
noun_sin_lex_80(longitude).
noun_sin_lex_80(million).
noun_sin_lex_80(number).
noun_sin_lex_80(ocean).
noun_sin_lex_80(person).
noun_sin_lex_80(place).
noun_sin_lex_80(population).
noun_sin_lex_80(region).
noun_sin_lex_80(river).
noun_sin_lex_80(sea).
noun_sin_lex_80(seamass).
noun_sin_lex_80(sqmile).
noun_sin_lex_80(sum).
noun_sin_lex_80(thousand).
noun_sin_lex_80(time).
noun_sin_lex_80(total).

rel_adj_lex(bigger,big).
rel_adj_lex(greater,great).
rel_adj_lex(larger,large).
rel_adj_lex(less,small).
rel_adj_lex(newer,new).
rel_adj_lex(older,old).
rel_adj_lex(smaller,small).

sup_adj_lex(biggest,big).
sup_adj_lex(largest,large).
sup_adj_lex(newest,new).
sup_adj_lex(oldest,old).
sup_adj_lex(smallest,small).

comp_adv_lex(less).
comp_adv_lex(more).

sup_adv_lex(least).
sup_adv_lex(most).
