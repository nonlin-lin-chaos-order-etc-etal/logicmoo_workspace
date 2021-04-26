/*
 _________________________________________________________________________
|       Copyright _db(C) 1982                                                |
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
ag_number(N,pl) :- N>1.


chk_word(Word) :- conj_db(Word).
chk_word(Word) :- adverb_db(Word).
chk_word(Word) :- sup_adj_db(Word,_).
chk_word(Word) :- rel_adj_db(Word,_).
chk_word(Word) :- adj_db(Word,_).
chk_word(Word) :- name_LF(Word).
chk_word(Word) :- terminator_db(Word,_).
chk_word(Word) :- pers_pron_db(Word,_,_,_,_).
chk_word(Word) :- poss_pron_db(Word,_,_,_).
chk_word(Word) :- rel_pron_db(Word,_).
chk_word(Word) :- verb_form_db(Word,_,_,_).
chk_word(Word) :- noun_form_db(Word,_,_).
chk_word(Word) :- prep_db(Word).
chk_word(Word) :- quantifier_pron_db(Word,_,_).
chk_word(Word) :- number_db(Word,_,_).
chk_word(Word) :- det_db(Word,_,_,_).
chk_word(Word) :- int_art_db(Word,_,_,_).
chk_word(Word) :- int_pron_db(Word,_).
chk_word(Word) :- loc_pred_db(_,Word,_).
chk_word(how).
chk_word(whose).
chk_word(there).
chk_word(of).
chk_word('''').
chk_word((',')).
chk_word(s).
chk_word(than).
chk_word(at).
chk_word(the).
chk_word(not).
chk_word(as).
chk_word(that).
chk_word(less).
chk_word(more).
chk_word(least).
chk_word(most).
chk_word(many).
chk_word(where).
chk_word(when).

conj_db(and).
conj_db(or).

det_db(a,sg,a,indef).
det_db(all,pl,all,indef).
det_db(an,sg,a,indef).
det_db(any,_,any,indef).
det_db(each,sg,each,indef).
det_db(every,sg,every,indef).
det_db(no,_,no,indef).
det_db(some,_,some,indef).
det_db(the,No,the(No),def).

int_art_db(what,X,_,int_det(X)).
int_art_db(which,X,_,int_det(X)).
int_pron_db(what,undef).
int_pron_db(which,undef).
int_pron_db(who,subj).
int_pron_db(whom,compl).

name_LF(Name) :- name_template_LF(Name,_), !.

noun_form_db(Plu,Sin,pl) :- noun_plu_db(Plu,Sin).
noun_form_db(Sin,Sin,sg) :- noun_sin_db(Sin).

noun_form_db(proportion,proportion,_).
noun_form_db(percentage,percentage,_).

number_db(W,I,Nb) :- 
        tr_number(W,I),
        ag_number(I,Nb).

pers_pron_db(he,masc,3,sg,subj).
pers_pron_db(her,fem,3,sg,compl(_)).
pers_pron_db(him,masc,3,sg,compl(_)).
pers_pron_db(i,_,1,sg,subj).
pers_pron_db(it,neut,3,sg,_).
pers_pron_db(me,_,1,sg,compl(_)).
pers_pron_db(she,fem,3,sg,subj).
pers_pron_db(them,_,3,pl,compl(_)).
pers_pron_db(them,_,3,pl,subj).
pers_pron_db(us,_,1,pl,compl(_)).
pers_pron_db(we,_,1,pl,subj).
pers_pron_db(you,_,2,_,_).

poss_pron_db(her,fem,3,sg).
poss_pron_db(his,masc,3,sg).
poss_pron_db(its,neut,3,sg).
poss_pron_db(my,_,1,sg).
poss_pron_db(our,_,1,pl).
poss_pron_db(their,_,3,pl).
poss_pron_db(your,_,2,_).

prep_db(as).
prep_db(at).
prep_db(by).
prep_db(from).
prep_db(in).
prep_db(into).
prep_db(of).
prep_db(on).
prep_db(through).
prep_db(to).
prep_db(with).

quantifier_pron_db(anybody,any,person).
quantifier_pron_db(anyone,any,person).
quantifier_pron_db(anything,any,thing).
quantifier_pron_db(everybody,every,person).
quantifier_pron_db(everyone,every,person).
quantifier_pron_db(everything,every,thing).
quantifier_pron_db(nobody,no,person).
quantifier_pron_db(nothing,no,thing).
quantifier_pron_db(somebody,some,person).
quantifier_pron_db(someone,some,person).
quantifier_pron_db(something,some,thing).

regular_past_db(had,have).

regular_past_db(bordered,border).
regular_past_db(contained,contain).
regular_past_db(drained,drain).
regular_past_db(exceeded,exceed).
regular_past_db(flowed,flow).
regular_past_db(governed,govern).
regular_pres_db(border).
regular_pres_db(contain).
regular_pres_db(drain).
regular_pres_db(exceed).
regular_pres_db(flow).
regular_pres_db(govern).
regular_pres_db(rise).

regular_pres_db(do).
regular_pres_db(have).

rel_pron_db(which,undef).
rel_pron_db(who,subj).
rel_pron_db(whom,compl).

% wordt niet gebruikt:
root_form_db(1+pl).
root_form_db(1+sg).
root_form_db(2+_).
root_form_db(3+pl).

terminator_db(!,!).
terminator_db(.,_).
terminator_db(?,?).

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

verb_form_db(am,be,pres+fin,1+sg).
verb_form_db(are,be,pres+fin,2+sg).
verb_form_db(are,be,pres+fin,_+pl).
verb_form_db(been,be,past+part,_).
verb_form_db(being,be,pres+part,_).
verb_form_db(did,do,past+fin,_).
verb_form_db(does,do,pres+fin,3+sg).
verb_form_db(doing,do,pres+part,_).
verb_form_db(done,do,past+part,_).
verb_form_db(has,have,pres+fin,3+sg).
verb_form_db(having,have,pres+part,_).
verb_form_db(is,be,pres+fin,3+sg).
verb_form_db(was,be,past+fin,1+sg).
verb_form_db(was,be,past+fin,3+sg).
verb_form_db(were,be,past+fin,2+sg).
verb_form_db(were,be,past+fin,_+pl).

verb_form_db(bordering,border,pres+part,_).
verb_form_db(borders,border,pres+fin,3+sg).
verb_form_db(containing,contain,pres+part,_).
verb_form_db(contains,contain,pres+fin,3+sg).
verb_form_db(draining,drain,pres+part,_).
verb_form_db(drains,drain,pres+fin,3+sg).
verb_form_db(exceeding,exceed,pres+part,_).
verb_form_db(exceeds,exceed,pres+fin,3+sg).
verb_form_db(flowing,flow,pres+part,_).
verb_form_db(flows,flow,pres+fin,3+sg).
verb_form_db(governing,govern,pres+part,_).
verb_form_db(governs,govern,pres+fin,3+sg).
verb_form_db(risen,rise,past+part,_).
verb_form_db(rises,rise,pres+fin,3+sg).
verb_form_db(rose,rise,past+fin,_).

verb_form_db(Verb,Verb,pres+fin,_+pl) :- Verb = V, verb_root_db(V).
% ... because [which,countries,border,france,?] was not properly parsed _db(the singular form was)
verb_form_db(Verb,Verb,inf,_) :-  Verb = V,  verb_root_db(V).
% ... because [does,france,border,belgium,?] was not properly parsed
verb_form_db(Verb,Inf,past+part,_) :- regular_past_db(Verb,Inf).
% ... because [is,france,bordered,by,belgium,?] was not properly parsed. Deduced from verb_form_db(done,do,past+part,_) above.

%verb_form_db(A,A,C,D) :-
%  writef_db("********************************** verb_form_db {0} failed", [[A,A,C,D]]).
%  !,
%  fail.

verb_root_db(be).
verb_root_db(do).
verb_root_db(have).

verb_root_db(border).
verb_root_db(contain).
verb_root_db(drain).
verb_root_db(exceed).
verb_root_db(flow).
verb_root_db(govern).
verb_root_db(rise).

verb_type_db(be,aux+be).
verb_type_db(do,aux+dv(_Prep)).
verb_type_db(have,aux+have).

verb_type_db(border,main+tv).
verb_type_db(contain,main+tv).
verb_type_db(drain,main+iv).
verb_type_db(exceed,main+tv).
verb_type_db(flow,main+iv).
verb_type_db(govern,main+tv).
verb_type_db(rise,main+iv).


% =================================================================
% Specialised Dictionary

adj_db(African,restr):- agentitive_trans_db(_,_,African).
adj_db(Baltic,restr):- agentitive_symmetric_type(_,Baltic).
%adj_db(american,restr).
%adj_db(asian,restr).
%adj_db(european,restr).
adj_db(average,restr).
adj_db(big,quant).
adj_db(great,quant).
adj_db(great,quant).
adj_db(large,quant).
adj_db(maximum,restr).
adj_db(minimum,restr).
adj_db(new,quant).
adj_db(old,quant).
adj_db(small,quant).
adj_db(total,restr).
adverb_db(tomorrow).
adverb_db(yesterday).

loc_pred_db(of,east,prep(cp(east,of))).
loc_pred_db(of,north,prep(cp(north,of))).
loc_pred_db(of,south,prep(cp(south,of))).
loc_pred_db(of,west,prep(cp(west,of))).

noun_plu_db(areas,area).
noun_plu_db(averages,average).
noun_plu_db(capitals,capital).
noun_plu_db(cities,city).
noun_plu_db(continents,continent).
noun_plu_db(countries,country).
noun_plu_db(nations,nation).
noun_plu_db(states,state).
noun_plu_db(degrees,degree).
noun_plu_db(ksqmiles,ksqmile).
noun_plu_db(latitudes,latitude).
noun_plu_db(longitudes,longitude).
noun_plu_db(million,million).
noun_plu_db(numbers,number).
noun_plu_db(oceans,ocean).
noun_plu_db(persons,person).
noun_plu_db(people,person).
noun_plu_db(places,place).
noun_plu_db(populations,population).
noun_plu_db(regions,region).
noun_plu_db(rivers,river).
noun_plu_db(seamasses,seamass).
noun_plu_db(seas,sea).
noun_plu_db(sqmiles,sqmile).
noun_plu_db(sums,sum).
noun_plu_db(thousand,thousand).
noun_plu_db(times,time).
noun_plu_db(totals,total).

noun_sin_db(area).
noun_sin_db(average).
noun_sin_db(capital).
noun_sin_db(city).
noun_sin_db(continent).
noun_sin_db(country).
noun_sin_db(degree).
noun_sin_db(ksqmile).
noun_sin_db(latitude).
noun_sin_db(longitude).
noun_sin_db(million).
noun_sin_db(number).
noun_sin_db(ocean).
noun_sin_db(person).
noun_sin_db(place).
noun_sin_db(population).
noun_sin_db(region).
noun_sin_db(river).
noun_sin_db(sea).
noun_sin_db(seamass).
noun_sin_db(sqmile).
noun_sin_db(sum).
noun_sin_db(thousand).
noun_sin_db(time).
noun_sin_db(total).

rel_adj_db(bigger,big).
rel_adj_db(greater,great).
rel_adj_db(larger,large).
rel_adj_db(less,small).
rel_adj_db(newer,new).
rel_adj_db(older,old).
rel_adj_db(smaller,small).

sup_adj_db(biggest,big).
sup_adj_db(largest,large).
sup_adj_db(newest,new).
sup_adj_db(oldest,old).
sup_adj_db(smallest,small).

