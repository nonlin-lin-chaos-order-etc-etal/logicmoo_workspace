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

/* Simplifying and executing the logical form of a NL query. */

:-op(500,xfy,--).
:-op(359,xf,ject).

write_tree(T):- !, print_tree80(T),!.
write_tree(T):-
   numbervars(T,101,_),
   wt(T,0),
   fail.
write_tree(_).

wt((P:-Q),L) :- !, L1 is L+3,
   write(P), tab(1), write((:-)), nl,
   tab(L1), wt(Q,L1).
wt(','(P,Q),L) :- !, L1 is L-2,
   wt(P,L), nl,
   tab(L1), put("&"), tab(1), wt(Q,L).
wt({P},L) :- complex(P), !, L1 is L+2,
   put("{"), tab(1), wt(P,L1), tab(1), put("}").
wt(E,L) :- decomp(E,H,P), !, L1 is L+2,
   header(H), nl,
   tab(L1), wt(P,L1).
wt(E,_) :- write(E).

header([]).
header([X|H]) :- write(X), tab(1), header(H).

decomp(setof(X,P,S),[S,=,setof,X],P).  
decomp(\+(P),[\+],P) :- complex(P).
decomp(numberof(_Mz,X,P,N),[N,=,numberof,X],P).
decomp(X^P,[exists,X|XX],P1) :- othervars(P,XX,P1).

othervars(X^P,[X|XX],P1) :- !, othervars(P,XX,P1).
othervars(P,[],P).

complex(','(_,_)).
complex({_}).
complex(setof(_,_,_)).
complex(numberof(_,_,_,_)).
complex(_^_).
complex(\+P) :- complex(P).

% Query execution.

respond([]) :- write('Nothing satisfies your question.'), nl.
respond([A|L]) :- reply(A), replies(L).

answer80((assertion80(X):-E)) :- !, assertion80(+(X),E),!. 
answer80((answer80([]):-E)) :- !, holds_truthvalue(?([]),E,B), yesno(B).
answer80((answer80([X]):-E)) :- !, seto(?([X]),X,E,S), respond(S).
answer80((answer80(X):-E)) :- seto(?(X),X,E,S), respond(S).

%answer80((assertion80([]):-E)) :- negated_e(E,NE),Mz = (?),  holds_truthvalue(Mz,NE,B), !, wdmsg(holds_truthvalue(Mz,NE,B)), !, fail.

negated_e(~(E),E):- nonvar(E),!.
negated_e(E,~(E)).

seto(Mz,X,E,S):- get_ex_set(ExV),seto2(Mz,ExV,X,E,S).
seto2(Mz,ExV,X,G^E,Set):- must(callable(E)), !,seto2(Mz,G^ExV,X,E,Set).
seto2(Mz,ExV,X,E,Set):- findall(S,setof(X,ExV^satisfy(Mz,E),S),L),ll_to_set(L,Set),!.
%seto(Mz,X,E,S) :- setof(X,satisfy(Mz,E),S), !.
%seto(Mz,_X,_E,[]).

get_ex_set(ExV):- nb_current(ex_set,ExV)->true;ExV=[].

bago(Mz,X,E,S):- get_ex_set(ExV),bago2(Mz,ExV,X,E,S).
bago2(Mz,ExV,X,E,Set):- seto2(Mz,ExV,X,E,Set).



/*
seto2(Mz,ExV,X,E,S):- % nop(ExV\==[]),
   !,
  (setof(X,ExV^satisfy(Mz,E),S)*->true;S=[]).

seto2(Mz,[],X,E,S):- 
  term_variables(E,EVars),
   term_singletons(X+X+EVars,EOnlyVars),
   term_singletons(E,ESingles),
   term_singletons(X+X+ESingles,ESinglesNoX),
   locally(b_setval(ex_set,ESinglesNoX),
     (setof(X,EOnlyVars^satisfy(Mz,E),S)*->true;S=[])).
*/

holds_truthvalue(Mz,E,true) :- satisfy(Mz,E), !.
holds_truthvalue(?,_E,false):- !.
holds_truthvalue(_,_E,unknown).

yesno(true) :- write('Yes.').
yesno(false) :- write('No.').

replies([]) :- write('.').
replies([A]) :- write(' and '), reply(A), write('.').
replies([A|X]) :- write(', '), reply(A), replies(X).

reply(N--U) :- !, write(N), write(' '), write(U).
reply(X) :- write(X).

% and_reordered(S,T):- satisfy(Mz,','(S,T)).

must_be_callable(P):- notrace(( assertion(callable(P)),assertion(\+ is_list(P)))).


is_mz_query(?(_)).
is_mz_tell(+(_)).

satisfy(Mz,call(P)) :- !, satisfy(Mz,P).
satisfy(Mz,','(P,Q)) :- is_mz_query(Mz), !, satisfy(Mz,P), satisfy(Mz,Q).
satisfy(Mz,(P;Q)) :- is_mz_query(Mz), !, satisfy(Mz,P) ; satisfy(Mz,Q).
satisfy(Mz,(P*->Q;R)) :- is_mz_query(Mz), !, satisfy(Mz,P) *-> satisfy(Mz,Q) ; satisfy(Mz,R).
satisfy(Mz,(P->Q;R)) :- is_mz_query(Mz), !, satisfy(Mz,P) -> satisfy(Mz,Q) ; satisfy(Mz,R).
satisfy(Mz,(P->Q)) :- is_mz_query(Mz), !, satisfy(Mz,P) -> satisfy(Mz,Q).
satisfy(Mz,{P}) :- is_mz_query(Mz), !, satisfy(Mz,P), !.
satisfy(Mz,_X^P) :- is_mz_query(Mz), !, satisfy(Mz,P).
satisfy(Mz,\+P) :- is_mz_query(Mz), satisfy(Mz,P), !, fail.
satisfy(Mz,\+_P) :- is_mz_query(Mz), !.
satisfy(Mz,numberof(Mz,X,P,N)) :-  is_mz_query(Mz), !, setof(X,satisfy(Mz,P),S), length(S,N).
satisfy(Mz,setof(X,P,S)) :-  is_mz_query(Mz), !, setof(X,satisfy(Mz,P),S).
satisfy(Mz,+P) :- is_mz_query(Mz), exceptionto(Mz,P), !, fail.
satisfy(Mz,+_) :- is_mz_query(Mz), !.
satisfy(Mz,setof(X,P,S)) :- !, setof80(Mz,X,P,S).
% satisfy(Mz,setof(X,P,S)) :- !, seto(Mz,X,P,S), S\==[].
satisfy(Mz,bagof(X,P,S)) :- !, bago(Mz,X,satisfy(Mz,P),S), S\==[].
satisfy(Mz,findall(X,P,S)) :- !, bago(Mz,X,satisfy(Mz,P),S).
satisfy(Mz,numberof(Mz,X,P,N)) :- !, numberof(Mz,X,P,N).
/*
satisfy(_Mz,X<Y) :- !, X<Y.
satisfy(_Mz,X=<Y) :- !, X=<Y.
satisfy(_Mz,X>=Y) :- !, X>=Y.
satisfy(_Mz,X>Y) :- !, X>Y.
*/
satisfy(Mz,assertion80(P)) :- !, assertion80(Mz,P).
satisfy(Mz,P) :- is_mz_tell(Mz), !, assertion80(Mz,P).
satisfy(Mz,P) :- is_mz_query(Mz), !, catch(call(P),E,(dmsg(call(P)-->E),break,fail)).

assertion80(X,assertion80(P)):- !, assertion80(X,P).
assertion80(+(X),E):- satisfy(?(X),{E}), wdmsg('Already True'=X^E),!.
assertion80(Mz,','(P,Q)) :- satisfy(Mz,P),!, assertion80(Mz,Q).
assertion80(Mz,','(P,Q)) :- !, ignore(show_failure((satisfy(Mz,Q), assertion80(Mz,P)))).
assertion80(+(Mz),X^P):-!,assertion80(+([X|Mz]),P).
assertion80(?(Mz),X^P):-!,assertion80(?([X|Mz]),P).
assertion80(Mz,P):- predicate_property(P, number_of_rules(N)),N>0,!, clause(P,What),What\==true,assertion80(Mz,What).
assertion80(_,P):- on_x_rtrace(assert(P)).
/*
:- meta_predicate(satisfy(Mz,?)).
:- export(satisfy/1).
satisfy(Mz,P):- must_be_callable(P), fail.
satisfy(Mz,M:P):- !, M:satisfy(Mz,P).
satisfy(Mz,','(P,Q)) :- !, satisfy(Mz,P), satisfy(Mz,Q).

satisfy(Mz,once(P)) :- !, once(satisfy(Mz,P)).
satisfy(Mz,{P}) :- !, satisfy(Mz,P), !.
satisfy(Mz,X^P) :- !, ^(X,P).
satisfy(Mz,\+P) :- !, \+ satisfy(Mz,P).
satisfy(Mz,X<Y) :- !, X<Y.
satisfy(Mz,X=<Y) :- !, X=<Y.
satisfy(Mz,X>=Y) :- !, X>=Y.
satisfy(Mz,X>Y) :- !, X>Y.
satisfy(Mz,satisfy(Mz,P)):- !, must_be_callable(P), !, satisfy(Mz,P).
% satisfy(Mz,P) :- P.
*/

:- module_transparent((^)/2).
^(X,P) :- get_ex_set(WazV), with_ex_v([X|WazV],satisfy(?([X]),P)).

ex_satisfy(Mz,X,P) :- get_ex_set(WazV), with_ex_v([X|WazV],satisfy(Mz,P)).
% :- system:import((^)/2).

with_ex_v(WazV,G) :- locally(b_setval(ex_set,WazV),G).


%+P :- !, satisfy_0(+P).
%satisfy_0(+P) :- exceptionto(Mz,P), !, fail.
%satisfy_0(+_P) :- !.

% setof(C,(D^(continent(C),in_ploc(D,C)),E^(country(E),in_ploc(E,C),F^(numberof(Mz,G,(city(G),in_ploc(G,E)),F),F>3))),B)
%WAS  numberof(Mz,X,P,N) :- !, seto(Mz,X,satisfy(Mz,P),S), length(S,N).
numberof(Mz,X,Ex^P,N) :- !, ex_satisfy(Mz,(N,Ex),setof(X,P,S)), length(S,N).
% numberof(Mz,X,Ex^P,N) :- !, locally(b_setval(ex_set,[Ex]),seto(Mz,X,P,S)), S\==[], length(S,N).
numberof(Mz,X,P,N) :- setof(X,N^satisfy(Mz,P),S), S\==[], length(S,N).

% setof80(Mz,X,P,S):- var(X),!,findall(E,seto(Mz,X,P,E),L),ll_to_set(L,S).
setof80(Mz,X,P,S):- findall(E,seto(Mz,X,P,E),L),ll_to_set(L,S).
ll_to_set(S0,S):- append(S0,F),list_to_set(F,S).

into_set(S0,S):-flatten([S0],F),list_to_set(F,S).
exceptionto(Mz,P) :-
   functor(P,F,N), functor(P1,F,N),
   pickargs(N,P,P1),
   exception(Mz,P1).

exception(Mz,P) :- is_mz_query(Mz), P, !, fail.
exception(Mz,_P):- is_mz_query(Mz),!.
exception(Mz,P):- throw(assert_exception(Mz,P)).

pickargs(0,_,_) :- !.
pickargs(N,P,P1) :- N1 is N-1,
   arg(N,P,S),
   pick(S,X),
   arg(N,P1,X),
   pickargs(N1,P,P1).

pick([X|_S],X).
pick([_|S],X) :- !, pick(S,X).
pick([],_) :- !, fail.
pick(X,X).

:- fixup_exports.
