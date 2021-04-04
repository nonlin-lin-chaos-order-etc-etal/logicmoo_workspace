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

% Chat-80 : A small subset of English for database querying.

/* Control loop */

hi80 :-
   hi80(user).

hi80(File) :-
   repeat,
  writeln('------------------------------------------------------'),
      ask80(File,P),
      control80(P), !,
      end80(File).

ask80(user,P) :- !,
   prompt(_,'Question: '),
   read_in(P).
ask80(File,P) :-
   seeing(Old),
   see(File),
   read_in(P),
   nl,
   doing80(P,0),
   nl,
   see(Old).

doing80([],_) :- !.
doing80([X|L],N0) :-
   out80(X),
   advance80(X,N0,N),
   doing80(L,N).


out80(nb(X)) :- !,
   write(X).
out80(A) :-
   write(A).

advance80(X,N0,N) :-
   uses80(X,K),
   M is N0+K,
 ( M>72, !,
      nl,
      N is 0;
   N is M+1,
      put(" ")).

uses80(nb(X),N) :- !,
   chars80(X,N).
uses80(X,N) :-
   chars80(X,N).

chars80(X,N) :- atomic(X), !,
   name(X,L),
   length(L,N).
chars80(_,2).

end80(user) :- !.
end80(_F) :-
   seen.

:- dynamic(t_l:tracing80/0).

atom_or_nb(nb(A)):-!,number(A).
atom_or_nb(A):-atom(A).
string_nb_to_atom(nb(N),N):- number(N),!.
string_nb_to_atom(N,nb(N)):- number(N),!.
string_nb_to_atom(S,A):- string_to_atom(S,A),!.
control80(NotList):-  
 ( \+ is_list(NotList); \+ maplist(atom_or_nb,NotList) ) ->
   to_word_list(NotList,List),
   maplist(string_nb_to_atom,List,ListIn),
   dmsg(NotList \=@= ListIn),!,   
   control80(ListIn).

control80([bye,'.']) :- !,
   write('Cheerio.'),
   nl.
control80([trace,'.']) :- !,
   assert(t_l:tracing80),
   write('Tracing from now on!'), nl, fail.
control80([do,not,trace,'.']) :-
   retract(t_l:tracing80), !,
   write('No longer tracing.'), nl, fail.
control80(U) :-
   process80(U),
   fail.

:- share_mp(trace_chat80/1).
trace_chat80(U):-
 locally(t_l:t_l:tracing80,
           locally(t_l:chat80_interactive,
            locally_hide(t_l:useOnlyExternalDBs,
             locally_hide(thglobal:use_cyc_database,
              ignore(control80(U)))))).
:- system:import(trace_chat80/1).

:- share_mp(test_chat80/1).
test_chat80(U):-
 locally(t_l:tracing80_nop,
           locally(t_l:chat80_interactive_nop,
            locally_hide(t_l:useOnlyExternalDBs,
             locally_hide(thglobal:use_cyc_database,
              ignore(control80(U)))))).
   
:- system:import(test_chat80/1).


process801(U) :-
 must_det_l((
   runtime(StartParse),
   sentence(E0,U,[],[],[]),
   unnumbervars(E0,E),
   runtime(StopParse),
   ParseTime is StopParse - StartParse,
   DISP = sent_to_prelogic(E,S),
   report(DISP,'Parse',ParseTime,tree))),
   runtime(StartSem),
   ignore(must_or_rtrace(sent_to_prelogic(E,S))), !,
   runtime(StopSem),
   SemTime is StopSem - StartSem,
   report(S,'Semantics',SemTime,expr),
   runtime(StartPlan),
   qplan(S,S1), !,
   runtime(StopPlan),
   TimePlan is StopPlan - StartPlan,
   (S1==S -> Show1= planned_same; Show1=S1),
   report(Show1,'Planning',TimePlan,expr),
   runtime(StartAns),
   writeln('-----------'),
   once((must_or_rtrace(answer(S1)),true)), !, nl,
   runtime(StopAns),
   TimeAns is StopAns - StartAns,
   report(_,'Reply',TimeAns,none).

process80(U):- process801(U), !.
process80(_) :-
   failure.

failure :-
   write('I don''t understand!'), nl.

report(Item,Label,Time,Mode) :-
   t_l:tracing80, !,
   nl, write(Label), write(': '), write(Time), write('sec.'), nl,
   \+ \+ report_item(Mode,Item),!.
report(_,_,_,_).

report_item(none,_).
report_item(T,Var):-var(Var),!,write('FAILED: '+T),nl.
report_item(portray,Item) :-
   portray_clause((Item:-Item)), nl.
report_item(expr,Item) :-
   write_tree(Item), nl.
report_item(tree,Item) :-
   print_tree80(Item), nl.

runtime(Time) :-
   statistics(runtime,[MSec,_]),
   Time is MSec/1000.

quote80('&'(A,R)) :-
   atom(A), !,
   quote_amp(R).
quote80(_-_).
quote80(_--_).
quote80(_+_).
quote80(verb(_,_,_,_,_)).
quote80(wh(_)).
quote80(name(_)).
quote80(prep(_)).
quote80(det(_)).
quote80(quant(_,_)).
quote80(int_det(_)).

quote_amp('$VAR'(_)) :- !.
quote_amp(R) :-
   quote80(R).

sent_to_prelogic(S0,S) :-
   unnumbervars(S0,S00),
   i_sentence(S00,S1),
   clausify(S1,S2),
   simplify80(S2,S).

simplify80(C,C0):-var(C),dmsg(var_simplify(C,C0)),!,fail.
simplify80(C,(P:-R)) :- !,
   unequalise(C,(P:-Q)),
   simplify80(Q,R,true).

simplify80(C,C0,C1):-var(C),dmsg(var_simplify(C,C0,C1)),fail.
simplify80(C,C,R):-var(C),!,R=C.
simplify80(setof(X,P0,S),R,R0) :- !,
   simplify80(P0,P,true),
   revand(R0,setof(X,P,S),R).
simplify80(','(P,Q),R,R0) :-
   simplify80(Q,R1,R0),
   simplify80(P,R,R1).
simplify80(true,R,R) :- !.
simplify80(X^P0,R,R0) :- !,
   simplify80(P0,P,true),
   revand(R0,X^P,R).
simplify80(numberof(X,P0,Y),R,R0) :- !,
   simplify80(P0,P,true),
   revand(R0,numberof(X,P,Y),R).
simplify80(\+P0,R,R0) :- !,
   simplify80(P0,P1,true),
   simplify_not(P1,P),
   revand(R0,P,R).
simplify80(P,R,R0) :-
   revand(R0,P,R).

simplify_not(\+P,P) :- !.
simplify_not(P,\+P).

revand(true,P,P) :- !.
revand(P,true,P) :- !.
revand(P,Q,(Q,P)).

unequalise(C0,C) :-
   numbervars(C0,1,N),
   functor(V,v,N),
   functor(M,v,N),
   inv_map(C0,V,M,C).

inv_map('$VAR'(I),V,_,X) :- !,
   arg(I,V,X).
inv_map(A=B,V,M,T) :- !,
   drop_eq(A,B,V,M,T).
inv_map(X^P0,V,M,P) :- !,
   inv_map(P0,V,M,P1),
   exquant(X,V,M,P1,P).
inv_map(A,_,_,A) :- atomic(A), !.
inv_map(T,V,M,R) :-
   functor(T,F,K),
   functor(R,F,K),
   inv_map_list(K,T,V,M,R).

inv_map_list(0,_,_,_,_) :- !.
inv_map_list(K0,T,V,M,R) :-
   arg(K0,T,A),
   arg(K0,R,B),
   inv_map(A,V,M,B),
   K is K0-1,
   inv_map_list(K,T,V,M,R).

drop_eq('$VAR'(I),'$VAR'(J),V,M,true) :- !,
 ( I=\=J, !,
      irev(I,J,K,L), 
      arg(K,M,L),
      arg(K,V,X),
      arg(L,V,X);
   true).
drop_eq('$VAR'(I),T,V,M,true) :- !,
   arg(I,V,T),
   arg(I,M,0).
drop_eq(T,'$VAR'(I),V,M,true) :- !,
   arg(I,V,T),
   arg(I,M,0).
drop_eq(X,Y,_,_,X=Y).

exquant('$VAR'(I),V,M,P0,P) :-
   arg(I,M,U),
 ( var(U), !,
      arg(I,V,X),
       P=(X^P0);
   P=P0).

irev(I,J,I,J) :- I>J, !.
irev(I,J,J,I).

parser_chat80:t11:- 
   test_chat80("how many postures are there?"),
   test_chat80("what are the postures?"),
   test_chat80("what are the types?"),
   %test_chat80("how many oceans are seas?"),
   %test_chat80("how many oceans are seamasses?"),
   test_chat80("how many types are there?"),
   test_chat80("how many formattypes are there?"),
   test_chat80("what formattypes are there?"),
   !.
parser_chat80:t13:-!.
parser_chat80:t12:-
   hi80('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/ext/chat80/original/demo').

:- fixup_exports.
