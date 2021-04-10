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

%:- ensure_loaded(readin).

% Chat-80 : A small subset of English for database querying.

:-public hi/0, hi/1, quote/1.

:- op(400,xfy,&).
:- op(200,xfx,--).
:- [chatops].


/* Control loop */

q1([what,are,the,continents,no,country,in,which,contains,more,than,
    two,cities,whose,population,exceeds,nb(1),million,? ]).

q2([which,country,that,borders,the,mediterranean,borders,a,country,
    that,is,bordered,by,a,country,whose,population,exceeds,
    the,population,of,india,? ]).
/* ----------------------------------------------------------------------
	Simple questions
	These question do not require setof/3 and are useful for early
	testing of a system.
   ---------------------------------------------------------------------- */

eg( [ does, america, contain, new_york, ? ] ).
eg( [ does, mexico, border, the, united_states, ? ] ).
eg( [ is, the, population, of, china, greater, than, nb(200), million, ? ] ).
eg( [ does, the, population, of, china, exceed, nb(1000), million, ? ] ).
eg( [ is, the, population, of, china, nb(840), million, ? ] ).
eg( [ does, the, population, of, china, exceed, the, population, of,
      india, ? ] ).
eg( [ is, spain, bordered, by, the, pacific, ? ] ).
eg( [ does, the, atlantic, border, spain, ? ] ).
eg( [ is, the, rhine, in, switzerland, ? ] ).
eg( [ is, the, united_kingdom, in, europe, ? ] ).


/* ----------------------------------------------------------------------
	Standard question set
	This is the standard chat question set, originally put together
	by David and Fernando and use in their papers. Quintus uses this
	set as a standard for performance comparisons.
   ---------------------------------------------------------------------- */

ed(  1, [ what, rivers, are, there, ? ],

		[amazon, amu_darya, amur, brahmaputra, colorado,
		congo_river, cubango, danube, don, elbe, euphrates, ganges,
		hwang_ho, indus, irrawaddy, lena, limpopo, mackenzie,
		mekong, mississippi, murray, niger_river, nile, ob, oder,
		orange, orinoco, parana, rhine, rhone, rio_grande, salween,
		seine, senegal_river, tagus, vistula, volga, volta, yangtze,
		yenisei, yukon, zambesi]  ).

ed(  2, [ does, afghanistan, border, china, ? ],

		[true]  ).

ed(  3, [ what, is, the, capital, of, upper_volta, ? ],

		[ouagadougou]  ).

ed(  4, [ where, is, the, largest, country, ? ],

		[asia, northern_asia]  ).

ed(  5, [ which, countries, are, european, ? ],

		[albania, andorra, austria, belgium, bulgaria, cyprus,
		czechoslovakia, denmark, east_germany, eire, finland,
		france, greece, hungary, iceland, italy, liechtenstein,
		luxembourg, malta, monaco, netherlands, norway, poland,
		portugal, romania, san_marino, spain, sweden, switzerland,
		united_kingdom, west_germany, yugoslavia]  ).

ed(  6, [ which, country, '''', s, capital, is, london, ? ],

		[united_kingdom]  ).

ed(  7, [ which, is, the, largest, african, country, ? ],

		[sudan]  ).

ed(  8, [ how, large, is, the, smallest, american, country, ? ],

		[0--ksqmiles]  ).

ed(  9, [ what, is, the, ocean, that, borders, african, countries,
	  and, that, borders, asian, countries, ? ],

		[indian_ocean]  ).

ed( 10, [ what, are, the, capitals, of, the, countries, bordering, the,
	  baltic, ? ],

		[[[denmark]:[copenhagen], [east_germany]:[east_berlin],
		[finland]:[helsinki], [poland]:[warsaw],
		[soviet_union]:[moscow], [sweden]:[stockholm],
		[west_germany]:[bonn]]]  ).

ed( 11, [ which, countries, are, bordered, by, two, seas, ? ],

		[egypt, iran, israel, saudi_arabia, turkey]  ).

ed( 12, [ how, many, countries, does, the, danube, flow, through, ? ],

		[6]  ).

ed( 13, [ what, is, the, total, area, of, countries, south, of, the, equator,
	  and, not, in, australasia, ? ],

		[10228--ksqmiles]  ).

ed( 14, [ what, is, the, average, area, of, the, countries, in, each,
	  continent, ? ],

		[[africa,233--ksqmiles], [america,496--ksqmiles],
		[asia,485--ksqmiles], [australasia,543--ksqmiles],
		[europe,58--ksqmiles]]  ).

ed( 15, [ is, there, more, than, one, country, in, each, continent, ? ],

		[false]  ).

ed( 16, [ is, there, some, ocean, that, does, not, border, any, country, ? ],

		[true]  ).

ed( 17, [ what, are, the, countries, from, which, a, river, flows, into,
	  the, black_sea, ? ],

		[[romania,soviet_union]]  ).

ed( 18, [ what, are, the, continents, no, country, in, which, contains, more,
	  than, two, cities, whose, population, exceeds, nb(1), million, ? ],

		[[africa,antarctica,australasia]]  ).

ed( 19, [ which, country, bordering, the, mediterranean, borders, a, country,
	  that, is, bordered, by, a, country, whose, population, exceeds,
	  the, population, of, india, ? ],

		[turkey]  ).

ed( 20, [ which, countries, have, a, population, exceeding, nb(10),
	  million, ? ],

		[afghanistan, algeria, argentina, australia, bangladesh,
		brazil, burma, canada, china, colombia, czechoslovakia,
		east_germany, egypt, ethiopia, france, india, indonesia,
		iran, italy, japan, kenya, malaysia, mexico, morocco, nepal,
		netherlands, nigeria, north_korea, pakistan, peru,
		philippines, poland, south_africa, south_korea,
		soviet_union, spain, sri_lanka, sudan, taiwan, tanzania,
		thailand, turkey, uganda, united_kingdom, united_states, venezuela,
		vietnam, west_germany, yugoslavia, zaire]  ).

ed( 21, [ which, countries, with, a, population, exceeding, nb(10), million,
	  border, the, atlantic, ? ],

		[argentina, brazil, canada, colombia, france, mexico,
		morocco, netherlands, nigeria, south_africa, spain,
		united_kingdom, united_states, venezuela, west_germany,
		zaire]  ).

ed( 22, [ what, percentage, of, countries, border, each, ocean, ? ],

		[[arctic_ocean,2], [atlantic,35], [indian_ocean,14],
		[pacific,20]]  ).

ed( 23, [ what, countries, are, there, in, europe, ? ],

		[albania, andorra, austria, belgium, bulgaria, cyprus,
		czechoslovakia, denmark, east_germany, eire, finland,
		france, greece, hungary, iceland, italy, liechtenstein,
		luxembourg, malta, monaco, netherlands, norway, poland,
		portugal, romania, san_marino, spain, sweden, switzerland,
		united_kingdom, west_germany, yugoslavia]  ).


/* ----------------------------------------------------------------------
	Simple Access to demonstrations
   ---------------------------------------------------------------------- */

demo(Type) :- demo(Type,L), inform(L), check_words(L,S), process(S).

demo(mini,List) :- eg(List).
demo(main,List) :- ed(_,List,_).

inform(L) :- nl, write('Question: '), inform1(L), nl, !.

inform1([]).
inform1([H|T]) :- write(H), put(32), inform1(T).


/* ----------------------------------------------------------------------
	Top level processing for verification and performance analysis
   ---------------------------------------------------------------------- */

test_chat :- test_chat(_,off).

test_chat(N):- test_chat(N, on).

test_chat(N, OnOff) :-
	show_title,
	ed(N,Sentence,CorrectAnswer),
	  process(Sentence,CorrectAnswer,Status,Times),
	  show_results(N,Status,Times),
    OnOff \= off,
    tracing ~= OnOff,
    process(Sentence),
	fail.
test_chat(_,_).

test :-
	time(rtest_chats(20)).

					% added JW
rtest_chats(0) :- !.
rtest_chats(N) :-
	rtest_chat(1),
	NN is N - 1,
	rtest_chats(NN).

rtest_chat(N) :-
	ed(N,Sentence,CorrectAnswer), !,
	  process(Sentence,CorrectAnswer,Status,_Times),
	  (   Status == true
	  ->  true
	  ;   format(user_error, 'Test ~w failed!~n', [N])
	  ),
	NN is N + 1,
	rtest_chat(NN).
rtest_chat(_).

show_title :-
	format('Chat Natural Language Question Anwering Test~n~n',[]),
	show_format(F),
	format(F, ['Test','Parse','Semantics','Planning','Reply','TOTAL']),
	nl.

show_results(N,Status,Times) :-
	show_format(F),
	format(F, [N|Times]),
	( Status = true ->
		nl
	; true ->
		tab(2), write(Status), nl
	).

show_format( '~t~w~10+ |~t~w~12+~t~w~10+~t~w~10+~t~w~10+~t~w~10+' ).


process(Sentence,CorrectAnswer,Status,Times) :-
	process(Sentence,Answer,Times),
	!,
	check_answer(Sentence,Answer,CorrectAnswer,Status).
process(_,_,failed,[0,0,0,0,0]).


process(Sentence,Answer,[Time1,Time2,Time3,Time4,TotalTime]) :-
	statistics(runtime, [T0, _]),

	  sentence(E,Sentence,[],[],[]),

	statistics(runtime, [T1, _]),
	Time1 is T1 - T0,
	statistics(runtime, [T2, _]),

	  i_sentence(E,QT),
	  clausify(QT,UE),
	  simplify(UE,S),

	statistics(runtime, [T3, _]),
	Time2 is T3 - T2,
	statistics(runtime, [T4, _]),

	  qplan(S,S1), !,

	statistics(runtime, [T5, _]),
	Time3 is T5 - T4,
	statistics(runtime, [T6, _]),

	  answer(S1,Answer), !,

	statistics(runtime, [T7, _]),
	Time4 is T7 - T6,
	TotalTime is Time1 + Time2 + Time3 + Time4.


	% Version of answer/1 from TALKR which returns answer
answer((answer([]):-E),[B]) :- !, holds(E,B).
answer((answer([X]):-E),S) :- !, seto(X,E,S).
answer((answer(X):-E),S) :- seto(X,E,S).

check_answer(_Sentence,A,B,true) :- close_answer(A,B),!.
check_answer(Sentence,A,B,'wrong answer'):-
  pprint_ecp_cmt(red,check_answer(Sentence,A,B,'wrong answer')),
  tracing ~= on,
  once(process(Sentence)).
  
close_answer(A,A).
close_answer(A,B):- number(A),number(B),X is integer(A),Y is integer(A),X=Y.
%close_answer(A,B):- is_list(A), sort(A,AA), A\==AA, !, close_answer(AA,B).
%close_answer(B,A):- is_list(A), sort(A,AA), A\==AA, !, close_answer(B,AA).
close_answer(A,B):- 
  compound(A),compound(B),
  compound_name_arguments(A,AA,AAA),
  compound_name_arguments(B,AA,BBB),!,
  maplist(close_answer,AAA,BBB).

/* ----------------------------------------------------------------------
	Top level for runtime version, and interactive demonstrations
   ---------------------------------------------------------------------- */

runtime_entry(start) :-
   version,
   format(user,'~nChat Demonstration Program~n~n',[]),
   hi.

hi :-
%   assert(tracing),
%   tell('hi_out.txt'),
   hi(user)
%   ,told
   .

hi1 :-
   assert(tracing),
%   tell('hi_out.txt'),
   q1(P),
   control(P)
%   ,fail
%   ,told.
  .

hi2 :-
   assert(tracing),
%   tell('hi_out.txt'),
   q2(P),
   control(P)
%   ,fail
%   ,told.
  .

hi(File) :-
   repeat,
      ask(File,P),
      control(P), !,
      end(File).

ask(user,P) :- !,
   write('Question: '),
   ttyflush,
   read_in(P).
   %originally: read_in(P).
ask(File,P) :-
   seeing(Old),
   see(File),
   read_in(P),
   nl,
   pprint_ecp_cmt(yellow,read_in(P)),
   doing(P,0),
   nl,
   see(Old).

   /*
% added by John Pool
read_in2( L) :-
  readatoms( A),
  atoms_to_lower( A, L).

atoms_to_lower( [A|As], [L|Ls]) :-
  !,
  atom_string( A, S),
  Low is lowcase( S),
  atom_string( L0, Low),
  getnum( L0, L),
  atoms_to_lower( As, Ls).
atoms_to_lower( [], []).

getnum( A, nb(A)) :-
  integer( A),
  !.
getnum( A, A).
%%%%%%%%%%%%%%%%%%%%%%%%%
*/

doing([],_) :- !.
doing([X|L],N0) :-
   out1(X),
   advance(X,N0,N),
   doing(L,N).

out1(nb(X)) :- !,
   write(X).
out1(A) :-
   write(A).

advance(X,N0,N) :-
   uses(X,K),
   M is N0+K,
 ( M>72, !,
     nl,
      N is 0;
     N is M+1,
      put(" ")).

uses(nb(X),N) :- !,
   chars(X,N).
uses(X,N) :-
   chars(X,N).

chars(X,N) :- atomic(X), !,
   name(X,L),
   length(L,N).
chars(_,2).

end(user) :- !.
end(F) :- seeing(F) -> seen ; true. % close(F).

control([bye,'.']) :- !,
   nl, nl,
   write('Cheerio.'),
   nl.
control([x,'.']) :- !,
   halt.
control([trace,'.']) :- !,
   tracing ~= on,
   write('Tracing from now on!'), nl, fail.
control([do,not,trace,'.']) :- !,
   tracing ~= off,
   write('No longer tracing.'), nl, fail.
control([do,mini,demo,'.']) :- !,
   write('Executing mini demo...'), nl,
   demo(mini), fail.
control([#|Text]) :- write(Text), nl, !, fail.
control([do,main,demo,'.']) :- !,
   write('Executing main demo...'), nl,
   demo(main), fail.
control([test,chat,'.']) :- !,
   test_chat, fail.
control(U0) :-
   check_words(U0,U),
   process(U),
   fail.

process(U) :-
   runtime(StartParse),
   sentence(E,U,[],[],[]),
   runtime(StopParse),
   ParseTime is StopParse - StartParse,
   report(E,'Parse',ParseTime,tree),
   % !, %%%%%%%%%%%%%%%% added by JPO but breaks "london"
   runtime(StartSem),
   parsed_to_logic(E,S),
   runtime(StopSem),
   SemTime is StopSem - StartSem,
   report(S,'Semantics',SemTime,expr),
   runtime(StartPlan),
   qplan(S,S1),
   runtime(StopPlan),
   TimePlan is StopPlan - StartPlan,
   (S=@=S1->S1R=S1;S1R=same),
   report(S1R,'Planning',TimePlan,expr),
   runtime(StartAns),
   answer(S1), !, nl,
   runtime(StopAns),
   TimeAns is StopAns - StartAns,
   report(_,'Reply',TimeAns,none).
process(_) :-
   failure.

failure :-
   nl, nl,
   write('I don''t understand!'), nl.

report(Item,Label,Time,Mode) :-
   tracing =: on, !,
   nl, write(Label), write(': '), write(Time), write('msec.'), nl,
   \+ \+ report_item(Mode,Item),!.
report(_,_,_,_).

report_item(none,_).
report_item(_,Item):- pprint_ecp_cmt(yellow,Item),!.
report_item(expr,Item) :-
   write_tree(Item), nl.
report_item(_Tree,Item) :-
   print_tree(Item).

runtime(MSec) :-
   statistics(runtime,[MSec,_]).

quote(A&R) :-
   atom(A), !,
   quote_amp(R).
quote(_-_).
quote(_--_).
quote(_+_).
quote(verb(_,_,_,_,_)).
quote(wh(_)).
quote(name(_)).
quote(prep(_)).
quote(det(_)).
quote(quant(_,_)).
quote(int_det(_)).

quote_amp('$VAR'(_)) :- !.
quote_amp(R) :-
   quote(R).

parsed_to_logic(S0,S) :-
   i_sentence(S0,S1),
   clausify(S1,S2),
   simplify(S2,S).

simplify(C,(P:-R)) :- !,
   unequalise(C,(P:-Q)),
   simplify(Q,R,true).

simplify(setof(X,P0,S),R,R0) :- !,
   simplify(P0,P,true),
   revand(R0,setof(X,P,S),R).
simplify((P,Q),R,R0) :-
   simplify(Q,R1,R0),
   simplify(P,R,R1).
simplify(true,R,R) :- !.
simplify(X^P0,R,R0) :- !,
   simplify(P0,P,true),
   revand(R0,X^P,R).
simplify(numberof(X,P0,Y),R,R0) :- !,
   simplify(P0,P,true),
   revand(R0,numberof(X,P,Y),R).
simplify(\+P0,R,R0) :- !,
   simplify(P0,P1,true),
   simplify_not(P1,P),
   revand(R0,P,R).
simplify(P,R,R0) :-
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
   arg(L,V,X)
   ;
   true).
drop_eq('$VAR'(I),T,V,M,true) :- !,
   deref(I,M,J),
   arg(J,V,T),
   arg(J,M,0).
drop_eq(T,'$VAR'(I),V,M,true) :- !,
   deref(I,M,J),
   arg(J,V,T),
   arg(J,M,0).
drop_eq(X,Y,_,_,X=Y).

deref(I,M,J) :-
   arg(I,M,X),
  (var(X), !, I=J;
   deref(X,M,J)).

exquant('$VAR'(I),V,M,P0,P) :-
   arg(I,M,U),
 ( var(U), !,
   arg(I,V,X),
   P=(X^P0);
   P=P0 ).

irev(I,J,I,J) :- I>J, !.
irev(I,J,J,I).

%:- mode check_words(+,-).

check_words([],[]).
check_words([Word|Words],[RevWord|RevWords]) :-
   check_word(Word,RevWord),
   check_words(Words,RevWords).

%:- mode check_word(+,-).

check_word(Word,Word) :- word(Word), !.
check_word(Word,NewWord) :-
   % write('? '), write(Word), write(' -> (!. to abort) '), ttyflush, read(NewWord0), NewWord0 \== !,
   % check_word(NewWord0,NewWord)
   ignore(NewWord=Word),
   !.

%:- mode ~=(+,+), =+(+,-), =:(+,?).

Var ~= Val :-
 ( recorded(Var,val(_),P), erase(P)
 ; true), !,
 recordz(Var,val(Val),_).

Var =+ Val :-
 ( recorded(Var,val(Val0),P), erase(P)
 ; Val0 is 0), !,
   Val is Val0+1,
   recordz(Var,val(Val),_).

Var =: Val :-
   recorded(Var,val(Val),_).
