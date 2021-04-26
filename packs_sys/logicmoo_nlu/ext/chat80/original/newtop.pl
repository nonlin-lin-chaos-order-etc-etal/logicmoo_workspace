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
:- ensure_loaded('/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/prolog/logicmoo_nlu/parser_tokenize').

%theTextC(W1,CYCPOS,Y=W1)  ---> {t_l:old_text,!},[W1],{W1=Y}.
theTextC(A,_,F=A,B,C,D,E) :- t_l:old_text, !,terminal(A, B, C, D, E),A=F, is_sane_nv(A).
theTextC(A,_,F=A,B,C,D,E) :- !,terminal(w(A, _), B, C, D, E),A=F,is_sane_nv(A).
%theTextC(W1,CYCPOS,Y=W1)  ---> {!},[w(W1,_)],{W1=Y}.
%theTextC(W1,CYCPOS,WHY) ---> [W2],{memoize_pos_to_lex80(WHY,CYCPOS,W2,W1)}.
%theTextC(H,F,E,A,B,C,D) :- fail, is_sane(C), terminal(G, A, B, C, D),memoize_pos_to_lex80(E, F, G, H),is_sane_nv(H).

is_sane_nv(C) :-
    must((nonvar(C), term_depth(C, D), D<100)).

/*
theTextC(W1,_CYCPOS,Y=W1) ---> {t_l:old_text,!},[W1],{W1=Y}.
%theTextC(W1,_CYCPOS,Y=W1) ---> {!},[w(W1,_)],{W1=Y}.
theTextC(A,_,F=A,B,C,D,E) :- !,terminal(w(A, _), B, C, D, E),A=F,is_sane_nv(A).
theTextC(W1,_CYCPOS,WHY) ---> {t_l:old_text,!},[W1],WHY.
% theTextC(W1,CYCPOS,WHY) ---> {trace_or_throw(memoize_pos_to_lex80(WHY,CYCPOS,W2,W1))},[W2],{memoize_pos_to_lex80(WHY,CYCPOS,W2,W1)}.
*/

% Chat-80 : A small subset of English for database querying.

:-public hi80/0, hi80/1, quote/1.

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

ed( [ does, america, contain, new_york, ? ] ,mini).
ed( [ does, mexico, border, the, united_states, ? ] ,mini).
ed( [ is, the, population, of, china, greater, than, nb(200), million, ? ] ,mini).
ed( [ does, the, population, of, china, exceed, nb(1000), million, ? ] ,mini).
ed( [ is, the, population, of, china, nb(840), million, ? ] ,mini).
ed( [ does, the, population, of, china, exceed, the, population, of,
      india, ? ] ,mini).
ed( [ is, spain, bordered, by, the, pacific, ? ] ,mini).
ed( [ does, the, atlantic, border, spain, ? ] ,mini).
ed( [ is, the, rhine, in, switzerland, ? ] ,mini).
ed( [ is, the, united_kingdom, in, europe, ? ] ,mini).


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


ed( 24, [ what, are, the, areas, of, the, countries, bordering, the,
	  baltic, ? ],

		[[ [denmark]:[--(16.615,ksqmiles)],
                 [east_germany]:[--(40.646,ksqmiles)],
                 [finland]:[--(130.119,ksqmiles)],
                 [poland]:[--(120.359,ksqmiles)],
                 [soviet_union]:[--(8347.25,ksqmiles)],
                 [sweden]:[--(173.665,ksqmiles)],
                 [west_germany]:[--(95.815,ksqmiles)]
               ]]  ).

ed( N, [ what, are, the, rivers, that, flow, through, the, countries, bordering, the,
	  baltic, ? ],

		[[[denmark]:[copenhagen], [east_germany]:[east_berlin],
		[finland]:[helsinki], [poland]:[warsaw],
		[soviet_union]:[moscow], [sweden]:[stockholm],
		[west_germany]:[bonn]]]  ):- N==25.


ed( N, [ what, are, the, rivers, that, flow, through, each, country, bordering, the,
	  baltic, ? ],

		[[[denmark]:[copenhagen], [east_germany]:[east_berlin],
		[finland]:[helsinki], [poland]:[warsaw],
		[soviet_union]:[moscow], [sweden]:[stockholm],
		[west_germany]:[bonn]]]  ):- N==26.


ed( N, [ what, are, the, capitals, of, the, countries, bordering, the,
	  baltic, ? ],

		[[[denmark]:[copenhagen], [east_germany]:[east_berlin],
		[finland]:[helsinki], [poland]:[warsaw],
		[soviet_union]:[moscow], [sweden]:[stockholm],
		[west_germany]:[bonn]]]  ):- N==27.

ed( N, [ what, are, the, cities, in, countries, bordering, the,
	  baltic, ? ],

		[[[denmark]:[copenhagen], [east_germany]:[east_berlin],
		[finland]:[helsinki], [poland]:[warsaw],
		[soviet_union]:[moscow], [sweden]:[stockholm],
		[west_germany]:[bonn]]]  ):- N==28.

ed( N, [ what, cities, do, the, countries, bordering, the,
	  baltic, contain, ? ],

		[[[denmark]:[copenhagen], [east_germany]:[east_berlin],
		[finland]:[helsinki], [poland]:[warsaw],
		[soviet_union]:[moscow], [sweden]:[stockholm],
		[west_germany]:[bonn]]]  ):- N==29.

ed( N, W, _):- 
  clause(ed( W, mini), true, Ref),
  nth_clause(_,N0,Ref),N is N0+29.

/* ----------------------------------------------------------------------
	Simple Access to demonstrations
   ---------------------------------------------------------------------- */

demo(Type) :- demo(Type,L), ignore(control80(L)).

demo(Type,List) :- ed( List, Type).
demo(main,List) :- ed(_,List,_).

inform(L) :- nl, write('Question/Statement: '), inform1(L), nl, !.

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
    report_item0(print_test,Sentence),
	  process5(test,Sentence,CorrectAnswer,Status,Times),
	  show_results(N,Status,Times),
    OnOff \= off,
    tracing ~= OnOff,
    process(normal,Sentence),
	fail.
test_chat(_,_).

test :-
	time(rtest_chats(30)).

					% added JW
rtest_chats(0) :- !.
rtest_chats(N) :-
	rtest_chat(1),
	NN is N - 1,
	rtest_chats(NN).

rtest_chat(N) :-
	ed(N,Sentence,CorrectAnswer), !,
	  process5(test,Sentence,CorrectAnswer,Status,_Times),
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



% Version of answer/1 from TALKR which returns answer
answer((answer([]):-E),[B]) :- !, holds(E,B).
answer((answer([X]):-E),S) :- !, seto(X,E,S).
answer((answer(X):-E),S) :- seto(X,E,S).

check_answer(_Sentence,A,B,true) :- close_answer(A,B),!.
check_answer(Sentence,A,B,'wrong answer'):-
  pprint_ecp_cmt(red,check_answer(Sentence,A,B,'wrong answer')),
  tracing ~= on,
  once(process(debug,Sentence)).

close_answer(A,A).
close_answer(A,B):- number(A),number(B),!, X is integer(A*10),Y is integer(A*10),!, X=Y.
close_answer(A,B):- is_list(A), sort(A,AA), A\==AA, !, close_answer(AA,B).
close_answer(B,A):- is_list(A), sort(A,AA), A\==AA, !, close_answer(B,AA).
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
   hi80.

hi80 :-
%   tracing ~= on,
%   tell('hi_out.txt'),
   hi80(user)
%   ,told
   .

hi1 :-
   tracing ~= on,
%   tell('hi_out.txt'),
   q1(P),
   control80(P)
%   ,fail
%   ,told.
  .

hi2 :-
   tracing ~= on,
%   tell('hi_out.txt'),
   q2(P),
   control80(P)
%   ,fail
%   ,told.
  .

hi80(File) :-
   repeat,
      ask(File,P),
      control80(P), !,
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
   % pprint_ecp_cmt(yellow,read_in(P)),
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

print_test(X):- doing(X ,0).

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

chat80_test(L):- ignore(control80(L)).
test_chat80(L):- ignore(control80(L)).

control80(L):- check_words(L,S)-> L\==S, !, control80(S).
control80([bye,'.']) :- !,
   nl, nl,
   write('Cheerio.'),
   nl.
control80([x,'.']) :- !,
   halt.
control80([trace,'.']) :- !,
   tracing ~= on,
   write('Tracing from now on!'), nl, fail.
control80([do,not,trace,'.']) :- !,
   tracing ~= off,
   write('No longer tracing.'), nl, fail.
control80([do,mini,demo,'.']) :- !,
   write('Executing mini demo...'), nl,
   demo(mini), fail.
control80([#|Text]) :- write(Text), nl, !, fail.
control80([do,Main,demo,'.']) :- !,
   write('Executing main demo...'), nl,
   demo(Main), fail.
control80([test,chat,'.']) :- !,
   test_chat, fail.
control80(U) :-   
   process(normal, U),
   fail.

process5(How,Sentence,CorrectAnswer,Status,Times) :-
	process4(How,Sentence,Answer,Times),
	!,
	check_answer(Sentence,Answer,CorrectAnswer,Status).
process5(_How,_,_,failed,[0,0,0,0,0]).

process(How,Sentence) :-
  process4(How,Sentence,Answer,_Times), !, Answer\==failed.
process(normal,U) :-
   nl, nl,
   write('I don''t understand! '+U), nl,fail.
process(_,_).

eng_to_logic(U,S):- sentence80(E,U,[],[],[]), parsed_to_logic(E,S).

process4(How,Sentence,Answer,Times) :-
   Times = [ParseTime,SemTime,TimePlan,TimeAns,TotalTime],
   check_words(Sentence,U),
   runtime(StartParse),  
 ((sentence80(E,U,[],[],[]),
   notrace((runtime(StopParse),
    ParseTime is StopParse - StartParse,
    report(How,E,'Parse',ParseTime,tree),
    % !, %%%%%%%%%%%%%%%% added by JPO but breaks "london"
    runtime(StartSem))),
   must_or_rtrace(i_sentence(E,E1)),
   report(How,E1,'i_sentence',ParseTime,cmt),
   clausify(E1,E2),
   report(How,E2,'clausify',ParseTime,cmt),
   simplify(E2,E3),
   simplify(E3,S))),
   runtime(StopSem),
   SemTime is StopSem - StartSem,
   report(How,S,'Semantics',SemTime,expr),
   runtime(StartPlan),
  ((
   qplan(S,S1),
   pprint_ecp_cmt(green,S),
   runtime(StopPlan),
   TimePlan is StopPlan - StartPlan,
   (S\=@=S1->(S1R=S1,report(How,S1R,'Planning',TimePlan,expr));(_S1R=same)),
   runtime(StartAns),
   answer(S1,Answer), !,
   runtime(StopAns),
   TimeAns is StopAns - StartAns,
   TotalTime is ParseTime+SemTime+TimePlan+TimeAns,
   report(How,U,'Question',TotalTime,print_test),
   ignore((How\==test, report(always,Answer,'Reply',TimeAns,respond))))),!.
   

report(none,_,_,_,_):- !.
report(test,_,_,_,_):- !.
report(How,Item,Label,Time,Mode) :-
   ((tracing =: on); How==debug; How==always), !,
   nl, write(Label), write(': '), write(Time), write('msec.'), nl,
   \+ \+ report_item(Mode,Item),!.
report(_,_,_,_,_).

report_item(none,_).
report_item(Tree,Item):- copy_term(Item,Nat), once(report_item0(Tree,Nat)),fail.
report_item(_,_).

%report_item(_,Item):- pprint_ecp_cmt(yellow,Item),!.
report_item0(respond,Item) :- !,
   respond(Item), nl.
report_item0(print_test,Item) :- !,
   write('?- chat80_test("'),print_test(Item),write('").'), nl.
report_item0(expr,Item) :- !,
   \+ \+ write_tree(Item), nl.

%report_item0(_,Item) :- !, \+ \+ write_tree(Item), nl.

report_item0(tree,Item) :- \+ \+ print_tree80(Item),!, nl.

report_item0(cmt,Item) :-
    pprint_ecp_cmt(yellow,Item),!.
report_item0(tree,Item) :-
   print_tree(Item),!.
report_item0(P,Item) :-
   must_or_rtrace(call(P,Item)),!.


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
quote(nameOf(_)).
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
   simplify(S2,S3),
   simplify(S3,S).

reduce1(P,Q):- \+ compound(P), Q=P.
reduce1((P,Q),Q):- P ==Q,!.
reduce1(Ex^(exceeds(Value1, Ex1) & exceeds(Value2, Ex2)),exceeds(Value2, Value1)):- Ex==Ex1, Ex1==Ex2,!.
reduce1(Ex^(exceeds(Value1, Ex1), exceeds(Value2, Ex2)),exceeds(Value2, Value1)):- Ex==Ex1, Ex1==Ex2,!.
reduce1(Ex^(exceeds(X,Y),exceeds(A,B)),exceeds(X,B)):- Ex==Y, Y==A,!.
reduce1(Ex^(exceeds(A,B),exceeds(X,Y)),exceeds(X,B)):- Ex==Y, Y==A,!.
reduce1(P,Q):- compound_name_arguments(P,F,A),
   maplist(reduce1,A,AA), 
   compound_name_arguments(Q,F,AA).

simplify(C,(P:-R)) :- !,
   unequalise(C,(P:-Q)),
   simplify(Q,R,true).

simplify(setof(X,P0,S),R,R0) :- !,
   simplify(P0,P,true),
   revand(R0,setof(X,P,S),R).

simplify(P,R,R0):-
  reduce1(P,Q)-> P\==Q, !,
  simplify(Q,R,R0).

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

simplify_not(P,\+P):- var(P),!.
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

inv_map(A,_,_,A) :- \+ compound(A), !.
inv_map('$VAR'(I),V,_,X) :- !,
   arg(I,V,X).
inv_map(A=B,V,M,T) :- !,
   drop_eq(A,B,V,M,T).
inv_map(X^P0,V,M,P) :- !,
   inv_map(P0,V,M,P1),
   exquant(X,V,M,P1,P).
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
check_words(NonList,Out):-
 \+ is_list(NonList),
 into_text80_atoms(NonList,M),!,
 check_words(M,Out).
check_words([],[]).
check_words([Word1,Word2|Words],RevWords) :- atomic(Word1),atomic(Word2),atomic_list_concat([Word1,'_',Word2],Word),chk_word(Word),!,
  check_words([Word|Words],RevWords).
check_words([Word1,'_',Word2|Words],RevWords) :- atomic(Word1),atomic(Word2),atomic_list_concat([Word1,'_',Word2],Word),!,
  check_words([Word|Words],RevWords).
check_words([Word|Words],[RevWord|RevWords]) :-
   check_word(Word,RevWord),
   check_words(Words,RevWords).

%:- mode check_word(+,-).

check_word(Word,nb(Word)) :- number(Word),!.
check_word(Word,Word) :- chk_word(Word), !.
check_word(Word,Word):- compound(Word),!.
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
