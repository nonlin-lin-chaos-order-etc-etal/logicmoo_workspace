:-module(parser_charniak, [
  test_charniak/0,
  test_charniak_parse1/0,
  test_charniak_parse2/0,
  test_charniak/1,
  charniak_stream/2,
  charniak_pos/2,charniak_lparse/2,charniak_parse/2]).

:- set_module(class(library)).
:- set_module(base(system)).

charniak_stream(Text,Out):-
  process_create(path(bash), [('/opt/logicmoo_workspace/packs_xtra/logicmoo_pldata/bllip-parser/CharniakParse.sh'), Text ],
  [ stdout(pipe(Out))]).

charniak_parse(Text, Lines) :-
  setup_call_cleanup(
  charniak_stream(Text,Out),
  read_lines(Out, Lines),
  close(Out)).

test_charniak_parse1 :-
  Text = "Can the can do the Can Can?",
  setup_call_cleanup(
  process_create(path(bash), [('/opt/logicmoo_workspace/packs_xtra/logicmoo_pldata/bllip-parser/CharniakParse.sh'), Text ],
  [ stdout(pipe(Out))
  ]),
  read_line_to_string(Out, Lines),
  close(Out)),
  pprint_ecp_cmt(yellow,test_charniak_parse1=Lines).

test_charniak_parse2 :-
  Text = "Can the can do the Can Can?",
  charniak_parse(Text,Lines),
  pprint_ecp_cmt(yellow,test_charniak_parse2=Lines).

test_charniak_parse3 :-
  Text = "Can the can do the Can Can?",
  charniak_pos(Text,Lines),
  pprint_ecp_cmt(yellow,test_charniak_parse2=Lines).

read_lines(Out, Result) :-
  read_line_to_string(Out, StringIn),
  read_lines(StringIn, Out, Lines),
  into_result(Lines,Result).

into_result(Lines,Result):- string(Lines)=Result,!.

read_lines(end_of_file, _, "") :- !.
read_lines(StringIn, Out, AllCodes) :-  
  read_line_to_string(Out, Line2),
  read_lines(Line2, Out, Lines),
  atomics_to_string([StringIn,'\n',Lines],AllCodes).


charniak_pos(Text,PosW2s,Info+LExpr):-
  charniak_lparse(Text,LExpr),
  with_reset_segs(lxpr_to_segs(LExpr,Segs)),flatten(Segs,SegsF),
  writeq(charniak_pos=SegsF),
  apply:partition(\=(w(_,_)), SegsF, Info, PosW2s).

charniak_pos(Text,SegsF):- charniak_pos(Text,SegsF,_LExpr).

%can_be_partof('Obj',W):-!, member(W,['Situation','Event']).
%can_be_partof(W,W):-!,fail.
%can_be_partof('Situation','Event'):-!,fail.
can_be_partof(_,_).


marked_segs(['VP'-'Situation','SBAR'-'Thing','NP'-'Obj','PP'-'Thing','S'-'Situation','S1'-'Event']).
marked_seg_type(Mark,Type):- marked_segs(S),member(Mark-Type,S).
with_reset_segs(G):- marked_segs(Segs), with_reset_segs(Segs,G).
with_reset_segs([],G):-!,call(G).
with_reset_segs([NP-_Type|S],G):- setup_call_cleanup(flag(NP,Was,0),with_reset_segs(S,G), flag(NP,_,Was)).

lxpr_to_segs([],[]):- !.
lxpr_to_segs(WORD,[POS]):- is_pos(WORD,POS),!. 
lxpr_to_segs([WORD|MORE],[POS|POSS]):- is_pos(WORD,POS),lxpr_to_segs(MORE,POSS).
lxpr_to_segs([[WORD]|MORE],[POS|POSS]):- is_pos(WORD,POS),lxpr_to_segs(MORE,POSS).
lxpr_to_segs([P|MORE],Out):- atom(P),maplist(lxpr_to_segs,MORE,MORES),add_p_to_words(P,MORES,Out).
lxpr_to_segs([H|T],POS):- lxpr_to_segs(H,POSH),lxpr_to_segs(T,POST), !, append(POSH,POST,POS).

%add_p_to_words1(_,[],[]):-!.
add_p_to_words(NP,MORES,Out):- atom(NP), marked_segs(Segs), member(NP-Type,Segs),flag(NP,N,N+1), NPN=..[NP,Var],
  add_var_to_env_now(Type,Var),
  add_p_to_words1(NPN,MORES,Out0),
  Out=[isa(Var,Type)|Out0].
add_p_to_words(NPN,MORES,Out):-add_p_to_words1(NPN,MORES,Out).
%add_p_to_words1(_,[],[]):-!.
%add_p_to_words1(P,[H|T],[HH|TT]):- !, add_p_to_words1(P,H,HH),add_p_to_words1(P,T,TT).
% add_p_to_words1(P,I,O):- add_p_to_words1(P,I,O).
%add_p_to_words1(S,H,H):- member(S,['S','S1234']),!.

%add_p_to_words1(P,I,O):- compound(P),I=w(Word,_), arg(_,P,Var),var(Var),append_varname(Word,Var),!, add_p_to_words1(P,I,O).

% add_p_to_words1(P,I,O):- add_p_to_words1(P,I,O).
add_p_to_words1(P,w(S,[Pos]),w(S,[Pos,P])):-!.
% add_p_to_words1(P,isa(X,Y),w(S,[Pos,P])):-!.
add_p_to_words1(P,w(S,[Pos,P2]),w(S,[Pos,P2,P])):- P2\=='NP',!.
add_p_to_words1(P,w(S,List),w(S,List)):- last(List,P), P='VP'(_),!.
add_p_to_words1(P,w(S,Pos),w(S,PosP)):- append(Pos,[P],PosP),!.
add_p_to_words1(_,[],[]):- !.
add_p_to_words1(P,[H|T],[HH|TT]):- !, add_p_to_words1(P,H,HH),add_p_to_words1(P,T,TT).
add_p_to_words1(Cmp,isa(X,Type1),[isa(X,Type1),partOf(X,Var)]):- compound(Cmp), functor(Cmp,Mark,_),
  marked_seg_type(Mark,Type2),
  can_be_partof(Type1,Type2),
  arg(1,Cmp,Var),var(Var),!.
add_p_to_words1(_,H,H):- member(H,[partOf(_,_),isa(_,_)]),!.
add_p_to_words1(P,H,H):-
  pprint_ecp_cmt(yellow,add_p_to_words1(P,H)),
  !.
%add_p_to_words1(P,[Atom|T],TT):- atom(Atom),trace,add_p_to_words1(P,T,TT).
%lxpr_to_segs([WORD],[POS]):- is_pos(WORD,POS),!. 

is_pos([Pos,[quote,Head]],Out):-!,is_pos([Pos,Head],Out).
is_pos(['DT',Head],w(DC,[dt])):- maplist(atom,[Head]),downcase_atom(Head,DC),!.
is_pos([Pos,Head],w(Head,[DC])):- maplist(atom,[Pos,Head]),downcase_atom(Pos,DC),!.
is_pos([Word],Out):-!,is_pos(Word,Out).

term_depth(C,TD):-notrace(term_depth0(C,TD)).
term_depth0(C,1):-var(C),!.
term_depth0(C,0):-not(compound(C)),!.
term_depth0(C,TDO):-is_list(C),!,findall(D,(member(T,C),term_depth0(T,D)),DL), max_list([0|DL],TD),TDO is TD+1,!.
term_depth0(C,TDO):-C=..[_|LIST],findall(D,(member(T,LIST),term_depth0(T,D)),DL), max_list([0|DL],TD),TDO is TD+1,!.


is_sane(C):-must((term_depth(C,D),D<100)).
is_sane_nv(C):-must((nonvar(C),term_depth(C,D),D<100)).

:-meta_predicate(deepen_local_0(+,0)).
deepen_local_0(Local, Call):-
  ( \+ retract(Local) -> setup_call_cleanup(true, one_must(Call,locally(Local,Call)), ignore(retract(Local)))  ; 
     (setup_call_cleanup(true, 
       one_must(Call,locally(Local,Call)), 
        asserta(Local)))).


:- share_mp(deepen_pos/1).
:- export(deepen_pos/1).
:-meta_predicate(deepen_pos(0)).
% temp hack
deepen_pos(Call):- !, call(Call).
deepen_pos(Call):- deepen_pos_0(Call) *->  true ; locally(t_l:useAltPOS,deepen_pos_0(Call)).

:- share_mp(deepen_pos_0/1).
:-meta_predicate(deepen_pos_0(0)).
deepen_pos_0(Call):- deepen_local_0(t_l:usePlTalk,Call).

/*
deepen_pos_0(Call):-
  ( \+ retract(t_l:usePlTalk) -> setup_call_cleanup(true, one_must(Call,locally(t_l:usePlTalk,Call)), ignore(retract(t_l:usePlTalk)))  ; 
     (setup_call_cleanup(true, 
       one_must(Call,locally(t_l:usePlTalk,Call)), 
        asserta(t_l:usePlTalk)))).
*/


call_until_failed([H,(!)|T]):- !,call_until_failed([(H,!)|T]).
call_until_failed([H|T]):- !,
  call(H)*->(call_until_failed(T),!);fmt(failed(H)).
call_until_failed([]).


charniak_lparse(Text,LExpr):-
  charniak_parse(Text, String),
  lxpr_to_list(String, LExpr).

call_charniak(Text):- 
  charniak_pos(Text,LExpr,W),
  nop(pprint_ecp_cmt(green,W)),
  pprint_ecp_cmt(yellow,LExpr),!.

lxpr_to_list(String, LExpr):- any_to_codelist(String,Codes), c2s(LExpr0,Codes,_) ,fix_c2s(LExpr0,LExpr).

fix_c2s(['S1',['S'|MORE]],['S'|MORE]):-!.
fix_c2s(['S',['S1'|MORE]],['S1'|MORE]):-!.
fix_c2s(S,S):-!.

c2s(L)  --> `(`, !, c2s_list(L),!.
c2s(L)  --> one_blank, !, c2s(L),!.
c2s(L)  --> c2w_chars(Chars),!,{any_to_atom(Chars,L)}.

c2w_chars([]) --> dcg_peek(one_blank;`(`;`)`),!.
c2w_chars([C|X]) --> [C],!,c2w_chars(X).

c2s_list(X) --> one_blank,!,c2s_list(X).
c2s_list([]) --> `)`, !.
c2s_list([Car|Cdr]) --> c2s(Car), !, c2s_list(Cdr),!.


test_charniak:- forall(test_charniak(X),call_charniak(X)).

test_charniak(X):- nonvar(X),!,once(call_charniak(X)).

% lisp_read(string('(S1 (S (NP (DT The) (JJ Norwegian)) (VP (VBZ lives) (PP (IN in) (NP (DT the) (JJ first) (NN house)))) ( "." "." )))'),X)
test_charniak("The Norwegian lives in the first house.").
test_charniak("Rydell used his straw to stir the foam and ice remaining at the bottom of his tall plastic cup, as though he were hoping to find a secret prize.").
test_charniak("There are 5 houses with five different owners.").
test_charniak("These five owners drink a certain type of beverage, smoke a certain brand of cigar and keep a certain pet.").
test_charniak("No owners have the same pet, smoke the same brand of cigar or drink the same beverage.").
test_charniak("The man who smokes Blends has a neighbor who drinks water.").
test_charniak("A red cat fastly jumped onto the table which is in the kitchen of the house.").
test_charniak("After Slitscan, Laney heard about another job from Rydell, the night security man at the Chateau.").
test_charniak("Rydell was a big quiet Tennessean with a sad shy grin, cheap sunglasses, and a walkie-talkie screwed permanently into one ear.").
test_charniak("Concrete beams overhead had been hand-painted to vaguely resemble blond oak.").
test_charniak("The chairs, like the rest of the furniture in the Chateau\'s lobby, were oversized to the extent that whoever sat in them seemed built to a smaller scale.").
test_charniak("Rydell used his straw to stir the foam and ice remaining at the bottom of his tall plastic cup, as though he were hoping to find a secret prize.").
test_charniak('A book called, "A little tribute to Gibson".').
test_charniak('"You look like the cat that swallowed the canary, " he said, giving her a puzzled look.').

test_charniak("The Brit lives in the red house. The Swede keeps dogs as pets.").
test_charniak("The Dane drinks tea.").
test_charniak("The green house is on the immediate left of the white house.").
test_charniak("The green house's owner drinks coffee.").
test_charniak("The owner who smokes Pall Mall rears birds.").
test_charniak("The owner of the yellow house smokes Dunhill.").
test_charniak("The owner living in the center house drinks milk.").
test_charniak("The Norwegian lives in the first house.").
test_charniak("The owner who smokes Blends lives next to the one who keeps cats.").
test_charniak("The owner who keeps the horse lives next to the one who smokes Dunhills.").
test_charniak("The owner who smokes Bluemasters drinks beer.").
test_charniak("The German smokes Prince.").
test_charniak("The Norwegian lives next to the blue house.").
test_charniak("The owner who smokes Blends lives next to the one who drinks water.").

:- fixup_exports.
