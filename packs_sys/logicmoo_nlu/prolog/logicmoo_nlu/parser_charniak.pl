:-module(parser_charniak, [
  test_charniak/0,
  test_charniak/1,
  test_charniak/2,
  call_charniak/1,
  test_charniak_parse1/0,
  test_charniak_parse2/0,  
  charniak_stream/2,
  charniak_pos/2,
  text_to_charniak/2,
  charniak_to_segs/2,
  charniak_segs_to_w2/3,
  charniak_parse/2]).

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

charniak_to_segs(LExpr,SegsF):- with_reset_segs(lxpr_to_segs(LExpr,Segs)),flatten(Segs,SegsF),!.

charniak_pos(Text,PosW2s,InfoS,LExpr):-
  text_to_charniak(Text,LExpr),
  charniak_to_segs(LExpr,SegsF),
  charniak_segs_to_w2(SegsF,Info,PosW2s),!,sort(Info,InfoS).
  
  %writeq(charniak_pos=SegsF),
charniak_segs_to_w2(SegsF,InfoS,PosW2s):-
    apply:partition(\=(w(_,_)), SegsF, Info, PosW2s),!,
    sort(Info,InfoS).

charniak_pos(Text,PosW2s):- charniak_pos(Text,PosW2s0,_Info,_LExpr),guess_pretty1(PosW2s0),!,PosW2s=PosW2s0.

%can_be_partof('Obj',W):-!, member(W,['Situation','Event']).
%can_be_partof(W,W):-!,fail.
%can_be_partof('Situation','Event'):-!,fail.
can_be_partof(_,_).


marked_segs(['VP'-'Situation',
  %'ADVP'-'Adv',
  'PP'-'Prep',
  'VP'-'VPhrase',
  'ROOT'-'Root','SBAR'-'Thing','NP'-'Obj','word'-'W','S'-'Situation','S1'-'Event']).
marked_seg_type(Mark,Type):- marked_segs(S),member(Mark-Type,S).
with_reset_segs(G):- marked_segs(Segs), with_reset_segs(Segs,G).
with_reset_segs([],G):-!,call(G).
with_reset_segs([NP-_Type|S],G):- setup_call_cleanup(flag(NP,Was,1),with_reset_segs(S,G), flag(NP,_,Was)).

charniak_to_parsed(A,C):- fix_c2s(A,B),!,charniak_to_segs(B,SegsF),
  charniak_segs_to_w2(SegsF,InfoS,PosW2s),reverse(InfoS,InfoR),append(InfoR,PosW2s,C).




fix_c2s(A,D):- with_reset_segs(fix_c2s0(A,D)).
fix_c2s0(A,D):- fix_c2s1(A,B),fix_c2s1(B,D).
fix_c2s1(A,D):- fix_tree_ses(A,B),fix_tree_w2s(B,C),fix_tree_vps(C,D),!.

fix_tree_ses(['S1',['S'|MORE]],OUT):- fix_tree_ses(['S'|MORE],OUT).
fix_tree_ses(['S',['S1'|MORE]],OUT):- fix_tree_ses(['S1'|MORE],OUT).
fix_tree_ses(['S',MORE],OUT):- !, fix_tree_ses(MORE,OUT).
fix_tree_ses(LIST,OUT):- is_list(LIST), maplist(fix_tree_ses,LIST,OUT),!.
fix_tree_ses(S,S):-!.

fix_tree_w2s(WORD,POS):- is_pos(WORD,POS),!. 
fix_tree_w2s(LIST,OUT):- is_list(LIST), maplist(fix_tree_w2s,LIST,OUT),!.
fix_tree_w2s(I,I).

fix_tree_vps([S|MORE],[S|OUT]):- atom(S),fix_tree_pp(MORE,OUT),!.
fix_tree_vps(LIST,OUT):- is_list(LIST), maplist(fix_tree_vps,LIST,OUT),!.
fix_tree_vps(I,I).
%fix_tree_pp([[H]|MORE],OUT):- fix_tree_pp([H|MORE],OUT),!.
fix_tree_pp([['PP',w(S,List)|PPMORE]|MORE],OUT):-  fix_tree_pp([w(S,['PP'|List])|PPMORE],PPOUT),fix_tree_pp(MORE,MMORE),append(PPOUT,MMORE,OUT).
fix_tree_pp(['PP',w(S,List)|PPMORE],[w(S,['PP'|List])|OUT]):- fix_tree_pp(PPMORE,OUT).
% fix_tree_pp([['ADVP',w(S,List)|PPMORE]|MORE],OUT):-  fix_tree_pp([w(S,['ADVP'|List])|PPMORE],PPOUT),fix_tree_pp(MORE,MMORE),append(PPOUT,MMORE,OUT).
fix_tree_pp([S|MORE],[S|OUT]):- atom(S),fix_tree_pp(MORE,OUT),!.
fix_tree_pp([H|MORE],[H|OUT]):- fix_tree_pp(MORE,OUT),!.
fix_tree_pp(I,I).
%fix_tree_pp([['ADVP',w(S,List)|PPMORE]|MORE],OUT):-  fix_tree_pp(['ADVP',w(S,List)|PPMORE],PPOUT),fix_tree_pp(MORE,MMORE),append(PPOUT,MMORE,OUT).

%fix_tree_pp(['PP',w(S,List)]|MORE],OUT):- fix_tree_pp([w(S,['PP'|List])|MORE],OUT).
%fix_tree_pp(['ADVP',w(S,List)]|MORE],OUT):- fix_tree_pp([w(S,['ADVP'|List])|MORE],OUT).


lxpr_to_segs([],[]):- !.

lxpr_to_segs(WORD,[POS]):- is_pos(WORD,POS),!. 
lxpr_to_segs([WORD|MORE],[POS|POSS]):- is_pos(WORD,POS),lxpr_to_segs(MORE,POSS).
lxpr_to_segs([[WORD]|MORE],[POS|POSS]):- is_pos(WORD,POS),lxpr_to_segs(MORE,POSS).
lxpr_to_segs([P|MORE],Out):- atom(P),maplist(lxpr_to_segs,MORE,MORES),create_coref(P,MORES,Out).
lxpr_to_segs([H|T],POS):- lxpr_to_segs(H,POSH),lxpr_to_segs(T,POST), !, append(POSH,POST,POS).
lxpr_to_segs(I,I).

%add_p_to_words(_Var,_,[],[]):-!.
% create_coref('S',MORES,MORES):- !.
create_coref('ROOT',MORES,MORES):- !.
create_coref(NP,MORES,Out):- atom(NP), marked_segs(Segs),member(NP-Type,Segs),
  %flag(NP,N,N+1), 
  %flag('ROOT',N,N+1), 
  NPN=span(sentNum,seg(start,end),Var,[NP,Type]),
  add_var_to_env_now(Type,Var),
  add_p_to_words(Var,NPN,MORES,Out0),
  Out=[NPN|Out0].
create_coref(NPN,MORES,Out):-add_p_to_words(_Var,NPN,MORES,Out).

%append_varname_h(_,_).
append_varname_h(X,Y):- append_varname(X,Y).

add_loc_to_span(PosL,P):- ignore(((member(loc(X),PosL),find_seg(P,Seg),Seg=seg(SW,_),add_loc_to_seg(X,SW,Seg)))),!.
add_loc_to_seg(X,SW,Seg):- SW=start,nb_setarg(1,Seg,X),!,nb_setarg(2,Seg,X).
add_loc_to_seg(X, _,Seg):- nb_setarg(2,Seg,X).

find_seg(P,Seg):- compound(P), sub_term(Seg,P),compound(Seg),Seg=seg(_,_).

resize_span(P):- ignore((find_seg(P,seg(S,E)),number(S),number(E),Size is E - S + 1, nb_setarg(1,P,Size))).

span_var_type(SPAN2,X,Type):- compound(SPAN2), SPAN2 = span(_,_,X,[Type|_]).

add_p_to_words(_Var,_,[],[]):- !.
add_p_to_words(Var,P,[[w(H,L)]|T],[HH|TT]):- add_p_to_word(Var,P,w(H,L),HH),add_p_to_words(Var,P,T,TT).
add_p_to_words(Var,P,[w(H,L)|T],[HH|TT]):- add_p_to_word(Var,P,w(H,L),HH),add_p_to_words(Var,P,T,TT).
add_p_to_words(Var,P,[H|T],[HH|TT]):- add_p_to_words(Var,P,H,HH),add_p_to_words(Var,P,T,TT).
add_p_to_words(Var,P,T,TT):- add_p_to_word(Var,P,T,TT),!.
add_p_to_words(Var,P,H,H):-
  pprint_ecp_cmt(yellow,add_p_to_words(Var,P,H)),
  !.

add_p_to_word(_OVar,SPAN2,SPAN1,OUT):-
  span_var_type(SPAN1,X,Type1),
  span_var_type(SPAN2,Y,Type2),
  can_be_partof(Type1,Type2),
  nop(X\==Y),
  OUT=SPAN1,!.
  %[SPAN1,partOf(X,Y)]

add_p_to_word(_Var,_,H,H):- member(H,[partOf(_,_),span(_,_,_,_)]),!.
add_p_to_word(Var,P,w(S,PosL),w(S,PosPO)):- 
 must_or_rtrace((
  add_loc_to_span(PosL,P),
  (compound(P)->(arg(1,P,SN),arg(3,P,CV),arg(4,P,Traits));(SN= (#),Traits=[P],CV=cv)),  
  resize_span(P),
  %(SN==sentNum->(flag('ROOT',C,C+1),setarg(1,P,#(C)));true),
   ( \+ member(traits(1,_,_),PosL) ->  append(PosL,[traits(1,Var,Traits)],PosP) ; append(PosL,[],PosP)),
  (( \+ member(traits(2,_,_),PosL), member(traits(1,_,_),PosL)) ->  append(PosP,[traits(2,Var,Traits)],PosPO0) ; append(PosP,[],PosPO0)),
  (( \+ member(traits(3,_,_),PosL), member(traits(2,_,_),PosL)) ->  append(PosPO0,[traits(3,Var,Traits)],PosPO) ; append(PosPO0,[],PosPO)),
  append_varname_h(S,Var))),!.



%add_p_to_words(_Var,P,[Atom|T],TT):- atom(Atom),trace,add_p_to_words(_Var,P,T,TT).
%lxpr_to_segs([WORD],[POS]):- is_pos(WORD,POS),!. 

is_pos([Pos,[quote,Head]],Out):-!,is_pos([Pos,Head],Out).
is_pos(['DT',Head],w(WD,[pos(dt),loc(X)])):- maplist(atom,[Head]),downcase_atom(Head,WD),!,flag(word,X,X+1).
is_pos([Pos, Head],w(WD,[pos(DC),loc(X)])):- maplist(atom,[Pos,Head]),downcase_atom(Head,WD),downcase_atom(Pos,DC),!,flag(word,X,X+1).
is_pos([Word],Out):-!,is_pos(Word,Out).


text_to_charniak(Text,LExpr):-
  charniak_parse(Text, String),
  lxpr_to_list(String, LExpr).

call_charniak(Text):- 
  charniak_pos(Text,W),
  nop(pprint_ecp_cmt(green,W)),
  %pprint_ecp_cmt(yellow,LExpr),
  !.

lxpr_to_list(String, LExpr):- any_to_codelist(String,Codes), c2s(LExpr0,Codes,_) ,fix_c2s(LExpr0,LExpr).

c2s(L)  --> `(`, !, c2s_list(L),!.
c2s(L)  --> one_blank, !, c2s(L),!.
c2s(L)  --> c2w_chars(Chars),!,{any_to_atom(Chars,L)}.

c2w_chars([]) --> dcg_peek(one_blank;`(`;`)`),!.
c2w_chars([C|X]) --> [C],!,c2w_chars(X).

c2s_list(X) --> one_blank,!,c2s_list(X).
c2s_list([]) --> `)`, !.
c2s_list([Car|Cdr]) --> c2s(Car), !, c2s_list(Cdr),!.


baseKB:regression_test:- test_charniak(1,X),!,call_charniak(X).
baseKB:sanity_test:- make, forall(test_charniak(1,X),call_charniak(X)).
baseKB:feature_test:- test_charniak.

test_charniak:- forall(test_charniak(X),call_charniak(X)).

test_charniak(N):- number(N),!, forall(test_charniak(N,X),call_charniak(X)). 
test_charniak(X):- test_charniak(_,X).

test_charniak(_,X):- nonvar(X),!, once(call_charniak(X)).

test_charniak(1,".\nThe Norwegian lives in the first house.\n.").

test_charniak(1,"Rydell used his straw to stir the foam and ice remaining at the bottom of his tall plastic cup, as though he were hoping to find a secret prize.").

                                
test_charniak(2,Each):- test_charniak(3,Atom),atomic_list_concat(List,'\n',Atom), member(Each,List).

% lisp_read(string('(S1 (S (NP (DT The) (JJ Norwegian)) (VP (VBZ lives) (PP (IN in) (NP (DT the) (JJ first) (NN house)))) ( "." "." )))'),X)
test_charniak(2,'A book called, "A little tribute to Gibson".').
test_charniak(2,'"You look like the cat that swallowed the canary, " he said, giving her a puzzled look.').

test_charniak(3,
'There are 5 houses with five different owners.
 These five owners drink a certain type of beverage, smoke a certain brand of cigar and keep a certain pet.
 No owners have the same pet, smoke the same brand of cigar or drink the same beverage.
 The man who smokes Blends has a neighbor who drinks water.
 A red cat fastly jumped onto the table which is in the kitchen of the house.
 After Slitscan, Laney heard about another job from Rydell, the night security man at the Chateau.
 Rydell was a big quiet Tennessean with a sad shy grin, cheap sunglasses, and a walkie-talkie screwed permanently into one ear.
 Concrete beams overhead had been hand-painted to vaguely resemble blond oak.
 The chairs, like the rest of the furniture in the Chateau\'s lobby, were oversized to the extent that whoever sat in them seemed built to a smaller scale.
 Rydell used his straw to stir the foam and ice remaining at the bottom of his tall plastic cup, as though he were hoping to find a secret prize.
 A book called, "A little tribute to Gibson".
 "You look like the cat that swallowed the canary, " he said, giving her a puzzled look.').


test_charniak(3,".
The Brit lives in the red house.
The Swede keeps dogs as pets.
The Dane drinks tea.
The green house is on the immediate left of the white house.
The green house's owner drinks coffee.
The owner who smokes Pall Mall rears birds.
The owner of the yellow house smokes Dunhill.
The owner living in the center house drinks milk.
The Norwegian lives in the first house.
The owner who smokes Blends lives next to the one who keeps cats.
The owner who keeps the horse lives next to the one who smokes Dunhills.
The owner who smokes Bluemasters drinks beer.
The German smokes Prince.
The Norwegian lives next to the blue house.
The owner who smokes Blends lives next to the one who drinks water.").





:- fixup_exports.

