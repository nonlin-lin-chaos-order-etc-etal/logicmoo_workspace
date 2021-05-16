:-module(parser_charniak, [
  test_charniak/0,
  test_charniak/1,
  test_charniak/2,
  test_charniak/1,
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

charniak_pos(Text,PosW2s):- charniak_pos(Text,PosW2s0,_Info,_LExpr),guess_pretty(PosW2s0),!,PosW2s=PosW2s0.

%can_be_partof('Obj',W):-!, member(W,['Situation','Event']).
%can_be_partof(W,W):-!,fail.
%can_be_partof('Situation','Event'):-!,fail.
can_be_partof(_,_).


marked_segs(['VP'-'Situation',
 'WHNP'-'WHNP',
 'SBARQ'-'SBARQ',
  'ADVP'-'Adv',
  'SQ'-'SQ',
  'PP'-'Prep',
  'VP'-'VPhrase',
  'ROOT'-'Root','SBAR'-'Thing','NP'-'Obj','NP-TMP'-'NP-TMP','word'-'W','S'-'Situation','S1'-'Event','NML'-'NML','ADJP'-'tCol','FRAG'-'FRAG']).
%marked_seg_type(Mark,Type):- marked_segs(S),member(Mark-Type,S).
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
create_coref(NP,MORES,Out):- atom(NP), % marked_segs(Segs),%member(NP-Type,Segs),
  %flag(NP,N,N+1), 
  flag(NP,N,N+1),
  atomic_list_concat([NP,'#',N],NPNRef),
  NPN=span([seg(start,end),size(0),lnks(0),'#'(NPNRef),/*xvar(Var),*/ txt([]),phrase(NP)/*,isa(Type)*/]),
  add_var_to_env_now(NP,Var),
  add_p_to_words(Var,NPN,MORES,Out0),
  Out=[NPN|Out0].
create_coref(NPN,MORES,Out):-add_p_to_words(_Var,NPN,MORES,Out).

%append_varname_h(_,_).
append_varname_h(X,Y):- append_varname(X,Y).


add_loc_to_span(PosL,P):- find_subterm(PosL,loc(X)), find_subterm(P,seg(SW,_),Seg),add_loc_to_span3(X,SW,Seg),!.

add_loc_to_span3(X,SW,Seg):- SW=start,nb_setarg(1,Seg,X),!,nb_setarg(2,Seg,X).
add_loc_to_span3(X,_,Seg):- nb_setarg(2,Seg,X).


:- use_module(library(editline)).
:- add_history((call(make),call(test_corenlp1))).


resize_span(P):- ignore((find_subterm(P,seg(S,E)),number(S),number(E),Z is E - S + 1,find_subterm(P,size(_),Size),nb_setarg(1,Size,Z))).

add_p_to_words(_Var,_,[],[]):- !.
add_p_to_words(Var,P,[[w(H,L)]|T],[HH|TT]):- add_p_to_word(Var,P,w(H,L),HH),add_p_to_words(Var,P,T,TT).
add_p_to_words(Var,P,[w(H,L)|T],[HH|TT]):- add_p_to_word(Var,P,w(H,L),HH),add_p_to_words(Var,P,T,TT).
add_p_to_words(Var,P,[H|T],[HH|TT]):- add_p_to_words(Var,P,H,HH),add_p_to_words(Var,P,T,TT).
add_p_to_words(Var,P,T,TT):- add_p_to_word(Var,P,T,TT),!.
add_p_to_words(Var,P,H,H):-
  pprint_ecp_cmt(yellow,add_p_to_words(Var,P,H)),
  !.

add_p_to_word(_Var,P,Child,OUT):-  
  find_subterm(P,phrase(Type)),
  find_subterm(P,'#'(ID)),
  find_subterm(P,txt(_),Txt),
  ignore(add_loc_to_span(Child,P)),
  resize_span(P),  
  %ignore((find_subterm(Child,txt(S)), append_varname_h(S,Var))),
  ignore(( \+ find_subterm(Child,link(_,Type,_ /*,Var*/)), 
      find_subterm(Child,lnks(OldN),Holder), 
      N is OldN + 1,
      nb_setarg(1,Holder,N), 
      nb_set_add(Child,link(N,Type,/*'#'*/(ID) /*,Var*/)))),
  OUT=Child,!,
  ignore((Child= w(_,_),find_subterm(Child,txt(W)), nb_set_add1(Txt,W))).
  %[Child,partOf(X,Y)]

 


%add_p_to_words(_Var,P,[Atom|T],TT):- atom(Atom),trace,add_p_to_words(_Var,P,T,TT).
%lxpr_to_segs([WORD],[POS]):- is_pos(WORD,POS),!. 

is_pos([Pos,[quote,Head]],Out):-!,is_pos([Pos,Head],Out).
is_pos(['DT',Head],w(WD,[pos(dt),loc(X),lnks(0),txt(SHead)])):- maplist(atom,[Head]),any_to_string(Head,SHead),downcase_atom(Head,WD),!,flag(word,X,X+1).
is_pos([Pos, Head],w(WD,[pos(DC),loc(X),lnks(0),txt(SHead)])):- maplist(atom,[Pos,Head]),any_to_string(Head,SHead),downcase_atom(Head,WD),downcase_atom(Pos,DC),!,flag(word,X,X+1).
is_pos([Word],Out):-!,is_pos(Word,Out).


text_to_charniak_segs(Text,Segs):-
  text_to_charniak_list(Text,LExpr),
  charniak_to_segs(LExpr,Segs),!.

text_to_charniak(Text,Sent):-
   text_to_charniak_segs(Text,Segs),!,
   charniak_segs_to_sentences(Segs,Sent),!.

charniak_segs_to_sentences(Segs,sentence(1,W2,Info)):-
  charniak_segs_to_w2(Segs,Info,W2).

text_to_charniak_list(Text,LExpr):-
  charniak_parse(Text, String),
  lxpr_to_list(String, LExpr).


lxpr_to_list(String, LExpr):- any_to_codelist(String,Codes), c2s(LExpr0,Codes,_) ,fix_c2s(LExpr0,LExpr).

c2s(L)  --> `(`, !, c2s_list(L),!.
c2s(L)  --> one_blank, !, c2s(L),!.
c2s(L)  --> c2w_chars(Chars),!,{any_to_atom(Chars,L)}.

c2w_chars([]) --> dcg_peek(one_blank;`(`;`)`),!.
c2w_chars([C|X]) --> [C],!,c2w_chars(X).

c2s_list(X) --> one_blank,!,c2s_list(X).
c2s_list([]) --> `)`, !.
c2s_list([Car|Cdr]) --> c2s(Car), !, c2s_list(Cdr),!.


baseKB:regression_test:- test_charniak(1,X),!,test_charniak(X).
baseKB:sanity_test:- make, forall(test_charniak(1,X),test_charniak(X)).
baseKB:feature_test:- test_charniak.

test_charniak1:- 
  %Txt = "Rydell used his straw to stir the foam and ice remaining at the bottom of his tall plastic cup, as though he were hoping to find a secret prize.",
  Txt = "The Norwegian dude lives happily in the first house.",
  test_charniak(Txt),
  ttyflush,writeln(('\n test_charniak1.')),!.
test_charniak2:- 
  Txt = "Rydell used his straw to stir the foam and ice remaining at the bottom of his tall plastic cup, as though he were hoping to find a secret prize.",
  %Txt = "The Norwegian dude lives happily in the first house.",
  test_charniak(Txt),
  ttyflush,writeln(('\n test_charniak2.')),!.

 test_charniak:- 
  Txt = "Rydell was a big quiet Tennessean with a sad shy grin, cheap sunglasses, and a walkie-talkie screwed permanently into one ear.",
  test_charniak(Txt),
  ttyflush,writeln(('\n test_charniak.')),!.
test_charniak:- forall(test_charniak(X),test_charniak(X)).

test_1charniak(Text):- 
  format('~N?- ~p.~n',[test_charniak(Text)]),
  charniak_pos(Text,W),
  print_tree(W),
  !.

test_charniak(N):- number(N),!, forall(test_charniak(N,X),test_1charniak(X)). 
test_charniak(X):- test_charniak(_,X),nop(lex_info(X)).

test_charniak(_,X):- nonvar(X), !, once(test_1charniak(X)).

test_charniak(1,".\nThe Norwegian lives in the first house.\n.").

test_charniak(1,"Rydell used his straw to stir the foam and ice remaining at the bottom of his tall plastic cup, as though he were hoping to find a secret prize.").


test_charniak(2,Each):- test_charniak(3,Atom),atomic_list_concat(List,'\n',Atom), member(Each,List).

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

