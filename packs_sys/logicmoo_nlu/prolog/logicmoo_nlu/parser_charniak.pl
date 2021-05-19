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
  charniak_to_segs/3,
  charniak_segs_to_w2/3,
  charniak_parse/2]).

:- set_module(class(library)).
:- set_module(base(system)).
:- reexport(library(logicmoo_nlu/parser_penn_trees)).

charniak_stream(Text,Out):-
  process_create(path(bash), [('/opt/logicmoo_workspace/packs_xtra/logicmoo_pldata/bllip-parser/CharniakParse.sh'), Text ],
  [ stdout(pipe(Out))]).

charniak_parse(Text, Lines) :-
  into_acetext(Text,String),
  setup_call_cleanup(
  charniak_stream(String,Out),
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

charniak_to_segs(Start,LExpr,Sorted):- 
  with_reset_flag('word',1,with_reset_segs(Start,lxpr_to_segs(LExpr,Segs))),flatten(Segs,SegsF),parser_stanford:sort_words(SegsF,Sorted),!.

charniak_pos_info(Text,PosW2s,InfoS,LExpr):-
  text_to_charniak(Text,LExpr),
  charniak_to_segs(1,LExpr,SegsF),
  charniak_segs_to_w2(SegsF,Info,PosW2s),!,sort(Info,InfoS).
  
  %writeq(charniak_pos=SegsF),
charniak_segs_to_w2(SegsF,InfoS,PosW2s):-
    apply:partition(\=(w(_,_)), SegsF, Info, PosW2s),!,
    sort(Info,InfoS).

charniak_pos(Text,PosW2s):- charniak_pos_info(Text,PosW2s0,_Info,_LExpr),guess_pretty(PosW2s0),!,PosW2s=PosW2s0.

charniak_to_parsed(Start,A,C):- fix_c2s(A,B),!,
  charniak_to_segs(Start,B,SegsF),
  charniak_segs_to_w2(SegsF,InfoS,PosW2s),reverse(InfoS,InfoR),append(InfoR,PosW2s,C).




%add_p_to_words(_Var,_,[],[]):-!.
% create_coref('S',MORES,MORES):- !.
create_coref('ROOT',MORES,MORES):- !.
create_coref(NP,MORES,Out):- atom(NP), % marked_segs(Segs),%member(NP-Type,Segs),
  %flag(NP,N,N+1), 
  flag(NP,N,N+1),
  atomic_list_concat([NP,'@',N],NPNRef),
  NPN=span([seg(start,end),phrase(NP),size(0),lnks(0),'#'(NPNRef),txt([]),childs(0)]),
  %add_var_to_env_now(NP,Var),
  add_p_to_words(NP,NPN,MORES,Out0),
  Out=[NPN|Out0].
create_coref(NPN,MORES,Out):- trace,add_p_to_words(NPN,NPN,MORES,Out).

%append_varname_h(_,_).
append_varname_h(X,Y):- append_varname(X,Y).


add_loc_to_span(PosL,P):- find_subterm(PosL,loc(X)), find_subterm(P,seg(SW,_),Seg),add_loc_to_span3(X,SW,Seg),!.

add_loc_to_span3(X,SW,Seg):- SW=start,nb_setarg(1,Seg,X),!,nb_setarg(2,Seg,X).
add_loc_to_span3(X,_,Seg):- nb_setarg(2,Seg,X).


:- use_module(library(editline)).
:- add_history((call(make),call(test_corenlp1))).


resize_span(P):- ignore((( find_subterm(P,seg(S,E)),number(S),number(E),Z is E - S + 1,find_subterm(P,size(_),Size),nb_setarg(1,Size,Z)))).

%add_p_to_words(_Var,_,[],[]):- !.
%add_p_to_words(Var,P,[[w(H,L)]|T],[HH|TT]):- add_p_to_word(Var,P,w(H,L),HH),add_p_to_words(Var,P,T,TT).
%add_p_to_words(Var,P,[w(H,L)|T],[HH|TT]):- add_p_to_word(Var,P,w(H,L),HH),add_p_to_words(Var,P,T,TT).
add_p_to_words(Var,P,List,Out):- is_list(List),!,maplist(add_p_to_words(Var+1,P),List,Out).
%add_p_to_words(Var,P,T,TT):- add_p_to_word(Var,P,T,TT),!.
add_p_to_words(Var,P,Child,OUT):-
 must_or_rtrace(( 
  functor(Child,ChildType,_),
  nop(pprint_ecp_cmt(yellow,add_p_to_words(Var,P,Child))),
  find_subterm(P,phrase(Type)),
  find_subterm(P,'#'(ID)),
  find_subterm(P,txt(_),Txt),
  ignore(add_loc_to_span(Child,P)),
  child_loc(Child,LOC),
  resize_span(P),  
  %ignore((find_subterm(Child,txt(S)), append_varname_h(S,Var))),
  ignore(((             
       %ChildType == span,       
       (Var=Atom+1+1,atom(Atom)),
       find_subterm(P,childs(Chldrn),ChldrnHolder), 
       (find_subterm(Child,phrase(ChildPhrase));(fail,find_subterm(Child,pos(ChildPhrase)))),
       (find_subterm(Child,'#'(ChildID));ChildID=LOC),       
       NChldrn is Chldrn + 1, 
       nb_setarg(1,ChldrnHolder,NChldrn),       
       nb_set_add(P,child(NChldrn,ChildPhrase,ChildID)),
       !))),  

  ignore(((
      %\+ find_subterm(Child,link(_,Type,_,_)), 
         find_subterm(Child,lnks(OldN),Holder),          
         LinkNum is OldN + 1,
         nb_setarg(1,Holder,LinkNum),          
         nb_set_add(Child,link(LinkNum,Type,ID))))),

  OUT=Child,!,
  ignore(((ChildType=w,find_subterm(Child,txt(W)), nb_set_add1(Txt,W)))))).
  %[Child,partOf(X,Y)]

 
child_loc(Child,LOC):- find_subterm(Child,loc(_),LOC),!.
child_loc(Child,LOC):- find_subterm(Child,seg(_,_),LOC),!.

%add_p_to_words(_Var,P,[Atom|T],TT):- atom(Atom),trace,add_p_to_words(_Var,P,T,TT).
%lxpr_to_segs([WORD],[POS]):- is_pos(WORD,POS),!. 

is_pos([Pos,[quote,Head]],Out):-!,is_pos([Pos,Head],Out).
is_pos([Pos, Head],     w(WD,[pos(DC),loc(X),lnks(0),txt(SHead)])):- cancle_pos(Pos), maplist(atom,[Pos,Head]),any_to_string(Head,SHead),downcase_atom(Head,WD),downcase_atom(Pos,DC),!,flag('word',X,X+1).
is_pos([Pos, Head],[Pos,w(WD,[pos(DC),loc(X),lnks(0),txt(SHead)])]):- maplist(atom,[Pos,Head]),any_to_string(Head,SHead),downcase_atom(Head,WD),downcase_atom(Pos,DC),!,flag('word',X,X+1).
is_pos([Word],Out):-!,is_pos(Word,Out).

sort_words(List,Sorted):- predsort(by_word_loc,List,Sorted).
:- export(sort_words/2).
:- system:import(sort_words/2).

by_word_loc(R,A,B):-into_loc_sort(A,AK),into_loc_sort(B,BK),compare(RK,AK,BK), (RK == (=) -> compare(R,A,B) ; R = RK).
into_loc_sort(w(_,List),Key):- member(loc(S),List), member(lnks(L),List), Key = [a,S,0,S,L],!.
into_loc_sort(span(List),Key):- member(seg(S,E),List),once(member(lnks(L),List);L=10),once(member(size(W),List);W=0),
  RS is 100-W, % E1 is E-1, 
  Key = [span, W, E,S,RS,L|List],!.
into_loc_sort(span(List),Key):- member(seg(S,E),List),
   once(member(lnks(L),List);L=0),once(member(childs(C),List);C=0),once(member(size(W),List);W=0),
   NW is 100-W, NL is 100-L, NC is 100-C, % E1 is E-1,
   Key = [span,NW,C,S,E,NL,NC,List],!.
into_loc_sort(span(L1),Key):- member(List,L1),member(seg(_,_),List),into_loc_sort(span(List),Key).
into_loc_sort(A,Key):- A=..[_|AA], findnsols(4,T, ((sub_term(T,AA),compound(T),arg(1,T,N),number(N));T=AA),Key).


text_to_charniak_segs_legacy(Text,SSegs):-
  text_to_charniak_list(Text,LExpr),
  charniak_to_segs(1,LExpr,CSegs),!,
  parser_stanford:corenlp_to_segs(CSegs,Segs),!,
  parser_stanford:sort_words(Segs,SSegs),!.

text_to_charniak_segs(Text,SSegs):-
  text_to_best_tree(Text,LExpr),
  charniak_to_segs(1,LExpr,CSegs),!,
  parser_stanford:corenlp_to_segs(CSegs,Segs),!,
  parser_stanford:sort_words(Segs,SSegs),!.

text_to_charniak(Text,Sent):-
   text_to_charniak_segs(Text,Segs),!,
   charniak_segs_to_sentences(Segs,Sent),!.

charniak_segs_to_sentences(Segs,sentence(1,W2,Info)):-
  charniak_segs_to_w2(Segs,Info,W2).

text_to_charniak_list(Text,LExpr):-
  charniak_parse(Text, String),
  lxpr_to_list(String, LExpr),
  nop(print_tree(charniak=LExpr)).



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

