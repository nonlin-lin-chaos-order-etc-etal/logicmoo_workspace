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
        wdmsg(test_charniak_parse1=Lines).

test_charniak_parse2 :-
  Text = "Can the can do the Can Can?",
        charniak_parse(Text,Lines),
        wdmsg(test_charniak_parse2=Lines).

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

charniak_pos(Text,(POS-Segs)):-
  charniak_lparse(Text,LExpr),
  sexpr_to_pos(LExpr,POS),!,
  sexpr_to_segs(LExpr,Segs).

sexpr_to_segs([],[]):- !.
sexpr_to_segs(WORD,[POS]):- is_pos(WORD,POS),!. 
sexpr_to_segs([WORD|MORE],[POS|POSS]):- is_pos(WORD,POS),sexpr_to_segs(MORE,POSS).
sexpr_to_segs([[WORD]|MORE],[POS|POSS]):- is_pos(WORD,POS),sexpr_to_segs(MORE,POSS).
sexpr_to_segs([P|MORE],Out):- atom(P),maplist(sexpr_to_segs,MORE,MORES),add_p_to_words(P,MORES,Out).
sexpr_to_segs([H|T],POS):- sexpr_to_segs(H,POSH),sexpr_to_segs(T,POST), !, append(POSH,POST,POS).

%add_p_to_words(_,[],[]):-!.
add_p_to_words(P,[H|T],[HH|TT]):-add_p_to_words(P,H,HH),add_p_to_words(P,T,TT).
add_p_to_words(P,w(S,Pos),w(S,PosP)):- append(Pos,P,PosP).
add_p_to_words(_,H,H):-!.
%sexpr_to_segs([WORD],[POS]):- is_pos(WORD,POS),!. 

sexpr_to_pos(In,Out):- sexpr_to_pos1(In,Mid),sexpr_to_pos2(Mid,Out),!.
is_pos([Pos,[quote,Head]],Out):-!,is_pos([Pos,Head],Out).
is_pos([Pos,Head],w(Head,[Pos])):- maplist(atom,[Pos,Head]),!.
is_pos([Word],Out):-!,is_pos(Word,Out).

sexpr_to_pos1([],[]):- !.
sexpr_to_pos1(WORD,[POS]):- is_pos(WORD,POS),!. 
sexpr_to_pos1([WORD|MORE],[POS|POSS]):- is_pos(WORD,POS),sexpr_to_pos1(MORE,POSS).
sexpr_to_pos1([H|T],POS):- sexpr_to_pos1(H,POSH),sexpr_to_pos1(T,POST), !, append(POSH,POST,POS).
sexpr_to_pos1(POS,[POS]).

sexpr_to_pos2([],[]).
%sexpr_to_pos2([w(S,Pos),'POS',w(S2,P2)|Mid],[w(S,[pos(S2,P2)|Pos])|Out]):- !,sexpr_to_pos2(Mid,Out).
sexpr_to_pos2([w(S,Pos)|Mid],[w(S,Pos)|Out]):- !,sexpr_to_pos2(Mid,Out).
sexpr_to_pos2([Atom,w(S,Pos)|Mid],[w(S,AP)|Out]):- atom(Atom),sexpr_to_pos2(Mid,Out),append(Pos,[(Atom)],AP).
sexpr_to_pos2([_|Mid],Out):- sexpr_to_pos2(Mid,Out).
%sexpr_to_pos2(Mid,Out):- !,exclude(atomic,Mid,Out).

charniak_lparse(Text,LExpr):-
  charniak_parse(Text, String),
  lisp_read(String, LExpr).

call_charniak(Text):- 
  charniak_pos(Text,LExpr),
  wdmsg(charniak=LExpr),!.


test_charniak:- forall(test_charniak(X),call_charniak(X)).

test_charniak(X):- nonvar(X),once(call_charniak(X)).

test_charniak("The Norwegian lives in the first house.").
test_charniak("Rydell used his straw to stir the foam and ice remaining at the bottom of his tall plastic cup, as though he were hoping to find a secret prize.").
test_charniak(
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


test_charniak(
".
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


