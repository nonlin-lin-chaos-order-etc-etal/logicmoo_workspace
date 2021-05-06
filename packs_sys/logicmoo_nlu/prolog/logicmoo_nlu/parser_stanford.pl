% ===================================================================
% File 'parser_stanford.pl'
% Purpose: English to KIF conversions from SWI-Prolog  
% This implementation is incomplete
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'parser_stanford.pl' 1.0.0
% Revision:  $Revision: 1.3 $
% Revised At:   $Date: 2002/06/06 15:43:15 $
% ===================================================================

:-module(parser_stanford,[
            call_corenlp/1, 
            call_corenlp/2, 
            call_corenlp/3,
            test_corenlp/0,
            test_corenlp1/0,
            test_corenlp2/0,
            test_corenlp/1,
            test_corenlp/2,
         into_text100_atoms/2

         ]).

:- set_module(class(library)).
:- set_module(base(system)).

baseKB:sanity_test:- test_corenlp.


:- use_module(library(http/http_client)).
%:- use_module(library(http/http_open)).
:- use_module(library(http/json_convert)).
:- use_module(library(http/http_json)).
:- use_module(library(http/json)).

text_to_corenlp(Text,LExpr):-
  call_corenlp(Text,_, LExpr).
  
call_corenlp(English):- call_corenlp(English, _Options).

call_corenlp(English, Options):-
  call_corenlp(English, Options, OutF),!,
  ttyflush, print_tree(OutF),!, ttyflush, 
  format('~N?- ~p.~n',[call_corenlp(English, Options)]),ttyflush,!.

call_corenlp(English, OptionsIn, Out):-
  DefaultOpts = [  quote, tokenize, ssplit, pos, depparse, ner, parse, coref,mwt,natlog,udfeats,
   relation,lemma, docdate, entitylink, openie, truecase, kbp, gender, cleanxml,
   %entitymentions, %sentiment,
   dcoref], ignore(OptionsIn=DefaultOpts),
  ignore('.\nSome quick brown foxes jumped over the lazy dog after we sang a song. X is Y .  Pee implies Queue.'=English),
  into_acetext(English,ACEEnglish),
  atomic_list_concat(['.\n',ACEEnglish,'\n.'], PostData),
  (OptionsIn==[]->Options=DefaultOpts;Options=OptionsIn),
  atomic_list_concat(Options, ',', OptionsStr),
  atomic_list_concat(['{"annotators":"', OptionsStr,'","outputFormat":"json"}'],For),
  uri_encoded(query_value, For, Encoded), 
  atomic_list_concat(['http://localhost:4090/?properties=',Encoded], URL),
 % wdmsg(uRL=http_post(URL, [PostData], json(Reply), [])),
  http_post(URL, [PostData], json(Reply), []),!,
  ttyflush,!,
  parse_reply([reply], Reply, Out),!.
% http://localhost:4090/stanford/?properties={%22annotators%22%3A%22quote,tokenize,ssplit,pos,lemma,depparse,natlog,coref,dcoref,nmat%22%3A%22json%22}
% http://localhost:4090/stanford/?properties={%22annotators%22%3A%22tokenize%2Cssplit%2Cpos%2Cdepparse%22%2C%22outputFormat%22%3A%22conllu%22}

is_loc_name(paragraph).
is_loc_name(index).


%parse_reply(_Ctx, InnerCtx=json(List), InnerCtx=List).
parse_reply(Ctx, InnerCtx=json(List), Out):- !,  parse_reply([InnerCtx|Ctx], List, Out).

parse_reply(Ctx, List, Out):- 
   parse_reply_sub_replace(Ctx, List, Sub, NewSub),
   % ignore((NewSub=='$',wdmsg(parse_reply_replace(_Ctx, Sub, NewSub)))),
   nonvar(Sub),nonvar(NewSub), Sub\==NewSub,
   subst(List, Sub, NewSub, NewList), 
   List\==NewList, !, 
   parse_reply(Ctx, NewList, Out).
%parse_reply(Ctx, [], Ctx=[]):- !.
parse_reply(Ctx, List, Out):- flatten([List], Mid), 
  List \== Mid, fail, !,
  parse_reply(Ctx, Mid, Out).
  
%parse_reply(Ctx, List, Out):- is_list(List),select(N=V,List,NewList), is_loc_name(N),!,parse_reply([N=V|Ctx], NewList, Out).
%parse_reply(Ctx, List, Out):- is_list(List),!, maplist(parse_reply(Ctx),List, Out).
parse_reply(_Ctx, List, Out):- flatten([List], Out),!.


label_tokens(_Index, TokensLabeled, TokensLabeled):-!.
label_tokens(Index, json(Tokens), TokensLabeled):- !, label_tokens(Index, Tokens, TokensLabeled), !.
label_tokens(Index, Tokens, TokensLabeled):- append_term(Tokens, Index, TokensLabeled), !.



% parse_reply_sub_replace(_Ctx, List, JSON, J):- compound(List), sub_term(JSON,List),compound(JSON),JSON=json(J).
parse_reply_sub_replace(Ctx, List, Sub, NewSub):-
  sub_term(Sub, List), compound(Sub),
  parse_reply_replace(Ctx, Sub, NewSub).

:- discontiguous parse_reply_replace/3.

parse_reply_replace(Ctx, List, Out):- is_list(List), once(flatten(List,Flat)),List\==Flat,!,parse_reply_replace(Ctx, Flat, Out).

unuseable_word(w('.',List)):- nonvar(List),!,member(loc(1),List).
unuseable_word(W2):- compound(W2),arg(1,W2,Atom),!,unuseable_word(Atom).
unuseable_word(Atom):- atomic(Atom),!,arg(_,v('---','162'),OneOf), atom_concat(OneOf,_,Atom).
unuseable_words(List):- member(W2,List), compound(W2), unuseable_word(W2).

parse_reply_replace(_Ctx, Sub, Replace):- is_list(Sub), 
  subtract_eq(Sub, ['$'], Replace),
  Replace\==Sub,!.

parse_reply_replace(_Ctx, List, NewList):- is_list(List),
 select(coref(N,Seg,Traits),List,NewList),
 member(sentence(N,_,Values),NewList),
 Span=span(Attrs),
 %trace,
 member(Span,Values),
 member(Seg,Attrs),
 merge_nb_values(Span,Traits),!.

parse_reply_replace(_Ctx, List, NewList):- is_list(List),
 select(coref(N,seg(X,X),Traits),List,NewList),
 member(sentence(N,Words,_Values),NewList),
 member(w(_S,Attrs),Words),
 member(loc(X),Attrs),
 merge_nb_values(Attrs,Traits),!.


parse_reply_replace(_Ctx, Sub, Replace):- is_list(Sub), 
  subtract_eq(Sub, ['$'], Replace),
  Replace\==Sub.

%parse_reply_replace(_Ctx, sentence(N,[_W2],_), '$'):- number(N).
parse_reply_replace(_Ctx, sentence(N,[W2|_],_), '$'):- number(N), arg(1,W2,'--------').

parse_reply_replace(_Ctx, sentences=W, W):-!, nonvar(W).
parse_reply_replace(_Ctx, corefs=W, W):- !,nonvar(W).
parse_reply_replace(_Ctx, Number=[Coref|More], [Coref|More]):- atom_number(Number,_), compound(Coref),functor(Coref,coref,_).
%parse_reply_replace(_Ctx, _=[Coref|More], [Coref|More]):- compound(Coref),functor(Coref,coref,_).

into_value_lex_value(A,V):- \+ atom(A) -> A=V ;  atom_to_term(A,V,Vs),maplist(call,Vs).

parse_reply_replace(_Ctx, Sub, '$'):- ground(Sub), 
  member(Sub, [entitymentions=[], speaker='PER0', openie=[], truecase='O', ner='O', entitylink='O']).

parse_reply_replace(_Ctx, NER=Value, Pred):- atom(NER),member(NER,[normalizedNER,ner,paragraph,openie,kbp,truecase,entitylink,repm]),Pred=..[NER,TValue],into_value_lex_value(Value,TValue).

parse_reply_replace(_Ctx, Remove=Rest, '$'):- nonvar(Rest),
  member(Remove, [
   basicDependencies,
   enhancedDependencies,                                      
   enhancedPlusPlusDependencies,
   entitymentions,
   sentimentTree,
   headIndex,
   truecaseText,
   position,
   %parse, 
   characterOffsetBegin, characterOffsetEnd, before, after]).
parse_reply_replace(_Ctx, isRepresentativeMention=TF, TF).

parse_reply_replace(_Ctx, _= json(SL), '$'):- member(_=json(Sub),SL), is_list(Sub), member(_ = V, Sub), atomic(V), atom_contains(V, '---'), !.
parse_reply_replace(_Ctx, _=json(Sub), '$'):- is_list(Sub), member(_ = V, Sub), atomic(V), atom_contains(V, '---'), !.


parse_reply_replace(_Ctx, Parse=Rest, Parse=SegsF):- 
  Rest\==[], atomic(Rest), atom_concat('(',_,Rest),atom_concat(_,')',Rest),  
  lxpr_to_list(Rest, LExpr),
  charniak_to_parsed(LExpr,SegsF),!.

parse_reply_replace(_Ctx, Sub, Replace):- 
 members([index=Index, originalText=String, word=String, 
    lemma=Root, pos=Pos], Sub, Attributes), !,
 my_cvt_to_real_string(String,AString),
 w_to_w2(AString,w(W,_)),
 downcase_atom(Pos,PosD),
 downcase_atom(W,WD),
 Replace = w(WD,[pos(PosD), root(Root),loc(Index), txt(AString)| Attributes]).

parse_reply_replace(_Ctx, Sub, Replace):- 
  members([index=SentID, tokens=Tokens], Sub, Attributes), !,
  maplist(label_tokens(SentID), Tokens, TokensLabeled),
  parse_reply([sentence(SentID)],Attributes,NewAttributes),
  flatten(NewAttributes,NewAttributesF),
  maybe_add_to_toks(TokensLabeled, NewAttributesF,TokensLabeledR, NewAttributesFR),
  Replace = sentence(SentID, TokensLabeledR, NewAttributesFR).

parse_reply_replace(_Ctx, Sub, Replace):- 
  members([relationSpan=[R1,R2],relation=Relation,subjectSpan=[S1,S2],subject=S,objectSpan=[O1,O2],object=O], Sub, Attributes), !,
  Replace = sro([subj(S,S1,S2),rel(Relation,R1,R2),obj(O,O1,O2)|Attributes]).

parse_reply_replace(_Ctx, Sub, Replace):- 
  members([relationSpan=RelationSpan,relation=Relation], Sub, Attributes), !,
  Replace = relation(RelationSpan,Relation,Attributes).

parse_reply_replace(_Ctx, Sub, Replace):- 
 members([id=Index, text=Text, type=Type, startIndex=SI, endIndex=EI, % headIndex=HI,
    sentNum=SentID, number=SINGULAR, gender=NEUTRAL, animacy=INANIMATE, isRepresentativeMention=TF], Sub, Attributes), !,
 into_text100_atoms(Text,Words),maplist(my_cvt_to_real_string,Words,WordStrings),
 SentIDMinus1 is SentID-1,
 SIm1 is SI-0,
 EIm1 is EI-1,
 Replace =  
  coref(SentIDMinus1, seg(SIm1,EIm1), ['#'(Index), txt(WordStrings),  
    type(Type), numb(SINGULAR), gender(NEUTRAL), animacy(INANIMATE),repm(TF)|Attributes]).       
                                                                                
parse_reply_replace(_Ctx, sentence(_,TokList,_), '$'):- ground(TokList),  
   member(tok(_, 'SYM', '--------', _, _),TokList), !.
parse_reply_replace(_Ctx, coref(_,_,TokList), '$'):- 
   unuseable_words(TokList), !.
parse_reply_replace(_Ctx, sentence(_,TokList,_), '$'):- 
   unuseable_words(TokList), !.

parse_reply_replace(_Ctx, json(Replace), Replace):- nonvar(Replace),!.

parse_reply_replace(_Ctx, sentence(N,Words,Values), sentence(N,NewWords,NValues)):-
 select(parse=Stuff,Values,NPValues),is_list(Stuff),
 ((
  %trace,
  apply:partition(\=(w(_,_)), Stuff, Info, NWords),
  %length(NWords,Len),
  merge_words(Words,NWords,NewWords),
  sort_words(Info,InfoS),
  %maplist(label_words_with_segs(NewWords),InfoS),
  append([NPValues,InfoS],NValues))).

label_words_with_segs(Words,span(Seg)):- member(seg(S,E),Seg),maplist(maybe_add_seg(S,E),Words).

maybe_add_seg(S,E,Word):- ignore((find_subterm(Word,loc(X)),between(S,E,X),nb_set_add1(Word,span(S,E)))).

merge_words([w(S,Attribs)|Words],NWords,[w(S,NAttribs)|NewWords]):-
  member(loc(X),Attribs),select(w(S,Attribs2),NWords,Rest),member(loc(X),Attribs2),
  merge_lists(Attribs,Attribs2,NAttribs),
  merge_words(Words,Rest,NewWords).
merge_words([],S,S).

merge_lists([],R,R):-!.
merge_lists(A,B,Set):-append(A,B,L),list_to_set(L,Set).


my_cvt_to_real_string(Text,Out):- any_to_string(Text,Out).

maybe_add_to_toks(TokensLabeled, NewAttributesF,TokensLabeled, NewAttributesF).
into_text100_atoms(Text,Words):- into_text80_atoms(Text,Words).

members([], List, List):-!.
members(EList, json(List), ListO):- !, members(EList, List, ListO).
members([E|EList], List, ListO):- select(E, List, ListM), !, members(EList, ListM, ListO).

sexpr_to_lexpr(SExpr, LExpr):-
  atomic_list_concat(S, '(. .)', SExpr), atomic_list_concat(S, '(. ".")', LSExpr), lisp_read(LSExpr, LExpr).

sentence_reply(Number, Toks, SExpr, In, Mid):- atomic(SExpr), sexpr_to_lexpr(SExpr, LExpr), !,
   sentence_reply(Number, Toks, LExpr, In, Mid).
sentence_reply(Number, Toks, SExpr, In, Out):- append(In, [sentence(Number)=Toks, SExpr], Out), !.
/*
sentence_reply(Number, Toks, SExpr, In, In):-
  print_reply_colored(Number=SExpr),
  print_reply_colored(Number=Toks), !.
*/
sort_words(List,Sorted):- predsort(by_word_loc,List,Sorted).
by_word_loc(R,A,B):-into_loc_sort(A,AK),into_loc_sort(B,BK),compare(RK,AK,BK), (RK == (=) -> compare(R,A,B) ; R = RK).
into_loc_sort(seg(List),Key):- member(seg(S,E),List),member(lnks(L),List),member(size(W),List),RS is 100-W, Key = seg(S,RS,L,E),!.
into_loc_sort(A,Key):- A=..[_|AA], findnsols(2,T, ((sub_term(T,AA),compound(T),arg(1,T,N),number(N));T=AA),Key).

baseKB:regression_test:- test_corenlp(1,X),!,call_corenlp(X).
baseKB:sanity_test:- make, forall(test_corenlp(1,X),call_corenlp(X)).
baseKB:feature_test:- test_corenlp.

test_corenlp1:- 
  %Txt = "Rydell used his straw to stir the foam and ice remaining at the bottom of his tall plastic cup, as though he were hoping to find a secret prize.",
  Txt = "The Norwegian dude lives happily in the first house.",
  test_corenlp(Txt),
  ttyflush,writeln(('\n test_corenlp1.')),!.
test_corenlp2:- 
  Txt = "Rydell used his straw to stir the foam and ice remaining at the bottom of his tall plastic cup, as though he were hoping to find a secret prize.",
  %Txt = "The Norwegian dude lives happily in the first house.",
  test_corenlp(Txt),
  ttyflush,writeln(('\n test_corenlp2.')),!.

 test_corenlp:- 
  Txt = "Rydell was a big quiet Tennessean with a sad shy grin, cheap sunglasses, and a walkie-talkie screwed permanently into one ear.",
  test_corenlp(Txt),
  ttyflush,writeln(('\n test_corenlp.')),!.
test_corenlp:- forall(test_corenlp(X),call_corenlp(X)).

test_corenlp(N):- number(N),!, forall(test_corenlp(N,X),call_corenlp(X)). 
test_corenlp(X):- test_corenlp(_,X),nop(lex_info(X)).

test_corenlp(_,X):- nonvar(X), !, once(call_corenlp(X)).

test_corenlp(1,".\nThe Norwegian lives in the first house.\n.").

test_corenlp(1,"Rydell used his straw to stir the foam and ice remaining at the bottom of his tall plastic cup, as though he were hoping to find a secret prize.").


test_corenlp(2,Each):- test_corenlp(3,Atom),atomic_list_concat(List,'\n',Atom), member(Each,List).

test_corenlp(3,
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


test_corenlp(3,".
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


end_of_file.



/*

(base) root@gitlab:~# cd /opt/corenlp/
(base) root@gitlab:/opt/corenlp# ls -Al
total 487312
-rwxr-xr-x 1 root root    224277 Aug 25 17:23 ant-contrib-1.0b3.jar
-rwxr-xr-x 1 root root    133599 Aug 25 17:23 appbundler-1.0.jar
-rwxr-xr-x 1 root root      4189 Aug 25 17:23 AppleJavaExtensions.jar
-rwxr-xr-x 1 root root    315805 Aug 25 17:23 commons-lang3-3.1.jar
-rwxr-xr-x 1 root root     60841 Aug 25 17:23 commons-logging.jar
-rwxr-xr-x 1 root root    185767 Aug 25 17:23 ejml-core-0.38.jar
-rwxr-xr-x 1 root root    334935 Aug 25 17:23 ejml-ddense-0.38.jar
-rwxr-xr-x 1 root root    174684 Aug 25 17:23 ejml-simple-0.38.jar
-rwxr-xr-x 1 root root    386079 Aug 25 17:23 javacc.jar
-rwxr-xr-x 1 root root  10700068 Aug 25 17:23 javanlp-core.jar
-rwxr-xr-x 1 root root     56674 Aug 25 17:23 javax.activation-api-1.2.0.jar
-rwxr-xr-x 1 root root     85147 Aug 25 17:23 javax.json.jar
-rwxr-xr-x 1 root root    243533 Aug 25 17:23 javax.servlet.jar
-rwxr-xr-x 1 root root    128032 Aug 25 17:23 jaxb-api-2.4.0-b180830.0359.jar
-rwxr-xr-x 1 root root    263966 Aug 25 17:23 jaxb-core-2.3.0.1.jar
-rwxr-xr-x 1 root root   1128097 Aug 25 17:23 jaxb-impl-2.4.0-b180830.0438.jar
-rwxr-xr-x 1 root root   1050746 Aug 25 17:23 jflex-1.6.1.jar
-rwxr-xr-x 1 root root    643043 Aug 25 17:23 joda-time.jar
-rwxr-xr-x 1 root root    213591 Aug 25 17:23 jollyday-0.4.9.jar
-rwxr-xr-x 1 root root    314932 Aug 25 17:23 junit.jar
-rwxr-xr-x 1 root root    481534 Aug 25 17:23 log4j-1.2.16.jar
-rwxr-xr-x 1 root root   1652342 Aug 25 17:23 lucene-analyzers-common-7.5.0.jar
-rwxr-xr-x 1 root root   3056210 Aug 25 17:23 lucene-core-7.5.0.jar
-rwxr-xr-x 1 root root     43962 Aug 25 17:23 lucene-demo-7.5.0.jar
-rwxr-xr-x 1 root root    381909 Aug 25 17:23 lucene-queryparser-7.5.0.jar
-rwxr-xr-x 1 root root   1635823 Aug 25 17:23 protobuf.jar
-rwxr-xr-x 1 root root     32127 Aug 25 17:23 slf4j-api.jar
-rwxr-xr-x 1 root root     10712 Aug 25 17:23 slf4j-simple.jar
-rwxr-xr-x 1 root root 474673461 Aug 25 17:23 stanford-corenlp-models-current.jar
-rwxr-xr-x 1 root root    327624 Aug 25 17:23 xom-1.3.2.jar
(base) root@gitlab:/opt/corenlp# java -mx4g -cp "*" edu.stanford.nlp.pipeline.StanfordCoreNLPServer -port 3090 -timeout 15000
[main] INFO CoreNLP - --- StanfordCoreNLPServer#main() called ---
[main] INFO CoreNLP - Server default properties:
                        (Note: unspecified annotator properties are English defaults)
                        inputFormat = text
                        outputFormat = json
                        prettyPrint = false
[main] INFO CoreNLP - Threads: 4
[main] INFO CoreNLP - Starting server...
[main] INFO CoreNLP - StanfordCoreNLPServer listening at /0.0.0.0:3090

*/

