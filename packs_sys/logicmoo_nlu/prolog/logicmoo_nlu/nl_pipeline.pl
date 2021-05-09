% ===================================================================
% File 'parser_all.pl'
% Purpose: English to KIF conversions from SWI-Prolog
% This implementation is incomplete
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'parser_all.pl' 1.0.0
% Revision:  $Revision: 1.3 $
% Revised At:   $Date: 2002/06/06 15:43:15 $
% ===================================================================

:- module(parser_all, [pipeline_file_loaded/0]).

:- use_module(library(pfc_lib)).


%:- '$set_typein_module'(baseKB).
%:- '$set_source_module'(baseKB).

% end_of_file.
% :- ensure_loaded(library(logicmoo_nlu/nl_pipeline)).


/*
% From /usr/lib/swi-prolog/library/apply_macros.pl:389
:- must(system:retract(((goal_expansion(GoalIn, PosIn, GoalOut, PosOut) :-
    apply_macros:expand_apply(GoalIn, PosIn, GoalOut, PosOut))))).
% From /usr/lib/swi-prolog/library/apply_macros.pl:386
:- must(system:retract(((goal_expansion(GoalIn, GoalOut) :-
    apply_macros:(\+ current_prolog_flag(xref, true),
    expand_apply(GoalIn, GoalOut)))))).
*/
:- use_module(library(apply_macros)).
:- (abolish(apply_macros:expand_apply, 4), assert((apply_macros:expand_apply(_In, _, _, _):- fail))).
:- (abolish(apply_macros:expand_apply, 2), assert((apply_macros:expand_apply(_In, _):- fail))).

:- use_module(library(logicmoo_utils)).

%:- use_module(library(logicmoo_lib)).

:- use_module(parser_sharing).
:- use_module(parser_tokenize).

%:- use_module(library(logicmoo_nlu)).
%:- ensure_loaded(library(wamcl_runtime)).

%:- dynamic(baseKB:installed_converter/4).
%:- rtrace.
:- shared_parser_data(baseKB:installed_converter/4).
:- export(baseKB:installed_converter/4).

:- shared_parser_data(talkdb:talk_db/2).
:- shared_parser_data(talkdb:talk_db/3).
:- shared_parser_data(talkdb:talk_db/6).

% ==============================================================================

:- volatile(t_l:disable_px/0).
:- thread_local(t_l:disable_px/0).
:- retractall(t_l:disable_px).
:- asserta(t_l:disable_px).

:- shared_parser_data(baseKB:type_action_info/3).


:- shared_parser_data(baseKB:agent_call_command/2).
:- shared_parser_data(baseKB:mud_test/2).
:- multifile(baseKB:sanity_test/0).
:- shared_parser_data(baseKB:sanity_test/0).
:- multifile(baseKB:regression_test/0).
:- shared_parser_data(baseKB:regression_test/0).
:- multifile(baseKB:feature_test/0).
:- shared_parser_data(baseKB:feature_test/0).
:- shared_parser_data(baseKB:sanity_test/1).
:- shared_parser_data(baseKB:regression_test/1).
:- shared_parser_data(baseKB:feature_test/1).



%trace_pipeline(X):- !, call(X).
trace_pipeline(_).

% ==============================================================================
%
% APE: Converter Pipeline
%   acetext, sentences, syntaxTrees, drs, drs0, sdrs, fol, pnf, (tokens),
%        sentencesToParse, paraphrase
%
% CHAT80:  acetext, text_no_punct, pos_sents_pre, parsed80, qplan
%
%  needConverter(syntaxTree, parsed80).
%
% =============================================================================


%% install_converter(+FunctorArgList).
%
%  ?- install_converter(tokens_to_paragraphs(+(tokens), -sentences:set)).
%  ?- install_converter(call_parser(+sentences:list, +(startID, 1), -syntaxtrees, -(drs0, reversed))).
%
:-meta_predicate(install_converter(*)).
:-share_mp(install_converter/1).

install_converter(M:XY):- !, install_converter(M, XY).
install_converter(XY):- strip_module(XY, M, CNV), install_converter(M, CNV).

ainz_installed_converter(M, CNVLST, Ins,Out):- ainz(installed_converter(M, CNVLST, Ins,Out)).

:-share_mp(install_converter/2).
install_converter(M, XY):- pi_splits(XY, X, Y), !, install_converter(M, X), install_converter(M, Y).
install_converter(M, XY):- pi_p(XY, PI), !, install_converter(M, PI).
install_converter(M, CNV):-
  strip_module(CNV, _OM, CNVLST),
  functor(CNVLST, F, A),  
  '@'(export(M:F/A), M),
  '@'(import(M:F/A), parser_all),
  '@'(import(M:F/A), baseKB),
  catch(system:import(M:F/A),_,true),
  %trace_pipeline(dmsg(installed_converter(M, CNVLST))),
  get_in_outs(CNVLST,Ins,Outs),
  must_maplist(ainz_installed_converter(M, CNVLST, Ins),Outs).
%install_converter(M, CNV):-strip_module(CNV, M, CNVLST), functor(CNVLST, F, A), '@'(export(M:F/A), M), must(assertz_new(installed_converter(M, CNVLST,Ins,Outs))).

get_in_outs(CNVLST,Ins,Outs):-
  findall(T,(sub_term(C,CNVLST),compound(C),(C= +(T))),Ins),
  findall(T,(sub_term(C,CNVLST),compound(C),(C= -(T))),Outs),!.

:-thread_local(tl:pipeline_pass_fail/3).

%% try_converter(+TID:key, +CNV:pred).
%
%  called by recusive code upon Transaction ID
%
try_converter(TID, CNV):-
 strip_module(CNV, M, CNVLST), CNVLST=..[F|Args], !,
  (((((
     maplist(make_io_closure(TID), Args, IOArgs, CLOSURES),
     IOCNVLST=..[F|IOArgs], !,
     warn_failure(deepen_pos(on_x_fail('@'((IOCNVLST), M)))),
     maplist(must_or_rtrace, CLOSURES), nop(flag(TID, X, X+1))))))).

warn_failure(X):- call(X)*-> true; (dmsg(failed(X)),true).
%% make_io_closure(+TID:key, +NameSpec, ?Value, -Closure).
%
% Make in-out closure on Convertor arg
%
:- export(make_io_closure/4).
make_io_closure(TID, + Name:Type, Value    , true):-!, get_pipeline_value(TID, Name:Type, Value, error), !,
  (Value\=error->true;((fail, trace_or_throw(unknown_get_pipeline_type_value(TID, Name:Type, Value))))).

make_io_closure(TID, (+ Name):Type , Value, O):- !, make_io_closure(TID, + Name:Type , Value, O).
make_io_closure(TID, + (Name:Type) , Value, O):- !, make_io_closure(TID, + Name:Type , Value, O).

make_io_closure(TID, +(Name, Else), Value, true):-!, get_pipeline_value(TID, Name, Value, Else).
make_io_closure(TID, + Name, Value    , true):- get_pipeline_value(TID, Name, Value, error), !,
  (Value\=error->true;((fail, trace_or_throw(unknown_get_pipeline_value(TID, Name, Value))))).

make_io_closure(TID, -Name:Type , Value, set_pipeline_value(TID, Name:Type, Value)):-!.

make_io_closure(TID, (- Name):Type , Value, O):- !, make_io_closure(TID, - Name:Type , Value, O).
make_io_closure(TID, - (Name:Type) , Value, O):- !, make_io_closure(TID, - Name:Type , Value, O).


make_io_closure(TID, -Name , Value, set_pipeline_value(TID, Name, Value)):-!.

make_io_closure(TID, NameType , Value, O):- trace_or_throw(make_io_closure(TID, NameType, Value, O)).

:-thread_local(tl:pipeline_value/5).

%% get_pipeline_value(+TID:key, +Name:varspec, -Value:term, +Else:term ).
%
% Get a variable in the Transaction ID or else a default
%
get_pipeline_value(TID, Name, Value, Else):-var(Name), !, trace_or_throw(var_get_pipeline_value(TID, Name, Value, Else)).
get_pipeline_value(TID, Name, Value, Else):- get_pipeline_val(TID, Name, Value, Else).

get_pipeline_val(TID, Name:list, ValueOut, Else):- findall(V, local_pipeline_value(1,TID, Name, V), Values), !, (Values==[]-> ValueOut=Else, ValueOut = Values).
get_pipeline_val(TID, Name:set, ValueOut, Else):- findall(V, local_pipeline_value(1,TID, Name, V), Values), !, (Values==[]-> ValueOut=Else, ValueOut = Values).
get_pipeline_val(TID, Name:unique, ValueOut, Else):- !, get_pipeline_val(TID, Name, ValueOut, Else).
get_pipeline_val(TID, Name:reversed, ValueOut, Else):- findall(V, local_pipeline_value(1,TID, Name, V), RBinders), reverse(RBinders, Values), !, (Values==[]-> ValueOut=Else, ValueOut = Values).
get_pipeline_val(TID, Name:reversed_set, ValueOut, Else):- findall(V, local_pipeline_value(1,TID, Name, V), RBinders), reverse(RBinders, Values), !, (Values==[]-> ValueOut=Else, ValueOut = Values).
get_pipeline_val(TID, Name:Other, Value, Else):-!, trace_or_throw(unk_get_pipeline_val(TID, Name:Other, Value, Else)).
get_pipeline_val(TID, Name, Value, _ ):- local_pipeline_value(1,TID, Name, Value), !.
get_pipeline_val(TID, (N1;Name), ValueOut, Else):- get_pipeline_val(TID, N1, Value, missing),
   (Value==missing ->  get_pipeline_val(TID, Name, ValueOut, Else) ; ValueOut= Value), !.
get_pipeline_val(TID, Name, Value, Else):- local_pipeline_value(1,TID, Name, Value) -> true;  Value=Else.
get_pipeline_val(TID, Name, Value, Else):- local_pipeline_value(1,TID, '&'(Name , _), Value) -> true;  Value=Else.

is_word_atomic(Value):-atomic(Value), !.
is_word_atomic(Value):-functor(Value, w, 2).

is_single_value(Value):- \+ is_list(Value), !.
is_single_value(Value):- is_worldlist_list(Value), !.

is_worldlist_list([Value|_]):-!, is_word_atomic(Value), !.

%% set_pipeline_value(+TID:key, +Name:varspec, +Value:term ).
%
% Set a variable in the Transaction ID
%

set_pipeline_value(ID,Name,Value):-
 %notrace
 ( nortrace, set_1pipeline_value(ID,Name,Value)).

set_1pipeline_value(_TID, _Name, Value):- var(Value) , !,
  %set_prolog_flag(no_pretty,true),
  %nop((set_prolog_flag(no_pretty,true), rtrace)), 
  must(Value=failed),!.
%set_1pipeline_value(_TID, _Name, Value):- nortrace,fail.
%set_1pipeline_value(TID, Name, Value):- \+ ground(Name), !, trace_or_throw(var_set_pipeline_value(TID, Name, Value)).
set_1pipeline_value(TID, Name:unique, V0):- !, set_unique_pipeline_value(TID, Name, V0).
set_1pipeline_value(TID, Name:set, Value):- is_single_value(Value), !, must(set_unique_pipeline_value(TID, Name, Value)).
set_1pipeline_value(TID, Name:set, Values):- must(( foreach(member_rev(V, Values), set_unique_pipeline_value(TID, Name, V)))).
set_1pipeline_value(TID, Name:list, Value):- is_single_value(Value), !, must(set_1pipeline_value(TID, Name, Value)).
set_1pipeline_value(TID, Name:list, Values):- must(( foreach(member_rev(V, Values), set_1pipeline_value(TID, Name, V)))).
set_1pipeline_value(TID, Name:reversed_set, RBinders):- reverse(RBinders, Values), set_1pipeline_value(TID, Name:set, Values).
set_1pipeline_value(TID, Name:reversed, RBinders):- reverse(RBinders, Values), set_1pipeline_value(TID, Name:list, Values).
set_1pipeline_value(TID, Name:Other, Value):-!, trace_or_throw(unknown_set_pipeline_value(TID, Name:Other, Value)).
% set_pipeline_value(TID, Name, Values):- \+ is_single_value(Values), !, must(( foreach(member_rev(V, Values), set_unique_pipeline_value(TID, Name, V)))).

set_1pipeline_value(TID, '&'(N1, Name), Value):-!, set_1pipeline_value(TID, N1, Value), set_1pipeline_value(TID, Name, Value).
set_1pipeline_value(TID, Name, V):- set_unique_pipeline_value(TID, Name, V).

member_rev(V, Values):- reverse(Values, Rev), member(V, Rev).


renumber_vars_from_0(_, V, UV):- copy_term(V, UM, _), duplicate_term(UM, UV), !.
renumber_vars_from_0(aceKif(_), V, UV):-V=UV, !.
renumber_vars_from_0(_, V, UV):- unnumbervars(V, UV). % get_ape_results:rename_vars(UV, UV). %, ape_numbervars(UV, 0, _).

renumber_vars_from_1(_, V, UV):- unnumbervars(V, UV). % get_ape_results:rename_vars(UV, UV). %, ape_numbervars(UV, 0, _).

:- rb_new(Y),nb_setval('$pipe',Y).

pipe_key(X,Key):- atom(X),!, atom_concat('$pipe_',X,Key).
pipe_key(_,_Key):- dumpST,break.

new_pipe(X):- pipe_key(X,Key),!,rb_new(Y),nb_setval(Key,Y).
rem_pipe(X):- pipe_key(X,Key),!,nb_delete(Key).

is_pipe(X,Y):- was_pipe(X),!,X=Y.
is_pipe(X,Y):- pipe_key(X,Key),!,nb_current(Key,Y).

was_pipe(X):- atom(X),!,fail.
was_pipe(X):- is_rbtree(X),!.
was_pipe(X):- is_ootree(X),!.

set_unique_pipeline_value(TID, Name, V):- 
    must_or_rtrace((pretty_numbervars(V, VarNames), renumber_vars_from_0(Name,V,V0))), 
    ignore((set_1unique_pipeline_value_or_fail(TID, Name, V, V0, VarNames)->flag(TID, OPs, 1+OPs))).

set_1unique_pipeline_value_or_fail(TID0, Name, V, V0, VarNames):-
  is_pipe(TID0,TID),!,
  (nb_rb_get_node(TID, Name, Node),(nb_rb_node_value(Node,Values))-> 
     ((member(v(_,V0,_),Values),!,fail);
      (nb_set_add1(Values,v(V, V0, VarNames))));
    nb_rb_insert(TID, Name, [v(V, V0, VarNames)])).

set_1unique_pipeline_value_or_fail(TID0, Name, V, V0, VarNames):-
  is_pipe(TID0,TID),!,
  (nb_get_value(TID, Name, Values)-> 
     ((member(v(_,V0,_),Values),!,fail);
      (nb_set_add1(Values,v(V, V0, VarNames))));
    nb_set_value(TID, Name, [v(V, V0, VarNames)])).

set_1unique_pipeline_value_or_fail(TID, Name, V, V0, VarNames):-
    \+ clause(tl:pipeline_value(TID, Name, _, V0, _), true), 
    asserta(tl:pipeline_value(TID, Name, V, V0, VarNames)).



system:ape_numbervars(DRSCopy, Zero, N):- numbervars(DRSCopy, Zero, N, [attvar(skip)]).

%% clear_pipeline(+TID:key)
%
%  Clean out the Transaction ID
%
clear_pipeline(TID0):- is_pipe(TID0,TID),!,nop(reset_oo(TID)),rem_pipe(TID0).
clear_pipeline(TID):- retractall(tl:pipeline_value(TID, _, _, _, _)), retractall(tl:pipeline_pass_fail(TID, _, _)).


%% init_pipeline(+TID:key)
%
%  Intialize the Transaction ID with defaults
%
%  when we switch to dictionaries.. we'd prebuild the keys
%
init_pipeline(TID):- new_pipe(TID), !.
init_pipeline(_ID).


:- export(set_pipeline_nvlist/2).
set_pipeline_nvlist(TID, Name=Value):- !, (nonvar(Value)-> set_pipeline_value(TID, Name, Value) ; true).
set_pipeline_nvlist(TID, Have):-
  maplist(set_pipeline_nvlist(TID),Have).

:- export(get_pipeline_nvlist/3).
get_pipeline_nvlist(N,TID, AllNameValues):-
        findall(Name=Values,
          ((no_repeats(Name, local_pipeline_value(1,TID, Name, _)),
                      findall(Value, local_pipeline_value(N, TID, Name, Value), Values))), AllNameValues).

local_pipeline_value(N,TID0, Name, Value):- is_pipe(TID0,TID),!, quietly((rb_in(Name, Values,TID),member(V3,Values),arg(N,V3,Value))).
local_pipeline_value(N,TID, Name, Value):- 
   tl:pipeline_value(TID, Name, V, V0, VarNames),arg(N,v(V, V0, VarNames),Value).


get_pipeline_value_or(Name,Pairs,Value,Else):- (member(N=V,Pairs),Name=N)->Value=V;Value=Else.

%default_pipeline_opts([aceKif(p_kif)=_, lf=_, text80=_, acetext=_, clause=_, reply_e2c=_, combined_info=_, results80=_]). 
% aceKif(p_kif)=_,
default_pipeline_opts([lf=_, clause=_, combined_info=_,  simplify80=_, results80=_, clause_e2c=_, reply_e2c=_, charniak_segs=_]).
%default_pipeline_opts([aceKif(p_kif)=_, lf=_, clause=_, combined_info=_,  simplify80=_, results80=_]).
%default_pipeline_opts([text80=_]).

%% run_pipeline( +Have:list, +NEEDs:list, -AllNameValues:list )
%
%  Run a pipeline to yeild NameValues list
%
run_pipeline(Text):- 
  default_pipeline_opts(DefaultOpts),
  run_pipeline(Text, DefaultOpts, O),  
  show_kvs(O).

ensure_pipline_spec(_Default,X=Value, [X=Value]):- nonvar(Value), !.
ensure_pipline_spec(_Default,NVPairs,Flat):- is_list(NVPairs), flatten(NVPairs,Flat),member(_=Value,Flat),nonvar(Value),!.
ensure_pipline_spec(Default,Text, [Default=Text]):-!.


fix_output_spec(X=Value, [X=Value]):- nonvar(X), !.
fix_output_spec(NVPairs,Flat):- is_list(NVPairs),!,maplist(fix_output_spec,NVPairs,Spec),flatten(Spec,Flat).
fix_output_spec(Name, [Name=_]):-!.

run_pipeline(Text, O):- var(O),
   default_pipeline_opts(DefaultOpts),
   run_pipeline(Text, DefaultOpts, O),!.
   
run_pipeline(Text, NEEDs):- nonvar(NEEDs),
  run_pipeline(Text, NEEDs, Out),!,
  show_kvs(Out).


run_pipeline(Have, NEEDs, Out):-
  (ensure_pipline_spec(input,Have, NewHave)-> Have\==NewHave),!,
  run_pipeline(NewHave, NEEDs, Out).

run_pipeline(Have, NEEDs, Out):- 
  (fix_output_spec(NEEDs, NewNEEDs)-> NEEDs\==NewNEEDs),!,
  run_pipeline(Have, NewNEEDs, Out).

run_pipeline(Have, NEEDs, Out):- member(tid=TID, Have),!,run_pipeline2(TID,Have, NEEDs, Out).
run_pipeline(Have, NEEDs, Out):- gensym(iPipeline, TID),!,run_pipeline2(TID,[tid=TID|Have], NEEDs, Out).


run_pipeline2(TID, Have, NEEDs, Out):-
    setup_call_cleanup(
      once((
         clear_pipeline(TID), init_pipeline(TID), set_pipeline_nvlist(TID, Have),         
         trace_pipeline(dmsg(start(run_pipeline_id(TID, Have, NEEDs)))),
         flag(TID,_,1),
         trace_pipeline(show_pipeline(TID)))),

      run_pipeline_id(TID, NEEDs, ExitWhy, Result),

      quietly((
         trace_pipeline(dmsg(end(run_pipeline_id(TID, ExitWhy, Result)))),
         must_or_rtrace(get_pipeline_nvlist(3, TID, AllNameValues)),
         (trace_pipeline(Result==true)->clear_pipeline(TID);true),
         %show_pipeline(TID),
         reverse(AllNameValues, RAllNameValues),
         %show_kvs(RAllNameValues),
         mapnvs(NEEDs, RAllNameValues, Out)))), !.

mapnvs(NEEDs, RAllNameValues, Out):-
   forall(member(NV, NEEDs),
     ((NV=(N=V)), member(N=V, RAllNameValues),
      nb_setarg(2, NV, V))), !, Out=NEEDs.
mapnvs(_, O, O).

show_kvs(V):- \+compound(V),!, print(V).
show_kvs(List):- is_list(List), sort_term_size(List,Set),!, maplist(show_kvs, Set).
show_kvs(O):- format('~N',[]),notrace(show_kvs0(O)),!.
show_kvs0(K=V):- !, print(K),write('='),show_kvs0(V). % print_tree_with_final(V,'.').
show_kvs0(V):- print_tree_with_final(V,'.').

:- export(sort_term_size/2).
sort_term_size(List,Sorted):- notrace((predsort(by_word_term_size,List,S),reverse(S,Sorted))).
by_word_term_size(R,A,B):-into_term_size_sort(A,AK),into_term_size_sort(B,BK),compare(RK,AK,BK), (RK == (=) -> compare(R,A,B) ; R = RK).
into_term_size_sort(seg(List),Key):- member(seg(S,E),List),member(lnks(L),List),member(size(W),List),RS is 100-W, Key = seg(S,RS,L,E),!.
into_term_size_sort(I,0):- cyclic_term(I),!.
into_term_size_sort(w(_,AA),Key):- findnsols(2,T, ((sub_term(T,AA),compound(T),arg(1,T,N),number(N));T=AA),Key),!.
into_term_size_sort(Term,Size):- into_term_size(Term,0,Size).

into_term_size(T,M,Size):- var(T), !, Size is M + 1.
into_term_size(T,M,Size):- \+ compound(T), !, Size is M + 1.
into_term_size([H|T],M,Size):-!,into_term_size(H,M,Size1),into_term_size(T,Size1,Size).
into_term_size(T,M,Size):- compound_name_arguments(T,_,Args), into_term_size(Args,M,Size1), Size is Size1 + 1.


/*
show_kvs0(V):- var(V), !, show_kvs0(var:-V).
show_kvs0(K:-V):- !, print_tree_with_final(K:-V,'.').
show_kvs0([H|List]):- !, show_kvs0(H), show_kvs0(List).
show_kvs0(K-V):- !, show_kvs0(K:-V).
show_kvs0(KV):- show_kvs0((kv:-KV)), !.
*/

%% text_pipeline( +Text:acetext, +NameValues:list )
%
%  Runs Transaction ID with acetext
%
text_pipeline(AceText, AllNameValues):-
  run_pipeline([input=AceText], [untildone=_], AllNameValues).

%% run_pipeline_id( +TID:key, +NEEDs:list )
%
%  Runs Transaction ID until NEEDs is grounded
%
run_pipeline_id(TID, NEEDs, ExitWhy, Result):-
  flag(TID, _, 1),
  trace_pipeline(show_pipeline(TID)),trace_pipeline(sleep(0.3)),
  run_pipeline_id(TID, NEEDs, ExitWhy, 0, Result).

run_pipeline_id(_TID, [] , complete , _N, true):- !.
run_pipeline_id( TID, _NEEDs, error(Name, Err), _N, fail):- local_pipeline_value(1,TID, Name, error(Err)), !.
run_pipeline_id( TID, _NEEDs, no_new_ops, _N, fail):- flag(TID, 0, 0), !.
run_pipeline_id(_TID, _NEEDs, overflow(N), N, fail):- N> 20, !.
run_pipeline_id( TID, _NEEDs, Err, _N, fail):- local_pipeline_value(1,TID, error, Err), !.
run_pipeline_id( TID, NEEDs, ExitWhy, N, Result):-
   partition(is_bound_value(TID), NEEDs, _Bound, Unbound),
   Unbound \== NEEDs, !,
   run_pipeline_id(TID, Unbound, ExitWhy, N, Result).
run_pipeline_id(TID, NEEDsIn, ExitWhy, N, Result):-
    flag(TID, _, 0),
    duplicate_term(NEEDsIn,NEEDs),
    must_or_rtrace((findall(In=Value,local_pipeline_value(1,TID, In, Value),Have),Have\==[])),
    findall(CNV,
      (converter_choice(N,TID,Have, NEEDs, CNV, 7), 
       (trace_pipeline(wdmsg(try_converter(TID, CNV))),
         ignore(try_converter(TID, CNV)))),CNVList),
    ((CNVList \== []) -> nop(flag(TID, _, 1)) ; nop(flag(TID, F, F+1))),
    N2 is N +1,!,
    run_pipeline_id(TID, NEEDs, ExitWhy, N2, Result).

is_bound_value(TID, Name=Value):- var(Value), !, local_pipeline_value(1,TID, Name, Value).
is_bound_value(_TID, _Name=Value):- !, assertion(nonvar(Value)).
is_bound_value(TID, Name):- local_pipeline_value(1,TID, Name, _Value).


currently_supplies(TID,_NEEDs,In):- local_pipeline_value(1,TID, In, _Value),!.
currently_supplies(_TID,NEEDs,In):- member(In=MaybeBound,NEEDs),nonvar(MaybeBound),!.

just_keys(Have,HKeys):- \+ compound(Have),Have=HKeys.
just_keys(Have,HKeys):- is_list(Have), !, maplist(just_keys,Have,HKeys).
just_keys(N=_,N):- nonvar(N),!.
just_keys(N,N).


converter_choice(N,TID,Have,NEEDs, MCNV, MaxDepth):- 
   trace_pipeline((just_keys(Have,HKeys),just_keys(NEEDs,NKeys),show_kvs([have=HKeys,needed=NKeys]))),
  (one_exact_converter_choice(N,TID,Have,NEEDs, MCNV, MaxDepth);%*-> true;
   transitive_converter_choice(N,TID,Have,NEEDs, MCNV, MaxDepth);%*-> true;
   any_converter_choice(N,TID,Have,NEEDs, MCNV, MaxDepth)*-> true;   
   fail),
   trace_pipeline(wdmsg(converter_choice(MCNV))).


one_exact_converter_choice(_, _TID, Have, NEEDs, M:CNV, _MaxDepth):- 
   installed_converter(M, CNV, Ins, Missing),
   \+ \+ member(Missing=_,NEEDs),
   \+ (member(Missing=F,Have),F\==failed),
   forall(member(In,Ins),member(In=_,Have)).

use_exact_converter_choice(_N,_TID,_Have,_NEEDs,_MCNV, MaxDepth):- MaxDepth<0, !, fail.
use_exact_converter_choice(N, TID,Have,NEEDs, MCNV, MaxDepth):- 
   one_exact_converter_choice(N, TID, Have, NEEDs, MCNV, MaxDepth) *-> true
   ; transitive_converter_choice(N, TID, Have, NEEDs, MCNV, MaxDepth).

transitive_converter_choice(N,TID, Have, NEEDs, MCNV, MaxDepth):- 
  findall(In=_,
   (installed_converter(_M, _CNV, Ins, Missing),
    \+ \+ member(Missing=_,NEEDs),
    \+ (member(Missing=F,Have),F\==failed),
    member(In,Ins),\+ member(In=_,Have)), NeededL),
 MaxDepthMinusOne is MaxDepth-1,
 list_to_set(NeededL,Needed), Needed \== [],
 use_exact_converter_choice(N,TID, Have, Needed, MCNV, MaxDepthMinusOne).

any_converter_choice(N,_TID,Have,_NEEDs, M:CNV, _MaxDepth):- 
   (1 is (N mod 2)), N<4,
   installed_converter(M, CNV, Ins, Missing),
   \+ (member(Missing=F,Have),F\==failed),
   forall(member(In,Ins),member(In=_,Have)).


% show stat
show_pipeline(TID):-
  wdmsg(show_pipeline(TID)),
  forall(local_pipeline_value(1,TID, Name, Value), wdmsg(local_pipeline_value(1,TID, Name, Value))),
  forall(tl:pipeline_pass_fail(TID, Name, Value), wdmsg(pipeline_pass_fail(TID, Name, Value))).

show_pipeline:-forall(installed_converter(M, CNV,_,_), wdmsg(installed_converter(M, CNV))).


maybe_display(G):- dmsg(call(writeq(G))).

:- ignore(( Z = ('/'), user:current_op(X, Y, Z), maybe_display(:-(op(X, Y, Z))), nl, fail)).
:- ignore((Z = (':'), user:current_op(X, Y, Z), maybe_display(:-(op(X, Y, Z))), nl, fail)).
:- ignore((Z = ('-'), user:current_op(X, Y, Z), maybe_display(:-(op(X, Y, Z))), nl, fail)).
:- dmsg(parser_all_start).


:- shared_parser_data(clex_iface:clex_noun/5).

:- export(load_parser_interface/1).
% load_parser_interface(File):- \+ exists_source(File), !, call(File:ensure_loaded_no_mpreds(logicmoo_nlu_ext(File))).
load_parser_interface(File):- call(File:ensure_loaded_no_mpreds(File)).
%:- parser_chat80:import(load_parser_interface/1).

% ================================================================================================
:- if(load_parser_interface(parser_e2c)).
% ================================================================================================

:- install_converter(parser_e2c, e2c_parse(+acetext, -lf_e2c)).
%:- install_converter(parser_e2c:e2c(+acetext, -lf_e2c)).
:- install_converter(parser_e2c, e2c_clausify(+lf_e2c, -clause_e2c)).
:- install_converter(parser_e2c, e2c_reply(+clause_e2c, -reply_e2c)).

%:- debug.

:- endif.

% ================================================================================================
%:- include(parser_ape).
:- if(load_parser_interface(parser_ape)).
%:- pfc_lib:load_parser_interface('AceRules/engine/run_testcases').
% ================================================================================================
:- use_module(ape(parser/ace_to_drs)).
:- use_module(ape(get_ape_results)).
:- user:import(get_ape_results:ace_to_pkif/2).
:- system:import(get_ape_results:ace_to_pkif/2).


system:my_aceparagraph_to_drs(AceText, Sentences, SyntaxTrees, UnresolvedDrsCopy, Drs, Messages):-
   ace_to_drs:aceparagraph_to_drs(AceText, on, off, 1, Sentences, SyntaxTrees, UnresolvedDrsCopy, Drs, Messages, _).

%:- install_converter(parser_tokenize:into_acetext(+input, -acetext)).
:- install_converter(parser_tokenize:into_acetext(+text80, -acetext)).
:- install_converter(parser_tokenize:into_text80(+input, -text80)).
:- install_converter(parser_tokenize:into_text80(+acetext, -text80)).
%:- install_converter(parser_tokenize:tokens_to_acetext(+(tokens), -acetext)).
%:- install_converter(tokenizer:tokenize(+input, -(tokens))).
%:- install_converter(get_ape_results:ace_to_pkif(+acetext, -aceKif(p_kif))).
%:- install_converter(ace_to_drs:call_tokenizer(+acetext, +(guess, on), -sentences:set, -sentencesToParse)).
%:- install_converter(ace_to_drs:paragraphs_to_drs(+sentences:list, +(guess, on), +(catch, off), +(startID, 1), -sentences, -syntaxTrees, -drs0, -messages, -time)).
%:- install_converter(ace_to_drs:call_parser(+sentences:list, +(startID, 1), -syntaxtrees, -drs0:reversed_set)).
:- install_converter(system:my_aceparagraph_to_drs(+acetext, -sentences_set, -syntaxTrees, -unresolvedDrs, -drs0, -messages)).
%:- install_converter(ace_to_drs:acetext_to_drs(+acetext, -sentences_set, -syntaxTrees, -drs0, -messages)).
%:- install_converter(tokens_to_sentences:tokens_to_sentences(+(tokens), -sentences:set)).
%:- install_converter(tokens_to_sentences:tokens_to_paragraphs(+(tokens), -sentences:set)).
:- install_converter(drs_fol_pnf:drs_pnf(+drs0, -fol)).
:- install_converter(drs_fol_pnf:drs_fol(+drs0, -pnf)).

:- install_converter(get_ape_results:fol_to_pkif(+pnf, -aceKif(p_kif))).
:- install_converter(get_ape_results:fol_to_pkif(+fol, -aceKif(f_kif))).
:- install_converter(get_ape_results:fol_to_pkif(+drs0, -aceKif(d_kif))).
:- install_converter(get_ape_results:fol_to_pkif(+sdrs, -aceKif(s_kif))).

:- install_converter(drs_to_ace:drs_to_ace(+drs0, -paraphrase_set)).
:- install_converter(drs_to_ace:drslist_to_ace(+drs_set, -paraphrase_set)).
:- install_converter(drs_to_drslist:drs_to_drslist(+drs0, -drs_set)).
:- install_converter(drs_to_sdrs:drs_to_sdrs(+drs, -sdrs)).
:- endif.


% ================================================================================================
% CHAT80:  acetext, text_no_punct, pos_sents_pre, parsed80, simplify80, qplan
:-  if(load_parser_interface(parser_chat80)).
% ================================================================================================

:- export(pa_domain/2).
pa_domain(Var, List):-freeze(Var, member(Var, List)).

:- export(was_punct/1).
was_punct(Remove):-
  pa_domain(WRemove, [(, ), (.), (?), (!)]),
   (pa_domain(Remove, [w(_, punc), w(WRemove, _)]);Remove=WRemove).

remove_punctuation(W2, NP):- is_list(W2), was_punct(Remove), delete(W2, Remove, W3), W2 \=@= W3, !, remove_punctuation(W3, NP).
remove_punctuation(W2, NP):- is_list(W2), !, maplist(remove_punctuation, W2, NP).
remove_punctuation(W2, NP):- atom(W2), member(P, [(, ), (.), (?), (!)]), (atom_concat(NP, P, W2);atom_concat(P, NP, W2)), !.
remove_punctuation(W2, NP):- string(W2), member(P, [(, ), (.), (?), (!)]), (string_concat(NP, P, W2);string_concat(P, NP, W2)), !.
remove_punctuation(W2, W2).

%:- install_converter(parser_all:remove_punctuation(+acetext, -acetext_no_punct)).

%:- install_converter(parser_chat80:words_to_w2(+acetext_no_punct, -pos_sents_pre)).
% :- install_converter(parser_chat80:into_text80(+(tokens), -text80)).
:- install_converter(parser_chat80:into_w2_segs(+text80, -corenlp_segs)).
:- install_converter(parser_chat80:sent_to_parsed(+corenlp_w2, -parsed80)).
:- install_converter(parser_chat80:i_sentence(+parsed80, -sent80)).
:- install_converter(parser_chat80:clausify_simplify80(+sent80, -clausify80)).
%:- install_converter(parser_chat80:simplify80(+clausify80, -simplify80)).
%:- install_converter(parser_chat80:qplan(+clausify80, -qplan80)).
:- install_converter(parser_chat80:results80(+clausify80, -results80)).

:-shared_parser_data(baseKB:partOfSpeech/3).
:-shared_parser_data(baseKB:determinerStrings/2).


:-asserta((type(SET):- call_u(tSet(SET)))).

:- endif.

% ================================================================================================
% TODO - grovel the API
:-  if(load_parser_interface(parser_charniak)).
% ================================================================================================
:- install_converter(parser_charniak:text_to_charniak(+acetext, -charniak)).
%:- install_converter(parser_charniak:charniak_segs_to_w2(+charniak_segs,-charniak_info,-charniak_w2)).
%:- install_converter(parser_charniak:charniak_segs_to_sentences(+charniak_segs,-charniak_info,-charniak_w2)).
:- install_converter(parser_charniak:charniak_to_segs(+syntaxTrees, -charniak_segs)).

:- endif.

% ================================================================================================
load_parser_stanford:-  load_parser_interface(parser_stanford).
:- if(load_parser_stanford).
% ================================================================================================

:- install_converter(parser_stanford:text_to_corenlp(+acetext, -corenlp)).
:- install_converter(parser_stanford:corenlp_to_w2(+corenlp, -corenlp_w2)).
:- install_converter(parser_stanford:corenlp_to_segs(+corenlp, -corenlp_segs)).
:- endif.

% ================================================================================================
% TODO - grovel the API
:- if(load_parser_interface(parser_lexical)).
% ================================================================================================

:- install_converter(parser_lexical:combined_w2(+charniak_w2, +corenlp_w2, -combined_w2)).
:- endif.

% ================================================================================================
% English2CycL:
:-  if((fail, load_parser_interface(parser_e2c))). % TODO confirm CHAT80 runs without E2C
% ================================================================================================

%:- debug.

:- endif.


% ================================================================================================
:-  if(load_parser_interface(parser_candc)).
% ================================================================================================

%:- debug.
%:- break.

:- endif.


% ================================================================================================
%:-  load_parser_interface(parser_chart89).
% ================================================================================================

% ================================================================================================
:-  if((false, load_parser_interface(parser_e2c))).
% ================================================================================================

eng_to_bratko(Sentence, LF, Type, Clause, FreeVars) :-
   show_call(bratko_parse(Sentence, LF, Type)),
   show_call(bratko_clausify(LF, Clause, FreeVars)), !.


:- install_converter(parser_all, eng_to_bratko(+(tokens), -lf, -type, -clause, +(freevars))).
:- install_converter(parser_bratko, bratko_parse(+(tokens), -lf, -type)).
:- install_converter(parser_bratko, bratko_clausify(+lf, -clause, -(freevars))).
:- install_converter(parser_bratko, bratko_reply(+type, +(freevars), +clause, -reply)).

%:- debug.

:- endif.





% :- get_pos_tagger(I), jpl_set(I, is_DEBUG, '@'(false)).


:- reexport(library('logicmoo/common_logic/common_logic_snark.pl')).


%% with_el_holds_enabled_4_nl( :Goal) is semidet.
%
% Using El Holds Enabled.
%
:- meta_predicate(with_el_holds_enabled_4_nl(0)).
with_el_holds_enabled_4_nl(Goal):-locally_hide(el_holds_DISABLED_KB, Goal).


:- dynamic is_cyckb_t_pred/2.
:- dynamic is_cyckb_t_pred_rename/2.

do_el_assertions :- 
   dmsg("Scanning el_assertions.pl for programatic definations (This may take 10-30 seconds)"),
%:- ain(cyckb_t(A, _, _) ==> is_cyckb_t_pred(A, 2)).
   with_el_holds_enabled_4_nl(gripe_time(10, forall(cyckb_t(A, _, _) , assert_if_new(is_cyckb_t_pred(A, 2))))),
%:- ain(cyckb_t(A, _, _, _ ) ==> is_cyckb_t_pred(A, 3)).
   with_el_holds_enabled_4_nl(gripe_time(2, forall(cyckb_t(A, _, _, _) , assert_if_new(is_cyckb_t_pred(A, 3))))),
%:- ain(cyckb_t(A, _, _, _, _ ) ==> is_cyckb_t_pred(A, 4)).
   with_el_holds_enabled_4_nl(gripe_time(2, forall(cyckb_t(A, _, _, _ , _ ) , assert_if_new(is_cyckb_t_pred(A, 4))))),
%:- ain(cyckb_t(A, _, _, _, _, _ ) ==> is_cyckb_t_pred(A, 5)).
   with_el_holds_enabled_4_nl(gripe_time(2, forall(cyckb_t(A, _, _, _ , _, _ ) , assert_if_new(is_cyckb_t_pred(A, 5))))),

  dmsg("Implementing programatic definations (This shoiuld take less than 2 seconds)"),
% :- ain((is_cyckb_t_pred(F, A) ==> {functor(H, F, A), H=..[F|ARGS], KB=..[cyckb_t, F|ARGS], assert_if_new((H:-KB))})).
  gripe_time(2, forall(is_cyckb_t_pred(F, A) , ignore((atom(F), functor(H, F, A), H=..[F|ARGS],
    KB=..[cyckb_t, F|ARGS],
       on_x_log_cont(assert_if_new((H:- \+ (t_l:el_holds_DISABLED_KB), KB))))))).

:- if(prolog_load_context(reloading,false)).
:- do_el_assertions.
:- endif.
% ================================================================================================



% ================================================================================================
% TODO Not yet started
:-  nop(load_parser_interface(parser_CURT)).
% ================================================================================================

% ================================================================================================
% TODO - grovel the API
:-  load_parser_interface(parser_regulus).
% ================================================================================================

% ================================================================================================
% TODO - grovel the API
:-  load_parser_interface(parser_SUPPLE).
% ================================================================================================

% ================================================================================================
% TODO - grovel the API
:-  load_parser_interface(parser_jpaine).
% ================================================================================================

% ================================================================================================
% TODO - grovel the API
:-  load_parser_interface(parser_SIRIDUS).
% ================================================================================================


% ================================================================================================
% TODO - grovel the API
:-  load_parser_interface(parser_ProNTo).
% ================================================================================================

:- dmsg("List of possible data transformations").


%:- dmsg(call(show_pipeline)).

:- ensure_loaded(parser_pldata).

% ================================================================================================
%:-  load_parser_interface(parser_fwd).
% ================================================================================================

% :- dmsg(parser_all_complete).


baseKB:sanity_test:- run_pipeline(input='A person who loves all animals is loved by someone.', [aceKif(p_kif)=_], O), show_kvs(O).
baseKB:sanity_test:- run_pipeline(input='All persons are happy.', [aceKif(p_kif)=_], O), wdmsg(O).
baseKB:regression_test:- run_pipeline('What are the oceans that border african countries and that border asian countries ?').
baseKB:regression_test:- run_pipeline('What is the ocean that border african countries and that border asian countries?', [qplan=_], O), wdmsg(O).
baseKB:regression_test:- run_pipeline(input='what countries are there in europe ?', [qplan=_], O), show_kvs(O).
baseKB:regression_test:- must_test_80(Tokens, _, _), run_pipeline([(tokens)=Tokens], [qplan=_], O), show_kvs(O).
baseKB:regression_test_TODO:- run_pipeline(input='A person who loves all animals is loved by someone.', [aceKif(p_kif)=_], O), show_kvs(O).
animals_test:- must_det_l((ace_to_pkif('A person who loves all animals is loved by someone.', X), kif_to_boxlog(X, BOX), portray_clause(user_error, (fol:-BOX)))).
baseKB:regression_test:- animals_test.

:- import(get_ape_results:ace_to_pkif/2).
:- baseKB:import(get_ape_results:rename_vars/2).

% som3how this next directive changes  -/1 op?
% :- animals_test.
:- op(300, fx, (-)).


baseKB:regression_test:- gripe_time(5, test_chat80_sanity).


% :- must(retract(t_l:disable_px)).

:- ensure_loaded(library(apply_macros)).


% set  -/1 op
:- op(200, fy, (-)).
:- must((current_op(P, FXY, (-)), ((arg(_, v(fy, fx), FXY), P =< 300)))).

:- ignore((Z = ('`'), user:current_op(X, Y, Z), dmsg(call((writeq(:-(op(X, Y, Z))), nl, fail))))).
% :- halt(666).

baseKB:sanity_test:- call(make),call(run_pipeline("The Norwegian dude lives happily in the first house.")).
baseKB:sanity_test:- run_pipeline("what countries are there in europe ?").
baseKB:regression_test:- run_pipeline("What countries are there in north_america ?").
baseKB:feature_test:- run_pipeline("What countries are there in europe ?").
baseKB:feature_test:- run_pipeline("What countries are there in north america ?").

/*
baseKB:feature_test(must_test_80):-
  forall(must_test_80(U, R, O),
    (ignore(\+ \+ process_run_diff(report, U, R, O)),
     ignore(\+ \+ (run_pipeline([input=U], [results80=_], OL), show_kvs(OL))))).
*/

baseKB:list_tests:- dmsg(call((
   listing(feature_test),
   listing(chat80/3),
   listing(chat80/1),
   listing(chat80/2),
   listing(test_e2c/1),
   listing(test_e2c/2),
   listing(regression_test),
   listing(sanity_test),
   !))).


%:- must_test_80.
%:- test_chat80_regressions.
:- dynamic(pipeline_file_loaded/0).

pipeline_file_loading:- 
 % show_pipeline,
 % baseKB:list_tests,
  (pipeline_file_loaded->Loaded=true;Loaded=false),
  assert_if_new(pipeline_file_loaded),!,
  (Loaded=false-> break ; true).

:- use_module(library(editline)).
:- add_history((call(make),call(pipeline_file_loading))).
:- findnsols(6,X,(chat80(X),add_history((call(make),call(run_pipeline(X))))),_).

:- fixup_exports.

% :- pipeline_file_loading.
:- initialization(pipeline_file_loading, after_load).

%:- break.

