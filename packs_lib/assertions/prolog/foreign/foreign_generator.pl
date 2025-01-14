/*  Part of Assertion Reader for SWI-Prolog

    Author:        Edison Mera Menendez
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/assertions
    Copyright (C): 2017, Process Design Center, Breda, The Netherlands.
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(foreign_generator, [generate_library/5,
                              collect_prop/4,
                              gen_foreign_library/3]).

:- use_module(library(apply)).
:- use_module(library(filesex)).
:- use_module(library(assertions)).
:- use_module(library(atomics_atom)).
:- use_module(library(call_ref)).
:- use_module(library(camel_snake)).
:- use_module(library(extend_args)).
:- use_module(library(extra_messages)).
:- use_module(library(foldnl)).
:- use_module(library(foreign/foreign_props)).
:- use_module(library(key_value)).
:- use_module(library(lists)).
:- use_module(library(metaprops)).
:- use_module(library(neck)).
:- use_module(library(process)).
:- use_module(library(solution_sequences)).
:- use_module(library(substitute)).
:- use_module(library(transpose)).
:- use_module(library(pairs)).

:- multifile
    foreign_dependency/2,
    gen_foreign_library/3,
    use_foreign_source/2,
    use_foreign_header/2,
    include_foreign_dir/2,
    library_foreign_dir/2,
    extra_compiler_opts/2,
    link_foreign_library/2,
    pkg_foreign_config/2.

:- discontiguous
    match_type//6,
    implement_type_unifier//3.

:- dynamic
    foreign_dependency/2,
    gen_foreign_library/3,
    use_foreign_source/2,
    use_foreign_header/2,
    include_foreign_dir/2,
    extra_compiler_opts/2,
    link_foreign_library/2,
    pkg_foreign_config/2.

% :- table
%     type_props/5.

:- meta_predicate current_foreign_prop(1,?,?,?,?, ?,?,?,?,?, ?,?,?,?,?).

% user:prolog_exception_hook(ExceptionIn, ExceptionOut, Frame, CatcherFrame) :-
%     backtrace(40).


% Predefined foreign dependencies:

foreign_dependency(M, HAlias) :- use_foreign_header(M, HAlias).
foreign_dependency(_, library('foreign/foreign_interface.h')).
foreign_dependency(_, library('foreign/foreign_swipl.h')).

command_to_atom(Command, Args, Atom) :-
    process_create(path(Command), Args, [stdout(pipe(Out))]),
    read_stream_to_codes(Out, String),
    string_to_atom(String, Atom).

language_command(for, M, path(gfortran), ValueL, ValueT) :-
    command_to_atom(swipl, ['--dump-runtime-variables'], Atom),
    atomic_list_concat(AtomL, ';\n', Atom),
    findall(Value,
            ( ( member(NameValue, AtomL),
                member(NameEq, ['PLCFLAGS="', 'PLLDFLAGS="']),
                atomics_atom([NameEq, Values, '"'], NameValue)
              ; extra_compiler_opts(M, Values)
              ),
              atomic_args(Values, ValueL1),
              member(Value, ValueL1)
            ),
            ValueL, ValueT).
language_command(c, M, path('swipl-ld'), ValueL, ValueT) :-
    findall(COpt, ( COpt = '-shared'
                  ; ( extra_compiler_opts(M, COpts)
                    ; pkg_foreign_config(M, Package),
                      command_to_atom('pkg-config', ['--cflags', Package], COpt1),
                      atom_concat(COpts, '\n', COpt1)
                    ),
                    atomic_args(COpts, COptL1),
                    member(COpt, COptL1)
                  ), ValueL, ValueT).

intermediate_obj(M, DirSO, OptL, LibL, Source, Object) -->
    { file_name_extension(Base, Ext, Source),
      file_base_name(Base, Name),
      ( Ext = for,
        memberchk(gfortran, LibL)
      ->true
      ; Ext = c
      ),
      intermediate_obj_cmd(Ext, Name, M, DirSO, OptL, Source, Object, Command)
    },
    !,
    ( {is_newer(Object, Source)}
    ->[]
    ; [Ext-Command]
    ).
intermediate_obj(_, _, _, _, Source, Source) --> [].

intermediate_obj_cmd(Ext, Name, M, DirSO, OptL, Source, Object, Compiler-Args) :-
    % Add a preffix to avoid problems with other files with the same base
    atomic_list_concat([Name, '_', Ext], NameFor),
    file_name_extension(NameFor, o, NameO),
    directory_file_path(DirSO, NameO, Object),
    append([OptL, ['-c', Source, '-o', Object]], FOptL),
    language_command(Ext, M, Compiler, Args, FOptL).

is_newer(File1, File2) :-
    exists_file(File1),
    exists_file(File2),
    time_file(File1, Time1),
    time_file(File2, Time2),
    Time1 > Time2.

generate_library(M, AliasSO, AliasSOPl, InitL, File) :-
    absolute_file_name(AliasSO, FileSO, [file_type(executable),
                                         relative_to(File)]),
    findall(FSource, ( ( use_foreign_source(M, FAlias)
                       ; FAlias = library('foreign/foreign_interface.c')
                       ; FAlias = library('foreign/foreign_swipl.c')
                       ),
                       absolute_file_name(FAlias, FSource,
                                          [extensions(['.c', '']),
                                           access(read),
                                           relative_to(File)])
                     ), FSourceL),
    ( forall(( Dep = File
             ; member(Alias, [library(foreign/foreign_generator),
                              library(foreign/foreign_props),
                              library(foreign/foreign_interface)
                             ]),
               absolute_file_name(Alias, Dep, [file_type(prolog),
                                               access(read),
                                               relative_to(File)])),
             is_newer(FileSO, Dep))
    ->print_message(informational,
                    format('Skipping generation of ~w interface: is up to date', [File])),
      compile_library(M, FileSO, File, FSourceL)
    ; do_generate_library(M, FileSO, File, InitL),
      do_generate_wrapper(M, AliasSO, AliasSOPl, File),
      do_compile_library(M, FileSO, File, FSourceL)
    ).

compile_library(M, FileSO, File, FSourceL) :-
    ( forall(( member(Dep, FSourceL)
             ; foreign_dependency(M, HAlias),
               absolute_file_name(HAlias, Dep,
                                  [extensions(['.h','']),
                                   access(read),
                                   relative_to(File)])
             ),
             is_newer(FileSO, Dep))
    ->print_message(informational,
                    format('Skipping compilation of ~w: is up to date', [FileSO]))
    ; do_compile_library(M, FileSO, File, FSourceL)
    ).

% Beyond MaxFLIArgs arguments we should pack foreign arguments due to a
% hard-coded limitation of SWI-Prolog:
max_fli_args(10 ).

do_generate_wrapper(M, AliasSO, AliasSOPl, File) :-
    max_fli_args(MaxFLIArgs),
    neck,
    findall(F/A, ( current_foreign_prop(_, Head, M, _, _, _, _, _, _, _, _, _, _),
                   \+ ( predicate_property(M:Head, number_of_clauses(X)),
                        X>0
                      ),
                   functor(Head, F, A)
                 ), IntfPIU),
    sort(IntfPIU, IntfPIL),
    atom_concat(M, '$impl', IModule),
    absolute_file_name(AliasSOPl, FileSOPl, [file_type(prolog),
                                             relative_to(File)]),
    save_to_file(FileSOPl,
                 phrase(( add_autogen_note(M),
                          [(:- module(IModule, IntfPIL))],
                          generate_aux_clauses(M),
                          ['',
                           (:- use_foreign_library(AliasSO)),
                           % make these symbols public:
                           (:- shlib:current_library(AliasSO, _, F1, IModule, _),
                               open_shared_object(F1, _Handle, [global]))],
                          findall((Head :- Body),
                                  ( member(F/A, IntfPIL),
                                    A > MaxFLIArgs,
                                    atomic_list_concat(['__aux_pfa_', F, '_', A], NF),
                                    functor(Head, F, A),
                                    Body =.. [NF, Head]
                                  ))
                        ))).

atomic_args(String, ArgL) :-
    atomic_list_concat(ArgL1, ' ', String),
    subtract(ArgL1, [''], ArgL).

do_generate_library(M, FileSO, File, InitL) :-
    file_name_extension(BaseFile, _, FileSO),
    generate_foreign_interface(M, File, InitL, BaseFile).


do_compile_library(M, FileSO, File, FSourceL) :-
    file_name_extension(BaseFile, _, FileSO),
    absolute_file_name(library(foreign/foreign_interface),
                       IntfPl,
                       [file_type(prolog), access(read), relative_to(File)]),
    directory_file_path(DirIntf, _, IntfPl),
    directory_file_path(DirSO,   _, FileSO),
    atom_concat(BaseFile, '_intf.c', IntfFile),
    findall(IDir, ( ( Dir = DirSO
                    ; Dir = DirIntf
                    ; include_foreign_dir(M, DAlias),
                      absolute_file_name(DAlias, Dir, [file_type(directory),
                                                       relative_to(File)])
                    ),
                    atom_concat('-I', Dir, IDir)
                  ), IDirL),
    CommonOptL = ['-fPIC'|IDirL],
    foldl(intermediate_obj(M, DirSO, CommonOptL, LibL), [IntfFile|FSourceL], FTargetL, ExtCommands, []),
    once(append(LibL, [], _)),
    findall(COpt, ( COpt = '-shared'
                  ; ( extra_compiler_opts(M, COpts)
                    ; pkg_foreign_config(M, Package),
                      command_to_atom('pkg-config', ['--cflags', Package], COpt1),
                      atom_concat(COpts, '\n', COpt1)
                    ),
                    atomic_args(COpts, COptL1),
                    member(COpt, COptL1)
                  ), COptL),
    findall(CLib, ( ( link_foreign_library(M, Lib)
                    ; member(Lib, LibL)
                    ),
                    atom_concat('-l', Lib, CLib)
                  ; pkg_foreign_config(M, Package),
                    command_to_atom('pkg-config', ['--libs', Package], CLib1),
                    atom_concat(CLibs, '\n', CLib1),
                    atomic_args(CLibs, CLibL1),
                    member(CLib, CLibL1)
                  ), CLibL, ['-o', FileSO]),
    findall(LDir, ( library_foreign_dir(M, DAlias),
                    absolute_file_name(DAlias, Dir, [file_type(directory),
                                                     relative_to(File)]),
                    atom_concat('-L', Dir, LDir)
                  ),
            LDirL),
    append([COptL, CommonOptL, LDirL, FTargetL, CLibL], FArgsL),
    keysort(ExtCommands, Sorted),
    group_pairs_by_key(Sorted, Grouped),
    concurrent_maplist(compile_1, Grouped),
    compile_2(path('swipl-ld')-FArgsL).

compile_1(Ext-Commands) :- compile_1(Ext, Commands).

% Note: Due to the presence of Fortran modules, the compilation of Fortran can
% not be parallelized, since Prolog is not aware of Fortran dependencies, so we
% compile such modules serialized
compile_1(for, Commands) :-            maplist(compile_2, Commands).
compile_1(c,   Commands) :- concurrent_maplist(compile_2, Commands).

compile_2(Command-ArgL) :-
    process_create(Command, ArgL, [stdout(pipe(Out)),
                                   stderr(pipe(Err))]),
    read_string(Err, _, SErr),
    read_string(Out, _, SOut),
    close(Err),
    command_to_string(Command, ArgL, CommandS),
    catch(call_cleanup(
              close(Out),
              ( SOut = "",
                SErr = ""
              ->print_message(informational, format('~s', [CommandS]))
              ; print_message(warning, format("~s~s~nCommand: ~s", [SOut, SErr, CommandS]))
              )),
          Error,
          print_message(error, Error)).

command_to_string(Command, ArgL, CommandS) :-
    ( Command = path(RCommand)
    ->true
    ; RCommand = Command
    ),
    atomic_list_concat([RCommand|ArgL], ' ', CommandS).

:- meta_predicate with_output_to_file(+,0 ).

with_output_to_file(File, Goal) :- setup_call_cleanup(tell(File), Goal, told).

write_lines([]) :- !.
write_lines([E|L]) :- !,
    write_lines(E),
    write_lines(L).
write_lines(Line) :-
    write_line(Line).

write_line(Line) :-
    ( nonvar(Line),
      do_write_line_2(Line)
    ->true
    ; writeln(Line)
    ).

write_line_1(Line) :-
    ( nonvar(Line),
      do_write_line_1(Line)
    ->true
    ; write(Line)
    ).

do_write_line_1(F-A) :-
    format(F, A).
do_write_line_1(A+B) :-
    write_line_1(A),
    write_line_1(B).
do_write_line_1(A/S) :-
    maplist(line_atom, [S|A], [C|L]),
    atomic_list_concat(L, C, V),
    write(V).

line_atom(Line, Atom) :- with_output_to(atom(Atom), write_line_1(Line)).

do_write_line_2((:- A))    :- portray_clause((:- A)).
do_write_line_2((A :- B))  :- portray_clause((A :- B)).
do_write_line_2((A --> B)) :- portray_clause((A --> B)).
do_write_line_2(Line) :- write_line_1(Line), nl.

:- meta_predicate save_to_file(+,2).

save_to_file(File, Goal) :-
    call(Goal, Lines, []),
    with_output_to_file(File, write_lines(Lines)).

generate_foreign_interface(Module, FilePl, IntL, BaseFile) :-
    atom_concat(BaseFile, '_impl', BaseFileImpl),
    file_name_extension(BaseFileImpl, h, FileImpl_h),
    atom_concat(BaseFile, '_intf', BaseFileIntf),
    file_name_extension(BaseFileIntf, h, FileIntf_h),
    file_name_extension(BaseFileIntf, c, FileIntf_c),
    directory_file_path(_, Base, BaseFile),
    save_to_file(FileImpl_h, generate_foreign_impl_h(Module)),
    save_to_file(FileIntf_h, generate_foreign_intf_h(Module, FileImpl_h)),
    save_to_file(FileIntf_c, generate_foreign_c(Module, Base, IntL, FilePl, FileIntf_h)).

c_var_name(Arg, "_c_"+Arg).

generate_foreign_intf_h(Module, FileImpl_h) -->
    add_autogen_note(Module),
    ["#ifndef __"+Module+"_INTF_H",
     "#define __"+Module+"_INTF_H",
     '',
     '',
     '#include <foreign_swipl.h>',
     "#include \""+FileImpl_h+"\"",
     '',
     "extern module_t __"+Module+"_impl;"],
    findall_tp(Module, type_props_nft, declare_type_getter_unifier),
    findall('extern '+Decl+';',
            ( current_foreign_prop(_, Head, _, Module, _, _, _, _, Dict, _, _, BindName, _, Type),
              apply_dict(Head, Dict),
              declare_intf_head(Type, BindName, Head, Decl)
            )),
    ['',
     "#endif /* __"+Module+"_INTF_H */"].

declare_intf_head(fimport(_), BindName, _, Decl) :-
    !,
    declare_intf_fimp_head(BindName, Decl).
declare_intf_head(fimport(_, _), BindName, _, Decl) :-
    !,
    declare_intf_fimp_head(BindName, Decl).
declare_intf_head(_, BindName, Head, Decl) :-
    declare_intf_head(BindName, Head, Decl).

declare_intf_fimp_head(BindName, "predicate_t "+BindName+"").

generate_foreign_impl_h(Module) -->
    add_autogen_note(Module),
    ["#ifndef __"+Module+"_IMPL_H",
     "#define __"+Module+"_IMPL_H",
     '',
     '#include <foreign_interface.h>'],
    findall_tp(Module, type_props_nf, declare_struct),
    declare_foreign_bind(Module),
    ["#endif /* __"+Module+"_IMPL_H */"].

add_autogen_note(Module) -->
    ["/* NOTE: File generated automatically from "+Module+" */",
     ''].

generate_foreign_c(Module, Base, InitL, FilePl, FileIntf_h) -->
    add_autogen_note(Module),
    findall("#include \""+File_h+"\"",
            ( use_foreign_header(Module, HAlias),
              absolute_file_name(HAlias, File_h, [extensions(['.h', '']),
                                                  access(read),
                                                  relative_to(FilePl)])
            )),
    ["#include \""+FileIntf_h+"\"",
     '',
     "module_t __"+Module+";",
     "module_t __"+Module+"_impl;"
    ],
    findall_tp(Module, type_props_nft, implement_type_getter),
    findall_tp(Module, type_props_nft, implement_type_unifier),
    generate_foreign_register(Module, Base, InitL),
    generate_foreign_intf(Module).

generate_foreign_register(Module, Base, InitL) -->
    ["install_t install_"+Base+"() {",
     '    __system_dict_create        =PL_predicate("dict_create", 3, "system");',
     '    __system_get_dict           =PL_predicate("get_dict",    3, "system");',
     '    __system_put_dict           =PL_predicate("put_dict",    4, "system");',
     '    __foreign_generator_call_idx=PL_predicate("call_idx",    2, "foreign_generator");',
     '    __foreign_generator_idx_call=PL_predicate("idx_call",    2, "foreign_generator");',
     "    __"+Module+"     =PL_new_module(PL_new_atom(\""+Module+"\"));",
     "    __"+Module+"_impl=PL_new_module(PL_new_atom(\""+Module+"$impl\"));"],
    findall_tp(Module, type_props_nft, define_aux_variables),
    findall(Line,
            ( current_foreign_prop(_, _, M, Module, _, _, _, _, _, _, PredName, BindName, Arity, Type),
              write_register_sentence(Type, M, PredName, Arity, BindName, Line))),
    foldl(generate_init, InitL),
    ["} /* install_"+Base+" */",
    ''].

generate_init(Init) --> ["    "+Init+";"].

write_register_sentence(fimport(_),    M, PredName, Arity, BindName, Line) :- !,
    write_init_fimport_binding(M, PredName, Arity, BindName, Line).
write_register_sentence(fimport(_, _), M, PredName, Arity, BindName, Line) :- !,
    write_init_fimport_binding(M, PredName, Arity, BindName, Line).
write_register_sentence(_, _, PredName, Arity, BindName,
                        "    PL_register_foreign(\""+PredName+"\", "+Arity+", "+BindName+", 0);") :-
    max_fli_args(MaxFLIArgs),
    neck,
    Arity =< MaxFLIArgs.
write_register_sentence(_, _, PredName, Arity, BindName,
                        "    PL_register_foreign(\"__aux_pfa_"+PredName+"_"+Arity
                        +"\", 1, __aux_pfa_"+BindName+"_"+Arity+", 0);") :-
    max_fli_args(MaxFLIArgs),
    neck,
    Arity > MaxFLIArgs.

write_init_fimport_binding(M, PN, A, BN,
                           "    "+BN+" = PL_predicate(\""+PN+"\", "+A+", \""+M+"\");").

:- meta_predicate findall_tp(+,5,5,?,?).

findall_tp(Module, TypeProps, Call) -->
    findall(List,
            ( call(TypeProps, Module, _, TypePropLDictL, Pos, _Asr),
              maplist(apply_dict_tp, TypePropLDictL),
              phrase(type_components(Module, TypePropLDictL, Call, Pos), List)
            )).

apply_dict_tp(_-TypePropLDictL) :- maplist(apply_dict_tp_2, TypePropLDictL).

apply_dict_tp_2(t(Type, PropL, Dict)) :- apply_dict(Type-PropL, Dict).

type_props(M, Type, TypePropLDictL, Pos, Asr) :-
    type_props_(M, Type, TDict, Pos, Asr),
    type_props2(M, Type, TDict, TypePropLDictL, Asr).

type_props2(M, Type, TDict, TypePropLDictL, Asr) :-
    collect_prop(Asr, M, comp, TPropL),
    ( TPropL \= []
    ->TypePropLDictL1 = [t(Type, TPropL, TDict)]
    ; bind_type_names(M:Type, TypePropLDictL1)
    ->TypePropLDictL1 = TypePropLDictL1
    ; TypePropLDictL1 = [t(Type, [], TDict)]
    ),
    phrase(foldl(auto_generated_types(M), TypePropLDictL1, TypePropLDictL2),
           TypePropLDictL, [Type-TypePropLDictL2]).

auto_generated_types(M, t(Type, PropL, Dict), t(Type, PropS, Dict)) -->
    { functor(Type, Name, _),
      foldl(match_unknown_type(M, Name), PropL, PropTypeL1, []),
      foldl(cleanup_redundant(Type, PropL), PropTypeL1, PropTypeL, []),
      substitute_values(PropTypeL, PropL, PropS)
    },
    foldl(add_dict(Dict), PropTypeL).

cleanup_redundant(Type, PropL, Prop=SubType) -->
    ( { functor(Type, _, A),
        arg(A, Type, Arg),
        functor(SubType, _, SA),
        arg(SA, SubType, SubArg),
        Arg==SubArg,
        PropL==[Prop]
      }
    ->[]
    ; [Prop=SubType]
    ).

add_dict(Dict, Prop=Type) --> [Type-[t(Type, [Prop], Dict)]].

match_unknown_type(M, Name, Prop) --> match_type(Prop, M, unknown, Name, _, _), !.

type_props_(CM, Type, Dict, Pos, Asr) :-
    % We use asr_head_prop instead of prop_asr since the auto-generated types
    % should be only those defined in the current module, but not others that
    % could be imported in CM --EMM
    % prop_asr(Type, CM, check, prop, Dict, Pos, Asr),
    asr_head_prop(Asr, CM, Type, check, prop, Dict, Pos),
    once(prop_asr(glob, type(_), _, Asr)).

type_props_nf(Module, Type, TypePropLDictL, Pos, Asr) :-
    type_props(Module, Type, TypePropLDictL, Pos, Asr),
    \+ prop_asr(glob, foreign(_, _), _, Asr),
    \+ prop_asr(glob, native(_, _), _, Asr).

type_props_nft(Module, Type, TypePropLDictL, Pos, Asr) :-
    type_props_nf(Module, Type, TypePropLDictL, Pos, Asr),
    % Don't create getters and unifiers for
    % typedefs, they are just casts:
    \+ type_is_tdef(Module, Type, _, _).

define_aux_variables(dict_ini(_, Name, M, _), _, _) -->
    !,
    ["    __rtcwarn((__"+M+"_aux_keyid_index_"+Name+"=PL_pred(PL_new_functor(PL_new_atom(\"__aux_keyid_index_"+Name+"\"), 2), __"+M+"_impl))!=NULL);"].
define_aux_variables(dict_key_value(_, _, _, _), _, _) --> !, {fail}.
define_aux_variables(_, _, _) --> [].

implement_type_getter_ini(PName, CName, Spec, Name) -->
    { ctype_decl(Spec, Decl1),
      ( Spec = array(_, _)
      ->Decl = Decl1
      ; Decl = Decl1+"*"
      )
    },
    ["int FI_get_"+Name+"(root_t __root, term_t "+PName+", "+Decl+" "+CName+") {"].

c_get_argument_getter(Spec, CNameArg, PNameArg, GetArg) :-
    c_get_argument(Spec, in, CNameArg, PNameArg, GetArg).

implement_type_getter_union_ini_join(SubType, Spec, Term, Name, UType) -->
    { term_pcname(Term, Name, PName, CName),
      cname_utype(SubType, CName, UType1),
      ( \+ref_type(Spec)
      ->UType = "*"+UType1
      ; UType = UType1
      ),
      functor(Term, Func, _),
      '$current_source_module'(CM)
    },
    implement_type_getter_ini(PName, CName, Spec, Name),
    ['    term_t __args = PL_new_term_refs(2);',
     '    int __utype;',
     "    __rtcheck(PL_unify_functor(__args, PL_new_functor(PL_new_atom(\""+Func+"\"), 1)));",
     "    __rtcheck(PL_unify_arg(1, __args, "+PName+"));",
     "    __rtcheck(__rtctype(PL_call_predicate(__"+CM+", PL_Q_NORMAL,",
     '                                          __foreign_generator_call_idx, __args),',
     "                        __args, "+Name+"));",
     '    __rtcheck(PL_get_integer(__args + 1, &__utype));',
     "    "+UType+"=__utype;"
    ].

implement_type_getter_union_ini(union, Spec, Term, Name) -->
    implement_type_getter_union_ini_join(union, Spec, Term, Name, UType),
    ["    switch ("+UType+") {"].
implement_type_getter_union_ini(cdef,   _, _, _) --> [].
implement_type_getter_union_ini(struct, _, _, _) --> [].
implement_type_getter_union_ini(enum, Spec, Term, Name) -->
    implement_type_getter_union_ini_join(enum, Spec, Term, Name, _).

implement_type_getter_union_end(union) -->
    ['    default:',
     '        return FALSE;',
     '    };'],
    implement_type_end.
implement_type_getter_union_end(cdef  ) --> [].
implement_type_getter_union_end(struct) --> implement_type_end.
implement_type_getter_union_end(enum  ) --> implement_type_end.

implement_type_getter(union_ini(SubType, Spec, _), Term, Name) -->
    implement_type_getter_union_ini(SubType, Spec, Term, Name).
implement_type_getter(union_end(SubType), _, _) -->
    implement_type_getter_union_end(SubType).
implement_type_getter(func_ini(SubType, Spec), Term, Name) -->
    ( {SubType = union}
    ->{functor(Term, TName, _)},
      ["    case "+Name+"_"+TName+":",
       '    {']
    ; {func_pcname(Name, PName, CName)},
      implement_type_getter_ini(PName, CName, Spec, Name)
    ).
implement_type_getter(func_rec(SubType, N, Term, Name), Spec, Arg) -->
    { SubType = union
    ->functor(Term, TName, _),
      format(atom(CRecordName), '~w.~w', [TName, Arg]),
      format(atom(TNameArg), '~w_~w', [TName, Arg]),
      camel_snake(PRecordName, TNameArg),
      Indent = '        '
    ; CRecordName = Arg,
      camel_snake(PRecordName, Arg),
      Indent = '    '
    },
    { func_pcname(Name, PName, CName),
      CNameArg="&"+CName+"->"+CRecordName+"",
      PNameArg=PName+"_"+PRecordName
    },
    [Indent+'term_t '+PNameArg+'=PL_new_term_ref();',
     Indent+'__rtcheck(PL_get_arg('+N+','+PName+','+PNameArg+'));'],
    {c_get_argument_getter(Spec, CNameArg, PNameArg, GetArg)},
    [Indent+GetArg+';'].
implement_type_getter(func_end(SubType), _, _) -->
    ( {SubType = union}
    ->['        break;',
       '    }']
    ; []
    ).
implement_type_getter(atomic(SubType, Name), Spec, Term) -->
    {functor(Term, TName, _)},
    ( {SubType = union}
    ->{ func_pcname(Name, PName, CName1),
        CName = CName1+"->"+TName,
        Indent = '        '
      },
      ["    case "+Name+"_"+TName+":"]
    ; { func_pcname(Name, PName, CName),
        Indent = '    '
      },
      implement_type_getter_ini(PName, CName, Spec, Name)
    ),
    {c_get_argument_getter(Spec, CName, PName, GetArg)},
    [Indent+GetArg+';'],
    ( {SubType = union}
    ->[Indent+'break;']
    ; []
    ).
implement_type_getter(dict_ini(SubType, Name, M, _), Spec, Term) -->
    ( {SubType = union}
    ->{functor(Term, TName, _)},
      ["    case "+Name+"_"+TName+":",
       '    {']
    ; ["predicate_t __"+M+"_aux_keyid_index_"+Name+";"],
      {term_pcname(Term, Name, PName, CName)},
      %% TBD: This will fail for structures with dict_t
      implement_type_getter_dict_ini(M, PName, CName, Spec, Name)
    ).
implement_type_getter(dict_key_value(Dict, _, N, _), Key, Value) -->
    {key_value_from_dict(Dict, N, Key, Value)}.
implement_type_getter(dict_rec(SubType, _, Term, N, Name), Spec, Arg) -->
    { ( SubType = union
      ->functor(Term, TName, _),
        format(atom(CRecordName), '~w.~w', [TName, Arg]),
        Indent = '        '
      ; CRecordName = Arg,
        Indent = '    '
      ),
      term_pcname(Term, Name, PName, CName),
      CNameArg = "&"+CName+"->"+CRecordName,
      c_get_argument_getter(Spec, CNameArg, PName, GetArg)
    },
    [Indent+'    case '+N+': '+GetArg+'; break;'].
implement_type_getter(dict_end(SubType, _, _), _, _) -->
    ['        }'],
    ( {SubType = union}
    ->['        break;',
       '    }']
    ; []
    ).

implement_type_getter_dict_ini(Module, PName, CName, Spec, Name) -->
    {ctype_decl(Spec, Decl)},
    ["static int get_pair_"+Name+"(root_t __root, term_t __keyid, term_t "+PName+", "+Decl+"* "+CName+");",
     ''],
    implement_type_getter_ini(PName, CName, Spec, Name),
    ["    memset("+CName+", 0, sizeof("+Decl+"));",
     "    FI_get_dict_t("+Name+", "+PName+", "+CName+");"
    ],
    implement_type_end,
    ["static int get_pair_"+Name+"(root_t __root, term_t __keyid, term_t "+PName+", "+Decl+"* "+CName+") {",
     '    int __index;',
     "    FI_get_keyid_index(__"+Module+"_aux_keyid_index_"+Name
     +", __keyid, __index);",
     '    switch (__index) {'].

implement_type_end -->
    ['    return TRUE;',
     '}',
     ''].

term_pcname(Term, NameL, Name) :-
    ( compound(Term)
    ->functor(Term, Func, _)
    ; Func = Term
    ),
    ( valid_csym(Func)
    ->Name = Func
    ; Name = NameL
    ).

term_pcname(Term, NameL, PName, CName) :-
    term_pcname(Term, NameL, Name),
    func_pcname(Name, PName, CName).

func_pcname(NameL, PName, CName) :-
    ( is_list(NameL)
    ->atomic_list_concat(NameL, Name)
    ; Name = NameL
    ),
    camel_snake(PName, Name),
    c_var_name(Name, CName).

type_char(Type, Char) :- char_type(Char, Type).

valid_csym(Func) :-
    atom_codes(Func, Codes),
    maplist(type_char(csym), Codes).

implement_type_unifier(atomic(SubType, Name), Spec, Term) -->
    {functor(Term, TName, _)},
    ( {SubType = union}
    ->{ func_pcname(Name, PName, CName1),
        CName = CName1+"->"+TName,
        Indent = '        '
      },
      ["    case "+Name+"_"+TName+":"]
    ; { func_pcname(Name, PName, CName),
        Indent = '    '
      },
      implement_type_unifier_ini(PName, CName, Name, Spec)
    ),
    { ( SubType = union
      ->Mode = inout
      ; Mode = out
      ),
      c_set_argument(Spec, Mode, CName, PName, SetArg)
    },
    [Indent+SetArg+';'],
    ( {SubType = union}
    ->[Indent+'break;']
    ; []
    ).
implement_type_unifier(union_ini(SubType, Spec, _), Term, Name) -->
    implement_type_unifier_union_ini(SubType, Spec, Term, Name).

cname_utype(union, CName, CName+"->utype").
cname_utype(enum,  CName, CName).

implement_type_unifier_union_ini_join(SubType, Spec, Term, Name, UType) -->
    { term_pcname(Term, Name, PName, CName),
      cname_utype(SubType, CName, UType),
      functor(Term, Func, _),
      '$current_source_module'(CM)
    },
    implement_type_unifier_ini(PName, CName, Name, Spec),
    ["    term_t __args = PL_new_term_refs(2);",
     "    __rtcheck(PL_put_integer(__args, "+UType+"));",
     "    __rtcheck(PL_unify_functor(__args + 1, PL_new_functor(PL_new_atom(\""+Func+"\"), 1)));",
     "    __rtcheck(PL_unify_arg(1, __args + 1, "+PName+"));",
     "    __rtcheck(__rtctype(PL_call_predicate(__"+CM+", PL_Q_NORMAL,",
     '                                          __foreign_generator_idx_call, __args),',
     "                        __args, "+Name+"));"
    ].

implement_type_unifier_union_ini(union, Spec, Term, Name) -->
    implement_type_unifier_union_ini_join(union, Spec, Term, Name, UType),
    ["    switch ("+UType+") {"].
implement_type_unifier_union_ini(enum, Spec, Term, Name) -->
    implement_type_unifier_union_ini_join(enum, Spec, Term, Name, _).
implement_type_unifier_union_ini(cdef,   _, _, _) --> [].
implement_type_unifier_union_ini(struct, _, _, _) --> [].

implement_type_unifier(union_end(SubType), _, _) -->
    implement_type_unifier_union_end(SubType).

implement_type_unifier_union_end(union) -->
    ['    default:',
     '        return FALSE;',
     '    };'],
    implement_type_end.
implement_type_unifier_union_end(cdef  ) --> [].
implement_type_unifier_union_end(struct) --> implement_type_end.
implement_type_unifier_union_end(enum  ) --> implement_type_end.

implement_type_unifier(func_ini(SubType, Spec), Term, Name) -->
    {func_pcname(Name, PName, CName)},
    ( {SubType = union}
    ->{functor(Term, TName, _)},
      ["    case "+Name+"_"+TName+":",
       '    {']
    ; implement_type_unifier_ini(PName, CName, Name, Spec),
      {functor(Term, Func, Arity)},
      ["        __rtcheck(PL_unify_functor("+PName+", PL_new_functor(PL_new_atom(\""+Func+"\"), "+Arity+")));"]
    ).
implement_type_unifier(func_rec(SubType, N, Term, Name), Spec, Arg) -->
    {type_unifiers_elem_names(SubType, Term, Name, Arg, Indent, PName, CNameArg, PNameArg)},
    type_unifiers_elem_settle(Spec, Indent, CNameArg, PNameArg),
    [Indent+'__rtcheck(PL_unify_arg('+N+','+PName+','+PNameArg+'));'].

type_unifiers_elem_names(SubType, Term, Name, Arg, Indent, PName, CNameArg, PNameArg) :-
    func_pcname(Name, PName, CName),
    ( SubType = union
    ->functor(Term, TName, _),
      format(atom(CRecordName), '~w.~w', [TName, Arg]),
      format(atom(TNameArg), '~w_~w', [TName, Arg]),
      camel_snake(PRecordName, TNameArg),
      Indent = '        '
    ; CRecordName = Arg,
      camel_snake(PRecordName, Arg),
      Indent = '    '
    ),
    CNameArg = CName+"->"+CRecordName,
    PNameArg = PName+"_"+PRecordName.

type_unifiers_elem_settle(Spec, Indent, CNameArg, PNameArg) -->
    [Indent+'term_t '+PNameArg+'=PL_new_term_ref();'],
    {c_set_argument(Spec, out, CNameArg, PNameArg, SetArg)},
    [Indent+SetArg+';'].

implement_type_unifier(func_end(SubType), _, _) -->
    ( {SubType = union}
    ->['        break;',
       '    }']
    ; []
    ).
implement_type_unifier(dict_ini(SubType, Name, _, _), Spec, Term) -->
    ( {SubType = union}
    ->{functor(Term, TName, _)},
      ["    case "+Name+"_"+TName+":",
       '    {']
    ; {func_pcname(Term, PName, CName)},
      implement_type_unifier_ini(PName, CName, Name, Spec)
    ),
    ['    term_t __desc=PL_new_term_ref();',
     '    term_t __tail=PL_copy_term_ref(__desc);'].
implement_type_unifier(dict_key_value(Dict, _, N, _), Key, Value) -->
    {key_value_from_dict(Dict, N, Key, Value)}. % Placed in 'dict' order
implement_type_unifier(dict_rec(SubType, _, Term, _N, NameL), Spec, Arg) -->
    {term_pcname(Term, NameL, Name)},
    {type_unifiers_elem_names(SubType, Term, Name, Arg, Indent, _, CNameArg, PNameArg)},
    ( {spec_pointer(Spec)}
    ->with_wrapper(
          Indent+'if('+CNameArg+') {',
          type_unifiers_elem_dict_settle(Spec, Arg, Indent+'    ', CNameArg, PNameArg),
          Indent+'}')
    ; type_unifiers_elem_dict_settle(Spec, Arg, Indent, CNameArg, PNameArg)
    ).

type_unifiers_elem_dict_settle(Spec, Arg, Indent, CNameArg, PNameArg) -->
    type_unifiers_elem_settle(Spec, Indent, CNameArg, PNameArg),
    [Indent+'FI_put_desc(__tail, "'+Arg+'", '+PNameArg+');'].

with_wrapper(Ini, Goal, End) -->
    [Ini],
    call(Goal),
    [End].

implement_type_unifier(dict_end(SubType, _, Tag), Term, _) -->
    {func_pcname(Term, PName, _)},
    ['    __rtcheck(PL_unify_nil(__tail));',
     "    FI_dict_create("+PName+", \""+Tag+"\", __desc);"],
    ( {SubType = union}
    ->['        break;',
       '    }']
    ; []
    ).

spec_pointer(chrs(_)).
spec_pointer(string(_)).
spec_pointer(ptr(_)).
spec_pointer(pointer-_).
spec_pointer(list(_)).
spec_pointer(tdef(_, Spec)) :- spec_pointer(Spec).
% spec_pointer(type(_)).

implement_type_unifier_ini(PName, CName, Name, Spec) -->
    { ctype_decl(Spec, Decl),
      ( \+ref_type(Spec)
      ->DRef = ""
      ; DRef = "*"
      ),
      ctype_suff(Spec, Suff)
    },
    ["int FI_unify_"+Name+"(term_t "+PName+", "+Decl+DRef+" const "+CName+Suff+") {"].

apply_name(Name=Value) :-
    camel_snake(Name, Arg),
    ignore(Value=Arg).

apply_dict(Head, Dict) :-
    maplist(apply_name, Dict),
    term_variables(Head, Vars),
    fg_numbervars(Vars, 1, Dict).

fg_numbervars([], _, _).
fg_numbervars([V|Vs], N, Dict) :-
    format(atom(T), 'var_~d', [N]),
    succ(N, N1),
    ( memberchk(_=T, Dict)
    ->fg_numbervars([V|Vs], N1, Dict)
    ; V=T,
      fg_numbervars(Vs, N1, Dict)
    ).

bind_type_names(MType, TypeMPropLDictL) :-
    predicate_property(MType, interpreted),
    strip_module(MType, _, Type),
    findall(t(Type, MPropL, Dict),
            bind_tn_clause(MType, MPropL, Dict),
            TypeMPropLDictL).

:- meta_predicate
    bind_tn_clause(0, -, -).

bind_tn_clause(MType, MPropL, Dict) :-
    strip_module(MType, M, Type),
    catch(clause(MType, Body, Ref), _, fail),
    ( clause_property(Ref, file(File)),
      clause_property(Ref, line_count(Line)),
      get_dictionary(Type :- Body, File, Line, M, Dict)
    ->true
    ; Dict = []
    ),
    clause_property(Ref, module(CM)),
    sequence_list(Body, PropL, []),
    maplist(cond_qualify_with(CM), PropL, MPropL).

ds_union_ini_1(SubType, Name, Idx, t(Type, _, _)) -->
    { functor(Type, _, N),
      arg(N, Type, Term),
      ( SubType = enum
      ->format(codes(Codes), "~w", [Term]),
        sanitize_csym(Codes, [], CName, []),
        atom_codes(TName, CName)
      ; functor(Term, TName, _)
      )
    },
    ["    "+Name+"_"+TName+" = "+Idx+","].

sanitize_csym([],    _ ) --> [].
sanitize_csym([C|L], S1) -->
    ( {type_char(csym, C)}
    ->S1,
      [C],
      {S = []}
    ; [],
      {S = [0'_|S1]}
    ),
    sanitize_csym(L, S).

declare_struct_union_ini(union, TPDL, Name) -->
    ['typedef enum {'],
    foldnl(ds_union_ini_1(union, Name), 0, TPDL),
    ["} "+Name+"_utype;"],
    ["struct "+Name+" {",
     "  "+Name+"_utype utype;",
     '  union {'
    ].
declare_struct_union_ini(cdef, _, _) --> [].
declare_struct_union_ini(struct, _, _) --> [].
declare_struct_union_ini(enum, TPDL, Name) -->
    ["enum "+Name+" {"],
    foldnl(ds_union_ini_1(enum, Name), 0, TPDL),
    ["};"].

declare_struct_union_end(union) -->
    ['  };',
     '};'
    ].
declare_struct_union_end(cdef  ) --> [].
declare_struct_union_end(struct) --> [].
declare_struct_union_end(enum  ) --> [].

declare_struct(union_ini(SubType, _, TPDL), _, Name) -->
    declare_struct_union_ini(SubType, TPDL, Name).
declare_struct(union_end(SubType), _, _) -->
    declare_struct_union_end(SubType).
declare_struct(atomic(SubType, Name), Spec, Term) -->
    { ctype_decl(Spec, Decl),
      ctype_suff(Spec, Suff)
    },
    ( {SubType = union}
    ->{functor(Term, TName, _)},
      ["    "+Decl+" "+TName+Suff+";"]
    ; ["typedef "+Decl+" "+Name+Suff+";"]
    ).
declare_struct(func_ini(SubType, Spec), _, _) -->
    ( {SubType = union}
    ->{Decl = "  struct"}
    ; {ctype_decl(Spec, Decl)}
    ),
    [Decl+" {"].
declare_struct(func_end(SubType), Term, _) -->
    ( {SubType = union}
    ->{functor(Term, TName, _)},
      ["    } "+TName+";"]
    ; ["};"]
    ).
declare_struct(func_rec(_, _, _, _), Spec, Name) -->
    { ctype_decl(Spec, Decl),
      ctype_suff(Spec, Suff)
    },
    ["    "+Decl+" "+Name+Suff+";"].
%%
declare_struct(dict_ini(_, _, _, _), Spec, _) -->
    {ctype_decl(Spec, Decl)},
    ["",
     Decl+" {"].
declare_struct(dict_key_value(Dict, Desc, N, _), Key, Value) -->
    {key_value_from_desc(Dict, Desc, N, Key, Value)}.
declare_struct(dict_rec(_, _, _, _, _), Spec, Name) -->
    { ctype_decl(Spec, Decl),
      ctype_suff(Spec, Suff)
    },
    ["    "+Decl+" "+Name+Suff+";"].
declare_struct(dict_end(_, _, _), _, _) --> ['};'].

declare_type_getter_unifier_union_ini(union, Spec, Name) -->
    declare_type_getter_unifier(Name, Spec).
declare_type_getter_unifier_union_ini(cdef,   _, _) --> [].
declare_type_getter_unifier_union_ini(struct, _, _) --> [].
declare_type_getter_unifier_union_ini(enum, Spec, Name) -->
    declare_type_getter_unifier(Name, Spec).

declare_type_getter_unifier(atomic(_, _), _, _) --> [].
    % declare_type_getter_unifier(Name, Spec).
declare_type_getter_unifier(union_ini(SubType, Spec, _), _, Name) -->
    declare_type_getter_unifier_union_ini(SubType, Spec, Name).
declare_type_getter_unifier(union_end(_), _, _) --> [].
declare_type_getter_unifier(func_ini(SubType, Spec), _, Name) -->
    ( {SubType = union}
    ->[]
    ; declare_type_getter_unifier(Name, Spec)
    ).
declare_type_getter_unifier(func_end(_), _, _) --> [].
declare_type_getter_unifier(func_rec(_, _, _, _), _, _) --> [].
declare_type_getter_unifier(dict_ini(_, Name, M, _), _, _) -->
    ["predicate_t __"+M+"_aux_keyid_index_"+Name+";"].
declare_type_getter_unifier(dict_end(_, _, _), _, _) --> [].
declare_type_getter_unifier(dict_rec(_, _, _, _, _), _, _) --> [].

declare_type_getter_unifier(Name, Spec) -->
    { ctype_decl(Spec, Decl1),
      ( \+ref_type(Spec)
      ->DRef = Decl1
      ; DRef = Decl1+"*"
      ),
      ( Spec = array(_, _)
      ->Decl = Decl1
      ; Decl = Decl1+"*"
      )
    },
    ["int FI_get_"+Name+"(root_t __root, term_t, "+Decl+");",
     "int FI_unify_"+Name+"(term_t, "+DRef+" const);",
     ''].

generate_aux_clauses(Module) -->
    findall_tp(Module, type_props, generate_aux_clauses).

% This will create an efficient method to convert keys to indexes in the C side,
% avoiding string comparisons.
generate_aux_clauses(dict_ini(_, Name, _, _), _, _) -->
    !,
    {atom_concat('__aux_keyid_index_', Name, F)},
    [(:- public F/2)].
generate_aux_clauses(dict_key_value(Dict, _, N, _), Key, Value) -->
    !,
    {key_value_from_dict(Dict, N, Key, Value)}.
generate_aux_clauses(dict_rec(_, _, _, N, Name), _, Key) -->
    !,
    { atom_concat('__aux_keyid_index_', Name, F),
      Pred =.. [F, Key, N]
    },
    [(Pred :- true)].
generate_aux_clauses(_, _, _) --> [].

:- multifile
    prolog:message//1.

prolog:message(ignored_type(Name, Arg)) -->
    ["~w->~w ignored"-[Name, Arg]].

prolog:message(failed_binding(TypeComponents)) -->
    ['~w failed'-[TypeComponents]].

:- meta_predicate type_components(+,+,5,+,?,?).

type_components(M, TypePropLDictL, Call, Loc) -->
    foldl(type_components_1(M, Call, Loc), TypePropLDictL).

type_components_1(M, Call, Loc, Type-TypePropLDictL) -->
    { functor(Type, Name, _),
      ( TypePropLDictL = [t(_, [], _)]
      ->SubType = cdef,
        Spec = cdef(Name)
      ; forall(member(t(Type, PropL, _), TypePropLDictL), PropL = [])
      ->SubType = enum,
        length(TypePropLDictL, N),
        Spec = enum(Name, N)
      ; Spec = type(Name),
        ( TypePropLDictL = [_, _|_]
        ->SubType = union
        ; SubType = struct
        )
      )
    },
    call(Call, union_ini(SubType, Spec, TypePropLDictL), Type, Name),
    foldl(type_components_one(M, SubType, Spec, Name, Call, Loc), TypePropLDictL),
    call(Call, union_end(SubType), Type, Name).

type_components_one(M, SubType, TSpec, Name, Call, Loc, t(Type, PropL, _)) -->
    { functor(Type, _, Arity),
      arg(Arity, Type, Term)
    },
    ( {PropL = []}
    ->[]
    ; {compound(Term)}
    ->call(Call, func_ini(SubType, TSpec), Term, Name),
      findall(Lines,
              ( arg(N, Term, Arg),
                phrase(( { member(Prop, PropL),
                           match_known_type(Prop, M, Name, Spec, Arg)
                         },
                         call(Call, func_rec(SubType, N, Term, Name), Spec, Arg)
                       ->[]
                       ; {print_message(
                              warning,
                              at_location(Loc, ignored_type(func(Name), Arg)))}
                       ), Lines)
              )),
      call(Call, func_end(SubType), Term, Name)
    ; { select(dict_t(Desc, Term), PropL, PropL1)
      ; select(dict_t(Tag, Desc, Term), PropL, PropL1)
      ; select(dict_join_t(Tag, Type1, Type2, Term), PropL, PropL1),
        join_dict_types(Type1, M, Type2, M, Tag, Desc)
      ; select(dict_extend_t(Term, Type, Tag, Desc2), PropL, PropL1),
        join_type_desc(M:Type, Tag, Desc2, Desc)
      }
    ->{ is_dict(Desc, Tag)
      ->Dict=Desc
      ; dict_create(Dict, Tag, Desc)
      },
      {ignore(Tag = Name)},
      call(Call, dict_ini(SubType, Name, M, Dict), type(Name), Term),
      findall(Lines,
              phrase(( call(Call, dict_key_value(Dict, Desc, N, Name), Arg, Value),
                       ( { fetch_kv_prop_arg(Arg,  M, Value, PropL1, Prop),
                           match_known_type(Prop, M, Name, Spec, Arg)
                         },
                         call(Call, dict_rec(SubType, M, Term, N, Name), Spec, Arg)
                       ->[]
                       ; {print_message(
                              warning,
                              at_location(Loc, ignored_type(dict(Name), Arg)))}
                       )), Lines)),
      call(Call, dict_end(SubType, M, Tag), Term, Name)
    ; { member(Prop, PropL),
        match_known_type(Prop, M, Name, Spec, Term)
      }
    ->call(Call, atomic(SubType, Name), Spec, Term)
    ),
    !.
type_components_one(M, ST, TS, N, G, Loc, T) -->
    {print_message(
         error,
         at_location(
             Loc,
             failed_binding(type_components_one(M, ST, TS, N, G, Loc, T))))}.

key_value_from_dict(Dict, N, Key, Value) :-
    S = s(0),
    Value=Dict.Key,
    S = s(N),
    succ(N, N2),
    nb_setarg(1, S, N2).

key_value_from_list(Desc, N, Key, Value) :-
    nth0(N, Desc, KeyValue),
    key_value(KeyValue, Key, Value).

key_value_from_desc(_, Desc, N, Key, Value) :-
    is_list(Desc), !,
    key_value_from_list(Desc, N, Key, Value).
key_value_from_desc(Dict, _, N, Key, Value) :-
    key_value_from_dict(Dict, N, Key, Value).

fetch_kv_prop_arg(Key, CM, Value, PropL, M:Prop) :-
    ( member(MProp, PropL),
      strip_module(CM:MProp, M, Prop),
      functor(Prop, _, N),
      arg(N, Prop, Key)
    ; extend_args(Value, [Key], Prop),
      M=CM
    ).

declare_intf_head(PCN, Head, "foreign_t __aux_pfa_"+PCN+"_"+N+"(term_t __args)") :-
    max_fli_args(MaxFLIArgs),
    neck,
    functor(Head, _, N),
    N > MaxFLIArgs,
    !.
declare_intf_head(PCN, Head, "foreign_t "+PCN+"("+TxtL/", "+")") :-
    findall("term_t "+Arg,
            ( compound(Head),
              arg(_, Head, Arg)
            ), TxtL).

declare_foreign_bind(CM) -->
    findall(Line+";",
            ( read_foreign_properties(Head, M, CM, Comp, Call, Succ, Glob, Bind, _),
              declare_impl_head(Head, M, CM, Comp, Call, Succ, Glob, Bind, Line)
           )).

declare_impl_head(Head, M, CM, Comp, Call, Succ, Glob, Bind, Type+FHD) :-
    ( member(RS, [returns_state(_), type(_)]),
      memberchk(RS, Glob)
    ->Type = "int ",       % int to avoid SWI-Prolog.h dependency at this level
      CHead = Head
    ; member(returns(Var, _), Glob)
    ->bind_argument(Head, M, CM, Comp, Call, Succ, Glob, Var, Spec, Mode),
      ctype_arg_decl(Spec, Mode, Decl),
      Type = Decl+" ",
      Head =.. [F|Args],
      once(select(Var, Args, CArgs)),
      CHead =.. [F|CArgs]
    ; Type = "void ",
      CHead = Head
    ),
    declare_foreign_head(CHead, M, CM, Comp, Call, Succ, Glob, Bind, FHD),
    !.

declare_foreign_head(Head, M, CM, Comp, Call, Succ, Glob, (CN/_ as _ + _),
                     CN+"("+ArgL/", "+")") :-
    phrase(( ( {memberchk(memory_root(_), Glob)}
             ->["root_t __root"]
             ; []
             ),
             findall(
                 Line,
                 distinct(
                     Key,
                     ( compound(Head),
                       arg(_, Head, Arg),
                       bind_argument(Head, M, CM, Comp, Call, Succ, Glob, Arg, Spec, Mode),
                       curr_arg_decl(Arg, Spec, Mode, Key-Line)
                     )))
           ), ArgL).

extra_arg_decl(array(Spec, Dim), KeyLine) :-
    ( \+ integer(Dim),
      curr_arg_decl(Dim, size_t-size_t, in, KeyLine)
    ; extra_arg_decl(Spec, KeyLine)
    ).

curr_arg_decl(_, Spec, Mode, KeyLine) :-
    memberchk(Mode, [in, inout]),
    extra_arg_decl(Spec, KeyLine).
curr_arg_decl(Arg, Spec, Mode, Arg-(Decl+' '+Arg+Suff)) :-
    ctype_barg_decl(Spec, Mode, Decl),
    ctype_barg_suff(Spec, Suff).

ctype_barg_decl(Spec, Mode, Decl) :-
    ctype_barg_decl(Spec, Mode, Codes, []),
    atom_codes(Decl, Codes).

ctype_barg_suff(Spec, Suff) :-
    ctype_suff(Spec, Codes, []),
    atom_codes(Suff, Codes).

ctype_barg_decl(Spec, Mode) -->
    ctype_arg_decl(Spec, Mode),
    ({ Mode = in,
       \+ ref_type(Spec)
     ; Spec = array(_, _)
     } -> []
    ; "*"
    ),
    ( {Mode = in}
    ->" const"
    ; []
    ). % Ensure const correctness

ctype_arg_decl(Spec, Mode) -->
    ctype_decl(Spec),
    ({is_ref(Spec, Mode)} -> [] ; "*").

ctype_arg_decl(Spec, Mode, Decl) :-
    ctype_arg_decl(Spec, Mode, Codes, []),
    atom_codes(Decl, Codes).


ctype_suff(array(Spec, Dim), CDim) --> "[", call(CDim, Dim), "]", ctype_suff(Spec, CDim).
ctype_suff(Spec, _) -->
    {member(Spec, [list(_), ptr(_), chrs(_), string(_), type(_), enum(_, _),
                   term, tdef(_, _), setof(_, _), cdef(_), _-_])},
    neck.

ctype_suff(Spec) --> ctype_suff(Spec, acodes).

is_ref(term,      _) :- !.
is_ref(list(_),   _) :- !.        % Always ref
is_ref(ptr(_),    _) :- !.        % Always ref
is_ref(chrs(_),   _) :- !.
is_ref(string(_), _) :- !.
is_ref(array(_, _), _) :- !.
is_ref(_, in).
is_ref(_, out).
% is_ref(inout, _) :- fail.
% Allow pointer to NULL, the equivalent to free variables in imperative
% languages --EMM

% Types that are passed by reference
ref_type(type(_)).
ref_type(tdef(_, Spec)) :- ref_type(Spec).

ctype_decl(list(Spec))     --> ctype_decl(Spec), "*".
ctype_decl(array(Spec, _)) --> ctype_decl(Spec).
ctype_decl(ptr(Spec))      --> ctype_decl(Spec), "*".
ctype_decl(chrs(Name))     --> acodes(Name).
ctype_decl(string(Name))   --> acodes(Name).
ctype_decl(type(Name))     --> "struct ", acodes(Name).
ctype_decl(enum(Name, _))  --> "enum ", acodes(Name).
ctype_decl(term)           --> "term_t".
ctype_decl(tdef(Name, _))  --> acodes(Name).
ctype_decl(setof(Name, _)) --> acodes(Name).
ctype_decl(cdef(Name))     --> acodes(Name).
ctype_decl(_-CType)        --> acodes(CType).

ctype_decl(Spec, Decl) :-
    ctype_decl(Spec, Codes, []),
    atom_codes(Decl, Codes).

ctype_suff(Spec, Suff) :-
    ctype_suff(Spec, Codes, []),
    atom_codes(Suff, Codes).

acodes(Atom, List, Tail) :-
    atom_codes(Atom, Codes),
    append(Codes, Tail, List).

cond_qualify_with(CM, MProp, MProp) :-
    strip_module(CM:MProp, M, Prop),
    ( CM = M
    ->MProp = Prop
    ; MProp = M:Prop
    ).

foreign_native(foreign(_)).
foreign_native(foreign(_, _)).
foreign_native(native(_)).
foreign_native(native(_, _)).

current_foreign_prop(Asr, Head, Module, Context, CompL, CallL, SuccL, GlobL, DictL,
                     FuncName, PredName, BindName, Arity) :-
    current_foreign_prop(foreign_native, Asr, Head, Module, Context, CompL, CallL, SuccL, GlobL,
                         DictL, FuncName, PredName, BindName, Arity, _).

current_foreign_prop(Asr, Head, Module, Context, CompL, CallL, SuccL, GlobL, DictL,
                     FuncName, PredName, BindName, Arity, Type) :-
    current_foreign_prop(foreign_native_fimport, Asr, Head, Module, Context, CompL, CallL, SuccL, GlobL,
                         DictL, FuncName, PredName, BindName, Arity, Type).

:- meta_predicate collect(?,^,-).
collect(Tmpl, Goal, List) :-
    (bagof(Tmpl, Goal, List) *-> true ; List = []).

collect_props(Asr, CM, CompL, CallL, SuccL, GlobL) :-
    maplist(collect_prop(Asr, CM),
            [comp, call, succ, glob],
            [CompL, CallL, SuccL, GlobL]).

collect_prop(Asr, CM, Part, PropL) :-
    collect(MProp,
            (M, Prop, From)^( curr_prop_asr(Part, M:Prop, From, Asr),
                              ( M \= CM
                              ->MProp = M:Prop
                              ; MProp = Prop
                              )
                            ), PropL).

assertion_db(Asr, Head, M, CM, Status, Type, Comp, Call, Succ, Glob, Comm, Dict, Loc) :-
    asr_head_prop(Asr, CM, Head, Status, Type, Dict, Loc),
    ( curr_prop_asr(comm, Comm, _, Asr)
    ->true
    ; Comm = ""
    ),
    predicate_property(CM:Head, implementation_module(M)),
    collect_props(Asr, CM, Comp, Call, Succ, Glob).

current_foreign_prop(GenKeyProp, Asr, Head, Module, Context, CompL, CallL, SuccL, GlobL,
                     DictL, FuncName, PredName, BindName, Arity, KeyProp) :-
    asr_head_prop(Asr, Context, Head, check, Type, _, _),
    memberchk(Type, [pred, prop]),
    predicate_property(Context:Head, implementation_module(Module)),
    once(( call(GenKeyProp, KeyProp),
           prop_asr(glob, KeyProp, _, Asr)
         )),
    findall(Head-[MComp, MCall, MSucc, MGlob, Dict],
            ( assertion_db(_, Head, Module, CM, check, Type, Comp, Call, Succ,
                           Glob, _, Dict, _),
              maplist(maplist(cond_qualify_with(CM)),
                      [ Comp,  Call,  Succ,  Glob],
                      [MComp, MCall, MSucc, MGlob])
            ), KPropLL),
    maplist(=(Head-_), KPropLL),
    pairs_values(KPropLL, PropLL),
    transpose(PropLL, PropTL),
    maplist(append, PropTL, [CompU, CallU, SuccU, GlobU, DictL]),
    maplist(sort, [CompU, CallU, SuccU, GlobU], [CompL, CallL, SuccL, GlobL]),
    ( ( prop_asr(glob, native(_),    _, Asr)
      ; prop_asr(glob, native(_, _), _, Asr)
      )
    -> % Already considered
      \+ ( member(KeyProp2, [foreign(_), foreign(_, _)]),
           memberchk(KeyProp2, GlobL)
         )
    ; true
    ),
    functor(Head, PredName, Arity),
    ( ( memberchk(foreign(_), GlobL)
      ->FuncSpec = name(PredName)
      ; memberchk(foreign(FuncSpec, _), GlobL)
      ->true
      ; memberchk(fimport(_), GlobL)
      ->FuncSpec = name(PredName)
      ; memberchk(fimport(FuncSpec, _), GlobL)
      )
    ->resolve_name(FuncSpec, PredName, FuncName)
    ; true
    ),
    ( ( memberchk(native(_), GlobL)
      ->BindSpec=name(PredName)
      ; memberchk(native(BindSpec, _), GlobL)
      ->true
      )
    ->Name = PredName
    ; nonvar(FuncName)
    ->BindSpec=prefix(pl_),
      Name = FuncName
    ),
    resolve_name(BindSpec, Name, BindName).

resolve_name(BindName, _, BindName) :- atom(BindName), !.
resolve_name(name(BindName), _, BindName).
resolve_name(prefix(Prefix), PredName, BindName) :- atom_concat(Prefix, PredName, BindName).
resolve_name(suffix(Suffix), PredName, BindName) :- atom_concat(PredName, Suffix, BindName).

foreign_native_fimport(H) :- foreign_native(H).
foreign_native_fimport(fimport(_)).
foreign_native_fimport(fimport(_, _)).

read_foreign_properties(Head, M, CM, Comp, Call, Succ, Glob, CN/A as PN/BN + CheckMode, T) :-
    current_foreign_prop(_Asr, Head, M, CM, Comp, Call, Succ, Glob, Dict, CN, PN, BN, A, T),
    nonvar(CN),
    ( memberchk(type(_), Glob)
    ->CheckMode=(type)
    ; CheckMode=pred
    ),
    apply_dict(Head, Dict).

generate_foreign_intf(Module) -->
    findall(Lines,
            ( read_foreign_properties(Head, M, Module, Comp, Call, Succ, Glob, Bind, Type),
              phrase(declare_intf_impl(Type, Head, M, Module, Comp, Call, Succ, Glob, Bind),
                     Lines))).

declare_intf_impl(fimport(_), Head, M, Module, Comp, Call, Succ, Glob, Bind) -->
    !,
    declare_fimp_impl(Head, M, Module, Comp, Call, Succ, Glob, Bind).
declare_intf_impl(fimport(_, _), Head, M, Module, Comp, Call, Succ, Glob, Bind) -->
    !,
    declare_fimp_impl(Head, M, Module, Comp, Call, Succ, Glob, Bind).
declare_intf_impl(_, Head, M, Module, Comp, Call, Succ, Glob, Bind) -->
    declare_forg_impl(Head, M, Module, Comp, Call, Succ, Glob, Bind).

declare_fimp_impl(Head, M, Module, Comp, Call, Succ, Glob, Bind) -->
    { Bind = (_/A as PN/BN + _),
      declare_intf_fimp_head(BN, BNHead)
    },
    [BNHead+'=NULL;'],
    {declare_impl_head(Head, M, Module, Comp, Call, Succ, Glob, Bind, ImplHead)},
    [ImplHead+' {',
     "    term_t "+BN+"_args = PL_new_term_refs("+A+");"],
    ( {memberchk(parent(Var, _), Glob)}
    ->["    __leaf_t *__root = LF_ROOT(LF_PTR(FI_array_ptr("+Var+")));"]
    ; []
    ),
    bind_outs_arguments(Head, M, Module, Comp, Call, Succ, Glob, Bind),
    ["} /* "+PN/A+" */",
     ''].

declare_forg_impl(Head, M, Module, Comp, Call, Succ, Glob, Bind) -->
    { max_fli_args(MaxFLIArgs),
      neck,
      Bind = (PI as _/PCN + CheckMode),
      declare_intf_head(PCN, Head, PCNH)
    },
    [PCNH+' {'],
    ( { functor(Head, _, Arity),
        Arity > MaxFLIArgs
      }
    ->findall(["    term_t "+Arg+" = PL_new_term_ref();",
               "    __rtcheck(PL_get_arg("+N+", __args, "+Arg+"));"],
              arg(N, Head, Arg))
    ; []
    ),
    % If is variable then succeed (because is compatible)
    findall("    if(PL_is_variable("+Arg+")) return TRUE;",
            ( CheckMode==(type),
              arg(_, Head, Arg)
            )),
    ['    __mkroot(__root);'],
    bind_arguments(Head, M, Module, Comp, Call, Succ, Glob, Bind, Return),
    ['    __delroot(__root);',
     "    return "+Return+";",
     "} /* "+PI+" */",
     ''].

c_set_argument(list(S),     _, C, A, L) :- c_set_argument_rec(list, S, C, A, L).
c_set_argument(array(S, D), _, C, A, L) :- c_set_argument_array(S, D, C, A, L).
c_set_argument(ptr(S),      _, C, A, L) :- c_set_argument_rec(ptr, S, C, A, L).
c_set_argument(type(T),     M, C, A, L) :- c_set_argument_type(M, T, C, A, L).
c_set_argument(enum(T, _),  M, C, A, L) :- c_set_argument_one(M, T, C, A, L).
c_set_argument(cdef(T),     M, C, A, L) :- c_set_argument_one(M, T, C, A, L).
c_set_argument(T-_,         M, C, A, L) :- c_set_argument_one(M, T, C, A, L).
c_set_argument(chrs(_),     M, C, A, L) :- c_set_argument_chrs(M, C, A, L).
c_set_argument(string(_),   M, C, A, L) :- c_set_argument_string(M, C, A, L).
c_set_argument(tdef(_, S),  M, C, A, L) :- c_set_argument(S, M, C, A, L).
c_set_argument(setof(_, S), M, C, A, L) :- c_set_argument_setof(M, S, C, A, L).
c_set_argument(term,        _, C, A, "__rtcheck(PL_unify("+A+", "+C+"))").

c_set_argument_one(out,   Type, CArg, Arg, "__rtc_FI_unify("+Type+", "+Arg+", "+CArg+")").
c_set_argument_one(inout, Type, CArg, Arg, "FI_unify_inout("+Type+", "+Arg+", "+CArg+")").

c_set_argument_type(out,   Type, CArg, Arg, "__rtc_FI_unify("+Type+", "+Arg+", &"+CArg+")").
c_set_argument_type(inout, Type, CArg, Arg, "FI_unify_inout_type("+Type+", "+Arg+", "+CArg+")").

c_set_argument_chrs(out,   CArg, Arg, "__rtc_FI_unify(chrs, "+Arg+", "+CArg+")").
c_set_argument_chrs(inout, CArg, Arg, "FI_unify_inout_chrs("+Arg+", "+CArg+")").

c_set_argument_string(out,   CArg, Arg, "__rtc_FI_unify(string, "+Arg+", "+CArg+")").
c_set_argument_string(inout, CArg, Arg, "FI_unify_inout_string("+Arg+", "+CArg+")").

c_set_argument_array(Spec, Dim, CArg, Arg, "FI_unify_array("+L+", "+CDim+", "+Arg+")") :-
    Arg_ = Arg+"_",
    c_var_name(Arg_, CArg_),
    c_dim(Dim, CDim),
    c_set_argument(Spec, out, CArg+"["+CArg_+"]", Arg_, L).

c_set_argument_rec(Type, Spec, CArg, Arg, "FI_unify_"+Type+"("+L+", "+Arg+", "+CArg+")") :-
    Arg_ = Arg+"_",
    c_var_name(Arg_, CArg_),
    c_set_argument(Spec, out, CArg_, Arg_, L).

c_set_argument_setof(Mode, Spec, CArg, Arg, "FI_unify_"+Mode+"_setof("+L+", "+Type+", "+Arg+", "+CArg+")") :-
    Arg_ = Arg+"_",
    c_var_name(Arg_, CArg_),
    ctype_decl(Spec, Type),
    c_set_argument(Spec, out, CArg_, Arg_, L).

c_get_argument(list(S),     M, C, A, L) :- c_get_argument_rec(M, list, S, C, A, L).
c_get_argument(array(S, D), _, C, A, L) :- c_get_argument_array(S, D, C, A, L).
c_get_argument(ptr(S),      M, C, A, L) :- c_get_argument_rec(M, ptr,  S, C, A, L).
c_get_argument(type(T),     M, C, A, L) :- c_get_argument_type(M, T, C, A, L).
c_get_argument(enum(T, _),  M, C, A, L) :- c_get_argument_one(M, T, C, A, L).
c_get_argument(cdef(T),     M, C, A, L) :- c_get_argument_one(M, T, C, A, L).
c_get_argument(T-_,         M, C, A, L) :- c_get_argument_one(M, T, C, A, L).
c_get_argument(chrs(_),     M, C, A, L) :- c_get_argument_chrs(M, C, A, L).
c_get_argument(string(_),   M, C, A, L) :- c_get_argument_string(M, C, A, L).
c_get_argument(tdef(_, S),  M, C, A, L) :- c_get_argument(S, M, C, A, L).
c_get_argument(setof(_, S), M, C, A, L) :- c_get_argument_setof(M, S, C, A, L).
c_get_argument(term,        _, C, A, "*"+C+"=PL_copy_term_ref("+A+")").

c_get_argument_one(in, Type, CArg, Arg, "__rtc_FI_get("+Type+", "+Arg+", "+CArg+")").
c_get_argument_one(inout, Type, CArg, Arg, "FI_get_inout("+Type+", "+Arg+", "+CArg+")").

c_get_argument_type(in, Type, CArg, Arg, "__rtc_FI_get("+Type+", "+Arg+", "+CArg+")").
c_get_argument_type(inout, Type, CArg, Arg, "FI_get_inout("+Type+", "+Arg+", "+CArg+")").

c_get_argument_chrs(in, CArg, Arg, "__rtc_FI_get(chrs, "+Arg+", "+CArg+")").
c_get_argument_chrs(inout, CArg, Arg, "FI_get_inout_chrs("+Arg+", "+CArg+")").

c_get_argument_string(in, CArg, Arg, "__rtc_FI_get(string, "+Arg+", "+CArg+")").
c_get_argument_string(inout, CArg, Arg, "FI_get_inout_string("+Arg+", "+CArg+")").

c_get_argument_array(Spec, Dim, CArg, Arg, "FI_get_array("+L+","+CDim+", "+Arg+")") :-
    Arg_ = Arg+"_",
    c_var_name(Arg_, CArg_),
    c_dim(Dim, CDim),
    c_get_argument(Spec, in, CArg+"["+CArg_+"]", Arg_, L).

c_get_argument_rec(Mode, Type, Spec, CArg, Arg,
                   "FI_get_"+Mode+"_"+Type+"("+L+", "+Arg+", "+CArg+")") :-
    Arg_ = Arg+"_",
    c_var_name(Arg_, CArg_),
    c_get_argument(Spec, in, CArg_, Arg_, L).

c_get_argument_setof(Mode, Spec, CArg, Arg,
                     "FI_get_"+Mode+"_setof("+L+", "+Type+", "+Arg+", "+CArg+")") :-
    Arg_ = Arg+"_",
    c_var_name(Arg_, CArg_),
    ctype_decl(Spec, Type),
    c_get_argument(Spec, in, CArg_, Arg_, L).

c_dim(Dim) --> {integer(Dim)}, !, acodes(Dim).
c_dim(Dim) --> "_c_", acodes(Dim).

c_dim(Dim, CDim) :-
    c_dim(Dim, Codes, []),
    atom_codes(CDim, Codes).

ctype_c_suff(Spec) --> ctype_suff(Spec, c_dim).

ctype_c_suff(Spec, Suff) :-
    ctype_c_suff(Spec, Codes, []),
    atom_codes(Suff, Codes).

extra_var_def(array(Spec, Dim), Head, Arg, KeyLine) :-
    ( \+ integer(Dim),
      curr_bind_line(dim(Arg), Head, Dim, size_t-size_t, in, KeyLine)
    ; extra_var_def(Spec, Head, Arg+"_"+Dim, KeyLine)
    ).

curr_bind_line(arg, Head, Arg, Spec, Mode, KeyLine) :-
    memberchk(Mode, [in, inout]),
    extra_var_def(Spec, Head, Arg, KeyLine).
curr_bind_line(_, _, Arg, Spec, Mode, dec(Arg)-Line) :-
    ctype_arg_decl(Spec, Mode, Decl),
    c_var_name(Arg, CArg),
    ( Spec = term
    ->DN=" "+CArg+"=PL_new_term_ref();"
    ; ctype_c_suff(Spec, CSuff),
      DN=" "+CArg+CSuff+";"
    ),
    Line = "    "+Decl+DN.
curr_bind_line(arg, _, Arg, Spec, Mode, KeyLine) :-
    memberchk(Mode, [in, inout]),
    c_var_name(Arg, CArg1),
    CArg = "&"+CArg1,
    c_get_argument(Spec, Mode, CArg, Arg, GetArg),
    KeyLine = def(Arg)-["    "+GetArg+";"].
curr_bind_line(dim(Arg), Head, Dim, _, _, def(CDim1)-LineL) :-
    \+ arg(_, Head, Dim),
    c_var_name(Dim, CDim1),
    CDim = "&"+CDim1,
    Line = "    FI_get_dim("+Arg+", "+CDim+");",
    ( arg(_, Head, Arg)
    ->LineL = [Line]
    ; Arg = Arg2+"_"+_,
      LineL = ["    term_t "+Arg+"=PL_new_term_ref();",
               "    __rtcheck(PL_get_arg(1, "+Arg2+", "+Arg+"));",
               Line]
    ).

bind_arguments(Head, M, CM, Comp, Call, Succ, Glob, Bind, Return) -->
    ( {compound(Head)}
    ->findall(Line,
              distinct(
                  Key, % This hack allows automatic definition of dimensions on input arrays
                  ( arg(_, Head, Arg),
                    bind_argument(Head, M, CM, Comp, Call, Succ, Glob, Arg, Spec, Mode),
                    curr_bind_line(arg, Head, Arg, Spec, Mode, Key-Line)
                  )
              ))
    ; []
    ),
    {generate_foreign_call(Bind-Head, M, CM, Comp, Call, Succ, Glob, Return, ForeignCall)},
    [ForeignCall],
    ( {compound(Head)}
    ->findall('    '+SetArg+';',
              ( arg(_, Head, Arg),
                bind_argument(Head, M, CM, Comp, Call, Succ, Glob, Arg, Spec, Mode),
                memberchk(Mode, [out, inout]),
                c_var_name(Arg, CArg),
                c_set_argument(Spec, Mode, CArg, Arg, SetArg)
              ))
    ; []
    ).

invert_mode(in, out).
invert_mode(out, in).
invert_mode(inout, inout).

bind_outs_arguments(Head, M, CM, Comp, Call, Succ, Glob, (_ as _/BN +_)) -->
    findall("    "+Decl+Line,
            ( memberchk(returns(Arg, _), Glob)
            ->bind_argument(Head, M, CM, Comp, Call, Succ, Glob, Arg, Spec, Mode),
              memberchk(Mode, [out, inout]),
              ctype_arg_decl(Spec, Mode, Decl),
              ( Spec = term
              ->Line=" "+Arg+"=PL_new_term_ref();"
              ; Line=" "+Arg+";"
              )
            )),
    ( {compound(Head)}
    ->findall(["    term_t "+PArg+"="+BN+"_args + "+Idx1+";",
               "    "+SetArg+";"],
              ( arg(Idx, Head, Arg),
                succ(Idx1, Idx),
                bind_argument(Head, M, CM, Comp, Call, Succ, Glob, Arg, Spec, Mode),
                memberchk(Mode, [in, inout]),
                invert_mode(Mode, InvM),
                ( Mode = inout
                ->CArg = '*'+Arg
                ; CArg = Arg
                ),
                PArg = "_p_"+Arg,
                c_set_argument(Spec, InvM, CArg, PArg, SetArg)
              ))
    ; []
    ),
    {CallPred = "PL_call_predicate(__"+CM+", PL_Q_NORMAL, "+BN+", "+BN+"_args)"},
    ( {memberchk(returns_state(_), Glob)}
    ->['    int __result = '+CallPred+';']
    ; ['    __rtcwarn('+CallPred+');']
    ),
    ( {compound(Head)}
    ->findall(["    term_t "+PArg+"="+BN+"_args + "+Idx1+";",
               '    '+SetArg+';'],
              ( arg(Idx, Head, Arg),
                succ(Idx1, Idx),
                bind_argument(Head, M, CM, Comp, Call, Succ, Glob, Arg, Spec, Mode),
                memberchk(Mode, [out, inout]),
                invert_mode(Mode, InvM),
                ( memberchk(returns(Arg, _), Glob)
                ->CArg = "&"+Arg
                ; CArg = Arg
                ),
                PArg = "_p_"+Arg,
                c_get_argument(Spec, InvM, CArg, PArg, SetArg)
              )),
      ( { memberchk(returns(Arg, _), Glob)
        ; memberchk(returns_state(_), Glob),
          Arg = '__result'
        }
      ->["    return "+Arg+";"]
      ; []
      )
    ; []
    ).

generate_foreign_call((CN/_A as _ + _)-Head1, M, CM, Comp, Call, Succ, Glob, Return,
                      "    "+HLine+CN+"("+MR+LineL/", "+");") :-
    ( member(RS, [returns_state(_), type(_)]),
      memberchk(RS, Glob)
    ->HLine="foreign_t __result=",
      Head = Head1,
      Return = "__result"
    ; ( member(returns(Var, _), Glob)
      ->c_var_name(Var, CVar),
        HLine=CVar+"=",
        Head1 =.. [F|Args],
        once(select(Var, Args, CArgs)),
        Head =.. [F|CArgs]
      ; Head = Head1,
        HLine=""
      ),
      ( member(no_exception, Glob)
      ->Return = "TRUE"
      ; Return = "!PL_exception(0)"
      )
    ),
    ( memberchk(memory_root(_), Glob)
    ->MR="__root, "
    ; MR=""
    ),
    findall(Line,
            distinct(Key,
                     ( compound(Head),
                       arg(_, Head, Arg),
                       bind_argument(Head, M, CM, Comp, Call, Succ, Glob, Arg, Spec, Mode),
                       curr_arg_call(Arg, Spec, Mode, Key-Line)
                     )
                    ), LineL).

extra_arg_call(array(Spec, Dim), KeyLine) :-
    ( \+ integer(Dim),
      curr_arg_call(Dim, size_t-size_t, in, KeyLine)
    ; extra_arg_call(Spec, KeyLine)
    ).

curr_arg_call(_, Spec, Mode, KeyLine) :-
    memberchk(Mode, [in, inout]),
    extra_arg_call(Spec, KeyLine).
curr_arg_call(Arg, Spec, Mode, Arg-(Deref+CArg)) :-
    c_var_name(Arg, CArg),
    ( ( Mode = in,
        \+ ref_type(Spec)
      ; Spec = array(_, _)
      )
    ->Deref = ""
    ; Deref = "&"
    ).

:- use_module(library(sequence_list)).
:- use_module(library(prolog_clause), []).

get_dictionary(Term, File, Line, M, Dict) :-
    ( prolog_clause:read_term_at_line(File, Line, M, RawTerm1, _TermPos, Dict),
      ( RawTerm1 \= (_ :- _)
      ->RawTerm = (RawTerm1 :- true)
      ; RawTerm1 = RawTerm
      ),
      subsumes(RawTerm, Term) -> true
    ; Dict = []
    ).

match_known_type(Prop, M, Name, Spec, Arg) :-
    match_type(Prop, M, known, Name, Spec, Arg, _, _).

match_type(M:Prop,       _, K, Name, Spec, Arg) -->
    ( match_type(Prop, M, K, Name, Spec, Arg)
    ->[]
    ).
match_type(dict_t(Desc, A), _, _, Name, Spec, A) -->
    {is_dict(Desc, Tag)},
    !,
    match_known_type_dict(dict_t(Desc, A), Tag, A, Name, Spec).
match_type(dict_t(Tag, Desc, A), _, _, Name, Spec, A) -->
    {dict_create(_, Tag, Desc)},
    !,
    match_known_type_dict(dict_t(Tag, Desc, A), Tag, A, Name, Spec).
match_type(Prop, M, K, N, Spec, A) -->
    match_type_k(K, Prop, M, N, Spec, A).

match_type_k(known, Prop, M, N, Spec, A) --> match_known_type(Prop, M, N, Spec, A).
match_type_k(unknown, _, _, _, _, _) --> [].

match_known_type_type(Type, A, M, N, MSpec, A) -->
    {extend_args(Type, [A], Prop)},
    match_type(Prop, M, known, N, MSpec, A).

match_known_array([], T, A, M, N, MSpec, A) -->
    match_known_type_type(T, A, M, N, MSpec, A).
match_known_array([D|L], T, A, M, N, array(Spec, D), A) -->
    match_known_array(L, T, E, M, N, Spec, E).

match_known_type(atm(A),            _, _, chrs('char*'),   A) --> [].
match_known_type(atom(A),           _, _, chrs('char*'),   A) --> [].
match_known_type(str(A),            _, _, string('char*'), A) --> [].
match_known_type(string(A),         _, _, string('char*'), A) --> [].
match_known_type(ptr(A),            _, _, pointer-'void*', A) --> [].
match_known_type(long(A),           _, _, long-long,       A) --> [].
match_known_type(int(A),            _, _, integer-int,     A) --> [].
match_known_type(nnegint(A),        _, _, integer-'unsigned int', A) --> [].
match_known_type(integer(A),        _, _, integer-int,     A) --> [].
match_known_type(character_code(A), _, _, char_code-char,  A) --> [].
match_known_type(char(A),           _, _, char-char,       A) --> [].
match_known_type(num(A),            _, _, float-double,    A) --> [].
match_known_type(size_t(A),         _, _, size_t-size_t,   A) --> [].
match_known_type(float_t(A),        _, _, float_t-float,   A) --> [].
match_known_type(number(A),         _, _, float-double,    A) --> [].
match_known_type(term(A),           _, _, term,            A) --> [].
match_known_type(type(Type, A),     M, N, MSpec,           A) -->
    {nonvar(Type)},
    match_known_type_type(Type, A, M, N, MSpec, A).
match_known_type(array(Type, DimL, A), M, N, MSpec, A) -->
    {nonvar(Type)},
    match_known_array(DimL, Type, A, M, N, MSpec, A),
    !.
match_known_type(MType, M, N, MSpec, A) -->
    { member(MType-MSpec, [ptr( Type, A)-ptr( Spec),
                           list(Type, A)-list(Spec)])
    },
    neck,
    {nonvar(Type)},
    match_known_type_type(Type, E, M, N, Spec, E),
    !.
match_known_type(Type, M, _, tdef(Name, Spec), A) -->
    { type_is_tdef(M, Type, Spec, A),
      functor(Type, Name, _)
    },
    !.
match_known_type(setof(Type, A), M, N, Spec, A) -->
    { nonvar(Type),
      extend_args(Type, [E], Prop)
    },
    match_type(Prop, M, known, N, PSpec, E),
    { ( PSpec = tdef(Name, ESpec)
      ->true
      ; ESpec = PSpec,
        Name = TName
      ),
      ( ESpec = enum(_, C),
        ( C =< 16
        ->TName = short
        ; C =< 32
        ->TName = int
        ; C =< 64
        ->TName = long
        ; C =< 128,
          current_prolog_flag(address_bits, AB),
          AB >= 64
        ->TName = '__int128'
        )
      ->Spec = setof(Name, ESpec)
      ; Spec = list(PSpec)
      )
    }.
match_known_type(Type, M, _, Spec, A) -->
    { compound(Type),
      functor(Type, Name, Arity),
      arg(Arity, Type, A),
      functor(Head, Name, Arity),
      % Note: type_props will call match_unknown_type internally,
      % that is why this clause is only valid for match_known_type
      type_props_(M, TH, TDict, _, Asr),
      type_props2(M, TH, TDict, TypePropLDictL, Asr)
    },
    ( { TypePropLDictL = [Head-[t(Head2, [], _)]],
        Head == Head2
      }
    ->{Spec=cdef(Name)}
    ; { TypePropLDictL = [Head-L],
        forall(member(t(Head, PropL, _), L), PropL = [])
      }
    ->{ length(L, N),
        Spec=enum(Name, N)
      }
    ; { member(_-HeadPropLDictL, TypePropLDictL),
        member(t(Head, PropL, _), HeadPropLDictL),
        PropL \= []
      }
    ->( { PropL = [setof(EType, A)],
          nonvar(EType)
        }
      ->{ Spec=setof(Name, ESpec),
          extend_args(EType, [E], EProp)
        },
        match_type(EProp, M, known, Name, ESpec, E)
      ; {Spec=type(Name)}
      )
    ),
    !.

match_known_type_dict(Prop, Tag, A, Name, type(TypeName)) -->
    { atomic_list_concat([Name, '_', Tag], TypeName),
      Type =.. [TypeName, A]
    },
    [Prop=Type].

type_is_tdef(M, Type, Spec, A) :-
    compound(Type),
    functor(Type, TName, Arity),
    arg(Arity, Type, A),
    functor(Head, TName, Arity),
    type_props_(M, Head, _, _, Asr),
    \+ curr_prop_asr(comp, _, _, Asr),
    bind_type_names(M:Head, TypeMPropLDictL),
    TypeMPropLDictL = [t(Head, [Prop], _)],
    arg(Arity, Head, A),
    arg(Arity, Prop, B),
    A==B,
    match_known_type(Prop, M, TName, Spec, A),
    !.

bind_argument(Head, M, CM, CompL, CallL, SuccL, GlobL, Arg, Spec, Mode) :-
    functor(Head, Name, _),
    ( member(Comp, CompL),
      once(match_known_type(Comp, CM, Name, Spec, Arg1)),
      Arg1 == Arg
    ->true
    ; true
    ),
    ( member(Call, CallL),
      once(match_known_type(Call, CM, Name, Spec, Arg1)),
      Arg1 == Arg
    ->Mode = in
    ; true
    ),
    ( member(Succ, SuccL),
      once(match_known_type(Succ, CM, Name, Spec, Arg1)),
      Arg1 == Arg
    ->Mode = out
    ; true
    ),
    ( memberchk(type(_), GlobL),
      once(match_known_type(Head, M, Name, Spec, Arg1)),
      Arg1 == Arg
    ->Mode = in
    ; true
    ),
    ignore(Mode = inout),
    ignore(Spec = term).

:- public call_idx/2.
:- meta_predicate call_idx(0, -).
call_idx(Call, Idx) :-
    findall(Ref, once(call_ref(Call, Ref)), [Ref]), % avoid unifications
    nth_clause(_, Idx1, Ref),
    succ(Idx, Idx1).

:- public idx_call/2.
:- meta_predicate idx_call(+, 0).
idx_call(Idx1, Call) :-
    succ(Idx1, Idx),
    nth_clause(Call, Idx, Ref),
    clause(Call, _, Ref).
