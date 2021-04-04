/*
 _________________________________________________________________________
|	Copyright (C) 1982						  |
|									  |
|	David Warren,							  |
|		SRI International, 333 Ravenswood Ave., Menlo Park,	  |
|		California 94025, USA;					  |
|									  |
|	Fernando Pereira,						  |
|		Dept. of Architecture, University of Edinburgh,		  |
|		20 Chambers St., Edinburgh EH1 1JZ, Scotland		  |
|									  |
|	This program may be used, copied, altered mask_or included in other	  |
|	programs only for academic purposes and provided that the	  |
|	authorship of the initial program is aknowledged.		  |
|	Use for commercial purposes without the previous written 	  |
|	agreement of the authors is forbidden.				  |
|_________________________________________________________________________|

*/

% Normal form masks

mask_is_pp(#(1,_,_,_)).

mask_is_pred(#(_,1,_,_)).

mask_is_trace(#(_,_,1,_)).

mask_is_adv(#(_,_,_,1)).

mask_trace2(#(_,_,1,_),#(0,0,0,0)).

mask_trace1(#(0,0,1,0)).

mask_adv(#(0,0,0,1)).

mask_empty(#(0,0,0,0)).

mask_np_all(#(1,1,1,0)).

mask_s_all(#(1,0,1,1)).

mask_np_no_trace(#(1,1,0,0)).

% Mask operations

mask_plus(#(B1,B2,B3,B4),#(C1,C2,C3,C4),#(D1,D2,D3,D4)) :-
   mask_or(B1,C1,D1),
   mask_or(B2,C2,D2),
   mask_or(B3,C3,D3),
   mask_or(B4,C4,D4).

mask_minus(#(B1,B2,B3,B4),#(C1,C2,C3,C4),#(D1,D2,D3,D4)) :-
   mask_anot(B1,C1,D1),
   mask_anot(B2,C2,D2),
   mask_anot(B3,C3,D3),
   mask_anot(B4,C4,D4).

mask_or(1,_,1).
mask_or(0,1,1).
mask_or(0,0,0).

mask_anot(X,0,X).
mask_anot(_X,1,0).

% Noun phrase position features

mask_role(subj,_,#(1,0,0)).
mask_role(compl,_,#(0,_,_)).
mask_role(undef,main,#(_,0,_)).
mask_role(undef,aux,#(0,_,_)).
mask_role(undef,decl,_).
mask_role(nil,_,_).

mask_subj_case(#(1,0,0)).
mask_verb_case(#(0,1,0)).
mask_prep_case(#(0,0,1)).
mask_compl_case(#(0,_,_)).

portray_bit(Bit,Value,[?(Bit)|Bits],Bits) :- var(Value), !.
portray_bit(Bit,1,[Bit|Bits],Bits).
portray_bit(_Bit,0,Bits,Bits).

user:portray(Comp) :- compound(Comp),
   #(PP,Pred,Trace,Adv) = Comp,
   portray_bit(pp,PP,S0,S1),
   portray_bit(pred,Pred,S1,S2),
   portray_bit(trace,Trace,S2,S3),
   portray_bit(adv,Adv,S3,[]),
   write(S0).

:- fixup_exports.
