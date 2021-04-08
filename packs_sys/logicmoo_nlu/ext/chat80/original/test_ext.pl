% load.pl : Load Chat-80, for Quintus Prolog

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
|	This program may be used, copied, altered or ensure_loadedd in other	  |
|	programs only for academic purposes and provided that the	  |
|	authorship of the initial program is aknowledged.		  |
|	Use for commercial purposes without the previous written 	  |
|	agreement of the authors is forbidden.				  |
|_________________________________________________________________________|

*/

:- module(baseKB,[]).

:- use_module(library(statistics)).

:- autoload_all.
:- use_module(library(logicmoo_common)).
:- xlisting(lock_predicate/1).
:- autoload_all.

:- module(baseKB).


:- if( \+ current_predicate( (share_mp)/1)).
share_mp(MFA):- MFA=M:_FA,!, % FA = F/A,   
   (M:multifile(MFA)), 
   (M:module_transparent(MFA)),
   (M:dynamic(MFA)),
   (M:export(MFA)),
   (M:public(MFA)), !. 
share_mp(FA):- strip_module(FA,M,_),!,share_mp(M:FA).

:- endif.

use_pfc80:- fail.

:- 
 include(load).


:- fixup_exports.
