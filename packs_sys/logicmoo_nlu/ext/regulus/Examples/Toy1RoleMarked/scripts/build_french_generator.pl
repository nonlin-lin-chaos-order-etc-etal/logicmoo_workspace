
% Compile the main Regulus code
:- compile('$REGULUS/Prolog/load').

% Compile generator
:- regulus_batch('$REGULUS/Examples/Toy1RoleMarked/scripts/toy1_french_generation.cfg', ["LOAD_GENERATION"]).

:- halt.


