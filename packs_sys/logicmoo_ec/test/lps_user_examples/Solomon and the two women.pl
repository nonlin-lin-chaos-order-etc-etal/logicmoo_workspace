:- expects_dialect(lps).

% Logic Production Systems

% declarations, initial state, observations, reactive rules, logic programs, causal laws

% Your program goes here

% Story version of King Solomon and the two women(Kings 3: 3-28)
% Vanessa Marquez Logica B2018

maxtime(10).

% Event dispute between a and b, test a and b, make decision

events
     dispute_between(a, b), 
	 put_to_test(a, b),
     propose_drastic_output(agent), 
     make_decision.

% Action
% Solomon proposes a drastic exit
% Solomon proposes dividing the child in two
% Deliver the living child to the real mother
actions propose_cut_baby(agent),
  says(agent, message),
  declares(agent, verdict),
  decide(agent, judgment).

% I observe

observe dispute_between(a, b) from 1 to 2.

% If a dispute arises between a and b in T1 and T2
% Then I test a a and b in T2 and T3
if dispute_between(A, B) from T1 to T2
then put_a_test(A, B) from T2 to T3.

% I test a a and b in T2 and T3
% If I propose drastic exit in T2 and T3
% I propose drastic exit between T2 and T3
% If I divide the child in two in T2 and T3
put_a_test(A, B) from T1 to T2 if
   proposes_nasty_idea(salomon) from T1 to T2.

proposes_nasty_idea(A) from T1 to T2 if
  propose_cut_baby(A) from T1 to T2.

% If King Solomon proposes to divide the child in two in T2 and T3
% Then I say do not divide it I give it to her in T2 and
if propose_cut_baby(salomon) from T1 to T2,
   woman(X),
   i_am_your_mother(X)
then says(X, 'Dont kill him! Give it to Her') from T2 to T3.

% If King Solomon proposes to divide the child in two in T2 and T3
% So I say if divide it into T2 and T3
if propose_cut_baby(salomon) from T1 to T2,
   woman(X),
   not(i_am_your_mother(X))
then says(X, 'Yes, kill him') from T2 to T3.

% Yes I test a a and b in T2 and T3 and
% a says that if I divide it and
% b says dont split it
% So I know that the real mother is b in T2 and T3
if propose_cut_baby(Judge) from T1 to T2,
   says(X, 'Dont kill him! Give it to Her') from T3 to T4,
   says(_A, 'Yes, kill him') from T5 to T6
then 
  declare(Judge, the_true_mother_is(X)) from T7 to T8,
   decide(Judge, give_baby_to(X)) from T8 to T9.

woman(a).
woman(b).
i_am_your_mother(b).
/** <examples> 
?- go(Timeline).
*/
