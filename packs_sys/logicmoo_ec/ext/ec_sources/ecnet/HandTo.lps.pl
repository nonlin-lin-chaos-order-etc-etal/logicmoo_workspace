% From E: 
% 
% ':-'(call_pel_directive(translate(unskipped,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/HandTo.lps.pl'))).
:- call_pel_directive(translate(unskipped,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/HandTo.lps.pl')).
:-include(library('ec_planner/ec_test_incl')).
:-expects_dialect(lps).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Gun.e',82).
% From E: 
% 
% ':-'(call_pel_directive(translate(begining,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/HandTo.lps.pl'))).
:- call_pel_directive(translate(begining,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/HandTo.lps.pl')).
% Tue, 23 Mar 2021 19:06:50 GMT File: <stream>(0x555567199e00)%;
%; Copyright (c) 2005 IBM Corporation and others.
%; All rights reserved. This program and the accompanying materials
%; are made available under the terms of the Common Public License v1.0
%; which accompanies this distribution, and is available at
%; http://www.eclipse.org/legal/cpl-v10.html
%;
%; Contributors:
%; IBM - Initial implementation
%;

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/HandTo.e',12).
% event HandTo(agent,agent,physobj)
% From E: 
% 
% event(handTo(agent,agent,physobj)).
events([handTo/3]).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/HandTo.e',12).
mpred_prop(handTo(agent,agent,physobj),action).
actions([handTo/3]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/HandTo.e',14).
% [agent1,agent2,physobj,time]
% Initiates(HandTo(agent1,agent2,physobj),
%           Holding(agent2,physobj),
%           time).
% From E: 
% 
% initiates_at(
%    handTo(Agent1,Agent2,Physobj), 
%    holding(Agent2,Physobj), 
%    Time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/HandTo.e',14).
initiates(handTo(Agent1,Agent2,Physobj),
	  holding(Agent2,Physobj)).


% [agent1,agent2,physobj,time]
% Terminates(HandTo(agent1,agent2,physobj),
%            Holding(agent1,physobj),
%            time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/HandTo.e',20).
% From E: 
% 
% terminates_at(
%    handTo(Agent1,Agent2,Physobj), 
%    holding(Agent1,Physobj), 
%    Time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/HandTo.e',20).
terminates(handTo(Agent1,Agent2,Physobj),
	   holding(Agent1,Physobj)).


% [agent1,agent2,physobj,time]
% Happens(HandTo(agent1,agent2,physobj),time) ->
% HoldsAt(Holding(agent1,physobj),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/HandTo.e',25).
% From E: 
% 
% '->'(
%    happens(
%       handTo(Agent1,Agent2,Physobj), 
%       Time), 
%    holds(
%       holding(Agent1,Physobj), 
%       Time)).
if(at(holding(Agent1,Physobj),Time),
   happens(handTo(Agent1,Agent2,Physobj),Time)).

% event ShakeHands(agent,agent)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/HandTo.e',27).
% From E: 
% 
% event(shakeHands(agent,agent)).
events([shakeHands/2]).
mpred_prop(shakeHands(agent,agent),action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/HandTo.e',27).
actions([shakeHands/2]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/HandTo.e',30).
% event WriteOn(agent,paper,pen)
% From E: 
% 
% event(writeOn(agent,paper,pen)).
events([writeOn/3]).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/HandTo.e',30).
mpred_prop(writeOn(agent,paper,pen),action).
actions([writeOn/3]).
% From E: 
% 
% ':-'(call_pel_directive(translate(ending,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/HandTo.lps.pl'))).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/HandTo.e',30).
:- call_pel_directive(translate(ending,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/HandTo.lps.pl')).
