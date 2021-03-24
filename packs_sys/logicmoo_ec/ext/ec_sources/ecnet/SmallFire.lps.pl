% From E: 
% 
% ':-'(call_pel_directive(translate(unskipped,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SmallFire.lps.pl'))).
:- call_pel_directive(translate(unskipped,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SmallFire.lps.pl')).
:-include(library('ec_planner/ec_test_incl')).
:-expects_dialect(lps).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleeping.e',87).
% From E: 
% 
% ':-'(call_pel_directive(translate(begining,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SmallFire.lps.pl'))).
:- call_pel_directive(translate(begining,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SmallFire.lps.pl')).
% Tue, 23 Mar 2021 19:06:57 GMT File: <stream>(0x555567c04f00)%;
%; Copyright (c) 2005 IBM Corporation and others.
%; All rights reserved. This program and the accompanying materials
%; are made available under the terms of the Common Public License v1.0
%; which accompanies this distribution, and is available at
%; http://www.eclipse.org/legal/cpl-v10.html
%;
%; Contributors:
%; IBM - Initial implementation
%;
%; SmallFire: matches, lighters, cigarettes, etc.
%;

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SmallFire.e',14).
% event Light(agent,physobj)
% From E: 
% 
% event(light(agent,physobj)).
events([light/2]).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SmallFire.e',14).
mpred_prop(light(agent,physobj),action).
actions([light/2]).

% event LightWith(agent,physobj,physobj)
% From E: 
% 
% event(lightWith(agent,physobj,physobj)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SmallFire.e',14).
events([lightWith/3]).
mpred_prop(lightWith(agent,physobj,physobj),action).
actions([lightWith/3]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SmallFire.e',16).
% event PutOut(agent,physobj)
% From E: 
% 
% event(putOut(agent,physobj)).
events([putOut/2]).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SmallFire.e',16).
mpred_prop(putOut(agent,physobj),action).
actions([putOut/2]).

% event BlowOut(agent,physobj)
% From E: 
% 
% event(blowOut(agent,physobj)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SmallFire.e',16).
events([blowOut/2]).
mpred_prop(blowOut(agent,physobj),action).
actions([blowOut/2]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SmallFire.e',18).
% fluent IsBurning(physobj)
% From E: 
% 
% fluent(isBurning(physobj)).
mpred_prop(isBurning(physobj),fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SmallFire.e',18).
fluents([isBurning/1]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SmallFire.e',20).
% [agent,physobj1,physobj2,time]
% HoldsAt(IsBurning(physobj2),time) ->
% Initiates(LightWith(agent,physobj1,physobj2),
%           IsBurning(physobj1),
%           time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SmallFire.e',20).
% From E: 
% 
% '->'(
%    holds(
%       isBurning(Physobj2), 
%       Time), 
%    initiates_at(
%       lightWith(Agent,Physobj1,Physobj2), 
%       isBurning(Physobj1), 
%       Time)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SmallFire.e',20).
if(initiates(lightWith(Agent,Physobj1,Physobj2),
	     at(isBurning(Physobj1),Time)),
   at(isBurning(Physobj2),Time)).


% [agent,physobj1,physobj2,time]
% Happens(LightWith(agent,physobj1,physobj2),time) ->
% HoldsAt(Holding(agent,physobj1),time) &
% HoldsAt(Holding(agent,physobj2),time) &
% !HoldsAt(IsBurning(physobj1),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SmallFire.e',27).
% From E: 
% 
% '->'(
%    happens(
%       lightWith(Agent,Physobj1,Physobj2), 
%       Time), 
%    ','(
%       holds(
%          holding(Agent,Physobj1), 
%          Time), 
%       ','(
%          holds(
%             holding(Agent,Physobj2), 
%             Time), 
%          holds(
%             not(isBurning(Physobj1)), 
%             Time)))).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SmallFire.e',27).
if((at(holding(Agent, Physobj1), Time), at(holding(Agent, Physobj2), Time), at(not(isBurning(Physobj1)), Time)), happens(lightWith(Agent, Physobj1, Physobj2), Time)).


% [agent,physobj,time]
% Initiates(Light(agent,physobj),
%           IsBurning(physobj),
%           time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SmallFire.e',33).
% From E: 
% 
% initiates_at(
%    light(Agent,Physobj), 
%    isBurning(Physobj), 
%    Time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SmallFire.e',33).
initiates(light(Agent,Physobj),isBurning(Physobj)).


% [agent,physobj,time]
% Happens(Light(agent,physobj),time) ->
% HoldsAt(Holding(agent,physobj),time) &
% !HoldsAt(IsBurning(physobj),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SmallFire.e',38).
% From E: 
% 
% '->'(
%    happens(
%       light(Agent,Physobj), 
%       Time), 
%    ','(
%       holds(
%          holding(Agent,Physobj), 
%          Time), 
%       holds(
%          not(isBurning(Physobj)), 
%          Time))).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SmallFire.e',38).
if((at(holding(Agent, Physobj), Time), at(not(isBurning(Physobj)), Time)), happens(light(Agent, Physobj), Time)).


% [agent,physobj,time]
% Terminates(PutOut(agent,physobj),
%            IsBurning(physobj),
%            time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SmallFire.e',43).
% From E: 
% 
% terminates_at(
%    putOut(Agent,Physobj), 
%    isBurning(Physobj), 
%    Time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SmallFire.e',43).
terminates(putOut(Agent,Physobj),isBurning(Physobj)).


% [agent,physobj,time]
% Happens(PutOut(agent,physobj),time) ->
% HoldsAt(Holding(agent,physobj),time) &
% HoldsAt(IsBurning(physobj),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SmallFire.e',48).
% From E: 
% 
% '->'(
%    happens(
%       putOut(Agent,Physobj), 
%       Time), 
%    ','(
%       holds(
%          holding(Agent,Physobj), 
%          Time), 
%       holds(
%          isBurning(Physobj), 
%          Time))).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SmallFire.e',48).
if((at(holding(Agent, Physobj), Time), at(isBurning(Physobj), Time)), happens(putOut(Agent, Physobj), Time)).


% [agent,physobj,time]
% Terminates(BlowOut(agent,physobj),
%            IsBurning(physobj),
%            time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SmallFire.e',53).
% From E: 
% 
% terminates_at(
%    blowOut(Agent,Physobj), 
%    isBurning(Physobj), 
%    Time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SmallFire.e',53).
terminates(blowOut(Agent,Physobj),isBurning(Physobj)).


% [agent,physobj,time]
% Happens(BlowOut(agent,physobj),time) ->
% HoldsAt(Holding(agent,physobj),time) &
% HoldsAt(IsBurning(physobj),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SmallFire.e',58).
% From E: 
% 
% '->'(
%    happens(
%       blowOut(Agent,Physobj), 
%       Time), 
%    ','(
%       holds(
%          holding(Agent,Physobj), 
%          Time), 
%       holds(
%          isBurning(Physobj), 
%          Time))).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SmallFire.e',58).
if((at(holding(Agent, Physobj), Time), at(isBurning(Physobj), Time)), happens(blowOut(Agent, Physobj), Time)).


%; End of file.
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SmallFire.e',61).
% From E: 
% 
% ':-'(call_pel_directive(translate(ending,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SmallFire.lps.pl'))).
:- call_pel_directive(translate(ending,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SmallFire.lps.pl')).
