% From E: 
% 
% ':-'(call_pel_directive(translate(unskipped,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/HungerNeed.lps.pl'))).
:- call_pel_directive(translate(unskipped,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/HungerNeed.lps.pl')).
:-include(library('ec_planner/ec_test_incl')).
:-expects_dialect(lps).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/HandTo.e',30).
% From E: 
% 
% ':-'(call_pel_directive(translate(begining,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/HungerNeed.lps.pl'))).
:- call_pel_directive(translate(begining,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/HungerNeed.lps.pl')).
% Tue, 23 Mar 2021 19:06:50 GMT File: <stream>(0x5555681cd300)


%;
%; Copyright (c) 2005 IBM Corporation and others.
%; All rights reserved. This program and the accompanying materials
%; are made available under the terms of the Common Public License v1.0
%; which accompanies this distribution, and is available at
%; http://www.eclipse.org/legal/cpl-v10.html
%;
%; Contributors:
%; IBM - Initial implementation
%;
%; hunger need
%;

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/HungerNeed.e',14).
% fluent Hungry(agent)
% From E: 
% 
% fluent(hungry(agent)).
mpred_prop(hungry(agent),fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/HungerNeed.e',14).
fluents([hungry/1]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/HungerNeed.e',16).
% fluent Satiated(agent)
% From E: 
% 
% fluent(satiated(agent)).
mpred_prop(satiated(agent),fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/HungerNeed.e',16).
fluents([satiated/1]).

% noninertial Satiated
% From E: 
% 
% ':-'(call_pel_directive(noninertial(satiated))).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/HungerNeed.e',16).
:- call_pel_directive(noninertial(satiated)).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/HungerNeed.e',19).
% [agent,time]
 % HoldsAt(Hungry(agent),time) <-> !HoldsAt(Satiated(agent),time).
% From E: 
% 
% <->(
%    holds(
%       hungry(Agent), 
%       Time), 
%    holds(
%       not(satiated(Agent)), 
%       Time)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/HungerNeed.e',19).
at(hungry(Agent), Time) <->
    at(not(satiated(Agent)), Time).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/HungerNeed.e',21).
% event Eat(agent,food)
% From E: 
% 
% event(eat(agent,food)).
events([eat/2]).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/HungerNeed.e',21).
mpred_prop(eat(agent,food),action).
actions([eat/2]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/HungerNeed.e',23).
% [agent,food,time]
% Happens(Eat(agent,food),time) ->
% {location}% 
% HoldsAt(At(agent,location),time) &
% HoldsAt(At(food,location),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/HungerNeed.e',25).
% From E: 
% 
% exists(Location, 
%    '->'(
%       happens(
%          eat(Agent,Food), 
%          Time), 
%       ','(
%          holds(
%             at_loc(Agent,Location), 
%             Time), 
%          holds(
%             at_loc(Food,Location), 
%             Time)))).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/HungerNeed.e',25).
exists(Location, if((at(at_loc(Agent, Location), Time), at(at_loc(Food, Location), Time)), happens(eat(Agent, Food), Time))).


% [agent,food,time]
% Terminates(Eat(agent,food),Hungry(agent),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/HungerNeed.e',30).
% From E: 
% 
% terminates_at(
%    eat(Agent,Food), 
%    hungry(Agent), 
%    Time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/HungerNeed.e',30).
terminates(eat(Agent,Food),hungry(Agent)).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/HungerNeed.e',32).
%; End of file.
% From E: 
% 
% ':-'(call_pel_directive(translate(ending,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/HungerNeed.lps.pl'))).
:- call_pel_directive(translate(ending,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/HungerNeed.lps.pl')).
