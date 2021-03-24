% From E: 
% 
% ':-'(call_pel_directive(translate(unskipped,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Fire.lps.pl'))).
:- call_pel_directive(translate(unskipped,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Fire.lps.pl')).
:-include(library('ec_planner/ec_test_incl')).
:-expects_dialect(lps).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Feeling.e',133).
% From E: 
% 
% ':-'(call_pel_directive(translate(begining,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Fire.lps.pl'))).
:- call_pel_directive(translate(begining,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Fire.lps.pl')).
% Tue, 23 Mar 2021 19:06:50 GMT File: <stream>(0x555569f41500)%;
%; Copyright (c) 2005 IBM Corporation and others.
%; All rights reserved. This program and the accompanying materials
%; are made available under the terms of the Common Public License v1.0
%; which accompanies this distribution, and is available at
%; http://www.eclipse.org/legal/cpl-v10.html
%;
%; Contributors:
%; IBM - Initial implementation
%;
%; fire
%;
%; agent sets fire to physobj with burn time offset.

% event SetFireTo(agent,physobj,fire,offset)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Fire.e',14).
% From E: 
% 
% event(setFireTo(agent, physobj, fire, 
%          offset)).
events([setFireTo/4]).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Fire.e',14).
mpred_prop(setFireTo(agent,physobj,fire,offset),action).
actions([setFireTo/4]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Fire.e',17).
%; An effect axioms states that
%; if an agent sets a fire to a physical object with a burn time,
%; the physical object will be burning with the fire and burn time:
% [agent,physobj,fire,offset,time]
% Initiates(SetFireTo(agent,physobj,fire,offset),
%           Burning(physobj,fire,offset),
%           time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Fire.e',20).
% From E: 
% 
% initiates_at(
%    setFireTo(Agent, Physobj, Fire, 
%       Offset), 
%    burning(Physobj,Fire,Offset), 
%    Time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Fire.e',20).
initiates(setFireTo(Agent,Physobj,Fire,Offset),
	  burning(Physobj,Fire,Offset)).


%; agent puts out fire on physobj.

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Fire.e',26).
% event PutOutFire(agent,physobj,fire)
% From E: 
% 
% event(putOutFire(agent,physobj,fire)).
events([putOutFire/3]).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Fire.e',26).
mpred_prop(putOutFire(agent,physobj,fire),action).
actions([putOutFire/3]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Fire.e',28).
%; An effect axiom states that
%; if an agent puts out a fire on a physical object,
%; the physical object will no longer be burning:
% [agent,physobj,fire,offset,time]
% Terminates(PutOutFire(agent,physobj,fire),
%            Burning(physobj,fire,offset),
%            time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Fire.e',31).
% From E: 
% 
% terminates_at(
%    putOutFire(Agent,Physobj,Fire), 
%    burning(Physobj,Fire,Offset), 
%    Time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Fire.e',31).
terminates(putOutFire(Agent,Physobj,Fire),
	   burning(Physobj,Fire,Offset)).


%; A precondition axiom states that
%; for an agent to set fire to a physical object,
%; there must be a location such that
%; the agent is at the location and
%; the physical object is at the location:
% [agent,fire,physobj,offset,time]
% Happens(SetFireTo(agent,physobj,fire,offset),time) ->
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Fire.e',41).
% {location}%  HoldsAt(At(agent,location),time) &
%            HoldsAt(At(physobj,location),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Fire.e',43).
% From E: 
% 
% exists(Location, 
%    '->'(
%       happens(
%          setFireTo(Agent, Physobj, Fire, 
%             Offset), 
%          Time), 
%       ','(
%          holds(
%             at_loc(Agent,Location), 
%             Time), 
%          holds(
%             at_loc(Physobj,Location), 
%             Time)))).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Fire.e',43).
exists(Location, if((at(at_loc(Agent, Location), Time), at(at_loc(Physobj, Location), Time)), happens(setFireTo(Agent, Physobj, Fire, Offset), Time))).


%; A precondition axiom states that
%; for an agent to put out a fire on a physical object,
%; there must be a location such that
%; the agent is at the location and
%; the physical object is at the location:
% [agent,fire,physobj,time]
% Happens(PutOutFire(agent,physobj,fire),time) ->
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Fire.e',51).
% {location}%  HoldsAt(At(agent,location),time) &
%            HoldsAt(At(physobj,location),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Fire.e',53).
% From E: 
% 
% exists(Location, 
%    '->'(
%       happens(
%          putOutFire(Agent,Physobj,Fire), 
%          Time), 
%       ','(
%          holds(
%             at_loc(Agent,Location), 
%             Time), 
%          holds(
%             at_loc(Physobj,Location), 
%             Time)))).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Fire.e',53).
exists(Location, if((at(at_loc(Agent, Location), Time), at(at_loc(Physobj, Location), Time)), happens(putOutFire(Agent, Physobj, Fire), Time))).


%; physobj is burning with fire and burn time offset.

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Fire.e',57).
% fluent Burning(physobj,fire,offset)
% From E: 
% 
% fluent(burning(physobj,fire,offset)).
mpred_prop(burning(physobj,fire,offset),fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Fire.e',57).
fluents([burning/3]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Fire.e',59).
%; A state constraint says that a physical object burning with
%; a fire has at most one burn time at a time:
% [physobj,fire,offset1,offset2,time]
% HoldsAt(Burning(physobj,fire,offset1),time) &
% HoldsAt(Burning(physobj,fire,offset2),time) ->
% offset1=offset2.
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Fire.e',61).
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          burning(Physobj,Fire,Offset1), 
%          Time), 
%       holds(
%          burning(Physobj,Fire,Offset2), 
%          Time)), 
%    Offset1=Offset2).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Fire.e',61).
if(equals(Offset1, Offset2),  (at(burning(Physobj, Fire, Offset1), Time), at(burning(Physobj, Fire, Offset2), Time))).


%; The burn time of physobj is decremented.

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Fire.e',67).
% event DecrementBurning(physobj)
% From E: 
% 
% event(decrementBurning(physobj)).
mpred_prop(decrementBurning(physobj),event).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Fire.e',67).
events([decrementBurning/1]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Fire.e',69).
%; A trigger axiom states that
%; if a physical object is burning with a fire and a burn time and
%; the burn time is greater than zero,
%; the burn time of the physical object is decremented:
% [physobj,fire,offset,time]
% HoldsAt(Burning(physobj,fire,offset),time) &
% (offset > 0) ->
% Happens(DecrementBurning(physobj),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Fire.e',73).
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          burning(Physobj,Fire,Offset), 
%          Time), 
%       Offset>0), 
%    happens(
%       decrementBurning(Physobj), 
%       Time)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Fire.e',73).
if(happens(decrementBurning(Physobj), Time),  (at(burning(Physobj, Fire, Offset), Time), comparison(Offset, 0, >))).


%; An effect axiom states that if a physical object is
%; burning with a fire and a burn time, and the burn time of a physical
%; object is decremented, the burn time of the physical
%; object will be the burn time minus one:
% [physobj,fire,offset1,offset2,time]
% HoldsAt(Burning(physobj,fire,offset1),time) &
% offset2 = offset1-1 ->
% Initiates(DecrementBurning(physobj),
%           Burning(physobj,fire,offset2),
%           time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Fire.e',82).
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          burning(Physobj,Fire,Offset1), 
%          Time), 
%       Offset2=Offset1-1), 
%    initiates_at(
%       decrementBurning(Physobj), 
%       burning(Physobj,Fire,Offset2), 
%       Time)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Fire.e',82).
if(initiates(decrementBurning(Physobj), at(burning(Physobj, Fire, Offset2), Time)),  (at(burning(Physobj, Fire, Offset1), Time), equals(Offset2, Offset1-1))).


%; An effect axiom states that if a physical object is
%; burning with a fire and a burn time, and the burn time of a physical
%; object is decremented, the burn time of the physical
%; object will no longer be the burn time:
% [physobj,fire,offset,time]
% HoldsAt(Burning(physobj,fire,offset),time) ->
% Terminates(DecrementBurning(physobj),
%            Burning(physobj,fire,offset),
%            time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Fire.e',93).
% From E: 
% 
% '->'(
%    holds(
%       burning(Physobj,Fire,Offset), 
%       Time), 
%    terminates_at(
%       decrementBurning(Physobj), 
%       burning(Physobj,Fire,Offset), 
%       Time)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Fire.e',93).
if(terminates(decrementBurning(Physobj),
	      at(burning(Physobj,Fire,Offset),
		 Time)),
   at(burning(Physobj,Fire,Offset),Time)).


%; A trigger axiom states that
%; if a physical object is burning with a fire and a burn time
%; that is not equal to zero, the fire will damage the
%; physical object:
% [physobj,fire,offset,time]
% offset!=% 0 &
% HoldsAt(Burning(physobj,fire,offset),time) &
% HoldsAt(Intact(physobj),time) ->
% Happens(Damage(fire,physobj),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Fire.e',103).
% From E: 
% 
% '->'(
%    ','(
%       Offset\=0, 
%       ','(
%          holds(
%             burning(Physobj,Fire,Offset), 
%             Time), 
%          holds(
%             intact(Physobj), 
%             Time))), 
%    happens(
%       damage(Fire,Physobj), 
%       Time)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Fire.e',103).
if(happens(damage(Fire, Physobj), Time),  ({dif(Offset, 0)}, at(burning(Physobj, Fire, Offset), Time), at(intact(Physobj), Time))).


%; A trigger axiom states that
%; if a physical object is burning with a fire and a burn time
%; that is equal to zero, the fire will destroy the
%; physical object:
% [physobj,fire,time]
% HoldsAt(Burning(physobj,fire,0),time) &
% !HoldsAt(Destroyed(physobj),time) ->
% Happens(Destroy(fire,physobj),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Fire.e',113).
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          burning(Physobj,Fire,0), 
%          Time), 
%       holds(
%          not(destroyed(Physobj)), 
%          Time)), 
%    happens(
%       destroy(Fire,Physobj), 
%       Time)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Fire.e',113).
if(happens(destroy(Fire, Physobj), Time),  (at(burning(Physobj, Fire, 0), Time), at(not(destroyed(Physobj)), Time))).


%; An effect axiom states that if a fire destroys a physical
%; object, the physical object will no longer be burning:
% [physobj,fire,offset,time]
% Terminates(Destroy(fire,physobj),
%            Burning(physobj,fire,offset),
%            time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Fire.e',120).
% From E: 
% 
% terminates_at(
%    destroy(Fire,Physobj), 
%    burning(Physobj,Fire,Offset), 
%    Time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Fire.e',120).
terminates(destroy(Fire,Physobj),
	   burning(Physobj,Fire,Offset)).


%; End of file.
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Fire.e',124).
% From E: 
% 
% ':-'(call_pel_directive(translate(ending,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Fire.lps.pl'))).
:- call_pel_directive(translate(ending,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Fire.lps.pl')).
