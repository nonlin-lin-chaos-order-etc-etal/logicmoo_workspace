% From E: 
% 
% ':-'(call_pel_directive(translate(unskipped,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.lps.pl'))).
:- call_pel_directive(translate(unskipped,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.lps.pl')).
:-include(library('ec_planner/ec_test_incl')).
:-expects_dialect(lps).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/CTime.e',43).
% From E: 
% 
% ':-'(call_pel_directive(translate(begining,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.lps.pl'))).
:- call_pel_directive(translate(begining,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.lps.pl')).
% Tue, 23 Mar 2021 19:22:56 GMT File: <stream>(0x555567b86300)%;
%; Copyright (c) 2005 IBM Corporation and others.
%; All rights reserved. This program and the accompanying materials
%; are made available under the terms of the Common Public License v1.0
%; which accompanies this distribution, and is available at
%; http://www.eclipse.org/legal/cpl-v10.html
%;
%; Contributors:
%; IBM - Initial implementation
%;
%; scuba diving
%;

% sort object
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',14).
% From E: 
% 
% sort(object).
sort(object).

% sort agent: object
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',14).
% From E: 
% 
% subsort(agent,object).
subsort(agent, object).

% sort diver: agent
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',16).
% From E: 
% 
% subsort(diver,agent).
subsort(diver, agent).

% sort depth: integer
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',16).
% From E: 
% 
% subsort(depth,integer).
subsort(depth, integer).

% sort boat: object
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',18).
% From E: 
% 
% subsort(boat,object).
subsort(boat, object).
%; reference line, anchor line, shotline, SMB line, ...

% sort line: object
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',20).
% From E: 
% 
% subsort(line,object).
subsort(line, object).

% sort equipment: object
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',23).
% From E: 
% 
% subsort(equipment,object).
subsort(equipment, object).

% sort weight: equipment
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',23).
% From E: 
% 
% subsort(weight,equipment).
subsort(weight, equipment).

% sort fin: equipment
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',25).
% From E: 
% 
% subsort(fin,equipment).
subsort(fin, equipment).

% sort airtank: equipment
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',25).
% From E: 
% 
% subsort(airtank,equipment).
subsort(airtank, equipment).
%; buoyancy compensator (BC)
%; buoyancy control device (BCD)

% sort computer: equipment
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',30).
% From E: 
% 
% subsort(computer,equipment).
subsort(computer, equipment).

% sort bc: equipment
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',30).
% From E: 
% 
% subsort(bc,equipment).
subsort(bc, equipment).

% fluent AtDepth(object,depth)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',33).
% From E: 
% 
% fluent(atDepth(object,depth)).
mpred_prop(atDepth(object, depth), fluent).
fluents([atDepth/2]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',35).
% [object,depth1,depth2,time]
% HoldsAt(AtDepth(object,depth1),time) &
% HoldsAt(AtDepth(object,depth2),time) ->
% depth1 = depth2.
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          atDepth(Object,Depth1), 
%          Time), 
%       holds(
%          atDepth(Object,Depth2), 
%          Time)), 
%    Depth1=Depth2).
equals(Depth1, Depth2)if atDepth(Object, Depth1)at Time, atDepth(Object, Depth2)at Time.
 %  if(equals(Depth1, Depth2),  (at(atDepth(Object, Depth1), Time), at(atDepth(Object, Depth2), Time))).
 %  "% =================================".

% event Ascend(diver,depth)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',39).
% From E: 
% 
% event(ascend(diver,depth)).
events([ascend/2]).
mpred_prop(ascend(diver, depth), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',39).
actions([ascend/2]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',42).
% event Descend(diver,depth)
% From E: 
% 
% event(descend(diver,depth)).
events([descend/2]).
mpred_prop(descend(diver, depth), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',42).
actions([descend/2]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',44).
% [diver,depth1,depth2,time]
% HoldsAt(AtDepth(diver,depth1),time) &
% Happens(Descend(diver,depth2),time) ->
% depth2>depth1.
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          atDepth(Diver,Depth1), 
%          Time), 
%       happens(
%          descend(Diver,Depth2), 
%          Time)), 
%    Depth2>Depth1).
comparison(Depth2, Depth1, >)if atDepth(Diver, Depth1)at Time, happens(descend(Diver, Depth2), Time).
 %  if(comparison(Depth2, Depth1, >),  (at(atDepth(Diver, Depth1), Time), happens(descend(Diver, Depth2), Time))).
 %  "% =================================".


% [diver,depth1,depth2,time]
% HoldsAt(AtDepth(diver,depth1),time) &
% Happens(Ascend(diver,depth2),time) ->
% depth2<depth1.
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',50).
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          atDepth(Diver,Depth1), 
%          Time), 
%       happens(
%          ascend(Diver,Depth2), 
%          Time)), 
%    Depth2<Depth1).
comparison(Depth2, Depth1, <)if atDepth(Diver, Depth1)at Time, happens(ascend(Diver, Depth2), Time).
 %  if(comparison(Depth2, Depth1, <),  (at(atDepth(Diver, Depth1), Time), happens(ascend(Diver, Depth2), Time))).
 %  "% =================================".


% [diver,depth,time]
% Initiates(Descend(diver,depth),AtDepth(diver,depth),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',55).
% From E: 
% 
% initiates_at(
%    descend(Diver,Depth), 
%    atDepth(Diver,Depth), 
%    Time).
descend(Diver, Depth)initiates atDepth(Diver, Depth).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',55).

 /*  initiated(happens(descend(Diver,Depth),
     		  Time_from,
     		  Time_until),
     	  atDepth(Diver,Depth),
     	  []).
 */
 %  "% =================================".


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',57).
% [diver,depth1,depth2,time]
% HoldsAt(AtDepth(diver,depth1),time) ->
% Terminates(Descend(diver,depth2),AtDepth(diver,depth1),time).
% From E: 
% 
% '->'(
%    holds(
%       atDepth(Diver,Depth1), 
%       Time), 
%    terminates_at(
%       descend(Diver,Depth2), 
%       atDepth(Diver,Depth1), 
%       Time)).
descend(Diver, Depth2)terminates atDepth(Diver, Depth1)at Time if atDepth(Diver, Depth1)at Time.
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',57).

 /*  terminated(happens(descend(Diver,Depth2),
     		   Time,
     		   Time_until),
     	   at(atDepth(Diver,Depth1),Time),
     	   [holds(atDepth(Diver,Depth1),Time)]).
 */
 %  "% =================================".


% [diver,depth,time]
% Initiates(Ascend(diver,depth),AtDepth(diver,depth),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',62).
% From E: 
% 
% initiates_at(
%    ascend(Diver,Depth), 
%    atDepth(Diver,Depth), 
%    Time).
ascend(Diver, Depth)initiates atDepth(Diver, Depth).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',62).

 /*  initiated(happens(ascend(Diver,Depth),
     		  Time_from,
     		  Time_until),
     	  atDepth(Diver,Depth),
     	  []).
 */
 %  "% =================================".


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',64).
% [diver,depth1,depth2,time]
% HoldsAt(AtDepth(diver,depth1),time) ->
% Terminates(Ascend(diver,depth2),AtDepth(diver,depth1),time).
% From E: 
% 
% '->'(
%    holds(
%       atDepth(Diver,Depth1), 
%       Time), 
%    terminates_at(
%       ascend(Diver,Depth2), 
%       atDepth(Diver,Depth1), 
%       Time)).
ascend(Diver, Depth2)terminates atDepth(Diver, Depth1)at Time if atDepth(Diver, Depth1)at Time.
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',64).

 /*  terminated(happens(ascend(Diver,Depth2),
     		   Time,
     		   Time_until),
     	   at(atDepth(Diver,Depth1),Time),
     	   [holds(atDepth(Diver,Depth1),Time)]).
 */
 %  "% =================================".

% fluent Wearing(diver,equipment)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',67).
% From E: 
% 
% fluent(wearing(diver,equipment)).
mpred_prop(wearing(diver, equipment), fluent).
fluents([wearing/2]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',70).
% event PutOn(diver,equipment)
% From E: 
% 
% event(putOn(diver,equipment)).
events([putOn/2]).
mpred_prop(putOn(diver, equipment), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',70).
actions([putOn/2]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',72).
% event TakeOff(diver,equipment)
% From E: 
% 
% event(takeOff(diver,equipment)).
events([takeOff/2]).
mpred_prop(takeOff(diver, equipment), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',72).
actions([takeOff/2]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',74).
% event Lose(diver,equipment)
% From E: 
% 
% event(lose(diver,equipment)).
events([lose/2]).
mpred_prop(lose(diver, equipment), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',74).
actions([lose/2]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',76).
% [diver,equipment,depth,time]
% Releases(PutOn(diver,equipment),AtDepth(equipment,depth),time).
% From E: 
% 
% releases_at(
%    putOn(Diver,Equipment), 
%    atDepth(Equipment,Depth), 
%    Time).
releases(putOn(Diver, Equipment), atDepth(Equipment, Depth)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',76).

 /*  releases(putOn(Diver,Equipment),
     	 atDepth(Equipment,Depth)).
 */
 %  "% =================================".


% [diver,equipment,time]
% Releases(PutOn(diver,equipment),UnderWater(equipment),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',80).
% From E: 
% 
% releases_at(
%    putOn(Diver,Equipment), 
%    underWater(Equipment), 
%    Time).
releases(putOn(Diver, Equipment), underWater(Equipment)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',80).

 /*  releases(putOn(Diver,Equipment),
     	 underWater(Equipment)).
 */
 %  "% =================================".


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',82).
% [diver,equipment,time]
% Happens(PutOn(diver,equipment),time) ->
% !{diver1} HoldsAt(Wearing(diver1,equipment),time).
% From E: 
% 
% '->'(
%    happens(
%       putOn(Diver,Equipment), 
%       Time), 
%    not(thereExists(Diver1, 
%           holds(
%              wearing(Diver1,Equipment), 
%              Time)))).
not thereExists(Diver1, wearing(Diver1, Equipment)at Time)if happens(putOn(Diver, Equipment), Time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',82).

 /*  if(not(thereExists(Diver1,
     		   at(wearing(Diver1,Equipment),Time))),
        happens(putOn(Diver,Equipment),Time)).
 */
 %  "% =================================".


% [diver,depth,equipment,time]
% HoldsAt(Wearing(diver,equipment),time) ->
% (HoldsAt(AtDepth(diver,depth),time) <->
%  HoldsAt(AtDepth(equipment,depth),time)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',87).
% From E: 
% 
% '->'(
%    holds(
%       wearing(Diver,Equipment), 
%       Time), 
%    <->(
%       holds(
%          atDepth(Diver,Depth), 
%          Time), 
%       holds(
%          atDepth(Equipment,Depth), 
%          Time))).
<->(atDepth(Diver, Depth)at Time, atDepth(Equipment, Depth)at Time)if wearing(Diver, Equipment)at Time.
 %  if((at(atDepth(Diver, Depth), Time)<->at(atDepth(Equipment, Depth), Time)), at(wearing(Diver, Equipment), Time)).
 %  "% =================================".


% [diver,depth,object,time]
% HoldsAt(Holding(diver,object),time) ->
% (HoldsAt(AtDepth(diver,depth),time) <->
%  HoldsAt(AtDepth(object,depth),time)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',92).
% From E: 
% 
% '->'(
%    holds(
%       holding(Diver,Object), 
%       Time), 
%    <->(
%       holds(
%          atDepth(Diver,Depth), 
%          Time), 
%       holds(
%          atDepth(Object,Depth), 
%          Time))).
<->(atDepth(Diver, Depth)at Time, atDepth(Object, Depth)at Time)if holding(Diver, Object)at Time.
 %  if((at(atDepth(Diver, Depth), Time)<->at(atDepth(Object, Depth), Time)), at(holding(Diver, Object), Time)).
 %  "% =================================".


% [diver,equipment,time]
% HoldsAt(Wearing(diver,equipment),time) ->
% (HoldsAt(UnderWater(diver),time) <->
%  HoldsAt(UnderWater(equipment),time)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',97).
% From E: 
% 
% '->'(
%    holds(
%       wearing(Diver,Equipment), 
%       Time), 
%    <->(
%       holds(
%          underWater(Diver), 
%          Time), 
%       holds(
%          underWater(Equipment), 
%          Time))).
<->(underWater(Diver)at Time, underWater(Equipment)at Time)if wearing(Diver, Equipment)at Time.
 %  if((at(underWater(Diver), Time)<->at(underWater(Equipment), Time)), at(wearing(Diver, Equipment), Time)).
 %  "% =================================".


% [diver,object,time]
% HoldsAt(Holding(diver,object),time) ->
% (HoldsAt(UnderWater(diver),time) <->
%  HoldsAt(UnderWater(object),time)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',102).
% From E: 
% 
% '->'(
%    holds(
%       holding(Diver,Object), 
%       Time), 
%    <->(
%       holds(
%          underWater(Diver), 
%          Time), 
%       holds(
%          underWater(Object), 
%          Time))).
<->(underWater(Diver)at Time, underWater(Object)at Time)if holding(Diver, Object)at Time.
 %  if((at(underWater(Diver), Time)<->at(underWater(Object), Time)), at(holding(Diver, Object), Time)).
 %  "% =================================".


% [diver,depth,equipment,time]
% HoldsAt(AtDepth(diver,depth),time) &
% HoldsAt(Wearing(diver,equipment),time) ->
% Initiates(TakeOff(diver,equipment),AtDepth(equipment,depth),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',107).
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          atDepth(Diver,Depth), 
%          Time), 
%       holds(
%          wearing(Diver,Equipment), 
%          Time)), 
%    initiates_at(
%       takeOff(Diver,Equipment), 
%       atDepth(Equipment,Depth), 
%       Time)).
takeOff(Diver, Equipment)initiates atDepth(Equipment, Depth)at Time if atDepth(Diver, Depth)at Time, wearing(Diver, Equipment)at Time.
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',107).

 /*  initiated(happens(takeOff(Diver,Equipment),
     		  Time_from,
     		  Time_until),
     	  at(atDepth(Equipment,Depth),Time),
     	  [ holds(atDepth(Diver,Depth),Time),
     	    holds(wearing(Diver,Equipment),Time)
     	  ]).
 */
 %  "% =================================".


% [diver,depth,equipment,time]
% !HoldsAt(AtDepth(diver,depth),time) &
% HoldsAt(Wearing(diver,equipment),time) ->
% Terminates(TakeOff(diver,equipment),AtDepth(equipment,depth),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',112).
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          not(atDepth(Diver,Depth)), 
%          Time), 
%       holds(
%          wearing(Diver,Equipment), 
%          Time)), 
%    terminates_at(
%       takeOff(Diver,Equipment), 
%       atDepth(Equipment,Depth), 
%       Time)).
takeOff(Diver, Equipment)terminates atDepth(Equipment, Depth)at Time if not atDepth(Diver, Depth)at Time, wearing(Diver, Equipment)at Time.
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',112).

 /*  terminated(happens(takeOff(Diver,Equipment),
     		   Time_from,
     		   Time_until),
     	   at(atDepth(Equipment,Depth),Time),
     	   [ holds(not(atDepth(Diver,Depth)),Time),
     	     holds(wearing(Diver,Equipment),Time)
     	   ]).
 */
 %  "% =================================".


% [diver,equipment,time]
% HoldsAt(UnderWater(diver),time) ->
% Initiates(TakeOff(diver,equipment),UnderWater(equipment),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',117).
% From E: 
% 
% '->'(
%    holds(
%       underWater(Diver), 
%       Time), 
%    initiates_at(
%       takeOff(Diver,Equipment), 
%       underWater(Equipment), 
%       Time)).
takeOff(Diver, Equipment)initiates underWater(Equipment)at Time if underWater(Diver)at Time.

 /*  initiated(happens(takeOff(Diver,Equipment),
     		  Time,
     		  Time_until),
     	  at(underWater(Equipment),Time),
     	  [holds(underWater(Diver),Time)]).
 */
 %  "% =================================".


% [diver,equipment,time]
% !HoldsAt(UnderWater(diver),time) ->
% Terminates(TakeOff(diver,equipment),UnderWater(equipment),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',121).
% From E: 
% 
% '->'(
%    holds(
%       not(underWater(Diver)), 
%       Time), 
%    terminates_at(
%       takeOff(Diver,Equipment), 
%       underWater(Equipment), 
%       Time)).
takeOff(Diver, Equipment)terminates underWater(Equipment)at Time if not underWater(Diver)at Time.

 /*  terminated(happens(takeOff(Diver,Equipment),
     		   Time,
     		   Time_until),
     	   at(underWater(Equipment),Time),
     	   [holds(not(underWater(Diver)),Time)]).
 */
 %  "% =================================".


% [diver,equipment,depth,time]
% HoldsAt(AtDepth(diver,depth),time) &
% HoldsAt(Wearing(diver,equipment),time) ->
% Initiates(Lose(diver,equipment),AtDepth(equipment,depth),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',125).
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          atDepth(Diver,Depth), 
%          Time), 
%       holds(
%          wearing(Diver,Equipment), 
%          Time)), 
%    initiates_at(
%       lose(Diver,Equipment), 
%       atDepth(Equipment,Depth), 
%       Time)).
lose(Diver, Equipment)initiates atDepth(Equipment, Depth)at Time if atDepth(Diver, Depth)at Time, wearing(Diver, Equipment)at Time.
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',125).

 /*  initiated(happens(lose(Diver,Equipment),
     		  Time_from,
     		  Time_until),
     	  at(atDepth(Equipment,Depth),Time),
     	  [ holds(atDepth(Diver,Depth),Time),
     	    holds(wearing(Diver,Equipment),Time)
     	  ]).
 */
 %  "% =================================".


% [diver,equipment,depth,time]
% !HoldsAt(AtDepth(diver,depth),time) &
% HoldsAt(Wearing(diver,equipment),time) ->
% Terminates(Lose(diver,equipment),AtDepth(equipment,depth),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',130).
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          not(atDepth(Diver,Depth)), 
%          Time), 
%       holds(
%          wearing(Diver,Equipment), 
%          Time)), 
%    terminates_at(
%       lose(Diver,Equipment), 
%       atDepth(Equipment,Depth), 
%       Time)).
lose(Diver, Equipment)terminates atDepth(Equipment, Depth)at Time if not atDepth(Diver, Depth)at Time, wearing(Diver, Equipment)at Time.
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',130).

 /*  terminated(happens(lose(Diver,Equipment),
     		   Time_from,
     		   Time_until),
     	   at(atDepth(Equipment,Depth),Time),
     	   [ holds(not(atDepth(Diver,Depth)),Time),
     	     holds(wearing(Diver,Equipment),Time)
     	   ]).
 */
 %  "% =================================".


% [diver,equipment,time]
% HoldsAt(UnderWater(diver),time) ->
% Initiates(Lose(diver,equipment),UnderWater(equipment),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',135).
% From E: 
% 
% '->'(
%    holds(
%       underWater(Diver), 
%       Time), 
%    initiates_at(
%       lose(Diver,Equipment), 
%       underWater(Equipment), 
%       Time)).
lose(Diver, Equipment)initiates underWater(Equipment)at Time if underWater(Diver)at Time.

 /*  initiated(happens(lose(Diver,Equipment),
     		  Time,
     		  Time_until),
     	  at(underWater(Equipment),Time),
     	  [holds(underWater(Diver),Time)]).
 */
 %  "% =================================".


% [diver,equipment,time]
% !HoldsAt(UnderWater(diver),time) ->
% Terminates(Lose(diver,equipment),UnderWater(equipment),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',139).
% From E: 
% 
% '->'(
%    holds(
%       not(underWater(Diver)), 
%       Time), 
%    terminates_at(
%       lose(Diver,Equipment), 
%       underWater(Equipment), 
%       Time)).
lose(Diver, Equipment)terminates underWater(Equipment)at Time if not underWater(Diver)at Time.

 /*  terminated(happens(lose(Diver,Equipment),
     		   Time,
     		   Time_until),
     	   at(underWater(Equipment),Time),
     	   [holds(not(underWater(Diver)),Time)]).
 */
 %  "% =================================".

% fluent Holding(diver,object)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',141).
% From E: 
% 
% fluent(holding(diver,object)).
mpred_prop(holding(diver, object), fluent).
fluents([holding/2]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',144).
% [diver1,diver2,time]
% HoldsAt(Holding(diver1,diver2),time) ->
% !HoldsAt(Holding(diver2,diver1),time).
% From E: 
% 
% '->'(
%    holds(
%       holding(Diver1,Diver2), 
%       Time), 
%    holds(
%       not(holding(Diver2,Diver1)), 
%       Time)).
not holding(Diver2, Diver1)at Time if holding(Diver1, Diver2)at Time.
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',144).

 /*  l_int(holds(not(holding(Diver2,Diver1)),Time),
           [holds(holding(Diver1,Diver2),Time)]).
 */
 %  "% =================================".

% event Grab(diver,object)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',147).
% From E: 
% 
% event(grab(diver,object)).
events([grab/2]).
mpred_prop(grab(diver, object), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',147).
actions([grab/2]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',150).
% event LetGoOf(diver,object)
% From E: 
% 
% event(letGoOf(diver,object)).
events([letGoOf/2]).
mpred_prop(letGoOf(diver, object), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',150).
actions([letGoOf/2]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',152).
% [diver,object,time]
% Initiates(Grab(diver,object),Holding(diver,object),time).
% From E: 
% 
% initiates_at(
%    grab(Diver,Object), 
%    holding(Diver,Object), 
%    Time).
grab(Diver, Object)initiates holding(Diver, Object).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',152).

 /*  initiated(happens(grab(Diver,Object),
     		  Time_from,
     		  Time_until),
     	  holding(Diver,Object),
     	  []).
 */
 %  "% =================================".


% [diver,object,time]
% Terminates(LetGoOf(diver,object),Holding(diver,object),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',156).
% From E: 
% 
% terminates_at(
%    letGoOf(Diver,Object), 
%    holding(Diver,Object), 
%    Time).
letGoOf(Diver, Object)terminates holding(Diver, Object).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',156).

 /*  terminated(happens(letGoOf(Diver,Object),
     		   Time_from,
     		   Time_until),
     	   holding(Diver,Object),
     	   []).
 */
 %  "% =================================".


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',158).
% [diver,object,depth,time]
% Releases(Grab(diver,object),AtDepth(object,depth),time).
% From E: 
% 
% releases_at(
%    grab(Diver,Object), 
%    atDepth(Object,Depth), 
%    Time).
releases(grab(Diver, Object), atDepth(Object, Depth)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',158).

 /*  releases(grab(Diver,Object),
     	 atDepth(Object,Depth)).
 */
 %  "% =================================".


% [diver,object,time]
% Releases(Grab(diver,object),UnderWater(object),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',162).
% From E: 
% 
% releases_at(
%    grab(Diver,Object), 
%    underWater(Object), 
%    Time).
releases(grab(Diver, Object), underWater(Object)).
 %  releases(grab(Diver,Object),underWater(Object)).
 %  "% =================================".


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',164).
% [diver,object,depth,time]
% HoldsAt(AtDepth(diver,depth),time) &
% HoldsAt(Holding(diver,object),time) ->
% Initiates(LetGoOf(diver,object),AtDepth(object,depth),time).
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          atDepth(Diver,Depth), 
%          Time), 
%       holds(
%          holding(Diver,Object), 
%          Time)), 
%    initiates_at(
%       letGoOf(Diver,Object), 
%       atDepth(Object,Depth), 
%       Time)).
letGoOf(Diver, Object)initiates atDepth(Object, Depth)at Time if atDepth(Diver, Depth)at Time, holding(Diver, Object)at Time.
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',164).

 /*  initiated(happens(letGoOf(Diver,Object),
     		  Time_from,
     		  Time_until),
     	  at(atDepth(Object,Depth),Time),
     	  [ holds(atDepth(Diver,Depth),Time),
     	    holds(holding(Diver,Object),Time)
     	  ]).
 */
 %  "% =================================".


% [diver,object,depth,time]
% !HoldsAt(AtDepth(diver,depth),time) &
% HoldsAt(Holding(diver,object),time) ->
% Terminates(LetGoOf(diver,object),AtDepth(object,depth),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',170).
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          not(atDepth(Diver,Depth)), 
%          Time), 
%       holds(
%          holding(Diver,Object), 
%          Time)), 
%    terminates_at(
%       letGoOf(Diver,Object), 
%       atDepth(Object,Depth), 
%       Time)).
letGoOf(Diver, Object)terminates atDepth(Object, Depth)at Time if not atDepth(Diver, Depth)at Time, holding(Diver, Object)at Time.
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',170).

 /*  terminated(happens(letGoOf(Diver,Object),
     		   Time_from,
     		   Time_until),
     	   at(atDepth(Object,Depth),Time),
     	   [ holds(not(atDepth(Diver,Depth)),Time),
     	     holds(holding(Diver,Object),Time)
     	   ]).
 */
 %  "% =================================".


% [diver,object,time]
% HoldsAt(UnderWater(diver),time) ->
% Initiates(LetGoOf(diver,object),UnderWater(object),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',175).
% From E: 
% 
% '->'(
%    holds(
%       underWater(Diver), 
%       Time), 
%    initiates_at(
%       letGoOf(Diver,Object), 
%       underWater(Object), 
%       Time)).
letGoOf(Diver, Object)initiates underWater(Object)at Time if underWater(Diver)at Time.

 /*  initiated(happens(letGoOf(Diver,Object),
     		  Time,
     		  Time_until),
     	  at(underWater(Object),Time),
     	  [holds(underWater(Diver),Time)]).
 */
 %  "% =================================".


% [diver,object,time]
% !HoldsAt(UnderWater(diver),time) ->
% Terminates(LetGoOf(diver,object),UnderWater(object),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',179).
% From E: 
% 
% '->'(
%    holds(
%       not(underWater(Diver)), 
%       Time), 
%    terminates_at(
%       letGoOf(Diver,Object), 
%       underWater(Object), 
%       Time)).
letGoOf(Diver, Object)terminates underWater(Object)at Time if not underWater(Diver)at Time.

 /*  terminated(happens(letGoOf(Diver,Object),
     		   Time,
     		   Time_until),
     	   at(underWater(Object),Time),
     	   [holds(not(underWater(Diver)),Time)]).
 */
 %  "% =================================".


% [diver,equipment,time]
% Initiates(PutOn(diver,equipment),Wearing(diver,equipment),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',183).
% From E: 
% 
% initiates_at(
%    putOn(Diver,Equipment), 
%    wearing(Diver,Equipment), 
%    Time).
putOn(Diver, Equipment)initiates wearing(Diver, Equipment).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',183).

 /*  initiated(happens(putOn(Diver,Equipment),
     		  Time_from,
     		  Time_until),
     	  wearing(Diver,Equipment),
     	  []).
 */
 %  "% =================================".


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',185).
% [diver,equipment,time]
% Happens(PutOn(diver,equipment),time) ->
% !HoldsAt(UnderWater(diver),time).
% From E: 
% 
% '->'(
%    happens(
%       putOn(Diver,Equipment), 
%       Time), 
%    holds(
%       not(underWater(Diver)), 
%       Time)).
not underWater(Diver)at Time if happens(putOn(Diver, Equipment), Time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',185).

 /*  l_int(holds(not(underWater(Diver)),Time),
           [happens(putOn(Diver,Equipment),Time)]).
 */
 %  "% =================================".


% [diver,equipment,time]
% Terminates(TakeOff(diver,equipment),Wearing(diver,equipment),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',190).
% From E: 
% 
% terminates_at(
%    takeOff(Diver,Equipment), 
%    wearing(Diver,Equipment), 
%    Time).
takeOff(Diver, Equipment)terminates wearing(Diver, Equipment).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',190).

 /*  terminated(happens(takeOff(Diver,Equipment),
     		   Time_from,
     		   Time_until),
     	   wearing(Diver,Equipment),
     	   []).
 */
 %  "% =================================".


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',192).
% [diver,equipment,time]
% Terminates(Lose(diver,equipment),Wearing(diver,equipment),time).
% From E: 
% 
% terminates_at(
%    lose(Diver,Equipment), 
%    wearing(Diver,Equipment), 
%    Time).
lose(Diver, Equipment)terminates wearing(Diver, Equipment).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',192).

 /*  terminated(happens(lose(Diver,Equipment),
     		   Time_from,
     		   Time_until),
     	   wearing(Diver,Equipment),
     	   []).
 */
 %  "% =================================".

% fluent Vertical(diver)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',194).
% From E: 
% 
% fluent(vertical(diver)).
mpred_prop(vertical(diver), fluent).
fluents([vertical/1]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',197).
% fluent HorizontalDown(diver)
% From E: 
% 
% fluent(horizontalDown(diver)).
mpred_prop(horizontalDown(diver), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',197).
fluents([horizontalDown/1]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',199).
% fluent Inverted(diver)
% From E: 
% 
% fluent(inverted(diver)).
mpred_prop(inverted(diver), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',199).
fluents([inverted/1]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',201).
% fluent HorizontalUp(diver)
% From E: 
% 
% fluent(horizontalUp(diver)).
mpred_prop(horizontalUp(diver), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',201).
fluents([horizontalUp/1]).
fluents([horizontalUp/1]).
mpred_prop(horizontalUp(diver), fluent).
fluents([horizontalUp/1]).
