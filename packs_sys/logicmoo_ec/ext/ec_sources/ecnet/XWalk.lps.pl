% From E: 
% 
% ':-'(call_pel_directive(translate(unskipped,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.lps.pl'))).
:- call_pel_directive(translate(unskipped,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.lps.pl')).
:-include(library('ec_planner/ec_test_incl')).
:-expects_dialect(lps).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/WritingABook.e',97).
% From E: 
% 
% ':-'(call_pel_directive(translate(begining,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.lps.pl'))).
:- call_pel_directive(translate(begining,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.lps.pl')).
% Tue, 23 Mar 2021 19:07:01 GMT File: <stream>(0x5555684a7d00)%;
%; Copyright (c) 2005 IBM Corporation and others.
%; All rights reserved. This program and the accompanying materials
%; are made available under the terms of the Common Public License v1.0
%; which accompanies this distribution, and is available at
%; http://www.eclipse.org/legal/cpl-v10.html
%;
%; Contributors:
%; IBM - Initial implementation
%;
%; XWalk: WALK x-schema representation of walking
%;
%; @phdthesis{Narayanan:1997,
%;   author = "Srinivas S. Narayanan",
%;   year = "1997",
%;   title = "Knowledge-based Action Representations for Metaphor and Aspect (\uppercase{KARMA})",
%;   address = "Berkeley, CA",
%;   school = "University of California, Berkeley",
%; }
%;

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',22).
% option trajectory on
% From E: 
% 
% ':-'(call_pel_directive(option(trajectory,on))).
:- call_pel_directive(option(trajectory, on)).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',24).
% sort xschema
% From E: 
% 
% sort(xschema).
sort(xschema).
%; parameters

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',28).
% predicate XWalkAgent(xschema,agent)
% From E: 
% 
% predicate(xWalkAgent(xschema,agent)).
mpred_prop(xWalkAgent(xschema,agent),predicate).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',28).
predicates([xWalkAgent/2]).

% function XWalkRate(xschema): offset ; step duration
% From E: 
% 
% function(
%    xWalkRate(xschema), 
%    [offset,;,step,duration]).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',28).
function(xWalkRate(xschema),[offset,;,step,duration]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',30).
% function XWalkSize(xschema): offset ; step size
% From E: 
% 
% function(
%    xWalkSize(xschema), 
%    [offset,;,step,size]).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',30).
function(xWalkSize(xschema),[offset,;,step,size]).
%; TTL input lines

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',34).
% fluent XWalkEnabled(xschema)
% From E: 
% 
% fluent(xWalkEnabled(xschema)).
mpred_prop(xWalkEnabled(xschema),fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',34).
fluents([xWalkEnabled/1]).

% fluent XWalkGroundStable(xschema)
% From E: 
% 
% fluent(xWalkGroundStable(xschema)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',34).
mpred_prop(xWalkGroundStable(xschema),fluent).
fluents([xWalkGroundStable/1]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',36).
% fluent XWalkPosture(xschema)
% From E: 
% 
% fluent(xWalkPosture(xschema)).
mpred_prop(xWalkPosture(xschema),fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',36).
fluents([xWalkPosture/1]).

% fluent XWalkFootingOK(xschema)
% From E: 
% 
% fluent(xWalkFootingOK(xschema)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',36).
mpred_prop(xWalkFootingOK(xschema),fluent).
fluents([xWalkFootingOK/1]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',38).
% noninertial XWalkEnabled, XWalkGroundStable, XWalkPosture, XWalkFootingOK
% From E: 
% 
% ':-'(call_pel_directive(noninertial(xWalkEnabled))).
:- call_pel_directive(noninertial(xWalkEnabled)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',38).
% From E: 
% 
% ':-'(call_pel_directive(noninertial(xWalkGroundStable))).
:- call_pel_directive(noninertial(xWalkGroundStable)).
% From E: 
% 
% ':-'(call_pel_directive(noninertial(xWalkPosture))).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',38).
:- call_pel_directive(noninertial(xWalkPosture)).
% From E: 
% 
% ':-'(call_pel_directive(noninertial(xWalkFootingOK))).
:- call_pel_directive(noninertial(xWalkFootingOK)).
%; fluents

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',42).
% fluent XWalkDistance(xschema,distance)
% From E: 
% 
% fluent(xWalkDistance(xschema,distance)).
mpred_prop(xWalkDistance(xschema,distance),fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',42).
fluents([xWalkDistance/2]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',44).
% [xschema,distance1,distance2,time]
% HoldsAt(XWalkDistance(xschema,distance1),time) &
% HoldsAt(XWalkDistance(xschema,distance2),time) ->
% distance1=distance2.
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          xWalkDistance(Xschema,Distance1), 
%          Time), 
%       holds(
%          xWalkDistance(Xschema,Distance2), 
%          Time)), 
%    Distance1=Distance2).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',44).
if(equals(Distance1, Distance2),  (at(xWalkDistance(Xschema, Distance1), Time), at(xWalkDistance(Xschema, Distance2), Time))).


%; logic gate behavior

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',51).
% fluent XWalkVision(xschema)
% From E: 
% 
% fluent(xWalkVision(xschema)).
mpred_prop(xWalkVision(xschema),fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',51).
fluents([xWalkVision/1]).

% fluent XWalkVisionOK(xschema)
% From E: 
% 
% fluent(xWalkVisionOK(xschema)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',51).
mpred_prop(xWalkVisionOK(xschema),fluent).
fluents([xWalkVisionOK/1]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',53).
% fluent XWalkAtDestination(xschema)
% From E: 
% 
% fluent(xWalkAtDestination(xschema)).
mpred_prop(xWalkAtDestination(xschema),fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',53).
fluents([xWalkAtDestination/1]).

% fluent XWalkDone(xschema)
% From E: 
% 
% fluent(xWalkDone(xschema)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',53).
mpred_prop(xWalkDone(xschema),fluent).
fluents([xWalkDone/1]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',55).
% noninertial XWalkVision, XWalkVisionOK, XWalkAtDestination, XWalkDone
% From E: 
% 
% ':-'(call_pel_directive(noninertial(xWalkVision))).
:- call_pel_directive(noninertial(xWalkVision)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',55).
% From E: 
% 
% ':-'(call_pel_directive(noninertial(xWalkVisionOK))).
:- call_pel_directive(noninertial(xWalkVisionOK)).
% From E: 
% 
% ':-'(call_pel_directive(noninertial(xWalkAtDestination))).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',55).
:- call_pel_directive(noninertial(xWalkAtDestination)).
% From E: 
% 
% ':-'(call_pel_directive(noninertial(xWalkDone))).
:- call_pel_directive(noninertial(xWalkDone)).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',57).
% [xschema,time]
% HoldsAt(XWalkGroundStable(xschema),time) <->
% HoldsAt(XWalkVision(xschema),time).
% From E: 
% 
% <->(
%    holds(
%       xWalkGroundStable(Xschema), 
%       Time), 
%    holds(
%       xWalkVision(Xschema), 
%       Time)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',57).
at(xWalkGroundStable(Xschema), Time) <->
    at(xWalkVision(Xschema), Time).


% [xschema,time]
% HoldsAt(XWalkEnabled(xschema),time) &
% HoldsAt(XWalkVision(xschema),time) &
% HoldsAt(XWalkPosture(xschema),time) <->
% HoldsAt(XWalkVisionOK(xschema),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',62).
% From E: 
% 
% <->(
%    ','(
%       holds(
%          xWalkEnabled(Xschema), 
%          Time), 
%       ','(
%          holds(
%             xWalkVision(Xschema), 
%             Time), 
%          holds(
%             xWalkPosture(Xschema), 
%             Time))), 
%    holds(
%       xWalkVisionOK(Xschema), 
%       Time)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',62).
at(xWalkEnabled(Xschema), Time), at(xWalkVision(Xschema), Time), at(xWalkPosture(Xschema), Time) <->
    at(xWalkVisionOK(Xschema), Time).


% [xschema,time]
% HoldsAt(XWalkDistance(xschema,0),time) <->
% HoldsAt(XWalkAtDestination(xschema),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',68).
% From E: 
% 
% <->(
%    holds(
%       xWalkDistance(Xschema,0), 
%       Time), 
%    holds(
%       xWalkAtDestination(Xschema), 
%       Time)).
at(xWalkDistance(Xschema, 0), Time) <->
    at(xWalkAtDestination(Xschema), Time).


% [xschema,time]
% HoldsAt(XWalkAtDestination(xschema),time) <->
% HoldsAt(XWalkDone(xschema),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',72).
% From E: 
% 
% <->(
%    holds(
%       xWalkAtDestination(Xschema), 
%       Time), 
%    holds(
%       xWalkDone(Xschema), 
%       Time)).
at(xWalkAtDestination(Xschema), Time) <->
    at(xWalkDone(Xschema), Time).


%; durative events
%; distance is the goal

% fluent XWalkStepping(xschema,distance) 
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',77).
% From E: 
% 
% fluent(xWalkStepping(xschema,distance)).
mpred_prop(xWalkStepping(xschema,distance),fluent).
fluents([xWalkStepping/2]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',79).
% event XWalkSteppingOn(xschema)
% From E: 
% 
% event(xWalkSteppingOn(xschema)).
mpred_prop(xWalkSteppingOn(xschema),event).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',79).
events([xWalkSteppingOn/1]).

% event XWalkSteppingOff(xschema)
% From E: 
% 
% event(xWalkSteppingOff(xschema)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',79).
mpred_prop(xWalkSteppingOff(xschema),event).
events([xWalkSteppingOff/1]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',82).
% [xschema,distance1,distance2,time]
% HoldsAt(XWalkDistance(xschema,distance1),time) &
% distance2 = distance1 - XWalkSize(xschema) ->
% Initiates(XWalkSteppingOn(xschema),XWalkStepping(xschema,distance2),time).
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          xWalkDistance(Xschema,Distance1), 
%          Time), 
%       Distance2=Distance1-xWalkSize(Xschema)), 
%    initiates_at(
%       xWalkSteppingOn(Xschema), 
%       xWalkStepping(Xschema,Distance2), 
%       Time)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',82).
if(initiates(xWalkSteppingOn(Xschema), at(xWalkStepping(Xschema, Distance2), Time)),  (at(xWalkDistance(Xschema, Distance1), Time), equals(Distance2, Distance1-xWalkSize(Xschema)))).


% [xschema,distance,time]
% Terminates(XWalkSteppingOff(xschema),XWalkStepping(xschema,distance),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',88).
% From E: 
% 
% terminates_at(
%    xWalkSteppingOff(Xschema), 
%    xWalkStepping(Xschema,Distance), 
%    Time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',88).
terminates(xWalkSteppingOff(Xschema),
	   xWalkStepping(Xschema,Distance)).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',90).
% [xschema,distance,time]
% Releases(XWalkSteppingOn(xschema),XWalkDistance(xschema,distance),time).
% From E: 
% 
% releases_at(
%    xWalkSteppingOn(Xschema), 
%    xWalkDistance(Xschema,Distance), 
%    Time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',90).
releases(xWalkSteppingOn(Xschema),
	 xWalkDistance(Xschema,Distance)).


% [xschema,distance1,distance2,time]
% HoldsAt(XWalkDistance(xschema,distance1),time) &
% distance1 != distance2 ->
% Terminates(XWalkSteppingOff(xschema),XWalkDistance(xschema,distance2),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',94).
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          xWalkDistance(Xschema,Distance1), 
%          Time), 
%       Distance1\=Distance2), 
%    terminates_at(
%       xWalkSteppingOff(Xschema), 
%       xWalkDistance(Xschema,Distance2), 
%       Time)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',94).
if(terminates(xWalkSteppingOff(Xschema), at(xWalkDistance(Xschema, Distance2), Time)),  (at(xWalkDistance(Xschema, Distance1), Time), {dif(Distance1, Distance2)})).


% [xschema,distance,time]
% HoldsAt(XWalkDistance(xschema,distance),time) ->
% Initiates(XWalkSteppingOff(xschema),XWalkDistance(xschema,distance),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',99).
% From E: 
% 
% '->'(
%    holds(
%       xWalkDistance(Xschema,Distance), 
%       Time), 
%    initiates_at(
%       xWalkSteppingOff(Xschema), 
%       xWalkDistance(Xschema,Distance), 
%       Time)).
if(initiates(xWalkSteppingOff(Xschema),
	     at(xWalkDistance(Xschema,Distance),Time)),
   at(xWalkDistance(Xschema,Distance),Time)).


% [xschema,distance01,distance02,distance03,offset,time]
% HoldsAt(XWalkDistance(xschema,distance01),time) &
% (distance03=(distance01-(offset*(XWalkSize(xschema)/XWalkRate(xschema))))) ->
% Trajectory(XWalkStepping(xschema,distance02),
%            time,
%            XWalkDistance(xschema,distance03),
%            offset).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',103).
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          xWalkDistance(Xschema,Distance01), 
%          Time), 
%       Distance03=Distance01-Offset*(xWalkSize(Xschema)/xWalkRate(Xschema))), 
%    trajectory(
%       xWalkStepping(Xschema,Distance02), 
%       Time, 
%       xWalkDistance(Xschema,Distance03), 
%       Offset)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',103).
if(trajectory(xWalkStepping(Xschema, Distance02), Time, xWalkDistance(Xschema, Distance03), Offset),  (at(xWalkDistance(Xschema, Distance01), Time), equals(Distance03, Distance01-Offset*(xWalkSize(Xschema)/xWalkRate(Xschema))))).


% [xschema,distance,time]
% HoldsAt(XWalkStepping(xschema,distance),time) &
% HoldsAt(XWalkDistance(xschema,distance),time) ->
% Happens(XWalkSteppingOff(xschema),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',111).
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          xWalkStepping(Xschema,Distance), 
%          Time), 
%       holds(
%          xWalkDistance(Xschema,Distance), 
%          Time)), 
%    happens(
%       xWalkSteppingOff(Xschema), 
%       Time)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',111).
if(happens(xWalkSteppingOff(Xschema), Time),  (at(xWalkStepping(Xschema, Distance), Time), at(xWalkDistance(Xschema, Distance), Time))).


%; punctual events

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',117).
% event XWalkTestFooting(xschema)
% From E: 
% 
% event(xWalkTestFooting(xschema)).
mpred_prop(xWalkTestFooting(xschema),event).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',117).
events([xWalkTestFooting/1]).

% event XWalkMoveFoot(xschema)
% From E: 
% 
% event(xWalkMoveFoot(xschema)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',117).
mpred_prop(xWalkMoveFoot(xschema),event).
events([xWalkMoveFoot/1]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',120).
% [xschema,time]
% Happens(XWalkTestFooting(xschema),time) &
% !HoldsAt(XWalkFootingOK(xschema),time) ->
% Happens(XWalkMoveFoot(xschema),time+1).
% From E: 
% 
% '->'(
%    ','(
%       happens(
%          xWalkTestFooting(Xschema), 
%          Time), 
%       holds(
%          not(xWalkFootingOK(Xschema)), 
%          Time)), 
%    happens(
%       xWalkMoveFoot(Xschema), 
%       Time+1)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',120).
if(happens(xWalkMoveFoot(Xschema), Time+1),  (happens(xWalkTestFooting(Xschema), Time), at(not(xWalkFootingOK(Xschema)), Time))).


% [xschema,time]
% Happens(XWalkMoveFoot(xschema),time) ->
% Happens(XWalkReadyOn(xschema),time+1).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',126).
% From E: 
% 
% '->'(
%    happens(
%       xWalkMoveFoot(Xschema), 
%       Time), 
%    happens(
%       xWalkReadyOn(Xschema), 
%       Time+1)).
if(happens(xWalkReadyOn(Xschema),Time+1),
   happens(xWalkMoveFoot(Xschema),Time)).


%; Petri net behavior

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',131).
% fluent XWalkReady(xschema)
% From E: 
% 
% fluent(xWalkReady(xschema)).
mpred_prop(xWalkReady(xschema),fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',131).
fluents([xWalkReady/1]).

% event XWalkReadyOn(xschema)
% From E: 
% 
% event(xWalkReadyOn(xschema)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',131).
mpred_prop(xWalkReadyOn(xschema),event).
events([xWalkReadyOn/1]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',133).
% event XWalkReadyOff(xschema)
% From E: 
% 
% event(xWalkReadyOff(xschema)).
mpred_prop(xWalkReadyOff(xschema),event).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',133).
events([xWalkReadyOff/1]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',135).
% [xschema,time]
% HoldsAt(XWalkEnabled(xschema),time) &
% HoldsAt(XWalkVision(xschema),time) &
% HoldsAt(XWalkPosture(xschema),time) &
% !({distance} HoldsAt(XWalkStepping(xschema,distance),time)) & ; !!! pulse
% !HoldsAt(XWalkReady(xschema),time) ->
% Happens(XWalkReadyOn(xschema),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',135).
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          xWalkEnabled(Xschema), 
%          Time), 
%       ','(
%          holds(
%             xWalkVision(Xschema), 
%             Time), 
%          ','(
%             holds(
%                xWalkPosture(Xschema), 
%                Time), 
%             ','(
%                not(thereExists(Distance, 
%                       holds(
%                          xWalkStepping(Xschema,Distance), 
%                          Time))), 
%                holds(
%                   not(xWalkReady(Xschema)), 
%                   Time))))), 
%    happens(
%       xWalkReadyOn(Xschema), 
%       Time)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',135).
if(happens(xWalkReadyOn(Xschema), Time),  (at(xWalkEnabled(Xschema), Time), at(xWalkVision(Xschema), Time), at(xWalkPosture(Xschema), Time), not(thereExists(Distance, at(xWalkStepping(Xschema, Distance), Time))), at(not(xWalkReady(Xschema)), Time))).


% [xschema,time]
% Initiates(XWalkReadyOn(xschema),XWalkReady(xschema),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',144).
% From E: 
% 
% initiates_at(
%    xWalkReadyOn(Xschema), 
%    xWalkReady(Xschema), 
%    Time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',144).
initiates(xWalkReadyOn(Xschema),xWalkReady(Xschema)).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',146).
% [xschema,time]
% Terminates(XWalkReadyOff(xschema),XWalkReady(xschema),time).
% From E: 
% 
% terminates_at(
%    xWalkReadyOff(Xschema), 
%    xWalkReady(Xschema), 
%    Time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',146).
terminates(xWalkReadyOff(Xschema),xWalkReady(Xschema)).


%; bypass_ok
% [xschema,time]
% !(% {distance} HoldsAt(XWalkStepping(xschema,distance),time)) &
% HoldsAt(XWalkVisionOK(xschema),time) &
% HoldsAt(XWalkReady(xschema),time) ->
% Happens(XWalkSteppingOn(xschema),time) &
% Happens(XWalkReadyOff(xschema),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',150).
% From E: 
% 
% exists(Distance, 
%    '->'(
%       ','(
%          holds(
%             not(xWalkStepping(Xschema,Distance)), 
%             Time), 
%          ','(
%             holds(
%                xWalkVisionOK(Xschema), 
%                Time), 
%             holds(
%                xWalkReady(Xschema), 
%                Time))), 
%       ','(
%          happens(
%             xWalkSteppingOn(Xschema), 
%             Time), 
%          happens(
%             xWalkReadyOff(Xschema), 
%             Time)))).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',150).
exists(Distance, if((happens(xWalkSteppingOn(Xschema), Time), happens(xWalkReadyOff(Xschema), Time)),  (at(not(xWalkStepping(Xschema, Distance)), Time), at(xWalkVisionOK(Xschema), Time), at(xWalkReady(Xschema), Time)))).


%; !bypass_ok
% [xschema,time]
% !(% {distance} HoldsAt(XWalkStepping(xschema,distance),time)) &
% !HoldsAt(XWalkVisionOK(xschema),time) &
% HoldsAt(XWalkReady(xschema),time) ->
% Happens(XWalkTestFooting(xschema),time) &
% Happens(XWalkReadyOff(xschema),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',158).
% From E: 
% 
% exists(Distance, 
%    '->'(
%       ','(
%          holds(
%             not(xWalkStepping(Xschema,Distance)), 
%             Time), 
%          ','(
%             holds(
%                not(xWalkVisionOK(Xschema)), 
%                Time), 
%             holds(
%                xWalkReady(Xschema), 
%                Time))), 
%       ','(
%          happens(
%             xWalkTestFooting(Xschema), 
%             Time), 
%          happens(
%             xWalkReadyOff(Xschema), 
%             Time)))).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',158).
exists(Distance, if((happens(xWalkTestFooting(Xschema), Time), happens(xWalkReadyOff(Xschema), Time)),  (at(not(xWalkStepping(Xschema, Distance)), Time), at(not(xWalkVisionOK(Xschema)), Time), at(xWalkReady(Xschema), Time)))).


% [xschema,distance,time]
% HoldsAt(XWalkStepping(xschema,distance),time) &
% HoldsAt(XWalkDistance(xschema,distance),time) &
% (distance > 0) ->
% Happens(XWalkReadyOn(xschema),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',166).
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          xWalkStepping(Xschema,Distance), 
%          Time), 
%       ','(
%          holds(
%             xWalkDistance(Xschema,Distance), 
%             Time), 
%          Distance>0)), 
%    happens(
%       xWalkReadyOn(Xschema), 
%       Time)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',166).
if(happens(xWalkReadyOn(Xschema), Time),  (at(xWalkStepping(Xschema, Distance), Time), at(xWalkDistance(Xschema, Distance), Time), comparison(Distance, 0, >))).


%; End of file.
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',170).
% From E: 
% 
% ':-'(call_pel_directive(translate(ending,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.lps.pl'))).
:- call_pel_directive(translate(ending,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.lps.pl')).
