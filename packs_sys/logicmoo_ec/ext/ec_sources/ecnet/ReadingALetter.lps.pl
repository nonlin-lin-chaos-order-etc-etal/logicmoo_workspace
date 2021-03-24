% From E: 
% 
% ':-'(call_pel_directive(translate(unskipped,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingALetter.lps.pl'))).
:- call_pel_directive(translate(unskipped,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingALetter.lps.pl')).
:-include(library('ec_planner/ec_test_incl')).
:-expects_dialect(lps).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingABook.e',103).
% From E: 
% 
% ':-'(call_pel_directive(translate(begining,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingALetter.lps.pl'))).
:- call_pel_directive(translate(begining,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingALetter.lps.pl')).
% Tue, 23 Mar 2021 19:06:53 GMT File: <stream>(0x555555fa9b00)%;
%; Copyright (c) 2005 IBM Corporation and others.
%; All rights reserved. This program and the accompanying materials
%; are made available under the terms of the Common Public License v1.0
%; which accompanies this distribution, and is available at
%; http://www.eclipse.org/legal/cpl-v10.html
%;
%; Contributors:
%; IBM - Initial implementation
%;
%; @article{Mueller:2004c,
%;   author = "Erik T. Mueller",
%;   year = "2004",
%;   title = "Understanding script-based stories using commonsense reasoning",
%;   journal = "Cognitive Systems Research",
%;   volume = "5",
%;   number = "4",
%;   pages = "307--340",
%; }
%;

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingALetter.e',22).
% option modeldiff on
% From E: 
% 
% ':-'(call_pel_directive(option(modeldiff,on))).
:- call_pel_directive(option(modeldiff, on)).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingALetter.e',24).
% ignore Love, ThreatenedBy
% From E: 
% 
% ':-'(call_pel_directive(ignore(love))).
:- call_pel_directive(ignore(love)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingALetter.e',24).
% From E: 
% 
% ':-'(call_pel_directive(ignore(threatenedBy))).
:- call_pel_directive(ignore(threatenedBy)).

% ignore LookOutOnto, Floor, BuildingOf, SkyOf, GroundOf
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingALetter.e',24).
% From E: 
% 
% ':-'(call_pel_directive(ignore(lookOutOnto))).
:- call_pel_directive(ignore(lookOutOnto)).
% From E: 
% 
% ':-'(call_pel_directive(ignore(floor))).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingALetter.e',24).
:- call_pel_directive(ignore(floor)).
% From E: 
% 
% ':-'(call_pel_directive(ignore(buildingOf))).
:- call_pel_directive(ignore(buildingOf)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingALetter.e',24).
% From E: 
% 
% ':-'(call_pel_directive(ignore(skyOf))).
:- call_pel_directive(ignore(skyOf)).
% From E: 
% 
% ':-'(call_pel_directive(ignore(groundOf))).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingALetter.e',24).
:- call_pel_directive(ignore(groundOf)).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingALetter.e',26).
% ignore Near, WalkFrom, WalkFromTo, RunFromTo
% From E: 
% 
% ':-'(call_pel_directive(ignore(near))).
:- call_pel_directive(ignore(near)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingALetter.e',26).
% From E: 
% 
% ':-'(call_pel_directive(ignore(walkFrom))).
:- call_pel_directive(ignore(walkFrom)).
% From E: 
% 
% ':-'(call_pel_directive(ignore(walkFromTo))).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingALetter.e',26).
:- call_pel_directive(ignore(walkFromTo)).
% From E: 
% 
% ':-'(call_pel_directive(ignore(runFromTo))).
:- call_pel_directive(ignore(runFromTo)).

% ignore Side1, Side2
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingALetter.e',26).
% From E: 
% 
% ':-'(call_pel_directive(ignore(side1))).
:- call_pel_directive(ignore(side1)).
% From E: 
% 
% ':-'(call_pel_directive(ignore(side2))).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingALetter.e',26).
:- call_pel_directive(ignore(side2)).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingALetter.e',29).
% load foundations/Root.e
% From E: 
% 
% ':-'(call_pel_directive(load('foundations/Root.e'))).
:- call_pel_directive(load('foundations/Root.e')).

% load foundations/EC.e
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingALetter.e',29).
% From E: 
% 
% ':-'(call_pel_directive(load('foundations/EC.e'))).
:- call_pel_directive(load('foundations/EC.e')).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingALetter.e',31).
% load answers/Mueller2003/Ontology.e
% From E: 
% 
% ':-'(call_pel_directive(load('answers/Mueller2003/Ontology.e'))).
:- call_pel_directive(load('answers/Mueller2003/Ontology.e')).

% load answers/Mueller2004c/RTSpaceM.e
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingALetter.e',31).
% From E: 
% 
% ':-'(call_pel_directive(load('answers/Mueller2004c/RTSpaceM.e'))).
:- call_pel_directive(load('answers/Mueller2004c/RTSpaceM.e')).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingALetter.e',33).
% load answers/Mueller2004c/OTSpaceM.e
% From E: 
% 
% ':-'(call_pel_directive(load('answers/Mueller2004c/OTSpaceM.e'))).
:- call_pel_directive(load('answers/Mueller2004c/OTSpaceM.e')).

% load answers/Mueller2004c/Container.e
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingALetter.e',33).
% From E: 
% 
% ':-'(call_pel_directive(load('answers/Mueller2004c/Container.e'))).
:- call_pel_directive(load('answers/Mueller2004c/Container.e')).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingALetter.e',35).
% load answers/Mueller2004c/Cognition.e
% From E: 
% 
% ':-'(call_pel_directive(load('answers/Mueller2004c/Cognition.e'))).
:- call_pel_directive(load('answers/Mueller2004c/Cognition.e')).

% load answers/Mueller2003/Sleep.e
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingALetter.e',35).
% From E: 
% 
% ':-'(call_pel_directive(load('answers/Mueller2003/Sleep.e'))).
:- call_pel_directive(load('answers/Mueller2003/Sleep.e')).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingALetter.e',37).
% load answers/Mueller2003/Vision.e
% From E: 
% 
% ':-'(call_pel_directive(load('answers/Mueller2003/Vision.e'))).
:- call_pel_directive(load('answers/Mueller2003/Vision.e')).

% load answers/Mueller2004c/HandTo.e
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingALetter.e',37).
% From E: 
% 
% ':-'(call_pel_directive(load('answers/Mueller2004c/HandTo.e'))).
:- call_pel_directive(load('answers/Mueller2004c/HandTo.e')).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingALetter.e',40).
% location Location1
% From E: 
% 
% t(location,location1).
isa(location1,location).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingALetter.e',42).
% portal DummyPortal1
% From E: 
% 
% t(portal,dummyPortal1).
isa(dummyPortal1,portal).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingALetter.e',44).
% agent Recipient1
% From E: 
% 
% t(agent,recipient1).
isa(recipient1,agent).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingALetter.e',46).
% letter Letter1
% From E: 
% 
% t(letter,letter1).
isa(letter1,letter).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingALetter.e',48).
% container Envelope1
% From E: 
% 
% t(container,envelope1).
isa(envelope1,container).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingALetter.e',50).
% physobj Surface1
% From E: 
% 
% t(physobj,surface1).
isa(surface1,physobj).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingALetter.e',52).
% chair Chair1
% From E: 
% 
% t(chair,chair1).
isa(chair1,chair).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingALetter.e',54).
% content Content1
% From E: 
% 
% t(content,content1).
isa(content1,content).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingALetter.e',56).
% agent Carrier1
% From E: 
% 
% t(agent,carrier1).
isa(carrier1,agent).
%; prune

% sort ona, onb
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingALetter.e',58).
% From E: 
% 
% sort(ona).
sort(ona).
% From E: 
% 
% sort(onb).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingALetter.e',58).
sort(onb).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingALetter.e',60).
% fluent! On(ona,onb)
% From E: 
% 
% fluent(on(ona,onb)).
mpred_prop(on(ona,onb),fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingALetter.e',60).
fluents([on/2]).

% event! PlaceOn(agent,ona,onb)
% From E: 
% 
% event(placeOn(agent,ona,onb)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingALetter.e',60).
events([placeOn/3]).
mpred_prop(placeOn(agent,ona,onb),action).
actions([placeOn/3]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingALetter.e',62).
% event! TakeOffOf(agent,ona,onb)
% From E: 
% 
% event(takeOffOf(agent,ona,onb)).
events([takeOffOf/3]).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingALetter.e',62).
mpred_prop(takeOffOf(agent,ona,onb),action).
actions([takeOffOf/3]).

% ona! Envelope1, Letter1
% From E: 
% 
% t(ona,envelope1).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingALetter.e',62).
isa(envelope1,ona).
% From E: 
% 
% t(ona,letter1).
isa(letter1,ona).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingALetter.e',64).
% onb! Surface1
% From E: 
% 
% t(onb,surface1).
isa(surface1,onb).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingALetter.e',66).
% sort insidea, insideb
% From E: 
% 
% sort(insidea).
sort(insidea).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingALetter.e',66).
% From E: 
% 
% sort(insideb).
sort(insideb).

% fluent! Inside(insidea,insideb)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingALetter.e',66).
% From E: 
% 
% fluent(inside(insidea,insideb)).
mpred_prop(inside(insidea,insideb),fluent).
fluents([inside/2]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingALetter.e',68).
% event! PutInside(agent,insidea,insideb)
% From E: 
% 
% event(putInside(agent,insidea,insideb)).
events([putInside/3]).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingALetter.e',68).
mpred_prop(putInside(agent,insidea,insideb),action).
actions([putInside/3]).

% event! TakeOutOf(agent,insidea,insideb)
% From E: 
% 
% event(takeOutOf(agent,insidea,insideb)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingALetter.e',68).
events([takeOutOf/3]).
mpred_prop(takeOutOf(agent,insidea,insideb),action).
actions([takeOutOf/3]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingALetter.e',70).
% insidea! Letter1
% From E: 
% 
% t(insidea,letter1).
isa(letter1,insidea).

% insideb! Envelope1
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingALetter.e',70).
% From E: 
% 
% t(insideb,envelope1).
isa(envelope1,insideb).
%; initial state
% [agent,physobj]
 % !HoldsAt(SittingOn(agent,physobj),0).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingALetter.e',73).
% From E: 
% 
% holds(
%    not(sittingOn(Agent,Physobj)), 0).
initially(not(sittingOn(Agent,Physobj))).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingALetter.e',75).
% [agent,physobj]
 % !HoldsAt(LyingOn(agent,physobj),0).
% From E: 
% 
% holds(
%    not(lyingOn(Agent,Physobj)), 0).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingALetter.e',75).
initially(not(lyingOn(Agent,Physobj))).


% HoldsAt(Dressed(Recipient1),0).
% From E: 
% 
% holds(
%    dressed(recipient1), 0).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingALetter.e',75).
initially(dressed(recipient1)).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingALetter.e',77).
% HoldsAt(Dressed(Carrier1),0).
% From E: 
% 
% holds(
%    dressed(carrier1), 0).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingALetter.e',77).
initially(dressed(carrier1)).


% HoldsAt(Awake(Recipient1),0).
% From E: 
% 
% holds(
%    awake(recipient1), 0).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingALetter.e',77).
initially(awake(recipient1)).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingALetter.e',79).
% HoldsAt(Awake(Carrier1),0).
% From E: 
% 
% holds(
%    awake(carrier1), 0).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingALetter.e',79).
initially(awake(carrier1)).


% HoldsAt(Sleep3(Recipient1),0).
% From E: 
% 
% holds(
%    sleep3(recipient1), 0).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingALetter.e',79).
initially(sleep3(recipient1)).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingALetter.e',81).
% HoldsAt(Sleep3(Carrier1),0).
% From E: 
% 
% holds(
%    sleep3(carrier1), 0).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingALetter.e',81).
initially(sleep3(carrier1)).


% HoldsAt(Standing(Recipient1),0).
% From E: 
% 
% holds(
%    standing(recipient1), 0).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingALetter.e',81).
initially(standing(recipient1)).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingALetter.e',83).
% HoldsAt(Standing(Carrier1),0).
% From E: 
% 
% holds(
%    standing(carrier1), 0).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingALetter.e',83).
initially(standing(carrier1)).


% HoldsAt(ContainerClosed(Envelope1),0).
% From E: 
% 
% holds(
%    containerClosed(envelope1), 0).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingALetter.e',83).
initially(containerClosed(envelope1)).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingALetter.e',85).
% HoldsAt(Inside(Letter1,Envelope1),0).
% From E: 
% 
% holds(
%    inside(letter1,envelope1), 0).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingALetter.e',85).
initially(inside(letter1,envelope1)).


% [physobj1,physobj2]
% !(physobj1=Letter1 & physobj2=Envelope1) ->
% !HoldsAt(Inside(physobj1, physobj2),0).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingALetter.e',87).
% From E: 
% 
% '->'(
%    not(','(
%           Physobj1=letter1, 
%           Physobj2=envelope1)), 
%    holds(
%       not(inside(Physobj1,Physobj2)), 0)).
if(at(not(inside(Physobj1, Physobj2)), 0), not((equals(Physobj1, letter1), equals(Physobj2, envelope1)))).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingALetter.e',89).
% [agent,object]
 % !HoldsAt(See(agent,object),0).
% From E: 
% 
% holds(
%    not(see(Agent,Object)), 0).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingALetter.e',89).
initially(not(see(Agent,Object))).


% [agent,object]
% !(agent=Carrier1 & object=Envelope1) ->
% !HoldsAt(Holding(agent,object),0).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingALetter.e',91).
% From E: 
% 
% '->'(
%    not(','(
%           Agent=carrier1, 
%           Object=envelope1)), 
%    holds(
%       not(holding(Agent,Object)), 0)).
if(at(not(holding(Agent, Object)), 0), not((equals(Agent, carrier1), equals(Object, envelope1)))).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingALetter.e',93).
% HoldsAt(Holding(Carrier1,Envelope1),0).
% From E: 
% 
% holds(
%    holding(carrier1,envelope1), 0).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingALetter.e',93).
initially(holding(carrier1,envelope1)).


% [physobj1,physobj2]
 % !HoldsAt(On(physobj1, physobj2),0).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingALetter.e',93).
% From E: 
% 
% holds(
%    not(on(Physobj1,Physobj2)), 0).
initially(not(on(Physobj1,Physobj2))).


%; narrative


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingALetter.e',97).
% Happens(PlaceOn(Carrier1,Envelope1,Surface1),0).
% From E: 
% 
% happens(
%    placeOn(carrier1,envelope1,surface1), 0).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingALetter.e',97).
observe(at(placeOn(carrier1,envelope1,surface1),0)).


% Happens(TakeOffOf(Recipient1,Envelope1,Surface1),1).
% From E: 
% 
% happens(
%    takeOffOf(recipient1,envelope1,surface1), 1).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingALetter.e',97).
observe(at(takeOffOf(recipient1,envelope1,surface1),1)).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingALetter.e',99).
%;Happens(HandTo(Carrier1,Recipient1,Envelope1),0).


% Happens(SitOn(Recipient1,Chair1),2).
% From E: 
% 
% happens(
%    sitOn(recipient1,chair1), 2).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingALetter.e',99).
observe(at(sitOn(recipient1,chair1),2)).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingALetter.e',101).
% Happens(ContainerOpen(Recipient1,Envelope1),3).
% From E: 
% 
% happens(
%    containerOpen(recipient1,envelope1), 3).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingALetter.e',101).
observe(at(containerOpen(recipient1,envelope1),3)).


% Happens(TakeOutOf(Recipient1,Letter1,Envelope1),4).
% From E: 
% 
% happens(
%    takeOutOf(recipient1,letter1,envelope1), 4).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingALetter.e',101).
observe(at(takeOutOf(recipient1,letter1,envelope1),4)).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingALetter.e',103).
% Happens(LookAt(Recipient1,Letter1),5).
% From E: 
% 
% happens(
%    lookAt(recipient1,letter1), 5).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingALetter.e',103).
observe(at(lookAt(recipient1,letter1),5)).


% Happens(Read(Recipient1,Letter1,Content1),6).
% From E: 
% 
% happens(
%    read(recipient1,letter1,content1), 6).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingALetter.e',103).
observe(at(read(recipient1,letter1,content1),6)).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingALetter.e',105).
% Happens(ThinkAbout(Recipient1,Content1),7).
% From E: 
% 
% happens(
%    thinkAbout(recipient1,content1), 7).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingALetter.e',105).
observe(at(thinkAbout(recipient1,content1),7)).


% Happens(Understand(Recipient1,Content1),8).
% From E: 
% 
% happens(
%    understand(recipient1,content1), 8).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingALetter.e',105).
observe(at(understand(recipient1,content1),8)).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingALetter.e',107).
% Happens(PutInside(Recipient1,Letter1,Envelope1),9).
% From E: 
% 
% happens(
%    putInside(recipient1,letter1,envelope1), 9).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingALetter.e',107).
observe(at(putInside(recipient1,letter1,envelope1),9)).


% Happens(RiseFrom(Recipient1,Chair1),10).
% From E: 
% 
% happens(
%    riseFrom(recipient1,chair1), 10).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingALetter.e',107).
observe(at(riseFrom(recipient1,chair1),10)).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingALetter.e',109).
% Happens(PlaceOn(Recipient1,Envelope1,Surface1),11).
% From E: 
% 
% happens(
%    placeOn(recipient1,envelope1,surface1), 11).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingALetter.e',109).
observe(at(placeOn(recipient1,envelope1,surface1),11)).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingALetter.e',111).
% range time 0 12
% From E: 
% 
% ':-'(call_pel_directive(range(time,0,12))).
:- call_pel_directive(range(time, 0, 12)).

% range offset 0 0
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingALetter.e',111).
% From E: 
% 
% ':-'(call_pel_directive(range(offset,0,0))).
:- call_pel_directive(range(offset, 0, 0)).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingALetter.e',113).
% range diameter 0 0
% From E: 
% 
% ':-'(call_pel_directive(range(diameter,0,0))).
:- call_pel_directive(range(diameter, 0, 0)).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingALetter.e',115).
% completion Happens
% From E: 
% 
% ':-'(call_pel_directive(completion(happens))).
:- call_pel_directive(completion(happens)).
%; End of file.
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingALetter.e',117).
% From E: 
% 
% ':-'(call_pel_directive(translate(ending,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingALetter.lps.pl'))).
:- call_pel_directive(translate(ending,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingALetter.lps.pl')).
