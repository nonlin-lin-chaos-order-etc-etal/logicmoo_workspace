% From E: 
% 
% ':-'(call_pel_directive(translate(unskipped,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SpeechAct.lps.pl'))).
:- call_pel_directive(translate(unskipped,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SpeechAct.lps.pl')).
:-include(library('ec_planner/ec_test_incl')).
:-expects_dialect(lps).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Smoking.e',124).
% From E: 
% 
% ':-'(call_pel_directive(translate(begining,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SpeechAct.lps.pl'))).
:- call_pel_directive(translate(begining,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SpeechAct.lps.pl')).
% Tue, 23 Mar 2021 19:06:58 GMT File: <stream>(0x555567c04c00)%;
%; Copyright (c) 2005 IBM Corporation and others.
%; All rights reserved. This program and the accompanying materials
%; are made available under the terms of the Common Public License v1.0
%; which accompanies this distribution, and is available at
%; http://www.eclipse.org/legal/cpl-v10.html
%;
%; Contributors:
%; IBM - Initial implementation
%;
%; The SpeechAct representation deals with a few speech acts
%; \fullcite{Searle:1969}.
%;
%; @book{Searle:1969,
%;   author = "John R. Searle",
%;   year = "1969",
%;   title = "Speech Acts: An Essay in the Philosophy of Language",
%;   address = "Cambridge",
%;   publisher = "Cambridge University Press",
%; }
%;
%; We handle
%; the illocutionary acts of
%; inviting someone into one's house (a form of request) and
%; greeting someone,
%; and the expressive speech act of crying for joy.
%;
%; inviting in
%; agent1 invites agent2 into room.

% event InviteIn(agent,agent,room)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SpeechAct.e',31).
% From E: 
% 
% event(inviteIn(agent,agent,room)).
events([inviteIn/3]).
mpred_prop(inviteIn(agent,agent,room),action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SpeechAct.e',31).
actions([inviteIn/3]).


%; agent1 is invited into room by agent2.

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SpeechAct.e',34).
% fluent InvitedIn(agent,room,agent)
% From E: 
% 
% fluent(invitedIn(agent,room,agent)).
mpred_prop(invitedIn(agent,room,agent),fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SpeechAct.e',34).
fluents([invitedIn/3]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SpeechAct.e',36).
%; A precondition axiom states that for
%; an agent to invite another agent into a room,
%; the first agent must be in the room and
%; there must be an outside area such that
%; the second agent is at the outside area and
%; the outside area is adjacent to the room:
% [agent1,agent2,room,time]
% Happens(InviteIn(agent1,agent2,room),time) ->
% HoldsAt(At(agent1,room),time) &
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SpeechAct.e',42).
% {outside}% 
% HoldsAt(At(agent2,outside),time) &
% Adjacent(room,outside).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SpeechAct.e',45).
% From E: 
% 
% exists(Outside, 
%    '->'(
%       happens(
%          inviteIn(Agent1,Agent2,Room), 
%          Time), 
%       ','(
%          holds(
%             at_loc(Agent1,Room), 
%             Time), 
%          ','(
%             holds(
%                at_loc(Agent2,Outside), 
%                Time), 
%             adjacent(Room,Outside))))).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SpeechAct.e',45).
exists(Outside, if((at(at_loc(Agent1, Room), Time), at(at_loc(Agent2, Outside), Time), adjacent(Room, Outside)), happens(inviteIn(Agent1, Agent2, Room), Time))).


%; An effect axiom states that if
%; an agent invites another agent into a room,
%; the second agent will be invited into the room by the first agent:
% [agent1,agent2,room,time]
% Initiates(InviteIn(agent1,agent2,room),
%           InvitedIn(agent2,room,agent1),
%           time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SpeechAct.e',52).
% From E: 
% 
% initiates_at(
%    inviteIn(Agent1,Agent2,Room), 
%    invitedIn(Agent2,Room,Agent1), 
%    Time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SpeechAct.e',52).
initiates(inviteIn(Agent1,Agent2,Room),
	  invitedIn(Agent2,Room,Agent1)).


%; agent intends to walk into room.

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SpeechAct.e',58).
% event IntendToWalkIn(agent,room)
% From E: 
% 
% event(intendToWalkIn(agent,room)).
events([intendToWalkIn/2]).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SpeechAct.e',58).
mpred_prop(intendToWalkIn(agent,room),action).
actions([intendToWalkIn/2]).


%; agent has the intention to walk into room.

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SpeechAct.e',60).
% fluent IntentionToWalkIn(agent,room)
% From E: 
% 
% fluent(intentionToWalkIn(agent,room)).
mpred_prop(intentionToWalkIn(agent,room),fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SpeechAct.e',60).
fluents([intentionToWalkIn/2]).


%; agent acts on the intention to walk into room.

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SpeechAct.e',62).
% fluent ActOnIntentionToWalkIn(agent,room)
% From E: 
% 
% fluent(actOnIntentionToWalkIn(agent,room)).
mpred_prop(actOnIntentionToWalkIn(agent,room),fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SpeechAct.e',62).
fluents([actOnIntentionToWalkIn/2]).

% noninertial ActOnIntentionToWalkIn
% From E: 
% 
% ':-'(call_pel_directive(noninertial(actOnIntentionToWalkIn))).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SpeechAct.e',62).
:- call_pel_directive(noninertial(actOnIntentionToWalkIn)).
%; A trigger axiom states that
%; if an agent is invited into a room by another agent,
%; the first agent likes the second agent, and
%; the first agent does not already have the intention to
%; walk into the room,
%; the first agent intends to walk into the room:
% [agent1,agent2,room,time]
% HoldsAt(InvitedIn(agent1,room,agent2),time) &
% HoldsAt(Like(agent1,agent2),time) &
% !HoldsAt(IntentionToWalkIn(agent1,room),time) ->
% Happens(IntendToWalkIn(agent1,room),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SpeechAct.e',71).
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          invitedIn(Agent1,Room,Agent2), 
%          Time), 
%       ','(
%          holds(
%             like(Agent1,Agent2), 
%             Time), 
%          holds(
%             not(intentionToWalkIn(Agent1,Room)), 
%             Time))), 
%    happens(
%       intendToWalkIn(Agent1,Room), 
%       Time)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SpeechAct.e',71).
if(happens(intendToWalkIn(Agent1, Room), Time),  (at(invitedIn(Agent1, Room, Agent2), Time), at(like(Agent1, Agent2), Time), at(not(intentionToWalkIn(Agent1, Room)), Time))).


%; An effect axiom states that
%; if an agent intends to walk into a room,
%; the agent will have the intention to walk into the room:
% [agent,room,time]
% Initiates(IntendToWalkIn(agent,room),
%           IntentionToWalkIn(agent,room),
%           time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SpeechAct.e',80).
% From E: 
% 
% initiates_at(
%    intendToWalkIn(Agent,Room), 
%    intentionToWalkIn(Agent,Room), 
%    Time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SpeechAct.e',80).
initiates(intendToWalkIn(Agent,Room),
	  intentionToWalkIn(Agent,Room)).


%; Two trigger axioms state that
%; if an agent has the intention to walk into a room,
%; the agent acts on the intention to walk into the room,
%; the agent is at a location,
%; side one (two) of a door is the room,
%; side two (one) of the door is the location,
%; agent will walk through side two (one) of the door:
% [agent,room,location,door,time]
% HoldsAt(IntentionToWalkIn(agent,room),time) &
% HoldsAt(ActOnIntentionToWalkIn(agent,room),time) &
% HoldsAt(At(agent,location),time) &
% Side1(door)=room &
% Side2(door)=location ->
% Happens(WalkThroughDoor21(agent,door),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SpeechAct.e',92).
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          intentionToWalkIn(Agent,Room), 
%          Time), 
%       ','(
%          holds(
%             actOnIntentionToWalkIn(Agent,Room), 
%             Time), 
%          ','(
%             holds(
%                at_loc(Agent,Location), 
%                Time), 
%             ','(
%                '='(
%                   side1(Door), 
%                   Room), 
%                '='(
%                   side2(Door), 
%                   Location))))), 
%    happens(
%       walkThroughDoor21(Agent,Door), 
%       Time)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SpeechAct.e',92).
if(happens(walkThroughDoor21(Agent, Door), Time),  (at(intentionToWalkIn(Agent, Room), Time), at(actOnIntentionToWalkIn(Agent, Room), Time), at(at_loc(Agent, Location), Time), side1(Door, Room), side2(Door, Location))).


% [agent,room,location,door,time]
% HoldsAt(IntentionToWalkIn(agent,room),time) &
% HoldsAt(ActOnIntentionToWalkIn(agent,room),time) &
% HoldsAt(At(agent,location),time) &
% Side2(door)=room &
% Side1(door)=location ->
% Happens(WalkThroughDoor12(agent,door),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SpeechAct.e',101).
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          intentionToWalkIn(Agent,Room), 
%          Time), 
%       ','(
%          holds(
%             actOnIntentionToWalkIn(Agent,Room), 
%             Time), 
%          ','(
%             holds(
%                at_loc(Agent,Location), 
%                Time), 
%             ','(
%                '='(
%                   side2(Door), 
%                   Room), 
%                '='(
%                   side1(Door), 
%                   Location))))), 
%    happens(
%       walkThroughDoor12(Agent,Door), 
%       Time)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SpeechAct.e',101).
if(happens(walkThroughDoor12(Agent, Door), Time),  (at(intentionToWalkIn(Agent, Room), Time), at(actOnIntentionToWalkIn(Agent, Room), Time), at(at_loc(Agent, Location), Time), side2(Door, Room), side1(Door, Location))).


%; Two effect axioms state that
%; if side one (two) of a door is a room and
%; an agent walks through side two (one) of the door,
%; the agent will no longer have the intention to
%; walk into the room:
% [agent,room,door,time]
% Side1(door)=room ->
% Terminates(WalkThroughDoor21(agent,door),
%            IntentionToWalkIn(agent,room),
%            time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SpeechAct.e',113).
% From E: 
% 
% '->'(
%    '='(
%       side1(Door), 
%       Room), 
%    terminates_at(
%       walkThroughDoor21(Agent,Door), 
%       intentionToWalkIn(Agent,Room), 
%       Time)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SpeechAct.e',113).
if(terminates(walkThroughDoor21(Agent,Door),
	      at(intentionToWalkIn(Agent,Room),Time)),
   side1(Door,Room)).


% [agent,room,door,time]
% Side2(door)=room ->
% Terminates(WalkThroughDoor12(agent,door),
%            IntentionToWalkIn(agent,room),
%            time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SpeechAct.e',120).
% From E: 
% 
% '->'(
%    '='(
%       side2(Door), 
%       Room), 
%    terminates_at(
%       walkThroughDoor12(Agent,Door), 
%       intentionToWalkIn(Agent,Room), 
%       Time)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SpeechAct.e',120).
if(terminates(walkThroughDoor12(Agent,Door),
	      at(intentionToWalkIn(Agent,Room),Time)),
   side2(Door,Room)).


%; agent greets object.

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SpeechAct.e',126).
% event Greet(agent,object)
% From E: 
% 
% event(greet(agent,object)).
events([greet/2]).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SpeechAct.e',126).
mpred_prop(greet(agent,object),action).
actions([greet/2]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SpeechAct.e',128).
% event SayPleasedToMeet(agent,agent)
% From E: 
% 
% event(sayPleasedToMeet(agent,agent)).
events([sayPleasedToMeet/2]).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SpeechAct.e',128).
mpred_prop(sayPleasedToMeet(agent,agent),action).
actions([sayPleasedToMeet/2]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SpeechAct.e',130).
%; agent says goodbye to object.

% event SayGoodbye(agent,object)
% From E: 
% 
% event(sayGoodbye(agent,object)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SpeechAct.e',130).
events([sayGoodbye/2]).
mpred_prop(sayGoodbye(agent,object),action).
actions([sayGoodbye/2]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SpeechAct.e',133).
% event TalkAbout(agent,content)
% From E: 
% 
% event(talkAbout(agent,content)).
events([talkAbout/2]).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SpeechAct.e',133).
mpred_prop(talkAbout(agent,content),action).
actions([talkAbout/2]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SpeechAct.e',135).
% event Converse(agent,agent)
% From E: 
% 
% event(converse(agent,agent)).
events([converse/2]).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SpeechAct.e',135).
mpred_prop(converse(agent,agent),action).
actions([converse/2]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SpeechAct.e',137).
% [agent1,agent2,time]
% Happens(Converse(agent1,agent2),time) ->
% {location}% 
% HoldsAt(At(agent1,location),time) &
% HoldsAt(At(agent2,location),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SpeechAct.e',139).
% From E: 
% 
% exists(Location, 
%    '->'(
%       happens(
%          converse(Agent1,Agent2), 
%          Time), 
%       ','(
%          holds(
%             at_loc(Agent1,Location), 
%             Time), 
%          holds(
%             at_loc(Agent2,Location), 
%             Time)))).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SpeechAct.e',139).
exists(Location, if((at(at_loc(Agent1, Location), Time), at(at_loc(Agent2, Location), Time)), happens(converse(Agent1, Agent2), Time))).


%; A precondition axiom states that for
%; an agent to greet an object,
%; there must be a location such that
%; the agent is at the location and
%; the object is at the location:
% [agent,object,time]
% Happens(Greet(agent,object),time) ->
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SpeechAct.e',148).
% {location}% 
% HoldsAt(At(agent,location),time) &
% HoldsAt(At(object,location),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SpeechAct.e',150).
% From E: 
% 
% exists(Location, 
%    '->'(
%       happens(
%          greet(Agent,Object), 
%          Time), 
%       ','(
%          holds(
%             at_loc(Agent,Location), 
%             Time), 
%          holds(
%             at_loc(Object,Location), 
%             Time)))).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SpeechAct.e',150).
exists(Location, if((at(at_loc(Agent, Location), Time), at(at_loc(Object, Location), Time)), happens(greet(Agent, Object), Time))).


% [agent,object,time]
% Happens(SayGoodbye(agent,object),time) ->
% {location}% 
% HoldsAt(At(agent,location),time) &
% HoldsAt(At(object,location),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SpeechAct.e',155).
% From E: 
% 
% exists(Location, 
%    '->'(
%       happens(
%          sayGoodbye(Agent,Object), 
%          Time), 
%       ','(
%          holds(
%             at_loc(Agent,Location), 
%             Time), 
%          holds(
%             at_loc(Object,Location), 
%             Time)))).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SpeechAct.e',155).
exists(Location, if((at(at_loc(Agent, Location), Time), at(at_loc(Object, Location), Time)), happens(sayGoodbye(Agent, Object), Time))).


%; speech: expression of emotions
%; agent cries for joy.

% event CryForJoy(agent)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SpeechAct.e',162).
% From E: 
% 
% event(cryForJoy(agent)).
events([cryForJoy/1]).
mpred_prop(cryForJoy(agent),action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SpeechAct.e',162).
actions([cryForJoy/1]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SpeechAct.e',165).
%; A precondition axiom states that for
%; an agent to cry for joy,
%; the agent must be happy:
% [agent,time]
% Happens(CryForJoy(agent),time) ->
% HoldsAt(Happy(agent),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SpeechAct.e',168).
% From E: 
% 
% '->'(
%    happens(
%       cryForJoy(Agent), 
%       Time), 
%    holds(
%       happy(Agent), 
%       Time)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SpeechAct.e',168).
if(at(happy(Agent),Time),
   happens(cryForJoy(Agent),Time)).

% event Threaten(agent,agent,weapon)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SpeechAct.e',171).
% From E: 
% 
% event(threaten(agent,agent,weapon)).
events([threaten/3]).
mpred_prop(threaten(agent,agent,weapon),action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SpeechAct.e',171).
actions([threaten/3]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SpeechAct.e',174).
% event ReleaseFromThreat(agent,agent)
% From E: 
% 
% event(releaseFromThreat(agent,agent)).
events([releaseFromThreat/2]).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SpeechAct.e',174).
mpred_prop(releaseFromThreat(agent,agent),action).
actions([releaseFromThreat/2]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SpeechAct.e',176).
% fluent ThreatenedBy(agent,agent)
% From E: 
% 
% fluent(threatenedBy(agent,agent)).
mpred_prop(threatenedBy(agent,agent),fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SpeechAct.e',176).
fluents([threatenedBy/2]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SpeechAct.e',178).
% [agent1,agent2,weapon,time]
% Happens(Threaten(agent1,agent2,weapon), time) ->
% HoldsAt(Holding(agent1,weapon),time) &
% {location}% 
% HoldsAt(At(agent1,location),time) &
% HoldsAt(At(agent2,location),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SpeechAct.e',181).
% From E: 
% 
% exists(Location, 
%    '->'(
%       happens(
%          threaten(Agent1,Agent2,Weapon), 
%          Time), 
%       ','(
%          holds(
%             holding(Agent1,Weapon), 
%             Time), 
%          ','(
%             holds(
%                at_loc(Agent1,Location), 
%                Time), 
%             holds(
%                at_loc(Agent2,Location), 
%                Time))))).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SpeechAct.e',181).
exists(Location, if((at(holding(Agent1, Weapon), Time), at(at_loc(Agent1, Location), Time), at(at_loc(Agent2, Location), Time)), happens(threaten(Agent1, Agent2, Weapon), Time))).


% [agent1,agent2,weapon,time]
% Happens(Threaten(agent1,agent2,weapon), time) ->
% Happens(BecomeAngryAt(agent2,agent1),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SpeechAct.e',186).
% From E: 
% 
% '->'(
%    happens(
%       threaten(Agent1,Agent2,Weapon), 
%       Time), 
%    happens(
%       becomeAngryAt(Agent2,Agent1), 
%       Time)).
if(happens(becomeAngryAt(Agent2,Agent1),Time),
   happens(threaten(Agent1,Agent2,Weapon),Time)).


% [agent1,agent2,weapon,time]
% Initiates(Threaten(agent1,agent2,weapon),
%           ThreatenedBy(agent2,agent1),
%           time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SpeechAct.e',190).
% From E: 
% 
% initiates_at(
%    threaten(Agent1,Agent2,Weapon), 
%    threatenedBy(Agent2,Agent1), 
%    Time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SpeechAct.e',190).
initiates(threaten(Agent1,Agent2,Weapon),
	  threatenedBy(Agent2,Agent1)).


% [agent1,agent2,time]
% Terminates(ReleaseFromThreat(agent1,agent2),
%            ThreatenedBy(agent2,agent1),
%            time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SpeechAct.e',195).
% From E: 
% 
% terminates_at(
%    releaseFromThreat(Agent1,Agent2), 
%    threatenedBy(Agent2,Agent1), 
%    Time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SpeechAct.e',195).
terminates(releaseFromThreat(Agent1,Agent2),
	   threatenedBy(Agent2,Agent1)).

% event Order(agent,agent,physobj)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SpeechAct.e',198).
% From E: 
% 
% event(order(agent,agent,physobj)).
events([order/3]).
mpred_prop(order(agent,agent,physobj),action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SpeechAct.e',198).
actions([order/3]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SpeechAct.e',201).
% fluent KnowOrder(agent,agent,physobj)
% From E: 
% 
% fluent(knowOrder(agent,agent,physobj)).
mpred_prop(knowOrder(agent,agent,physobj),fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SpeechAct.e',201).
fluents([knowOrder/3]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SpeechAct.e',203).
% [agent1,agent2,physobj,time]
% Initiates(Order(agent1,agent2,physobj),
%           KnowOrder(agent2,agent1,physobj),
%           time).
% From E: 
% 
% initiates_at(
%    order(Agent1,Agent2,Physobj), 
%    knowOrder(Agent2,Agent1,Physobj), 
%    Time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SpeechAct.e',203).
initiates(order(Agent1,Agent2,Physobj),
	  knowOrder(Agent2,Agent1,Physobj)).


% [agent1,agent2,physobj,time]
% Happens(Order(agent1,agent2,physobj),time) ->
% {location}% 
% HoldsAt(At(agent1,location),time) &
% HoldsAt(At(agent2,location),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SpeechAct.e',209).
% From E: 
% 
% exists(Location, 
%    '->'(
%       happens(
%          order(Agent1,Agent2,Physobj), 
%          Time), 
%       ','(
%          holds(
%             at_loc(Agent1,Location), 
%             Time), 
%          holds(
%             at_loc(Agent2,Location), 
%             Time)))).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SpeechAct.e',209).
exists(Location, if((at(at_loc(Agent1, Location), Time), at(at_loc(Agent2, Location), Time)), happens(order(Agent1, Agent2, Physobj), Time))).

% event Request(agent,agent,physobj)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SpeechAct.e',213).
% From E: 
% 
% event(request(agent,agent,physobj)).
events([request/3]).
mpred_prop(request(agent,agent,physobj),action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SpeechAct.e',213).
actions([request/3]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SpeechAct.e',216).
% fluent KnowRequest(agent,agent,physobj)
% From E: 
% 
% fluent(knowRequest(agent,agent,physobj)).
mpred_prop(knowRequest(agent,agent,physobj),fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SpeechAct.e',216).
fluents([knowRequest/3]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SpeechAct.e',218).
% [agent1,agent2,physobj,time]
% Initiates(Request(agent1,agent2,physobj),
%           KnowRequest(agent2,agent1,physobj),
%           time).
% From E: 
% 
% initiates_at(
%    request(Agent1,Agent2,Physobj), 
%    knowRequest(Agent2,Agent1,Physobj), 
%    Time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SpeechAct.e',218).
initiates(request(Agent1,Agent2,Physobj),
	  knowRequest(Agent2,Agent1,Physobj)).


% [agent1,agent2,physobj,time]
% Happens(Request(agent1,agent2,physobj),time) ->
% {location}% 
% HoldsAt(At(agent1,location),time) &
% HoldsAt(At(agent2,location),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SpeechAct.e',224).
% From E: 
% 
% exists(Location, 
%    '->'(
%       happens(
%          request(Agent1,Agent2,Physobj), 
%          Time), 
%       ','(
%          holds(
%             at_loc(Agent1,Location), 
%             Time), 
%          holds(
%             at_loc(Agent2,Location), 
%             Time)))).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SpeechAct.e',224).
exists(Location, if((at(at_loc(Agent1, Location), Time), at(at_loc(Agent2, Location), Time)), happens(request(Agent1, Agent2, Physobj), Time))).


%; End of file.
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SpeechAct.e',228).
% From E: 
% 
% ':-'(call_pel_directive(translate(ending,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SpeechAct.lps.pl'))).
:- call_pel_directive(translate(ending,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SpeechAct.lps.pl')).
