% From E: 
% 
% ':-'(call_pel_directive(translate(unskipped,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Bomb.lps.pl'))).
:- call_pel_directive(translate(unskipped,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Bomb.lps.pl')).
:-include(library('ec_planner/ec_test_incl')).
:-expects_dialect(lps).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Baseball.e',157).
% From E: 
% 
% ':-'(call_pel_directive(translate(begining,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Bomb.lps.pl'))).
:- call_pel_directive(translate(begining,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Bomb.lps.pl')).
% Tue, 23 Mar 2021 19:25:05 GMT File: <stream>(0x5555684a6900)%;
%; Copyright (c) 2005 IBM Corporation and others.
%; All rights reserved. This program and the accompanying materials
%; are made available under the terms of the Common Public License v1.0
%; which accompanies this distribution, and is available at
%; http://www.eclipse.org/legal/cpl-v10.html
%;
%; Contributors:
%; IBM - Initial implementation
%;
%; bomb
%; agent is nondeterministically killed.

% fluent KilledDeterminingFluent(agent)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Bomb.e',13).
% From E: 
% 
% fluent(killedDeterminingFluent(agent)).
mpred_prop(killedDeterminingFluent(agent), fluent).
fluents([killedDeterminingFluent/1]).

% noninertial KilledDeterminingFluent
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Bomb.e',15).
% From E: 
% 
% ':-'(call_pel_directive(noninertial(killedDeterminingFluent))).
:- call_pel_directive(noninertial(killedDeterminingFluent)).
%; agent is nondeterministically injured.

% fluent InjuredDeterminingFluent(agent)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Bomb.e',17).
% From E: 
% 
% fluent(injuredDeterminingFluent(agent)).
mpred_prop(injuredDeterminingFluent(agent), fluent).
fluents([injuredDeterminingFluent/1]).

% noninertial InjuredDeterminingFluent
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Bomb.e',19).
% From E: 
% 
% ':-'(call_pel_directive(noninertial(injuredDeterminingFluent))).
:- call_pel_directive(noninertial(injuredDeterminingFluent)).
%; physobj is nondeterministically destroyed.

% fluent DestroyedDeterminingFluent(physobj)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Bomb.e',21).
% From E: 
% 
% fluent(destroyedDeterminingFluent(physobj)).
mpred_prop(destroyedDeterminingFluent(physobj), fluent).
fluents([destroyedDeterminingFluent/1]).

% noninertial DestroyedDeterminingFluent
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Bomb.e',23).
% From E: 
% 
% ':-'(call_pel_directive(noninertial(destroyedDeterminingFluent))).
:- call_pel_directive(noninertial(destroyedDeterminingFluent)).
%; physobj is nondeterministically damaged.

% fluent DamagedDeterminingFluent(physobj)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Bomb.e',25).
% From E: 
% 
% fluent(damagedDeterminingFluent(physobj)).
mpred_prop(damagedDeterminingFluent(physobj), fluent).
fluents([damagedDeterminingFluent/1]).

% noninertial DamagedDeterminingFluent
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Bomb.e',27).
% From E: 
% 
% ':-'(call_pel_directive(noninertial(damagedDeterminingFluent))).
:- call_pel_directive(noninertial(damagedDeterminingFluent)).
%; agent activates bomb.

% event BombActivate(agent,bomb)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Bomb.e',29).
% From E: 
% 
% event(bombActivate(agent,bomb)).
events([bombActivate/2]).
mpred_prop(bombActivate(agent, bomb), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Bomb.e',29).
actions([bombActivate/2]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Bomb.e',32).
%; agent deactivates bomb.

% event BombDeactivate(agent,bomb)
% From E: 
% 
% event(bombDeactivate(agent,bomb)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Bomb.e',32).
events([bombDeactivate/2]).
mpred_prop(bombDeactivate(agent, bomb), action).
actions([bombDeactivate/2]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Bomb.e',35).
%; bomb explodes.

% event BombExplode(bomb)
% From E: 
% 
% event(bombExplode(bomb)).
mpred_prop(bombExplode(bomb), event).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Bomb.e',35).
events([bombExplode/1]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Bomb.e',38).
%; bomb is activated.

% fluent BombActivated(bomb)
% From E: 
% 
% fluent(bombActivated(bomb)).
mpred_prop(bombActivated(bomb), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Bomb.e',38).
fluents([bombActivated/1]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Bomb.e',41).
%; The timer value of bomb is offset.

% fluent BombTimerValue(bomb,offset)
% From E: 
% 
% fluent(bombTimerValue(bomb,offset)).
mpred_prop(bombTimerValue(bomb, offset), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Bomb.e',41).
fluents([bombTimerValue/2]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Bomb.e',44).
%; The timer value of bomb is decremented.

% event BombDecrementTimer(bomb)
% From E: 
% 
% event(bombDecrementTimer(bomb)).
mpred_prop(bombDecrementTimer(bomb), event).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Bomb.e',44).
events([bombDecrementTimer/1]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Bomb.e',47).
%; The time delay of bomb is offset.

% function BombTimeDelay(bomb): offset
% From E: 
% 
% function(
%    bombTimeDelay(bomb), 
%    offset).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Bomb.e',47).
function(bombTimeDelay(bomb),offset).
%; A state constraint says that a bomb has one timer
%; value at a time:
% [bomb,offset1,offset2,time]
% HoldsAt(BombTimerValue(bomb,offset1),time) &
% HoldsAt(BombTimerValue(bomb,offset2),time) ->
% offset1=offset2.
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Bomb.e',52).
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          bombTimerValue(Bomb,Offset1), 
%          Time), 
%       holds(
%          bombTimerValue(Bomb,Offset2), 
%          Time)), 
%    Offset1=Offset2).
not equals(Offset1, Offset2)if not bombTimerValue(Bomb, Offset1)at Time;not bombTimerValue(Bomb, Offset2)at Time.
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Bomb.e',52).

 /*   l_int(holds(not(equals(Offset1, Offset2)), Time_at),
              [  (at(not(bombTimerValue(Bomb, Offset1)), Time);at(not(bombTimerValue(Bomb, Offset2)), Time))
              ]).
 */
 %  "% =================================".


%; An effect axiom states that if a bomb is intact and
%; an agent activates the bomb,
%; the bomb will be activated:
% [agent,bomb,time]
% HoldsAt(Intact(bomb),time) ->
% Initiates(BombActivate(agent,bomb),
%           BombActivated(bomb),
%           time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Bomb.e',60).
% From E: 
% 
% '->'(
%    holds(
%       intact(Bomb), 
%       Time), 
%    initiates_at(
%       bombActivate(Agent,Bomb), 
%       bombActivated(Bomb), 
%       Time)).
not (bombActivate(Agent, Bomb)initiates bombActivated(Bomb)at Time)if not intact(Bomb)at Time.
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Bomb.e',60).

 /*  l_int(holds(not(initiates(bombActivate(Agent,Bomb),
     			  at(bombActivated(Bomb),Time))),
     	    Time),
           [holds(not(intact(Bomb)),Time)]).
 */
 %  "% =================================".


%; A precondition axiom states that
%; for an agent to activate a bomb,
%; the agent must be holding the bomb:
% [agent,bomb,time]
% Happens(BombActivate(agent,bomb),time) ->
% HoldsAt(Holding(agent,bomb),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Bomb.e',69).
% From E: 
% 
% '->'(
%    happens(
%       bombActivate(Agent,Bomb), 
%       Time), 
%    holds(
%       holding(Agent,Bomb), 
%       Time)).
not holding(Agent, Bomb)at Time if not happens(bombActivate(Agent, Bomb), Time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Bomb.e',69).

 /*  l_int(holds(not(holding(Agent,Bomb)),Time),
           [ holds(not(happens(bombActivate(Agent,Bomb),
     			  Time)),
     	      Time)
           ]).
 */
 %  "% =================================".


%; An effect axiom states that if a bomb is intact and
%; an agent deactivates the bomb,
%; the bomb will no longer be activated:
% [agent,bomb,time]
% HoldsAt(Intact(bomb),time) ->
% Terminates(BombDeactivate(agent,bomb),
%            BombActivated(bomb),
%            time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Bomb.e',76).
% From E: 
% 
% '->'(
%    holds(
%       intact(Bomb), 
%       Time), 
%    terminates_at(
%       bombDeactivate(Agent,Bomb), 
%       bombActivated(Bomb), 
%       Time)).
not (bombDeactivate(Agent, Bomb)terminates bombActivated(Bomb)at Time)if not intact(Bomb)at Time.
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Bomb.e',76).

 /*  l_int(holds(not(terminates(bombDeactivate(Agent,Bomb),
     			   at(bombActivated(Bomb),Time))),
     	    Time),
           [holds(not(intact(Bomb)),Time)]).
 */
 %  "% =================================".


%; An axiom states that if a bomb explodes, the
%; bomb destroys the bomb:
% [bomb,time]
% Happens(BombExplode(bomb),time) ->
% Happens(Destroy(bomb,bomb),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Bomb.e',84).
% From E: 
% 
% '->'(
%    happens(
%       bombExplode(Bomb), 
%       Time), 
%    happens(
%       destroy(Bomb,Bomb), 
%       Time)).
not happens(destroy(Bomb, Bomb), Time)if not happens(bombExplode(Bomb), Time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Bomb.e',84).

 /*  l_int(holds(not(happens(destroy(Bomb,Bomb),Time)),
     	    Time_at),
           [ holds(not(happens(bombExplode(Bomb),Time)),
     	      Time_at)
           ]).
 */
 %  "% =================================".


%; An effect axiom states that if a bomb explodes,
%; the bomb is no longer activated:
% [bomb,time]
% Terminates(BombExplode(bomb),BombActivated(bomb),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Bomb.e',90).
% From E: 
% 
% terminates_at(
%    bombExplode(Bomb), 
%    bombActivated(Bomb), 
%    Time).
bombExplode(Bomb)terminates bombActivated(Bomb).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Bomb.e',90).

 /*  terminated(happens(bombExplode(Bomb),
     		   Time_from,
     		   Time_until),
     	   bombActivated(Bomb),
     	   []).
 */
 %  "% =================================".


%; A trigger axiom states that
%; if a bomb is activated,
%; the timer value of the bomb is a timer value, and
%; the timer value is greater than zero,
%; the timer value of the bomb will be decremented:
% [bomb,offset,time]
% HoldsAt(BombActivated(bomb),time) &
% HoldsAt(BombTimerValue(bomb,offset),time) &
% (offset > 0) ->
% Happens(BombDecrementTimer(bomb),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Bomb.e',98).
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          bombActivated(Bomb), 
%          Time), 
%       ','(
%          holds(
%             bombTimerValue(Bomb,Offset), 
%             Time), 
%          Offset>0)), 
%    happens(
%       bombDecrementTimer(Bomb), 
%       Time)).
not happens(bombDecrementTimer(Bomb), Time)if not bombActivated(Bomb)at Time;not bombTimerValue(Bomb, Offset)at Time;not comparison(Offset, 0, >).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Bomb.e',98).

 /*   l_int(holds(not(happens(bombDecrementTimer(Bomb), Time)),
                    Time_at),
              [  (at(not(bombActivated(Bomb)), Time);at(not(bombTimerValue(Bomb, Offset)), Time);not(comparison(Offset, 0, >)))
              ]).
 */
 %  "% =================================".


%; An effect axiom states that
%; if the timer value of the bomb is a timer value and
%; the timer value of the bomb is decremented,
%; the timer value of the bomb will be the timer value minus one:
% [bomb,offset1,offset2,time]
% HoldsAt(BombTimerValue(bomb,offset1),time) &
% offset2 = offset1-1 ->
% Initiates(BombDecrementTimer(bomb),
%           BombTimerValue(bomb,offset2),
%           time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Bomb.e',108).
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          bombTimerValue(Bomb,Offset1), 
%          Time), 
%       Offset2=Offset1-1), 
%    initiates_at(
%       bombDecrementTimer(Bomb), 
%       bombTimerValue(Bomb,Offset2), 
%       Time)).
not (bombDecrementTimer(Bomb)initiates bombTimerValue(Bomb, Offset2)at Time)if not bombTimerValue(Bomb, Offset1)at Time;not equals(Offset2, Offset1-1).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Bomb.e',108).

 /*   l_int(holds(not(initiates(bombDecrementTimer(Bomb),
                                  at(bombTimerValue(Bomb, Offset2),
                                     Time))),
                    Time_at),
              [  (at(not(bombTimerValue(Bomb, Offset1)), Time);not(equals(Offset2, Offset1-1)))
              ]).
 */
 %  "% =================================".


%; An effect axiom states that
%; if the timer value of the bomb is a timer value and
%; the timer value of the bomb is decremented,
%; the timer value of the bomb will no longer be the timer value:
% [bomb,offset,time]
% HoldsAt(BombTimerValue(bomb,offset),time) ->
% Terminates(BombDecrementTimer(bomb),
%            BombTimerValue(bomb,offset),
%            time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Bomb.e',119).
% From E: 
% 
% '->'(
%    holds(
%       bombTimerValue(Bomb,Offset), 
%       Time), 
%    terminates_at(
%       bombDecrementTimer(Bomb), 
%       bombTimerValue(Bomb,Offset), 
%       Time)).
not (bombDecrementTimer(Bomb)terminates bombTimerValue(Bomb, Offset)at Time)if not bombTimerValue(Bomb, Offset)at Time.
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Bomb.e',119).

 /*  l_int(holds(not(terminates(bombDecrementTimer(Bomb),
     			   at(bombTimerValue(Bomb,Offset),
     			      Time))),
     	    Time),
           [holds(not(bombTimerValue(Bomb,Offset)),Time)]).
 */
 %  "% =================================".


%; An effect axiom states that if a bomb explodes,
%; the bomb will no longer be activated:
% [bomb,time]
% Terminates(BombExplode(bomb),BombActivated(bomb),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Bomb.e',127).
% From E: 
% 
% terminates_at(
%    bombExplode(Bomb), 
%    bombActivated(Bomb), 
%    Time).
bombExplode(Bomb)terminates bombActivated(Bomb).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Bomb.e',127).

 /*  terminated(happens(bombExplode(Bomb),
     		   Time_from,
     		   Time_until),
     	   bombActivated(Bomb),
     	   []).
 */
 %  "% =================================".


%; A trigger axiom states that if the timer value
%; of a bomb is zero, the bomb will explode:
% [bomb,time]
% HoldsAt(BombTimerValue(bomb,0),time) ->
% Happens(BombExplode(bomb),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Bomb.e',132).
% From E: 
% 
% '->'(
%    holds(
%       bombTimerValue(Bomb,0), 
%       Time), 
%    happens(
%       bombExplode(Bomb), 
%       Time)).
not happens(bombExplode(Bomb), Time)if not bombTimerValue(Bomb, 0)at Time.
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Bomb.e',132).

 /*  l_int(holds(not(happens(bombExplode(Bomb),Time)),
     	    Time),
           [holds(not(bombTimerValue(Bomb,0)),Time)]).
 */
 %  "% =================================".


%; An axiom states that if an agent is at a location,
%; a bomb is at the location,
%; the agent is nondeterministically injured, and
%; the bomb explodes, then
%; the bomb will injure the agent:
% [agent,location,bomb,time]
% HoldsAt(At(agent,location),time) &
% HoldsAt(At(bomb,location),time) &
% HoldsAt(InjuredDeterminingFluent(agent),time) &
% Happens(BombExplode(bomb),time) ->
% Happens(Injure(bomb,agent),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Bomb.e',141).
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          at_loc(Agent,Location), 
%          Time), 
%       ','(
%          holds(
%             at_loc(Bomb,Location), 
%             Time), 
%          ','(
%             holds(
%                injuredDeterminingFluent(Agent), 
%                Time), 
%             happens(
%                bombExplode(Bomb), 
%                Time)))), 
%    happens(
%       injure(Bomb,Agent), 
%       Time)).
not happens(injure(Bomb, Agent), Time)if not at_loc(Agent, Location)at Time;not at_loc(Bomb, Location)at Time;not injuredDeterminingFluent(Agent)at Time;not happens(bombExplode(Bomb), Time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Bomb.e',141).

 /*   l_int(holds(not(happens(injure(Bomb, Agent), Time)),
                    Time_at),
              [  (at(not(at_loc(Agent, Location)), Time);at(not(at_loc(Bomb, Location)), Time);at(not(injuredDeterminingFluent(Agent)), Time);not(happens(bombExplode(Bomb), Time)))
              ]).
 */
 %  "% =================================".


%; An axiom states that if an agent is at a location,
%; a bomb is at the location,
%; the agent is nondeterministically killed, and
%; the bomb explodes, then
%; the bomb will kill the agent:
% [agent,location,bomb,time]
% HoldsAt(At(agent,location),time) &
% HoldsAt(At(bomb,location),time) &
% HoldsAt(KilledDeterminingFluent(agent),time) &
% Happens(BombExplode(bomb),time) ->
% Happens(Kill(bomb,agent),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Bomb.e',153).
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          at_loc(Agent,Location), 
%          Time), 
%       ','(
%          holds(
%             at_loc(Bomb,Location), 
%             Time), 
%          ','(
%             holds(
%                killedDeterminingFluent(Agent), 
%                Time), 
%             happens(
%                bombExplode(Bomb), 
%                Time)))), 
%    happens(
%       kill(Bomb,Agent), 
%       Time)).
not happens(kill(Bomb, Agent), Time)if not at_loc(Agent, Location)at Time;not at_loc(Bomb, Location)at Time;not killedDeterminingFluent(Agent)at Time;not happens(bombExplode(Bomb), Time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Bomb.e',153).

 /*   l_int(holds(not(happens(kill(Bomb, Agent), Time)),
                    Time_at),
              [  (at(not(at_loc(Agent, Location)), Time);at(not(at_loc(Bomb, Location)), Time);at(not(killedDeterminingFluent(Agent)), Time);not(happens(bombExplode(Bomb), Time)))
              ]).
 */
 %  "% =================================".


%; An axiom states that if an physical object is at a location,
%; a bomb is at the location,
%; the physical object is nondeterministically damaged, and
%; the bomb explodes, then
%; the bomb will damage the physical object:
% [physobj,location,bomb,time]
% HoldsAt(At(physobj,location),time) &
% HoldsAt(At(bomb,location),time) &
% HoldsAt(DamagedDeterminingFluent(physobj),time) &
% Happens(BombExplode(bomb),time) ->
% Happens(Damage(bomb,physobj),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Bomb.e',165).
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          at_loc(Physobj,Location), 
%          Time), 
%       ','(
%          holds(
%             at_loc(Bomb,Location), 
%             Time), 
%          ','(
%             holds(
%                damagedDeterminingFluent(Physobj), 
%                Time), 
%             happens(
%                bombExplode(Bomb), 
%                Time)))), 
%    happens(
%       damage(Bomb,Physobj), 
%       Time)).
not happens(damage(Bomb, Physobj), Time)if not at_loc(Physobj, Location)at Time;not at_loc(Bomb, Location)at Time;not damagedDeterminingFluent(Physobj)at Time;not happens(bombExplode(Bomb), Time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Bomb.e',165).

 /*   l_int(holds(not(happens(damage(Bomb, Physobj), Time)),
                    Time_at),
              [  (at(not(at_loc(Physobj, Location)), Time);at(not(at_loc(Bomb, Location)), Time);at(not(damagedDeterminingFluent(Physobj)), Time);not(happens(bombExplode(Bomb), Time)))
              ]).
 */
 %  "% =================================".


%; An axiom states that if an physical object is at a location,
%; a bomb is at the location,
%; the physical object is nondeterministically destroyed, and
%; the bomb explodes, then
%; the bomb will destroy the physical object:
% [physobj,location,bomb,time]
% HoldsAt(At(physobj,location),time) &
% HoldsAt(At(bomb,location),time) &
% HoldsAt(DestroyedDeterminingFluent(physobj),time) &
% Happens(BombExplode(bomb),time) ->
% Happens(Destroy(bomb,physobj),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Bomb.e',177).
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          at_loc(Physobj,Location), 
%          Time), 
%       ','(
%          holds(
%             at_loc(Bomb,Location), 
%             Time), 
%          ','(
%             holds(
%                destroyedDeterminingFluent(Physobj), 
%                Time), 
%             happens(
%                bombExplode(Bomb), 
%                Time)))), 
%    happens(
%       destroy(Bomb,Physobj), 
%       Time)).
not happens(destroy(Bomb, Physobj), Time)if not at_loc(Physobj, Location)at Time;not at_loc(Bomb, Location)at Time;not destroyedDeterminingFluent(Physobj)at Time;not happens(bombExplode(Bomb), Time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Bomb.e',177).

 /*   l_int(holds(not(happens(destroy(Bomb, Physobj), Time)),
                    Time_at),
              [  (at(not(at_loc(Physobj, Location)), Time);at(not(at_loc(Bomb, Location)), Time);at(not(destroyedDeterminingFluent(Physobj)), Time);not(happens(bombExplode(Bomb), Time)))
              ]).
 */
 %  "% =================================".


%; End of file.
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Bomb.e',183).
% From E: 
% 
% ':-'(call_pel_directive(translate(ending,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Bomb.lps.pl'))).
:- call_pel_directive(translate(ending,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Bomb.lps.pl')).
