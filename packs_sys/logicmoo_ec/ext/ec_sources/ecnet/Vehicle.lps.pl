% From E: 
% 
% ':-'(call_pel_directive(translate(unskipped,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.lps.pl'))).
:- call_pel_directive(translate(unskipped,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.lps.pl')).
:-include(library('ec_planner/ec_test_incl')).
:-expects_dialect(lps).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/TimeDelayBombing.e',147).
% From E: 
% 
% ':-'(call_pel_directive(translate(begining,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.lps.pl'))).
:- call_pel_directive(translate(begining,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.lps.pl')).
% Tue, 23 Mar 2021 19:06:59 GMT File: <stream>(0x555567be9900)%;
%; Copyright (c) 2005 IBM Corporation and others.
%; All rights reserved. This program and the accompanying materials
%; are made available under the terms of the Common Public License v1.0
%; which accompanies this distribution, and is available at
%; http://www.eclipse.org/legal/cpl-v10.html
%;
%; Contributors:
%; IBM - Initial implementation
%;
%; Vehicle: transportation vehicles
%;

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',14).
% sort vehicle: physobj
% From E: 
% 
% subsort(vehicle,physobj).
subsort(vehicle,physobj).

% sort vehiclein: vehicle
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',14).
% From E: 
% 
% subsort(vehiclein,vehicle).
subsort(vehiclein,vehicle).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',16).
% sort vehicleon: vehicle
% From E: 
% 
% subsort(vehicleon,vehicle).
subsort(vehicleon,vehicle).

% sort train: vehicleon
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',16).
% From E: 
% 
% subsort(train,vehicleon).
subsort(train,vehicleon).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',18).
% sort carriage: vehiclein
% From E: 
% 
% subsort(carriage,vehiclein).
subsort(carriage,vehiclein).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',20).
% sort vehicledoor
% From E: 
% 
% sort(vehicledoor).
sort(vehicledoor).
%; RideTrack

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',24).
% event RideTrack12(train,track)
% From E: 
% 
% event(rideTrack12(train,track)).
mpred_prop(rideTrack12(train,track),event).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',24).
events([rideTrack12/2]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',26).
% event RideTrack21(train,track)
% From E: 
% 
% event(rideTrack21(train,track)).
mpred_prop(rideTrack21(train,track),event).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',26).
events([rideTrack21/2]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',28).
% [train,track,time]
% Happens(RideTrack12(train,track),time) ->
% HoldsAt(At(train,Side1(track)),time).
% From E: 
% 
% '->'(
%    happens(
%       rideTrack12(Train,Track), 
%       Time), 
%    holds(
%       at_loc(Train, 
%          side1(Track)), 
%       Time)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',28).
if(at(at_loc(Train,side1(Track)),Time),
   happens(rideTrack12(Train,Track),Time)).


% [train,track,time]
% Happens(RideTrack21(train,track),time) ->
% HoldsAt(At(train,Side2(track)),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',33).
% From E: 
% 
% '->'(
%    happens(
%       rideTrack21(Train,Track), 
%       Time), 
%    holds(
%       at_loc(Train, 
%          side2(Track)), 
%       Time)).
if(at(at_loc(Train,side2(Track)),Time),
   happens(rideTrack21(Train,Track),Time)).


% [train,track,location,time]
% Side2(track)=location ->
% Initiates(RideTrack12(train,track),At(train,location),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',37).
% From E: 
% 
% '->'(
%    '='(
%       side2(Track), 
%       Location), 
%    initiates_at(
%       rideTrack12(Train,Track), 
%       at_loc(Train,Location), 
%       Time)).
if(initiates(rideTrack12(Train,Track),
	     at(at_loc(Train,Location),Time)),
   side2(Track,Location)).


% [train,track,location,time]
% Side1(track)=location ->
% Initiates(RideTrack21(train,track),At(train,location),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',41).
% From E: 
% 
% '->'(
%    '='(
%       side1(Track), 
%       Location), 
%    initiates_at(
%       rideTrack21(Train,Track), 
%       at_loc(Train,Location), 
%       Time)).
if(initiates(rideTrack21(Train,Track),
	     at(at_loc(Train,Location),Time)),
   side1(Track,Location)).


% [train,track,location,time]
% Side1(track)=location ->
% Terminates(RideTrack12(train,track),At(train,location),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',45).
% From E: 
% 
% '->'(
%    '='(
%       side1(Track), 
%       Location), 
%    terminates_at(
%       rideTrack12(Train,Track), 
%       at_loc(Train,Location), 
%       Time)).
if(terminates(rideTrack12(Train,Track),
	      at(at_loc(Train,Location),Time)),
   side1(Track,Location)).


% [train,track,location,time]
% Side2(track)=location ->
% Terminates(RideTrack21(train,track),At(train,location),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',49).
% From E: 
% 
% '->'(
%    '='(
%       side2(Track), 
%       Location), 
%    terminates_at(
%       rideTrack21(Train,Track), 
%       at_loc(Train,Location), 
%       Time)).
if(terminates(rideTrack21(Train,Track),
	      at(at_loc(Train,Location),Time)),
   side2(Track,Location)).


%; DriveStreet

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',54).
% event DriveStreet12(vehicle,street)
% From E: 
% 
% event(driveStreet12(vehicle,street)).
mpred_prop(driveStreet12(vehicle,street),event).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',54).
events([driveStreet12/2]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',56).
% event DriveStreet21(vehicle,street)
% From E: 
% 
% event(driveStreet21(vehicle,street)).
mpred_prop(driveStreet21(vehicle,street),event).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',56).
events([driveStreet21/2]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',58).
% [vehicle,street,time]
% Happens(DriveStreet12(vehicle,street),time) ->
% HoldsAt(At(vehicle,Side1(street)),time).
% From E: 
% 
% '->'(
%    happens(
%       driveStreet12(Vehicle,Street), 
%       Time), 
%    holds(
%       at_loc(Vehicle, 
%          side1(Street)), 
%       Time)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',58).
if(at(at_loc(Vehicle,side1(Street)),Time),
   happens(driveStreet12(Vehicle,Street),Time)).


% [vehicle,street,time]
% Happens(DriveStreet21(vehicle,street),time) ->
% HoldsAt(At(vehicle,Side2(street)),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',63).
% From E: 
% 
% '->'(
%    happens(
%       driveStreet21(Vehicle,Street), 
%       Time), 
%    holds(
%       at_loc(Vehicle, 
%          side2(Street)), 
%       Time)).
if(at(at_loc(Vehicle,side2(Street)),Time),
   happens(driveStreet21(Vehicle,Street),Time)).


% [vehicle,street,location,time]
% Side2(street)=location ->
% Initiates(DriveStreet12(vehicle,street),At(vehicle,location),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',67).
% From E: 
% 
% '->'(
%    '='(
%       side2(Street), 
%       Location), 
%    initiates_at(
%       driveStreet12(Vehicle,Street), 
%       at_loc(Vehicle,Location), 
%       Time)).
if(initiates(driveStreet12(Vehicle,Street),
	     at(at_loc(Vehicle,Location),Time)),
   side2(Street,Location)).


% [vehicle,street,location,time]
% Side1(street)=location ->
% Initiates(DriveStreet21(vehicle,street),At(vehicle,location),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',71).
% From E: 
% 
% '->'(
%    '='(
%       side1(Street), 
%       Location), 
%    initiates_at(
%       driveStreet21(Vehicle,Street), 
%       at_loc(Vehicle,Location), 
%       Time)).
if(initiates(driveStreet21(Vehicle,Street),
	     at(at_loc(Vehicle,Location),Time)),
   side1(Street,Location)).


% [vehicle,street,location,time]
% Side1(street)=location ->
% Terminates(DriveStreet12(vehicle,street),At(vehicle,location),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',75).
% From E: 
% 
% '->'(
%    '='(
%       side1(Street), 
%       Location), 
%    terminates_at(
%       driveStreet12(Vehicle,Street), 
%       at_loc(Vehicle,Location), 
%       Time)).
if(terminates(driveStreet12(Vehicle,Street),
	      at(at_loc(Vehicle,Location),Time)),
   side1(Street,Location)).


% [vehicle,street,location,time]
% Side2(street)=location ->
% Terminates(DriveStreet21(vehicle,street),At(vehicle,location),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',79).
% From E: 
% 
% '->'(
%    '='(
%       side2(Street), 
%       Location), 
%    terminates_at(
%       driveStreet21(Vehicle,Street), 
%       at_loc(Vehicle,Location), 
%       Time)).
if(terminates(driveStreet21(Vehicle,Street),
	      at(at_loc(Vehicle,Location),Time)),
   side2(Street,Location)).


%; Pulling

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',84).
% event PointToward(agent,horse,street)
% From E: 
% 
% event(pointToward(agent,horse,street)).
events([pointToward/3]).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',84).
mpred_prop(pointToward(agent,horse,street),action).
actions([pointToward/3]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',86).
% fluent PointedToward(horse,street)
% From E: 
% 
% fluent(pointedToward(horse,street)).
mpred_prop(pointedToward(horse,street),fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',86).
fluents([pointedToward/2]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',88).
% [horse,street1,street2,time]
% HoldsAt(PointedToward(horse,street1),time) &
% HoldsAt(PointedToward(horse,street2),time) ->
% street1=street2.
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          pointedToward(Horse,Street1), 
%          Time), 
%       holds(
%          pointedToward(Horse,Street2), 
%          Time)), 
%    Street1=Street2).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',88).
if(equals(Street1, Street2),  (at(pointedToward(Horse, Street1), Time), at(pointedToward(Horse, Street2), Time))).


% [agent,horse,street,time]
% Initiates(PointToward(agent,horse,street),
%           PointedToward(horse,street),
%           time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',94).
% From E: 
% 
% initiates_at(
%    pointToward(Agent,Horse,Street), 
%    pointedToward(Horse,Street), 
%    Time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',94).
initiates(pointToward(Agent,Horse,Street),
	  pointedToward(Horse,Street)).


% [agent,horse,street1,street2,time]
% HoldsAt(PointedToward(horse,street1),time) ->
% Terminates(PointToward(agent,horse,street2),
%            PointedToward(horse,street1),
%            time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',99).
% From E: 
% 
% '->'(
%    holds(
%       pointedToward(Horse,Street1), 
%       Time), 
%    terminates_at(
%       pointToward(Agent,Horse,Street2), 
%       pointedToward(Horse,Street1), 
%       Time)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',99).
if(terminates(pointToward(Agent,Horse,Street2),
	      at(pointedToward(Horse,Street1),Time)),
   at(pointedToward(Horse,Street1),Time)).


% [horse,vehicle,street,time]
% Terminates(PullStreet12(horse,vehicle,street),
%            PointedToward(horse,street),
%            time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',105).
% From E: 
% 
% terminates_at(
%    pullStreet12(Horse,Vehicle,Street), 
%    pointedToward(Horse,Street), 
%    Time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',105).
terminates(pullStreet12(Horse,Vehicle,Street),
	   pointedToward(Horse,Street)).


% [horse,vehicle,street,time]
% Terminates(PullStreet21(horse,vehicle,street),
%            PointedToward(horse,street),
%            time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',110).
% From E: 
% 
% terminates_at(
%    pullStreet21(Horse,Vehicle,Street), 
%    pointedToward(Horse,Street), 
%    Time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',110).
terminates(pullStreet21(Horse,Vehicle,Street),
	   pointedToward(Horse,Street)).


% [horse,street,time]
% HoldsAt(PointedToward(horse,street),time) ->
% HoldsAt(NearPortal(horse,street),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',115).
% From E: 
% 
% '->'(
%    holds(
%       pointedToward(Horse,Street), 
%       Time), 
%    holds(
%       nearPortal(Horse,Street), 
%       Time)).
if(at(nearPortal(Horse,Street),Time),
   at(pointedToward(Horse,Street),Time)).

% event Lash(agent,horse)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',117).
% From E: 
% 
% event(lash(agent,horse)).
events([lash/2]).
mpred_prop(lash(agent,horse),action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',117).
actions([lash/2]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',120).
% fluent HitchedTo(horse,vehicle)
% From E: 
% 
% fluent(hitchedTo(horse,vehicle)).
mpred_prop(hitchedTo(horse,vehicle),fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',120).
fluents([hitchedTo/2]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',122).
% [horse,vehicle,location,time]
% HoldsAt(HitchedTo(horse,vehicle),time) &
% HoldsAt(At(vehicle,location),time) ->
% HoldsAt(At(horse,location),time).
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          hitchedTo(Horse,Vehicle), 
%          Time), 
%       holds(
%          at_loc(Vehicle,Location), 
%          Time)), 
%    holds(
%       at_loc(Horse,Location), 
%       Time)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',122).
if(at(at_loc(Horse, Location), Time),  (at(hitchedTo(Horse, Vehicle), Time), at(at_loc(Vehicle, Location), Time))).


% [agent,horse,vehicle,street,time]
% Happens(Lash(agent,horse),time) &
% HoldsAt(PointedToward(horse,street),time) &
% HoldsAt(HitchedTo(horse,vehicle),time) &
% HoldsAt(At(horse,Side1(street)),time) ->
% Happens(PullStreet12(horse,vehicle,street),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',128).
% From E: 
% 
% '->'(
%    ','(
%       happens(
%          lash(Agent,Horse), 
%          Time), 
%       ','(
%          holds(
%             pointedToward(Horse,Street), 
%             Time), 
%          ','(
%             holds(
%                hitchedTo(Horse,Vehicle), 
%                Time), 
%             holds(
%                at_loc(Horse, 
%                   side1(Street)), 
%                Time)))), 
%    happens(
%       pullStreet12(Horse,Vehicle,Street), 
%       Time)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',128).
if(happens(pullStreet12(Horse, Vehicle, Street), Time),  (happens(lash(Agent, Horse), Time), at(pointedToward(Horse, Street), Time), at(hitchedTo(Horse, Vehicle), Time), at(at_loc(Horse, side1(Street)), Time))).


% [agent,horse,vehicle,street,time]
% Happens(Lash(agent,horse),time) &
% HoldsAt(PointedToward(horse,street),time) &
% HoldsAt(HitchedTo(horse,vehicle),time) &
% HoldsAt(At(horse,Side2(street)),time) ->
% Happens(PullStreet21(horse,vehicle,street),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',135).
% From E: 
% 
% '->'(
%    ','(
%       happens(
%          lash(Agent,Horse), 
%          Time), 
%       ','(
%          holds(
%             pointedToward(Horse,Street), 
%             Time), 
%          ','(
%             holds(
%                hitchedTo(Horse,Vehicle), 
%                Time), 
%             holds(
%                at_loc(Horse, 
%                   side2(Street)), 
%                Time)))), 
%    happens(
%       pullStreet21(Horse,Vehicle,Street), 
%       Time)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',135).
if(happens(pullStreet21(Horse, Vehicle, Street), Time),  (happens(lash(Agent, Horse), Time), at(pointedToward(Horse, Street), Time), at(hitchedTo(Horse, Vehicle), Time), at(at_loc(Horse, side2(Street)), Time))).

% event PullStreet12(horse,vehicle,street)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',140).
% From E: 
% 
% event(pullStreet12(horse,vehicle,street)).
mpred_prop(pullStreet12(horse,vehicle,street),event).
events([pullStreet12/3]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',143).
% event PullStreet21(horse,vehicle,street)
% From E: 
% 
% event(pullStreet21(horse,vehicle,street)).
mpred_prop(pullStreet21(horse,vehicle,street),event).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',143).
events([pullStreet21/3]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',145).
% [horse,vehicle,street,time]
% Happens(PullStreet12(horse,vehicle,street),time) ->
% Happens(DriveStreet12(vehicle,street),time).
% From E: 
% 
% '->'(
%    happens(
%       pullStreet12(Horse,Vehicle,Street), 
%       Time), 
%    happens(
%       driveStreet12(Vehicle,Street), 
%       Time)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',145).
if(happens(driveStreet12(Vehicle,Street),Time),
   happens(pullStreet12(Horse,Vehicle,Street),
	   Time)).


% [horse,vehicle,street,time]
% Happens(PullStreet21(horse,vehicle,street),time) ->
% Happens(DriveStreet21(vehicle,street),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',150).
% From E: 
% 
% '->'(
%    happens(
%       pullStreet21(Horse,Vehicle,Street), 
%       Time), 
%    happens(
%       driveStreet21(Vehicle,Street), 
%       Time)).
if(happens(driveStreet21(Vehicle,Street),Time),
   happens(pullStreet21(Horse,Vehicle,Street),
	   Time)).


% [horse,vehicle,street,time]
% Happens(PullStreet12(horse,vehicle,street),time) ->
% HoldsAt(At(horse,Side1(street)),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',154).
% From E: 
% 
% '->'(
%    happens(
%       pullStreet12(Horse,Vehicle,Street), 
%       Time), 
%    holds(
%       at_loc(Horse, 
%          side1(Street)), 
%       Time)).
if(at(at_loc(Horse,side1(Street)),Time),
   happens(pullStreet12(Horse,Vehicle,Street),
	   Time)).


% [horse,vehicle,street,time]
% Happens(PullStreet21(horse,vehicle,street),time) ->
% HoldsAt(At(horse,Side2(street)),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',158).
% From E: 
% 
% '->'(
%    happens(
%       pullStreet21(Horse,Vehicle,Street), 
%       Time), 
%    holds(
%       at_loc(Horse, 
%          side2(Street)), 
%       Time)).
if(at(at_loc(Horse,side2(Street)),Time),
   happens(pullStreet21(Horse,Vehicle,Street),
	   Time)).


% [horse,vehicle,street,location,time]
% Side2(street)=location ->
% Initiates(PullStreet12(horse,vehicle,street),At(horse,location),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',162).
% From E: 
% 
% '->'(
%    '='(
%       side2(Street), 
%       Location), 
%    initiates_at(
%       pullStreet12(Horse,Vehicle,Street), 
%       at_loc(Horse,Location), 
%       Time)).
if(initiates(pullStreet12(Horse,Vehicle,Street),
	     at(at_loc(Horse,Location),Time)),
   side2(Street,Location)).


% [horse,vehicle,street,location,time]
% Side1(street)=location ->
% Initiates(PullStreet21(horse,vehicle,street),At(horse,location),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',166).
% From E: 
% 
% '->'(
%    '='(
%       side1(Street), 
%       Location), 
%    initiates_at(
%       pullStreet21(Horse,Vehicle,Street), 
%       at_loc(Horse,Location), 
%       Time)).
if(initiates(pullStreet21(Horse,Vehicle,Street),
	     at(at_loc(Horse,Location),Time)),
   side1(Street,Location)).


% [horse,vehicle,street,location,time]
% Side1(street)=location ->
% Terminates(PullStreet12(horse,vehicle,street),At(horse,location),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',170).
% From E: 
% 
% '->'(
%    '='(
%       side1(Street), 
%       Location), 
%    terminates_at(
%       pullStreet12(Horse,Vehicle,Street), 
%       at_loc(Horse,Location), 
%       Time)).
if(terminates(pullStreet12(Horse,Vehicle,Street),
	      at(at_loc(Horse,Location),Time)),
   side1(Street,Location)).


% [horse,vehicle,street,location,time]
% Side2(street)=location ->
% Terminates(PullStreet21(horse,vehicle,street),At(horse,location),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',174).
% From E: 
% 
% '->'(
%    '='(
%       side2(Street), 
%       Location), 
%    terminates_at(
%       pullStreet21(Horse,Vehicle,Street), 
%       at_loc(Horse,Location), 
%       Time)).
if(terminates(pullStreet21(Horse,Vehicle,Street),
	      at(at_loc(Horse,Location),Time)),
   side2(Street,Location)).


%; OnVehicle

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',179).
% fluent OnVehicle(object,vehicleon)
% From E: 
% 
% fluent(onVehicle(object,vehicleon)).
mpred_prop(onVehicle(object,vehicleon),fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',179).
fluents([onVehicle/2]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',181).
% event GetOnVehicle(agent,vehicleon)
% From E: 
% 
% event(getOnVehicle(agent,vehicleon)).
events([getOnVehicle/2]).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',181).
mpred_prop(getOnVehicle(agent,vehicleon),action).
actions([getOnVehicle/2]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',183).
% event GetOffVehicle(agent,vehicleon)
% From E: 
% 
% event(getOffVehicle(agent,vehicleon)).
events([getOffVehicle/2]).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',183).
mpred_prop(getOffVehicle(agent,vehicleon),action).
actions([getOffVehicle/2]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',185).
% [vehicleon1,vehicleon2,time]
% HoldsAt(OnVehicle(vehicleon1,vehicleon2),time) ->
% vehicleon1!=vehicleon2.
% From E: 
% 
% '->'(
%    holds(
%       onVehicle(Vehicleon1,Vehicleon2), 
%       Time), 
%    Vehicleon1\=Vehicleon2).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',185).
if({dif(Vehicleon1,Vehicleon2)},
   at(onVehicle(Vehicleon1,Vehicleon2),Time)).


% [vehicleon1,vehicleon2,time]
% HoldsAt(OnVehicle(vehicleon1,vehicleon2),time) ->
% !HoldsAt(OnVehicle(vehicleon2,vehicleon1),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',190).
% From E: 
% 
% '->'(
%    holds(
%       onVehicle(Vehicleon1,Vehicleon2), 
%       Time), 
%    holds(
%       not(onVehicle(Vehicleon2,Vehicleon1)), 
%       Time)).
if(at(not(onVehicle(Vehicleon2,Vehicleon1)),Time),
   at(onVehicle(Vehicleon1,Vehicleon2),Time)).


% [agent,vehicleon,time]
% Initiates(GetOnVehicle(agent,vehicleon),OnVehicle(agent,vehicleon),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',194).
% From E: 
% 
% initiates_at(
%    getOnVehicle(Agent,Vehicleon), 
%    onVehicle(Agent,Vehicleon), 
%    Time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',194).
initiates(getOnVehicle(Agent,Vehicleon),
	  onVehicle(Agent,Vehicleon)).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',196).
% [agent,vehicleon,time]
% Happens(GetOnVehicle(agent,vehicleon),time) ->
% {location}% 
%  HoldsAt(At(agent,location),time) &
%  HoldsAt(At(vehicleon,location),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',198).
% From E: 
% 
% exists(Location, 
%    '->'(
%       happens(
%          getOnVehicle(Agent,Vehicleon), 
%          Time), 
%       ','(
%          holds(
%             at_loc(Agent,Location), 
%             Time), 
%          holds(
%             at_loc(Vehicleon,Location), 
%             Time)))).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',198).
exists(Location, if((at(at_loc(Agent, Location), Time), at(at_loc(Vehicleon, Location), Time)), happens(getOnVehicle(Agent, Vehicleon), Time))).


% [agent,vehicleon,time]
% Terminates(GetOffVehicle(agent,vehicleon),OnVehicle(agent,vehicleon),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',203).
% From E: 
% 
% terminates_at(
%    getOffVehicle(Agent,Vehicleon), 
%    onVehicle(Agent,Vehicleon), 
%    Time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',203).
terminates(getOffVehicle(Agent,Vehicleon),
	   onVehicle(Agent,Vehicleon)).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',205).
% [agent,vehicleon,time]
% Happens(GetOffVehicle(agent,vehicleon),time) ->
% HoldsAt(OnVehicle(agent,vehicleon),time).
% From E: 
% 
% '->'(
%    happens(
%       getOffVehicle(Agent,Vehicleon), 
%       Time), 
%    holds(
%       onVehicle(Agent,Vehicleon), 
%       Time)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',205).
if(at(onVehicle(Agent,Vehicleon),Time),
   happens(getOffVehicle(Agent,Vehicleon),Time)).


% [agent,vehicleon,location,time]
% Releases(GetOnVehicle(agent,vehicleon),
%          At(agent,location),
%          time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',210).
% From E: 
% 
% releases_at(
%    getOnVehicle(Agent,Vehicleon), 
%    at_loc(Agent,Location), 
%    Time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',210).
releases(getOnVehicle(Agent,Vehicleon),
	 at_loc(Agent,Location)).


%;[agent,vehicleon,location1,location2,time]
%;HoldsAt(At(vehicleon,location1),time) &
%;location1 != location2 ->
%;Terminates(GetOffVehicle(agent,vehicleon),
%;           At(agent,location2),
%;           time).
% [agent,vehicleon,location,time]
% HoldsAt(At(vehicleon,location),time) ->
% Initiates(GetOffVehicle(agent,vehicleon),
%           At(agent,location),
%           time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',221).
% From E: 
% 
% '->'(
%    holds(
%       at_loc(Vehicleon,Location), 
%       Time), 
%    initiates_at(
%       getOffVehicle(Agent,Vehicleon), 
%       at_loc(Agent,Location), 
%       Time)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',221).
if(initiates(getOffVehicle(Agent,Vehicleon),
	     at(at_loc(Agent,Location),Time)),
   at(at_loc(Vehicleon,Location),Time)).


% [object,vehicleon,location,time]
% HoldsAt(OnVehicle(object,vehicleon),time) &
% HoldsAt(At(vehicleon,location),time) ->
% HoldsAt(At(object,location),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',228).
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          onVehicle(Object,Vehicleon), 
%          Time), 
%       holds(
%          at_loc(Vehicleon,Location), 
%          Time)), 
%    holds(
%       at_loc(Object,Location), 
%       Time)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',228).
if(at(at_loc(Object, Location), Time),  (at(onVehicle(Object, Vehicleon), Time), at(at_loc(Vehicleon, Location), Time))).


%; InVehicle

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',234).
% fluent InVehicle(object,vehiclein)
% From E: 
% 
% fluent(inVehicle(object,vehiclein)).
mpred_prop(inVehicle(object,vehiclein),fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',234).
fluents([inVehicle/2]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',236).
% event GetInVehicle(agent,vehiclein)
% From E: 
% 
% event(getInVehicle(agent,vehiclein)).
events([getInVehicle/2]).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',236).
mpred_prop(getInVehicle(agent,vehiclein),action).
actions([getInVehicle/2]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',238).
% event GetOutOfVehicle(agent,vehiclein)
% From E: 
% 
% event(getOutOfVehicle(agent,vehiclein)).
events([getOutOfVehicle/2]).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',238).
mpred_prop(getOutOfVehicle(agent,vehiclein),action).
actions([getOutOfVehicle/2]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',240).
% [vehiclein1,vehiclein2,time]
% HoldsAt(InVehicle(vehiclein1,vehiclein2),time) ->
% vehiclein1!=vehiclein2.
% From E: 
% 
% '->'(
%    holds(
%       inVehicle(Vehiclein1,Vehiclein2), 
%       Time), 
%    Vehiclein1\=Vehiclein2).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',240).
if({dif(Vehiclein1,Vehiclein2)},
   at(inVehicle(Vehiclein1,Vehiclein2),Time)).


% [vehiclein1,vehiclein2,time]
% HoldsAt(InVehicle(vehiclein1,vehiclein2),time) ->
% !HoldsAt(InVehicle(vehiclein2,vehiclein1),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',245).
% From E: 
% 
% '->'(
%    holds(
%       inVehicle(Vehiclein1,Vehiclein2), 
%       Time), 
%    holds(
%       not(inVehicle(Vehiclein2,Vehiclein1)), 
%       Time)).
if(at(not(inVehicle(Vehiclein2,Vehiclein1)),Time),
   at(inVehicle(Vehiclein1,Vehiclein2),Time)).


% [agent,vehiclein,time]
% Initiates(GetInVehicle(agent,vehiclein),InVehicle(agent,vehiclein),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',249).
% From E: 
% 
% initiates_at(
%    getInVehicle(Agent,Vehiclein), 
%    inVehicle(Agent,Vehiclein), 
%    Time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',249).
initiates(getInVehicle(Agent,Vehiclein),
	  inVehicle(Agent,Vehiclein)).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',251).
% [agent,vehiclein,time]
% Happens(GetInVehicle(agent,vehiclein),time) ->
% {location}% 
%  HoldsAt(At(agent,location),time) &
%  HoldsAt(At(vehiclein,location),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',253).
% From E: 
% 
% exists(Location, 
%    '->'(
%       happens(
%          getInVehicle(Agent,Vehiclein), 
%          Time), 
%       ','(
%          holds(
%             at_loc(Agent,Location), 
%             Time), 
%          holds(
%             at_loc(Vehiclein,Location), 
%             Time)))).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',253).
exists(Location, if((at(at_loc(Agent, Location), Time), at(at_loc(Vehiclein, Location), Time)), happens(getInVehicle(Agent, Vehiclein), Time))).


% [agent,vehiclein,time]
% Terminates(GetOutOfVehicle(agent,vehiclein),InVehicle(agent,vehiclein),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',258).
% From E: 
% 
% terminates_at(
%    getOutOfVehicle(Agent,Vehiclein), 
%    inVehicle(Agent,Vehiclein), 
%    Time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',258).
terminates(getOutOfVehicle(Agent,Vehiclein),
	   inVehicle(Agent,Vehiclein)).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',260).
% [agent,vehiclein,time]
% Happens(GetOutOfVehicle(agent,vehiclein),time) ->
% HoldsAt(InVehicle(agent,vehiclein),time).
% From E: 
% 
% '->'(
%    happens(
%       getOutOfVehicle(Agent,Vehiclein), 
%       Time), 
%    holds(
%       inVehicle(Agent,Vehiclein), 
%       Time)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',260).
if(at(inVehicle(Agent,Vehiclein),Time),
   happens(getOutOfVehicle(Agent,Vehiclein),Time)).


% [agent,vehiclein,location,time]
% Releases(GetInVehicle(agent,vehiclein),
%          At(agent,location),
%          time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',265).
% From E: 
% 
% releases_at(
%    getInVehicle(Agent,Vehiclein), 
%    at_loc(Agent,Location), 
%    Time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',265).
releases(getInVehicle(Agent,Vehiclein),
	 at_loc(Agent,Location)).


%;[agent,vehiclein,location1,location2,time]
%;HoldsAt(At(vehiclein,location1),time) &
%;location1 != location2 ->
%;Terminates(GetOutOfVehicle(agent,vehiclein),
%;           At(agent,location2),
%;           time).
% [agent,vehiclein,location,time]
% HoldsAt(At(vehiclein,location),time) ->
% Initiates(GetOutOfVehicle(agent,vehiclein),
%           At(agent,location),
%           time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',276).
% From E: 
% 
% '->'(
%    holds(
%       at_loc(Vehiclein,Location), 
%       Time), 
%    initiates_at(
%       getOutOfVehicle(Agent,Vehiclein), 
%       at_loc(Agent,Location), 
%       Time)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',276).
if(initiates(getOutOfVehicle(Agent,Vehiclein),
	     at(at_loc(Agent,Location),Time)),
   at(at_loc(Vehiclein,Location),Time)).


% [object,vehiclein,location,time]
% HoldsAt(InVehicle(object,vehiclein),time) &
% HoldsAt(At(vehiclein,location),time) ->
% HoldsAt(At(object,location),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',283).
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          inVehicle(Object,Vehiclein), 
%          Time), 
%       holds(
%          at_loc(Vehiclein,Location), 
%          Time)), 
%    holds(
%       at_loc(Object,Location), 
%       Time)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',283).
if(at(at_loc(Object, Location), Time),  (at(inVehicle(Object, Vehiclein), Time), at(at_loc(Vehiclein, Location), Time))).


%; vehicle door
%; door does not have to be open for entry; passenger can jump in

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',290).
% event VehicleDoorOpen(agent,vehicledoor)
% From E: 
% 
% event(vehicleDoorOpen(agent,vehicledoor)).
events([vehicleDoorOpen/2]).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',290).
mpred_prop(vehicleDoorOpen(agent,vehicledoor),action).
actions([vehicleDoorOpen/2]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',292).
% event VehicleDoorClose(agent,vehicledoor)
% From E: 
% 
% event(vehicleDoorClose(agent,vehicledoor)).
events([vehicleDoorClose/2]).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',292).
mpred_prop(vehicleDoorClose(agent,vehicledoor),action).
actions([vehicleDoorClose/2]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',294).
% fluent VehicleDoorIsOpen(vehicledoor)
% From E: 
% 
% fluent(vehicleDoorIsOpen(vehicledoor)).
mpred_prop(vehicleDoorIsOpen(vehicledoor),fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',294).
fluents([vehicleDoorIsOpen/1]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',296).
% [agent,vehicledoor,time]
% Happens(VehicleDoorOpen(agent,vehicledoor),time) ->
% HoldsAt(Awake(agent),time) &
% !HoldsAt(VehicleDoorIsOpen(vehicledoor),time).
% From E: 
% 
% '->'(
%    happens(
%       vehicleDoorOpen(Agent,Vehicledoor), 
%       Time), 
%    ','(
%       holds(
%          awake(Agent), 
%          Time), 
%       holds(
%          not(vehicleDoorIsOpen(Vehicledoor)), 
%          Time))).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',296).
if((at(awake(Agent), Time), at(not(vehicleDoorIsOpen(Vehicledoor)), Time)), happens(vehicleDoorOpen(Agent, Vehicledoor), Time)).


% [agent,vehicledoor,time]
% Initiates(VehicleDoorOpen(agent,vehicledoor),
%           VehicleDoorIsOpen(vehicledoor),
%           time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',302).
% From E: 
% 
% initiates_at(
%    vehicleDoorOpen(Agent,Vehicledoor), 
%    vehicleDoorIsOpen(Vehicledoor), 
%    Time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',302).
initiates(vehicleDoorOpen(Agent,Vehicledoor),
	  vehicleDoorIsOpen(Vehicledoor)).


% [agent,vehicledoor,time]
% Happens(VehicleDoorClose(agent,vehicledoor),time) ->
% HoldsAt(Awake(agent),time) &
% HoldsAt(VehicleDoorIsOpen(vehicledoor),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',307).
% From E: 
% 
% '->'(
%    happens(
%       vehicleDoorClose(Agent,Vehicledoor), 
%       Time), 
%    ','(
%       holds(
%          awake(Agent), 
%          Time), 
%       holds(
%          vehicleDoorIsOpen(Vehicledoor), 
%          Time))).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',307).
if((at(awake(Agent), Time), at(vehicleDoorIsOpen(Vehicledoor), Time)), happens(vehicleDoorClose(Agent, Vehicledoor), Time)).


% [agent,vehicledoor,time]
% Terminates(VehicleDoorClose(agent,vehicledoor),
%            VehicleDoorIsOpen(vehicledoor),
%            time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',312).
% From E: 
% 
% terminates_at(
%    vehicleDoorClose(Agent,Vehicledoor), 
%    vehicleDoorIsOpen(Vehicledoor), 
%    Time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',312).
terminates(vehicleDoorClose(Agent,Vehicledoor),
	   vehicleDoorIsOpen(Vehicledoor)).


%; ticketagent

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',318).
% sort ticketagent: agent
% From E: 
% 
% subsort(ticketagent,agent).
subsort(ticketagent,agent).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',320).
% fluent BeTicketAgent0(ticketagent)
% From E: 
% 
% fluent(beTicketAgent0(ticketagent)).
mpred_prop(beTicketAgent0(ticketagent),fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',320).
fluents([beTicketAgent0/1]).

% fluent BeTicketAgent1(ticketagent)
% From E: 
% 
% fluent(beTicketAgent1(ticketagent)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',320).
mpred_prop(beTicketAgent1(ticketagent),fluent).
fluents([beTicketAgent1/1]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',322).
% fluent BeTicketAgent2(ticketagent)
% From E: 
% 
% fluent(beTicketAgent2(ticketagent)).
mpred_prop(beTicketAgent2(ticketagent),fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',322).
fluents([beTicketAgent2/1]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',324).
% xor BeTicketAgent0, BeTicketAgent1, BeTicketAgent2
% From E: 
% 
% xor([beTicketAgent0,beTicketAgent1,beTicketAgent2]).
xor([beTicketAgent0,beTicketAgent1,beTicketAgent2]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',326).
% [ticketagent,agent,ticket,time]
% HoldsAt(BeTicketAgent0(ticketagent),time) ->
% Terminates(Request(agent,ticketagent,ticket),
%            BeTicketAgent0(ticketagent),
%            time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',326).
% From E: 
% 
% '->'(
%    holds(
%       beTicketAgent0(Ticketagent), 
%       Time), 
%    terminates_at(
%       request(Agent,Ticketagent,Ticket), 
%       beTicketAgent0(Ticketagent), 
%       Time)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',326).
if(terminates(request(Agent,Ticketagent,Ticket),
	      at(beTicketAgent0(Ticketagent),Time)),
   at(beTicketAgent0(Ticketagent),Time)).


% [ticketagent,agent,ticket,time]
% HoldsAt(BeTicketAgent0(ticketagent),time) ->
% Initiates(Request(agent,ticketagent,ticket),
%           BeTicketAgent1(ticketagent),
%           time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',333).
% From E: 
% 
% '->'(
%    holds(
%       beTicketAgent0(Ticketagent), 
%       Time), 
%    initiates_at(
%       request(Agent,Ticketagent,Ticket), 
%       beTicketAgent1(Ticketagent), 
%       Time)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',333).
if(initiates(request(Agent,Ticketagent,Ticket),
	     at(beTicketAgent1(Ticketagent),Time)),
   at(beTicketAgent0(Ticketagent),Time)).


% [ticketagent,agent,ticket,time]
% HoldsAt(BeTicketAgent1(ticketagent),time) &
% HoldsAt(KnowRequest(ticketagent,agent,ticket),time) ->
% Happens(PickUp(ticketagent,ticket),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',339).
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          beTicketAgent1(Ticketagent), 
%          Time), 
%       holds(
%          knowRequest(Ticketagent,Agent,Ticket), 
%          Time)), 
%    happens(
%       pickUp(Ticketagent,Ticket), 
%       Time)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',339).
if(happens(pickUp(Ticketagent, Ticket), Time),  (at(beTicketAgent1(Ticketagent), Time), at(knowRequest(Ticketagent, Agent, Ticket), Time))).


% [ticketagent,ticket,time]
% HoldsAt(BeTicketAgent1(ticketagent),time) ->
% Terminates(PickUp(ticketagent,ticket),
%            BeTicketAgent1(ticketagent),
%            time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',344).
% From E: 
% 
% '->'(
%    holds(
%       beTicketAgent1(Ticketagent), 
%       Time), 
%    terminates_at(
%       pickUp(Ticketagent,Ticket), 
%       beTicketAgent1(Ticketagent), 
%       Time)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',344).
if(terminates(pickUp(Ticketagent,Ticket),
	      at(beTicketAgent1(Ticketagent),Time)),
   at(beTicketAgent1(Ticketagent),Time)).


% [ticketagent,ticket,time]
% HoldsAt(BeTicketAgent1(ticketagent),time) ->
% Initiates(PickUp(ticketagent,ticket),
%           BeTicketAgent2(ticketagent),
%           time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',350).
% From E: 
% 
% '->'(
%    holds(
%       beTicketAgent1(Ticketagent), 
%       Time), 
%    initiates_at(
%       pickUp(Ticketagent,Ticket), 
%       beTicketAgent2(Ticketagent), 
%       Time)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',350).
if(initiates(pickUp(Ticketagent,Ticket),
	     at(beTicketAgent2(Ticketagent),Time)),
   at(beTicketAgent1(Ticketagent),Time)).


% [ticketagent,agent,ticket,time]
% HoldsAt(BeTicketAgent2(ticketagent),time) &
% HoldsAt(KnowRequest(ticketagent,agent,ticket),time) ->
% Happens(HandTo(ticketagent,agent,ticket),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',356).
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          beTicketAgent2(Ticketagent), 
%          Time), 
%       holds(
%          knowRequest(Ticketagent,Agent,Ticket), 
%          Time)), 
%    happens(
%       handTo(Ticketagent,Agent,Ticket), 
%       Time)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',356).
if(happens(handTo(Ticketagent, Agent, Ticket), Time),  (at(beTicketAgent2(Ticketagent), Time), at(knowRequest(Ticketagent, Agent, Ticket), Time))).


% [ticketagent,ticket,agent,time]
% HoldsAt(BeTicketAgent2(ticketagent),time) ->
% Terminates(HandTo(ticketagent,agent,ticket),
%            BeTicketAgent2(ticketagent),
%            time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',361).
% From E: 
% 
% '->'(
%    holds(
%       beTicketAgent2(Ticketagent), 
%       Time), 
%    terminates_at(
%       handTo(Ticketagent,Agent,Ticket), 
%       beTicketAgent2(Ticketagent), 
%       Time)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',361).
if(terminates(handTo(Ticketagent,Agent,Ticket),
	      at(beTicketAgent2(Ticketagent),Time)),
   at(beTicketAgent2(Ticketagent),Time)).


% [ticketagent,ticket,agent,time]
% HoldsAt(BeTicketAgent2(ticketagent),time) ->
% Initiates(HandTo(ticketagent,agent,ticket),
%           BeTicketAgent0(ticketagent),
%           time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',367).
% From E: 
% 
% '->'(
%    holds(
%       beTicketAgent2(Ticketagent), 
%       Time), 
%    initiates_at(
%       handTo(Ticketagent,Agent,Ticket), 
%       beTicketAgent0(Ticketagent), 
%       Time)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',367).
if(initiates(handTo(Ticketagent,Agent,Ticket),
	     at(beTicketAgent0(Ticketagent),Time)),
   at(beTicketAgent2(Ticketagent),Time)).


%; End of file.
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',371).
% From E: 
% 
% ':-'(call_pel_directive(translate(ending,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.lps.pl'))).
:- call_pel_directive(translate(ending,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.lps.pl')).
