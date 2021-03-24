% From E: 
% 
% ':-'(call_pel_directive(translate(unskipped,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.lps.pl'))).
:- call_pel_directive(translate(unskipped,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.lps.pl')).
:-include(library('ec_planner/ec_test_incl')).
:-expects_dialect(lps).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OMSpace.e',358).
% From E: 
% 
% ':-'(call_pel_directive(translate(begining,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.lps.pl'))).
:- call_pel_directive(translate(begining,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.lps.pl')).
% Tue, 23 Mar 2021 19:06:52 GMT File: <stream>(0x55556a271a00)%;
%; Copyright (c) 2005 IBM Corporation and others.
%; All rights reserved. This program and the accompanying materials
%; are made available under the terms of the Common Public License v1.0
%; which accompanies this distribution, and is available at
%; http://www.eclipse.org/legal/cpl-v10.html
%;
%; Contributors:
%; IBM - Initial implementation
%;
%; integer
%;

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',14).
% sort diameter: integer
% From E: 
% 
% subsort(diameter,integer).
subsort(diameter,integer).
%; object

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',18).
% sort object
% From E: 
% 
% sort(object).
sort(object).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',20).
% sort agent: object
% From E: 
% 
% subsort(agent,object).
subsort(agent,object).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',22).
% sort physobj: object
% From E: 
% 
% subsort(physobj,object).
subsort(physobj,object).

% sort bed: physobj
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',22).
% From E: 
% 
% subsort(bed,physobj).
subsort(bed,physobj).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',24).
% sort snowflake: physobj
% From E: 
% 
% subsort(snowflake,physobj).
subsort(snowflake,physobj).

% sort sky: physobj
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',24).
% From E: 
% 
% subsort(sky,physobj).
subsort(sky,physobj).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',27).
% sort stuff: physobj
% From E: 
% 
% subsort(stuff,physobj).
subsort(stuff,physobj).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',29).
% sort surface: physobj
% From E: 
% 
% subsort(surface,physobj).
subsort(surface,physobj).

% sort ground: surface
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',29).
% From E: 
% 
% subsort(ground,surface).
subsort(ground,surface).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',32).
% sort snow: stuff
% From E: 
% 
% subsort(snow,stuff).
subsort(snow,stuff).

% sort ball
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',32).
% From E: 
% 
% sort(ball).
sort(ball).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',35).
% sort food: physobj
% From E: 
% 
% subsort(food,physobj).
subsort(food,physobj).

% sort fruit: food
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',35).
% From E: 
% 
% subsort(fruit,food).
subsort(fruit,food).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',37).
% sort orange: fruit
% From E: 
% 
% subsort(orange,fruit).
subsort(orange,fruit).

% sort salad: food
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',37).
% From E: 
% 
% subsort(salad,food).
subsort(salad,food).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',40).
% sort clothing: physobj
% From E: 
% 
% subsort(clothing,physobj).
subsort(clothing,physobj).

% sort scarf: clothing
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',40).
% From E: 
% 
% subsort(scarf,clothing).
subsort(scarf,clothing).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',42).
% sort hat: clothing
% From E: 
% 
% subsort(hat,clothing).
subsort(hat,clothing).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',44).
% sort vegetablematter: physobj
% From E: 
% 
% subsort(vegetablematter,physobj).
subsort(vegetablematter,physobj).

% sort coal: vegetablematter
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',44).
% From E: 
% 
% subsort(coal,vegetablematter).
subsort(coal,vegetablematter).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',47).
% sort bodypart: physobj
% From E: 
% 
% subsort(bodypart,physobj).
subsort(bodypart,physobj).

% sort hand: bodypart
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',47).
% From E: 
% 
% subsort(hand,bodypart).
subsort(hand,bodypart).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',50).
% sort papertowels: physobj
% From E: 
% 
% subsort(papertowels,physobj).
subsort(papertowels,physobj).

% sort device: physobj
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',50).
% From E: 
% 
% subsort(device,physobj).
subsort(device,physobj).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',52).
% sort electronicdevice: device
% From E: 
% 
% subsort(electronicdevice,device).
subsort(electronicdevice,device).

% sort lamp: electronicdevice
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',52).
% From E: 
% 
% subsort(lamp,electronicdevice).
subsort(lamp,electronicdevice).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',55).
% sort cat: physobj
% From E: 
% 
% subsort(cat,physobj).
subsort(cat,physobj).

% sort horse: physobj
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',55).
% From E: 
% 
% subsort(horse,physobj).
subsort(horse,physobj).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',58).
% sort weapon: physobj
% From E: 
% 
% subsort(weapon,physobj).
subsort(weapon,physobj).

% sort gun: weapon
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',58).
% From E: 
% 
% subsort(gun,weapon).
subsort(gun,weapon).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',60).
% sort bomb: weapon
% From E: 
% 
% subsort(bomb,weapon).
subsort(bomb,weapon).

% sort bullet: weapon
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',60).
% From E: 
% 
% subsort(bullet,weapon).
subsort(bullet,weapon).
%; location

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',65).
% sort location
% From E: 
% 
% sort(location).
sort(location).

% sort room: location, outside: location
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',65).
% From E: 
% 
% subsort(room,location).
subsort(room,location).
% From E: 
% 
% subsort(outside,location).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',65).
subsort(outside,location).
%; portal

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',70).
% sort portal
% From E: 
% 
% sort(portal).
sort(portal).

% sort door: portal, staircase: portal
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',70).
% From E: 
% 
% subsort(door,portal).
subsort(door,portal).
% From E: 
% 
% subsort(staircase,portal).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',70).
subsort(staircase,portal).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',72).
% sort street: portal
% From E: 
% 
% subsort(street,portal).
subsort(street,portal).

% sort track: portal
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',72).
% From E: 
% 
% subsort(track,portal).
subsort(track,portal).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',75).
% sort building
% From E: 
% 
% sort(building).
sort(building).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',77).
% sort fire: object
% From E: 
% 
% subsort(fire,object).
subsort(fire,object).

% sort smoke: physobj
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',77).
% From E: 
% 
% subsort(smoke,physobj).
subsort(smoke,physobj).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',80).
% sort furniture: physobj
% From E: 
% 
% subsort(furniture,physobj).
subsort(furniture,physobj).

% sort chair: furniture
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',80).
% From E: 
% 
% subsort(chair,furniture).
subsort(chair,furniture).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',82).
% sort table: furniture
% From E: 
% 
% subsort(table,furniture).
subsort(table,furniture).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',84).
% sort bill: physobj
% From E: 
% 
% subsort(bill,physobj).
subsort(bill,physobj).

% sort ticket: physobj
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',84).
% From E: 
% 
% subsort(ticket,physobj).
subsort(ticket,physobj).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',86).
% sort envelope: physobj
% From E: 
% 
% subsort(envelope,physobj).
subsort(envelope,physobj).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',88).
% sort text: physobj
% From E: 
% 
% subsort(text,physobj).
subsort(text,physobj).

% sort book: text
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',88).
% From E: 
% 
% subsort(book,text).
subsort(book,text).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',90).
% sort letter: text
% From E: 
% 
% subsort(letter,text).
subsort(letter,text).

% sort menu: text
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',90).
% From E: 
% 
% subsort(menu,text).
subsort(menu,text).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',93).
% sort paper: physobj
% From E: 
% 
% subsort(paper,physobj).
subsort(paper,physobj).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',95).
% sort content
% From E: 
% 
% sort(content).
sort(content).

% sort script
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',95).
% From E: 
% 
% sort(script).
sort(script).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',98).
% sort container: physobj
% From E: 
% 
% subsort(container,physobj).
subsort(container,physobj).

% sort cigarette: physobj
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',98).
% From E: 
% 
% subsort(cigarette,physobj).
subsort(cigarette,physobj).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',100).
% sort ashtray: physobj
% From E: 
% 
% subsort(ashtray,physobj).
subsort(ashtray,physobj).

% sort umbrella: physobj
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',100).
% From E: 
% 
% subsort(umbrella,physobj).
subsort(umbrella,physobj).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',103).
% sort pen: physobj
% From E: 
% 
% subsort(pen,physobj).
subsort(pen,physobj).
%; End of file.
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',105).
% From E: 
% 
% ':-'(call_pel_directive(translate(ending,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.lps.pl'))).
:- call_pel_directive(translate(ending,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.lps.pl')).
