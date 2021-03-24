% From E: 
% 
% ':-'(call_pel_directive(translate(unskipped,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReaderTest.lps.pl'))).
:- call_pel_directive(translate(unskipped,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReaderTest.lps.pl')).
:-include(library('ec_planner/ec_test_incl')).
:-expects_dialect(lps).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rain.e',41).
% From E: 
% 
% ':-'(call_pel_directive(translate(begining,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReaderTest.lps.pl'))).
:- call_pel_directive(translate(begining,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReaderTest.lps.pl')).
% Tue, 23 Mar 2021 19:06:53 GMT File: <stream>(0x5555672e2600)% [physobj1,physobj2]
% !(physobj1=Pen1 & physobj2=Desk1) &
% !(physobj1=Paper1 & physobj2=Desk1) ->
% !HoldsAt(On(physobj1, physobj2),0).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReaderTest.e',1).
% From E: 
% 
% '->'(
%    ','(
%       not(','(
%              Physobj1=pen1, 
%              Physobj2=desk1)), 
%       not(','(
%              Physobj1=paper1, 
%              Physobj2=desk1))), 
%    holds(
%       not(on(Physobj1,Physobj2)), 0)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReaderTest.e',1).
if(at(not(on(Physobj1, Physobj2)), 0),  (not((equals(Physobj1, pen1), equals(Physobj2, desk1))), not((equals(Physobj1, paper1), equals(Physobj2, desk1))))).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReaderTest.e',7).
% [agent,book,page,time]
% Happens(BookTurnPageTo(agent,book,page),time) ->
% HoldsAt(Awake(agent),time) &
% ({page1} page1 != page & HoldsAt(BookIsOpenTo(book,page1),time)) &
% HoldsAt(Holding(agent,book),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReaderTest.e',7).
% From E: 
% 
% '->'(
%    happens(
%       bookTurnPageTo(Agent,Book,Page), 
%       Time), 
%    ','(
%       holds(
%          awake(Agent), 
%          Time), 
%       ','(
%          thereExists(Page1, 
%             ','(
%                Page1\=Page, 
%                holds(
%                   bookIsOpenTo(Book,Page1), 
%                   Time))), 
%          holds(
%             holding(Agent,Book), 
%             Time)))).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReaderTest.e',7).
if((at(awake(Agent), Time), thereExists(Page1,  ({dif(Page1, Page)}, at(bookIsOpenTo(Book, Page1), Time))), at(holding(Agent, Book), Time)), happens(bookTurnPageTo(Agent, Book, Page), Time)).


% [book,time]
% HoldsAt(BookClosed(book),time) ->
% ! {page} HoldsAt(BookIsOpenTo1(book,page),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReaderTest.e',14).
% From E: 
% 
% '->'(
%    holds(
%       bookClosed(Book), 
%       Time), 
%    not(thereExists(Page, 
%           holds(
%              bookIsOpenTo1(Book,Page), 
%              Time)))).
if(not(thereExists(Page,
		   at(bookIsOpenTo1(Book,Page),Time))),
   at(bookClosed(Book),Time)).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReaderTest.e',18).
% [book,time]
% HoldsAt(BookClosed(book),time) ->
% {page}%  ! HoldsAt(BookIsOpenTo2(book,page),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReaderTest.e',20).
% From E: 
% 
% exists(Page, 
%    '->'(
%       holds(
%          bookClosed(Book), 
%          Time), 
%       holds(
%          not(bookIsOpenTo2(Book,Page)), 
%          Time))).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReaderTest.e',20).
exists(Page,
 if(at(not(bookIsOpenTo2(Book,Page)),Time),
   at(bookClosed(Book),Time))).
% From E: 
% 
% ':-'(call_pel_directive(translate(ending,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReaderTest.lps.pl'))).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReaderTest.e',20).
:- call_pel_directive(translate(ending,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReaderTest.lps.pl')).
