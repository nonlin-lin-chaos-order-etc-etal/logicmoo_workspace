/*  Copyright 1986-2020 David H. D. Warren, Fernando C. N. Pereira and
    Jan Wielemaker.

    Permission is hereby granted, free of charge, to any person obtaining a
    copy of this software and associated documentation files (the
    "Software"), to deal in the Software without restriction, including
    without limitation the rights to use, copy, modify, merge, publish,
    distribute, sublicense, and/or sell copies of the Software, and to
    permit persons to whom the Software is furnished to do so, subject to
    the following conditions:

    The above copyright notice and this permission notice shall be included
    in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
    OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
    MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
    IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
    CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
    TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
    SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

:- dynamic(direct_ss/3).

symmetric_pred(spatial,B,X,C):- nonvar(X),nonvar(C),!,symmetric_direct(B,X,C),!.
symmetric_pred(spatial,B,X,C):- symmetric_direct(B,X,C).

symmetric_direct(B,X,C) :- direct_ss(B,X,C).
symmetric_direct(B,X,C) :- direct_ss(B,C,X).

add_ss(B,X,C):- X @> C, !, add_ss(B,C,X).
add_ss(B,X,C):- direct_ss(B, X,C), !.
add_ss(B,X,C):- assertz(direct_ss(B, X,C)), !.

% Facts about Europe.
% ------------------

:-add_ss(borders,albania,greece).
:-add_ss(borders,albania,yugoslavia).
:-add_ss(borders,albania,mediterranean).

:-add_ss(borders,andorra,france).
:-add_ss(borders,andorra,spain).

:-add_ss(borders,austria,czechoslovakia).
:-add_ss(borders,austria,hungary).
:-add_ss(borders,austria,italy).
:-add_ss(borders,austria,liechtenstein).
:-add_ss(borders,austria,switzerland).
:-add_ss(borders,austria,west_germany).
:-add_ss(borders,austria,yugoslavia).

:-add_ss(borders,belgium,france).
:-add_ss(borders,belgium,luxembourg).
:-add_ss(borders,belgium,netherlands).
:-add_ss(borders,belgium,west_germany).
:-add_ss(borders,belgium,atlantic).

:-add_ss(borders,bulgaria,greece).
:-add_ss(borders,bulgaria,romania).
:-add_ss(borders,bulgaria,turkey).
:-add_ss(borders,bulgaria,yugoslavia).
:-add_ss(borders,bulgaria,black_sea).

:-add_ss(borders,cyprus,mediterranean).

:-add_ss(borders,czechoslovakia,austria).
:-add_ss(borders,czechoslovakia,east_germany).
:-add_ss(borders,czechoslovakia,hungary).
:-add_ss(borders,czechoslovakia,poland).
:-add_ss(borders,czechoslovakia,soviet_union).
:-add_ss(borders,czechoslovakia,west_germany).

:-add_ss(borders,denmark,west_germany).
:-add_ss(borders,denmark,atlantic).
:-add_ss(borders,denmark,baltic).

:-add_ss(borders,eire,united_kingdom).
:-add_ss(borders,eire,atlantic).

:-add_ss(borders,finland,norway).
:-add_ss(borders,finland,soviet_union).
:-add_ss(borders,finland,sweden).
:-add_ss(borders,finland,baltic).

:-add_ss(borders,france,andorra).
:-add_ss(borders,france,belgium).
:-add_ss(borders,france,italy).
:-add_ss(borders,france,luxembourg).
:-add_ss(borders,france,monaco).
:-add_ss(borders,france,spain).
:-add_ss(borders,france,switzerland).
:-add_ss(borders,france,west_germany).
:-add_ss(borders,france,atlantic).
:-add_ss(borders,france,mediterranean).

:-add_ss(borders,east_germany,czechoslovakia).
:-add_ss(borders,east_germany,poland).
:-add_ss(borders,east_germany,west_germany).
:-add_ss(borders,east_germany,baltic).

:-add_ss(borders,greece,albania).
:-add_ss(borders,greece,bulgaria).
:-add_ss(borders,greece,turkey).
:-add_ss(borders,greece,yugoslavia).
:-add_ss(borders,greece,mediterranean).

:-add_ss(borders,hungary,austria).
:-add_ss(borders,hungary,czechoslovakia).
:-add_ss(borders,hungary,romania).
:-add_ss(borders,hungary,soviet_union).
:-add_ss(borders,hungary,yugoslavia).

:-add_ss(borders,iceland,atlantic).

:-add_ss(borders,italy,austria).
:-add_ss(borders,italy,france).
:-add_ss(borders,italy,san_marino).
:-add_ss(borders,italy,switzerland).
:-add_ss(borders,italy,yugoslavia).
:-add_ss(borders,italy,mediterranean).

:-add_ss(borders,liechtenstein,austria).
:-add_ss(borders,liechtenstein,switzerland).

:-add_ss(borders,luxembourg,belgium).
:-add_ss(borders,luxembourg,france).
:-add_ss(borders,luxembourg,west_germany).

:-add_ss(borders,malta,mediterranean).

:-add_ss(borders,monaco,france).
:-add_ss(borders,monaco,mediterranean).

:-add_ss(borders,netherlands,belgium).
:-add_ss(borders,netherlands,west_germany).
:-add_ss(borders,netherlands,atlantic).

:-add_ss(borders,norway,finland).
:-add_ss(borders,norway,sweden).
:-add_ss(borders,norway,soviet_union).
:-add_ss(borders,norway,arctic_ocean).
:-add_ss(borders,norway,atlantic).

:-add_ss(borders,poland,czechoslovakia).
:-add_ss(borders,poland,east_germany).
:-add_ss(borders,poland,soviet_union).
:-add_ss(borders,poland,baltic).

:-add_ss(borders,portugal,spain).
:-add_ss(borders,portugal,atlantic).

:-add_ss(borders,romania,bulgaria).
:-add_ss(borders,romania,hungary).
:-add_ss(borders,romania,soviet_union).
:-add_ss(borders,romania,yugoslavia).
:-add_ss(borders,romania,black_sea).

:-add_ss(borders,san_marino,italy).
:-add_ss(borders,san_marino,mediterranean).

:-add_ss(borders,spain,andorra).
:-add_ss(borders,spain,france).
:-add_ss(borders,spain,portugal).
:-add_ss(borders,spain,atlantic).
:-add_ss(borders,spain,mediterranean).

:-add_ss(borders,sweden,finland).
:-add_ss(borders,sweden,norway).
:-add_ss(borders,sweden,atlantic).
:-add_ss(borders,sweden,baltic).

:-add_ss(borders,switzerland,austria).
:-add_ss(borders,switzerland,france).
:-add_ss(borders,switzerland,italy).
:-add_ss(borders,switzerland,liechtenstein).
:-add_ss(borders,switzerland,west_germany).

:-add_ss(borders,west_germany,austria).
:-add_ss(borders,west_germany,belgium).
:-add_ss(borders,west_germany,czechoslovakia).
:-add_ss(borders,west_germany,denmark).
:-add_ss(borders,west_germany,east_germany).
:-add_ss(borders,west_germany,france).
:-add_ss(borders,west_germany,luxembourg).
:-add_ss(borders,west_germany,netherlands).
:-add_ss(borders,west_germany,switzerland).
:-add_ss(borders,west_germany,atlantic).
:-add_ss(borders,west_germany,baltic).

:-add_ss(borders,united_kingdom,eire).
:-add_ss(borders,united_kingdom,atlantic).

:-add_ss(borders,yugoslavia,albania).
:-add_ss(borders,yugoslavia,austria).
:-add_ss(borders,yugoslavia,bulgaria).
:-add_ss(borders,yugoslavia,greece).
:-add_ss(borders,yugoslavia,hungary).
:-add_ss(borders,yugoslavia,italy).
:-add_ss(borders,yugoslavia,romania).
:-add_ss(borders,yugoslavia,mediterranean).

% Facts about Asia.
% ----------------

:-add_ss(borders,afghanistan,china).
:-add_ss(borders,afghanistan,iran).
:-add_ss(borders,afghanistan,pakistan).
:-add_ss(borders,afghanistan,soviet_union).

:-add_ss(borders,bahrain,persian_gulf).

:-add_ss(borders,bangladesh,burma).
:-add_ss(borders,bangladesh,india).
:-add_ss(borders,bangladesh,indian_ocean).

:-add_ss(borders,bhutan,china).
:-add_ss(borders,bhutan,india).

:-add_ss(borders,burma,bangladesh).
:-add_ss(borders,burma,china).
:-add_ss(borders,burma,india).
:-add_ss(borders,burma,laos).
:-add_ss(borders,burma,thailand).
:-add_ss(borders,burma,indian_ocean).

:-add_ss(borders,cambodia,laos).
:-add_ss(borders,cambodia,thailand).
:-add_ss(borders,cambodia,vietnam).
:-add_ss(borders,cambodia,pacific).

:-add_ss(borders,china,afghanistan).
:-add_ss(borders,china,bhutan).
:-add_ss(borders,china,burma).
:-add_ss(borders,china,india).
:-add_ss(borders,china,laos).
:-add_ss(borders,china,mongolia).
:-add_ss(borders,china,nepal).
:-add_ss(borders,china,north_korea).
:-add_ss(borders,china,pakistan).
:-add_ss(borders,china,soviet_union).
:-add_ss(borders,china,vietnam).
:-add_ss(borders,china,pacific).

:-add_ss(borders,india,bangladesh).
:-add_ss(borders,india,bhutan).
:-add_ss(borders,india,burma).
:-add_ss(borders,india,china).
:-add_ss(borders,india,nepal).
:-add_ss(borders,india,pakistan).
:-add_ss(borders,india,indian_ocean).

:-add_ss(borders,indonesia,malaysia).
:-add_ss(borders,indonesia,papua_new_guinea).
:-add_ss(borders,indonesia,indian_ocean).
:-add_ss(borders,indonesia,pacific).

:-add_ss(borders,iran,afghanistan).
:-add_ss(borders,iran,iraq).
:-add_ss(borders,iran,pakistan).
:-add_ss(borders,iran,soviet_union).
:-add_ss(borders,iran,turkey).
:-add_ss(borders,iran,caspian_sea).
:-add_ss(borders,iran,persian_gulf).
:-add_ss(borders,iran,indian_ocean).

:-add_ss(borders,iraq,iran).
:-add_ss(borders,iraq,jordan).
:-add_ss(borders,iraq,kuwait).
:-add_ss(borders,iraq,saudi_arabia).
:-add_ss(borders,iraq,syria).
:-add_ss(borders,iraq,turkey).
:-add_ss(borders,iraq,persian_gulf).

:-add_ss(borders,israel,egypt).
:-add_ss(borders,israel,jordan).
:-add_ss(borders,laos,burma).
:-add_ss(borders,laos,cambodia).
:-add_ss(borders,laos,china).
:-add_ss(borders,laos,thailand).
:-add_ss(borders,laos,vietnam).

:-add_ss(borders,israel,lebanon).
:-add_ss(borders,israel,syria).
:-add_ss(borders,israel,mediterranean).
:-add_ss(borders,israel,red_sea).

:-add_ss(borders,japan,pacific).

:-add_ss(borders,jordan,iraq).
:-add_ss(borders,jordan,israel).
:-add_ss(borders,jordan,saudi_arabia).
:-add_ss(borders,jordan,syria).
:-add_ss(borders,jordan,red_sea).

:-add_ss(borders,kuwait,iraq).
:-add_ss(borders,kuwait,saudi_arabia).
:-add_ss(borders,kuwait,persian_gulf).

:-add_ss(borders,lebanon,israel).
:-add_ss(borders,lebanon,syria).
:-add_ss(borders,lebanon,mediterranean).

:-add_ss(borders,malaysia,indonesia).
:-add_ss(borders,malaysia,singapore).
:-add_ss(borders,malaysia,thailand).
:-add_ss(borders,malaysia,indian_ocean).
:-add_ss(borders,malaysia,pacific).

:-add_ss(borders,maldives,indian_ocean).

:-add_ss(borders,mongolia,china).
:-add_ss(borders,mongolia,soviet_union).

:-add_ss(borders,nepal,china).
:-add_ss(borders,nepal,india).

:-add_ss(borders,north_korea,china).
:-add_ss(borders,north_korea,south_korea).
:-add_ss(borders,north_korea,soviet_union).
:-add_ss(borders,north_korea,pacific).

:-add_ss(borders,oman,saudi_arabia).
:-add_ss(borders,oman,united_arab_emirates).
:-add_ss(borders,oman,south_yemen).
:-add_ss(borders,oman,indian_ocean).

:-add_ss(borders,pakistan,afghanistan).
:-add_ss(borders,pakistan,china).
:-add_ss(borders,pakistan,india).
:-add_ss(borders,pakistan,iran).
:-add_ss(borders,pakistan,indian_ocean).

:-add_ss(borders,philippines,pacific).

:-add_ss(borders,qatar,saudi_arabia).
:-add_ss(borders,qatar,united_arab_emirates).
:-add_ss(borders,qatar,persian_gulf).

:-add_ss(borders,saudi_arabia,iraq).
:-add_ss(borders,saudi_arabia,jordan).
:-add_ss(borders,saudi_arabia,kuwait).
:-add_ss(borders,saudi_arabia,oman).
:-add_ss(borders,saudi_arabia,qatar).
:-add_ss(borders,saudi_arabia,south_yemen).
:-add_ss(borders,saudi_arabia,united_arab_emirates).
:-add_ss(borders,saudi_arabia,yemen).
:-add_ss(borders,saudi_arabia,persian_gulf).
:-add_ss(borders,saudi_arabia,red_sea).

:-add_ss(borders,singapore,malaysia).
:-add_ss(borders,singapore,pacific).

:-add_ss(borders,south_korea,north_korea).
:-add_ss(borders,south_korea,pacific).

:-add_ss(borders,south_yemen,oman).
:-add_ss(borders,south_yemen,saudi_arabia).
:-add_ss(borders,south_yemen,yemen).
:-add_ss(borders,south_yemen,indian_ocean).

:-add_ss(borders,soviet_union,afghanistan).
:-add_ss(borders,soviet_union,china).
:-add_ss(borders,soviet_union,czechoslovakia).
:-add_ss(borders,soviet_union,finland).
:-add_ss(borders,soviet_union,hungary).
:-add_ss(borders,soviet_union,iran).
:-add_ss(borders,soviet_union,mongolia).
:-add_ss(borders,soviet_union,north_korea).
:-add_ss(borders,soviet_union,norway).
:-add_ss(borders,soviet_union,poland).
:-add_ss(borders,soviet_union,romania).
:-add_ss(borders,soviet_union,turkey).
:-add_ss(borders,soviet_union,arctic_ocean).
:-add_ss(borders,soviet_union,baltic).
:-add_ss(borders,soviet_union,black_sea).
:-add_ss(borders,soviet_union,caspian_sea).
:-add_ss(borders,soviet_union,pacific).

:-add_ss(borders,sri_lanka,indian_ocean).

:-add_ss(borders,syria,iraq).
:-add_ss(borders,syria,israel).
:-add_ss(borders,syria,jordan).
:-add_ss(borders,syria,lebanon).
:-add_ss(borders,syria,turkey).
:-add_ss(borders,syria,mediterranean).

:-add_ss(borders,taiwan,pacific).

:-add_ss(borders,thailand,burma).
:-add_ss(borders,thailand,cambodia).
:-add_ss(borders,thailand,laos).
:-add_ss(borders,thailand,malaysia).
:-add_ss(borders,thailand,indian_ocean).
:-add_ss(borders,thailand,pacific).

:-add_ss(borders,turkey,bulgaria).
:-add_ss(borders,turkey,greece).
:-add_ss(borders,turkey,iran).
:-add_ss(borders,turkey,iraq).
:-add_ss(borders,turkey,soviet_union).
:-add_ss(borders,turkey,syria).
:-add_ss(borders,turkey,black_sea).
:-add_ss(borders,turkey,mediterranean).

:-add_ss(borders,united_arab_emirates,oman).
:-add_ss(borders,united_arab_emirates,qatar).
:-add_ss(borders,united_arab_emirates,saudi_arabia).
:-add_ss(borders,united_arab_emirates,persian_gulf).

:-add_ss(borders,vietnam,cambodia).
:-add_ss(borders,vietnam,china).
:-add_ss(borders,vietnam,laos).
:-add_ss(borders,vietnam,pacific).

:-add_ss(borders,yemen,south_yemen).
:-add_ss(borders,yemen,saudi_arabia).
:-add_ss(borders,yemen,red_sea).

% Facts about Africa.
% ------------------

:-add_ss(borders,algeria,libya).
:-add_ss(borders,algeria,mali).
:-add_ss(borders,algeria,mauritania).
:-add_ss(borders,algeria,morocco).
:-add_ss(borders,algeria,niger).
:-add_ss(borders,algeria,tunisia).
:-add_ss(borders,algeria,mediterranean).

:-add_ss(borders,angola,congo).
:-add_ss(borders,angola,south_africa).
:-add_ss(borders,angola,zaire).
:-add_ss(borders,angola,zambia).
:-add_ss(borders,angola,atlantic).

:-add_ss(borders,botswana,south_africa).
:-add_ss(borders,botswana,zimbabwe).

:-add_ss(borders,burundi,rwanda).
:-add_ss(borders,burundi,tanzania).
:-add_ss(borders,burundi,zaire).

:-add_ss(borders,cameroon,central_african_republic).
:-add_ss(borders,cameroon,chad).
:-add_ss(borders,cameroon,congo).
:-add_ss(borders,cameroon,equatorial_guinea).
:-add_ss(borders,cameroon,gabon).
:-add_ss(borders,cameroon,nigeria).
:-add_ss(borders,cameroon,atlantic).

:-add_ss(borders,central_african_republic,cameroon).
:-add_ss(borders,central_african_republic,chad).
:-add_ss(borders,central_african_republic,congo).
:-add_ss(borders,central_african_republic,sudan).
:-add_ss(borders,central_african_republic,zaire).

:-add_ss(borders,chad,cameroon).
:-add_ss(borders,chad,central_african_republic).
:-add_ss(borders,chad,libya).
:-add_ss(borders,chad,niger).
:-add_ss(borders,chad,nigeria).
:-add_ss(borders,chad,sudan).

:-add_ss(borders,congo,angola).
:-add_ss(borders,congo,cameroon).
:-add_ss(borders,congo,central_african_republic).
:-add_ss(borders,congo,gabon).
:-add_ss(borders,congo,zaire).
:-add_ss(borders,congo,atlantic).

:-add_ss(borders,dahomey,niger).
:-add_ss(borders,dahomey,nigeria).
:-add_ss(borders,dahomey,togo).
:-add_ss(borders,dahomey,upper_volta).
:-add_ss(borders,dahomey,atlantic).

:-add_ss(borders,djibouti,ethiopia).
:-add_ss(borders,djibouti,somalia).
:-add_ss(borders,djibouti,indian_ocean).

:-add_ss(borders,egypt,israel).
:-add_ss(borders,egypt,libya).
:-add_ss(borders,egypt,sudan).
:-add_ss(borders,egypt,mediterranean).
:-add_ss(borders,egypt,red_sea).

:-add_ss(borders,equatorial_guinea,cameroon).
:-add_ss(borders,equatorial_guinea,gabon).
:-add_ss(borders,equatorial_guinea,atlantic).

:-add_ss(borders,ethiopia,djibouti).
:-add_ss(borders,ethiopia,kenya).
:-add_ss(borders,ethiopia,somalia).
:-add_ss(borders,ethiopia,sudan).
:-add_ss(borders,ethiopia,red_sea).

:-add_ss(borders,gabon,cameroon).
:-add_ss(borders,gabon,congo).
:-add_ss(borders,gabon,equatorial_guinea).
:-add_ss(borders,gabon,atlantic).

:-add_ss(borders,gambia,senegal).
:-add_ss(borders,gambia,atlantic).

:-add_ss(borders,ghana,ivory_coast).
:-add_ss(borders,ghana,togo).
:-add_ss(borders,ghana,upper_volta).
:-add_ss(borders,ghana,atlantic).

:-add_ss(borders,guinea,guinea_bissau).
:-add_ss(borders,guinea,ivory_coast).
:-add_ss(borders,guinea,liberia).
:-add_ss(borders,guinea,mali).
:-add_ss(borders,guinea,senegal).
:-add_ss(borders,guinea,sierra_leone).
:-add_ss(borders,guinea,atlantic).

:-add_ss(borders,guinea_bissau,guinea).
:-add_ss(borders,guinea_bissau,senegal).
:-add_ss(borders,guinea_bissau,atlantic).

:-add_ss(borders,ivory_coast,ghana).
:-add_ss(borders,ivory_coast,guinea).
:-add_ss(borders,ivory_coast,liberia).
:-add_ss(borders,ivory_coast,mali).
:-add_ss(borders,ivory_coast,upper_volta).
:-add_ss(borders,ivory_coast,atlantic).

:-add_ss(borders,kenya,ethiopia).
:-add_ss(borders,kenya,somalia).
:-add_ss(borders,kenya,sudan).
:-add_ss(borders,kenya,tanzania).
:-add_ss(borders,kenya,uganda).
:-add_ss(borders,kenya,indian_ocean).

:-add_ss(borders,lesotho,south_africa).

:-add_ss(borders,liberia,ivory_coast).
:-add_ss(borders,liberia,guinea).
:-add_ss(borders,liberia,sierra_leone).
:-add_ss(borders,liberia,atlantic).

:-add_ss(borders,libya,algeria).
:-add_ss(borders,libya,chad).
:-add_ss(borders,libya,egypt).
:-add_ss(borders,libya,niger).
:-add_ss(borders,libya,sudan).
:-add_ss(borders,libya,tunisia).
:-add_ss(borders,libya,mediterranean).

:-add_ss(borders,malagasy,indian_ocean).

:-add_ss(borders,malawi,mozambique).
:-add_ss(borders,malawi,tanzania).
:-add_ss(borders,malawi,zambia).

:-add_ss(borders,mali,algeria).
:-add_ss(borders,mali,guinea).
:-add_ss(borders,mali,ivory_coast).
:-add_ss(borders,mali,mauritania).
:-add_ss(borders,mali,niger).
:-add_ss(borders,mali,senegal).
:-add_ss(borders,mali,upper_volta).

:-add_ss(borders,mauritania,algeria).
:-add_ss(borders,mauritania,mali).
:-add_ss(borders,mauritania,morocco).
:-add_ss(borders,mauritania,senegal).
:-add_ss(borders,mauritania,atlantic).

:-add_ss(borders,mauritius,indian_ocean).

:-add_ss(borders,morocco,algeria).
:-add_ss(borders,morocco,mauritania).
:-add_ss(borders,morocco,atlantic).
:-add_ss(borders,morocco,mediterranean).

:-add_ss(borders,mozambique,malawi).
:-add_ss(borders,mozambique,south_africa).
:-add_ss(borders,mozambique,swaziland).
:-add_ss(borders,mozambique,tanzania).
:-add_ss(borders,mozambique,zambia).
:-add_ss(borders,mozambique,zimbabwe).
:-add_ss(borders,mozambique,indian_ocean).

:-add_ss(borders,niger,algeria).
:-add_ss(borders,niger,chad).
:-add_ss(borders,niger,dahomey).
:-add_ss(borders,niger,libya).
:-add_ss(borders,niger,mali).
:-add_ss(borders,niger,nigeria).
:-add_ss(borders,niger,upper_volta).

:-add_ss(borders,nigeria,cameroon).
:-add_ss(borders,nigeria,chad).
:-add_ss(borders,nigeria,dahomey).
:-add_ss(borders,nigeria,niger).
:-add_ss(borders,nigeria,atlantic).

:-add_ss(borders,rwanda,burundi).
:-add_ss(borders,rwanda,tanzania).
:-add_ss(borders,rwanda,uganda).
:-add_ss(borders,rwanda,zaire).

:-add_ss(borders,senegal,gambia).
:-add_ss(borders,senegal,guinea).
:-add_ss(borders,senegal,guinea_bissau).
:-add_ss(borders,senegal,mali).
:-add_ss(borders,senegal,mauritania).
:-add_ss(borders,senegal,atlantic).

:-add_ss(borders,seychelles,indian_ocean).

:-add_ss(borders,sierra_leone,guinea).
:-add_ss(borders,sierra_leone,liberia).
:-add_ss(borders,sierra_leone,atlantic).

:-add_ss(borders,somalia,djibouti).
:-add_ss(borders,somalia,ethiopia).
:-add_ss(borders,somalia,kenya).
:-add_ss(borders,somalia,indian_ocean).

:-add_ss(borders,south_africa,angola).
:-add_ss(borders,south_africa,botswana).
:-add_ss(borders,south_africa,lesotho).
:-add_ss(borders,south_africa,mozambique).
:-add_ss(borders,south_africa,swaziland).
:-add_ss(borders,south_africa,zambia).
:-add_ss(borders,south_africa,zimbabwe).
:-add_ss(borders,south_africa,atlantic).
:-add_ss(borders,south_africa,indian_ocean).

:-add_ss(borders,sudan,chad).
:-add_ss(borders,sudan,central_african_republic).
:-add_ss(borders,sudan,egypt).
:-add_ss(borders,sudan,ethiopia).
:-add_ss(borders,sudan,kenya).
:-add_ss(borders,sudan,libya).
:-add_ss(borders,sudan,uganda).
:-add_ss(borders,sudan,zaire).
:-add_ss(borders,sudan,red_sea).

:-add_ss(borders,swaziland,mozambique).
:-add_ss(borders,swaziland,south_africa).

:-add_ss(borders,tanzania,burundi).
:-add_ss(borders,tanzania,kenya).
:-add_ss(borders,tanzania,malawi).
:-add_ss(borders,tanzania,mozambique).
:-add_ss(borders,tanzania,rwanda).
:-add_ss(borders,tanzania,uganda).
:-add_ss(borders,tanzania,zaire).
:-add_ss(borders,tanzania,zambia).
:-add_ss(borders,tanzania,indian_ocean).

:-add_ss(borders,togo,dahomey).
:-add_ss(borders,togo,ghana).
:-add_ss(borders,togo,upper_volta).
:-add_ss(borders,togo,atlantic).

:-add_ss(borders,tunisia,algeria).
:-add_ss(borders,tunisia,libya).
:-add_ss(borders,tunisia,mediterranean).

:-add_ss(borders,uganda,kenya).
:-add_ss(borders,uganda,rwanda).
:-add_ss(borders,uganda,sudan).
:-add_ss(borders,uganda,tanzania).
:-add_ss(borders,uganda,zaire).

:-add_ss(borders,upper_volta,dahomey).
:-add_ss(borders,upper_volta,ghana).
:-add_ss(borders,upper_volta,ivory_coast).
:-add_ss(borders,upper_volta,mali).
:-add_ss(borders,upper_volta,niger).
:-add_ss(borders,upper_volta,togo).

:-add_ss(borders,zaire,angola).
:-add_ss(borders,zaire,burundi).
:-add_ss(borders,zaire,central_african_republic).
:-add_ss(borders,zaire,congo).
:-add_ss(borders,zaire,rwanda).
:-add_ss(borders,zaire,sudan).
:-add_ss(borders,zaire,tanzania).
:-add_ss(borders,zaire,uganda).
:-add_ss(borders,zaire,zambia).
:-add_ss(borders,zaire,atlantic).

:-add_ss(borders,zambia,angola).
:-add_ss(borders,zambia,malawi).
:-add_ss(borders,zambia,mozambique).
:-add_ss(borders,zambia,south_africa).
:-add_ss(borders,zambia,tanzania).
:-add_ss(borders,zambia,zaire).
:-add_ss(borders,zambia,zimbabwe).

:-add_ss(borders,zimbabwe,botswana).
:-add_ss(borders,zimbabwe,mozambique).
:-add_ss(borders,zimbabwe,south_africa).
:-add_ss(borders,zimbabwe,zambia).


% Facts about America.
% -------------------

:-add_ss(borders,argentina,bolivia).
:-add_ss(borders,argentina,brazil).
:-add_ss(borders,argentina,chile).
:-add_ss(borders,argentina,paraguay).
:-add_ss(borders,argentina,uruguay).
:-add_ss(borders,argentina,atlantic).

:-add_ss(borders,bahamas,atlantic).

:-add_ss(borders,barbados,atlantic).

:-add_ss(borders,belize,guatemala).
:-add_ss(borders,belize,mexico).
:-add_ss(borders,belize,atlantic).

:-add_ss(borders,bolivia,argentina).
:-add_ss(borders,bolivia,brazil).
:-add_ss(borders,bolivia,chile).
:-add_ss(borders,bolivia,paraguay).
:-add_ss(borders,bolivia,peru).

:-add_ss(borders,brazil,argentina).
:-add_ss(borders,brazil,bolivia).
:-add_ss(borders,brazil,colombia).
:-add_ss(borders,brazil,french_guiana).
:-add_ss(borders,brazil,guyana).
:-add_ss(borders,brazil,paraguay).
:-add_ss(borders,brazil,peru).
:-add_ss(borders,brazil,surinam).
:-add_ss(borders,brazil,uruguay).
:-add_ss(borders,brazil,venezuela).
:-add_ss(borders,brazil,atlantic).

:-add_ss(borders,canada,united_states).
:-add_ss(borders,canada,arctic_ocean).
:-add_ss(borders,canada,atlantic).
:-add_ss(borders,canada,pacific).

:-add_ss(borders,chile,argentina).
:-add_ss(borders,chile,bolivia).
:-add_ss(borders,chile,peru).
:-add_ss(borders,chile,pacific).

:-add_ss(borders,colombia,brazil).
:-add_ss(borders,colombia,ecuador).
:-add_ss(borders,colombia,panama).
:-add_ss(borders,colombia,peru).
:-add_ss(borders,colombia,venezuela).
:-add_ss(borders,colombia,atlantic).
:-add_ss(borders,colombia,pacific).

:-add_ss(borders,costa_rica,nicaragua).
:-add_ss(borders,costa_rica,panama).
:-add_ss(borders,costa_rica,atlantic).
:-add_ss(borders,costa_rica,pacific).

:-add_ss(borders,cuba,atlantic).

:-add_ss(borders,dominican_republic,haiti).
:-add_ss(borders,dominican_republic,atlantic).

:-add_ss(borders,ecuador,colombia).
:-add_ss(borders,ecuador,peru).
:-add_ss(borders,ecuador,pacific).

:-add_ss(borders,el_salvador,guatemala).
:-add_ss(borders,el_salvador,honduras).
:-add_ss(borders,el_salvador,pacific).

:-add_ss(borders,french_guiana,brazil).
:-add_ss(borders,french_guiana,surinam).

:-add_ss(borders,greenland,arctic_ocean).
:-add_ss(borders,greenland,atlantic).

:-add_ss(borders,grenada,atlantic).

:-add_ss(borders,guatemala,belize).
:-add_ss(borders,guatemala,el_salvador).
:-add_ss(borders,guatemala,honduras).
:-add_ss(borders,guatemala,mexico).
:-add_ss(borders,guatemala,atlantic).
:-add_ss(borders,guatemala,pacific).

:-add_ss(borders,guyana,brazil).
:-add_ss(borders,guyana,surinam).
:-add_ss(borders,guyana,venezuela).
:-add_ss(borders,guyana,atlantic).

:-add_ss(borders,haiti,dominican_republic).
:-add_ss(borders,haiti,atlantic).

:-add_ss(borders,honduras,el_salvador).
:-add_ss(borders,honduras,guatemala).
:-add_ss(borders,honduras,nicaragua).
:-add_ss(borders,honduras,atlantic).
:-add_ss(borders,honduras,pacific).

:-add_ss(borders,jamaica,atlantic).

:-add_ss(borders,mexico,belize).
:-add_ss(borders,mexico,guatemala).
:-add_ss(borders,mexico,united_states).
:-add_ss(borders,mexico,atlantic).
:-add_ss(borders,mexico,pacific).

:-add_ss(borders,nicaragua,costa_rica).
:-add_ss(borders,nicaragua,honduras).
:-add_ss(borders,nicaragua,atlantic).
:-add_ss(borders,nicaragua,pacific).

:-add_ss(borders,panama,colombia).
:-add_ss(borders,panama,costa_rica).
:-add_ss(borders,panama,atlantic).
:-add_ss(borders,panama,pacific).

:-add_ss(borders,paraguay,argentina).
:-add_ss(borders,paraguay,bolivia).
:-add_ss(borders,paraguay,brazil).

:-add_ss(borders,peru,bolivia).
:-add_ss(borders,peru,brazil).
:-add_ss(borders,peru,chile).
:-add_ss(borders,peru,colombia).
:-add_ss(borders,peru,ecuador).
:-add_ss(borders,peru,pacific).

:-add_ss(borders,surinam,brazil).
:-add_ss(borders,surinam,french_guiana).
:-add_ss(borders,surinam,guyana).

:-add_ss(borders,trinidad_and_tobago,atlantic).

:-add_ss(borders,united_states,canada).
:-add_ss(borders,united_states,mexico).
:-add_ss(borders,united_states,arctic_ocean).
:-add_ss(borders,united_states,atlantic).
:-add_ss(borders,united_states,pacific).

:-add_ss(borders,uruguay,argentina).
:-add_ss(borders,uruguay,brazil).
:-add_ss(borders,uruguay,atlantic).

:-add_ss(borders,venezuela,brazil).
:-add_ss(borders,venezuela,colombia).
:-add_ss(borders,venezuela,guyana).
:-add_ss(borders,venezuela,atlantic).

% Facts about Australasia.
% -----------------------

:-add_ss(borders,australia,indian_ocean).
:-add_ss(borders,australia,pacific).

:-add_ss(borders,fiji,pacific).

:-add_ss(borders,new_zealand,pacific).

:-add_ss(borders,papua_new_guinea,indonesia).
:-add_ss(borders,papua_new_guinea,pacific).

:-add_ss(borders,tonga,pacific).

:-add_ss(borders,western_samoa,pacific).

:-add_ss(borders,antarctica,southern_ocean).

% Facts about oceans and seas.
% ---------------------------

:-add_ss(borders,arctic_ocean,atlantic).
:-add_ss(borders,arctic_ocean,pacific).

:-add_ss(borders,atlantic,arctic_ocean).
:-add_ss(borders,atlantic,indian_ocean).
:-add_ss(borders,atlantic,pacific).
:-add_ss(borders,atlantic,southern_ocean).
:-add_ss(borders,atlantic,baltic).
:-add_ss(borders,atlantic,mediterranean).

:-add_ss(borders,indian_ocean,atlantic).
:-add_ss(borders,indian_ocean,pacific).
:-add_ss(borders,indian_ocean,southern_ocean).
:-add_ss(borders,indian_ocean,persian_gulf).
:-add_ss(borders,indian_ocean,red_sea).

:-add_ss(borders,pacific,arctic_ocean).
:-add_ss(borders,pacific,atlantic).
:-add_ss(borders,pacific,indian_ocean).
:-add_ss(borders,pacific,southern_ocean).

:-add_ss(borders,southern_ocean,atlantic).
:-add_ss(borders,southern_ocean,indian_ocean).
:-add_ss(borders,southern_ocean,pacific).

:-add_ss(borders,baltic,atlantic).

:-add_ss(borders,black_sea,mediterranean).

:-add_ss(borders,mediterranean,atlantic).
:-add_ss(borders,mediterranean,black_sea).

:-add_ss(borders,persian_gulf,indian_ocean).

:-add_ss(borders,red_sea,indian_ocean).

% Countries bordering each ocean and sea.
% --------------------------------------

:-add_ss(borders,arctic_ocean,norway).
:-add_ss(borders,arctic_ocean,soviet_union).
:-add_ss(borders,arctic_ocean,canada).
:-add_ss(borders,arctic_ocean,greenland).
:-add_ss(borders,arctic_ocean,united_states).

:-add_ss(borders,atlantic,belgium).
:-add_ss(borders,atlantic,denmark).
:-add_ss(borders,atlantic,eire).
:-add_ss(borders,atlantic,france).
:-add_ss(borders,atlantic,iceland).
:-add_ss(borders,atlantic,netherlands).
:-add_ss(borders,atlantic,norway).
:-add_ss(borders,atlantic,portugal).
:-add_ss(borders,atlantic,spain).
:-add_ss(borders,atlantic,sweden).
:-add_ss(borders,atlantic,west_germany).
:-add_ss(borders,atlantic,united_kingdom).
:-add_ss(borders,atlantic,angola).
:-add_ss(borders,atlantic,cameroon).
:-add_ss(borders,atlantic,congo).
:-add_ss(borders,atlantic,dahomey).
:-add_ss(borders,atlantic,equatorial_guinea).
:-add_ss(borders,atlantic,gabon).
:-add_ss(borders,atlantic,gambia).
:-add_ss(borders,atlantic,ghana).
:-add_ss(borders,atlantic,guinea).
:-add_ss(borders,atlantic,guinea_bissau).
:-add_ss(borders,atlantic,ivory_coast).
:-add_ss(borders,atlantic,liberia).
:-add_ss(borders,atlantic,mauritania).
:-add_ss(borders,atlantic,morocco).
:-add_ss(borders,atlantic,nigeria).
:-add_ss(borders,atlantic,senegal).
:-add_ss(borders,atlantic,sierra_leone).
:-add_ss(borders,atlantic,south_africa).
:-add_ss(borders,atlantic,togo).
:-add_ss(borders,atlantic,zaire).
:-add_ss(borders,atlantic,argentina).
:-add_ss(borders,atlantic,bahamas).
:-add_ss(borders,atlantic,barbados).
:-add_ss(borders,atlantic,belize).
:-add_ss(borders,atlantic,brazil).
:-add_ss(borders,atlantic,canada).
:-add_ss(borders,atlantic,colombia).
:-add_ss(borders,atlantic,costa_rica).
:-add_ss(borders,atlantic,cuba).
:-add_ss(borders,atlantic,dominican_republic).
:-add_ss(borders,atlantic,french_guiana).
:-add_ss(borders,atlantic,greenland).
:-add_ss(borders,atlantic,grenada).
:-add_ss(borders,atlantic,guatemala).
:-add_ss(borders,atlantic,guyana).
:-add_ss(borders,atlantic,haiti).
:-add_ss(borders,atlantic,honduras).
:-add_ss(borders,atlantic,jamaica).
:-add_ss(borders,atlantic,mexico).
:-add_ss(borders,atlantic,nicaragua).
:-add_ss(borders,atlantic,panama).
:-add_ss(borders,atlantic,surinam).
:-add_ss(borders,atlantic,trinidad_and_tobago).
:-add_ss(borders,atlantic,united_states).
:-add_ss(borders,atlantic,uruguay).
:-add_ss(borders,atlantic,venezuela).

:-add_ss(borders,indian_ocean,bangladesh).
:-add_ss(borders,indian_ocean,burma).
:-add_ss(borders,indian_ocean,india).
:-add_ss(borders,indian_ocean,indonesia).
:-add_ss(borders,indian_ocean,iran).
:-add_ss(borders,indian_ocean,malaysia).
:-add_ss(borders,indian_ocean,maldives).
:-add_ss(borders,indian_ocean,oman).
:-add_ss(borders,indian_ocean,pakistan).
:-add_ss(borders,indian_ocean,south_yemen).
:-add_ss(borders,indian_ocean,sri_lanka).
:-add_ss(borders,indian_ocean,thailand).
:-add_ss(borders,indian_ocean,djibouti).
:-add_ss(borders,indian_ocean,kenya).
:-add_ss(borders,indian_ocean,malagasy).
:-add_ss(borders,indian_ocean,mauritius).
:-add_ss(borders,indian_ocean,mozambique).
:-add_ss(borders,indian_ocean,seychelles).
:-add_ss(borders,indian_ocean,somalia).
:-add_ss(borders,indian_ocean,south_africa).
:-add_ss(borders,indian_ocean,tanzania).
:-add_ss(borders,indian_ocean,australia).

:-add_ss(borders,pacific,cambodia).
:-add_ss(borders,pacific,china).
:-add_ss(borders,pacific,indonesia).
:-add_ss(borders,pacific,japan).
:-add_ss(borders,pacific,malaysia).
:-add_ss(borders,pacific,north_korea).
:-add_ss(borders,pacific,philippines).
:-add_ss(borders,pacific,singapore).
:-add_ss(borders,pacific,south_korea).
:-add_ss(borders,pacific,soviet_union).
:-add_ss(borders,pacific,taiwan).
:-add_ss(borders,pacific,thailand).
:-add_ss(borders,pacific,vietnam).
:-add_ss(borders,pacific,canada).
:-add_ss(borders,pacific,chile).
:-add_ss(borders,pacific,colombia).
:-add_ss(borders,pacific,costa_rica).
:-add_ss(borders,pacific,ecuador).
:-add_ss(borders,pacific,el_salvador).
:-add_ss(borders,pacific,guatemala).
:-add_ss(borders,pacific,honduras).
:-add_ss(borders,pacific,mexico).
:-add_ss(borders,pacific,nicaragua).
:-add_ss(borders,pacific,panama).
:-add_ss(borders,pacific,peru).
:-add_ss(borders,pacific,united_states).
:-add_ss(borders,pacific,australia).
:-add_ss(borders,pacific,fiji).
:-add_ss(borders,pacific,new_zealand).
:-add_ss(borders,pacific,papua_new_guinea).
:-add_ss(borders,pacific,tonga).
:-add_ss(borders,pacific,western_samoa).

:-add_ss(borders,southern_ocean,antarctica).

:-add_ss(borders,baltic,denmark).
:-add_ss(borders,baltic,finland).
:-add_ss(borders,baltic,east_germany).
:-add_ss(borders,baltic,poland).
:-add_ss(borders,baltic,sweden).
:-add_ss(borders,baltic,west_germany).
:-add_ss(borders,baltic,soviet_union).

:-add_ss(borders,black_sea,bulgaria).
:-add_ss(borders,black_sea,romania).
:-add_ss(borders,black_sea,soviet_union).
:-add_ss(borders,black_sea,turkey).

:-add_ss(borders,caspian_sea,iran).
:-add_ss(borders,caspian_sea,soviet_union).

:-add_ss(borders,mediterranean,albania).
:-add_ss(borders,mediterranean,cyprus).
:-add_ss(borders,mediterranean,france).
:-add_ss(borders,mediterranean,greece).
:-add_ss(borders,mediterranean,italy).
:-add_ss(borders,mediterranean,malta).
:-add_ss(borders,mediterranean,monaco).
:-add_ss(borders,mediterranean,san_marino).
:-add_ss(borders,mediterranean,spain).
:-add_ss(borders,mediterranean,yugoslavia).
:-add_ss(borders,mediterranean,israel).
:-add_ss(borders,mediterranean,lebanon).
:-add_ss(borders,mediterranean,syria).
:-add_ss(borders,mediterranean,turkey).
:-add_ss(borders,mediterranean,algeria).
:-add_ss(borders,mediterranean,egypt).
:-add_ss(borders,mediterranean,libya).
:-add_ss(borders,mediterranean,morocco).
:-add_ss(borders,mediterranean,tunisia).

:-add_ss(borders,persian_gulf,bahrain).
:-add_ss(borders,persian_gulf,iran).
:-add_ss(borders,persian_gulf,iraq).
:-add_ss(borders,persian_gulf,kuwait).
:-add_ss(borders,persian_gulf,qatar).
:-add_ss(borders,persian_gulf,saudi_arabia).
:-add_ss(borders,persian_gulf,united_arab_emirates).

:-add_ss(borders,red_sea,israel).
:-add_ss(borders,red_sea,jordan).
:-add_ss(borders,red_sea,saudi_arabia).
:-add_ss(borders,red_sea,yemen).
:-add_ss(borders,red_sea,egypt).
:-add_ss(borders,red_sea,ethiopia).
:-add_ss(borders,red_sea,sudan).
