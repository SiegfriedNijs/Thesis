%%% -*- Mode: Prolog; -*-

:- use_module('utilities.pl').
:- use_module('knowledge_processing.pl').

colour(extended,maroon,(128,0,0)).
colour(extended,dark_red,(139,0,0)).
colour(basic,brown,(165,42,42)).
colour(extended,firebrick,(178,34,34)).
colour(extended,crimson,(220,20,60)).
colour(basic,red,(255,0,0)).
colour(extended,tomato,(255,99,71)).
colour(extended,coral,(255,127,80)).
colour(extended,indian_red,(205,92,92)).
colour(extended,light_coral,(240,128,128)).
colour(extended,dark_salmon,(233,150,122)).
colour(extended,salmon,(250,128,114)).
colour(extended,light_salmon,(255,160,122)).
colour(extended,orange_red,(255,69,0)).
colour(extended,dark_orange,(255,140,0)).
colour(basic,orange ,(255,165,0)).
colour(basic,gold,(255,215,0)).
colour(extended,dark_golden_rod,(184,134,11)).
colour(extended,golden_rod,(218,165,32)).
colour(extended,pale_golden_rod,(238,232,170)).
colour(extended,dark_khaki,(189,183,107)).
colour(extended,khaki,(240,230,140)).
colour(extended,olive,(128,128,0)).
 colour(basic,yellow,(255,255,0)).
 colour( extended,yellow_green,(154,205,50)).
 colour( extended,dark_olive_green,(85,107,47)).
 colour( extended,olive_drab,(107,142,35)).
 colour( extended,lawn_green,(124,252,0)).
 colour( extended,chart_reuse,(127,255,0)).
 colour( extended,green_yellow,(173,255,47)).
 colour(extended,dark_green,(0,100,0)).
  colour(basic,green,(0,128,0)).
  colour(extended,forest_green,(34,139,34)).
  colour(extended,lime,(0,255,0)).
  colour(extended,lime_green,(50,205,50)).
  colour(extended,light_green,(144,238,144)).
  colour(extended,pale_green,(152,251,152)).
  colour(extended,dark_sea_green,(143,188,143)).
  colour(extended,medium_spring_green,(0,250,154)).
  colour(extended,spring_green,(0,255,127)).
  colour(extended,sea_green,(46,139,87)).
  colour(extended,medium_aqua_marine ,(102,205,170)).
  colour(extended,medium_sea_green,(60,179,113)).
  colour(extended,light_sea_green,(32,178,170)).
  colour(extended,dark_slate_gray,(47,79,79)).
  colour(extended,teal,(0,128,128)).
  colour(extended,dark_cyan,(0,139,139)).
  colour(extended,aqua,(0,255,255)).
  colour(extended,cyan,(0,255,255)).
  colour(extended,light_cyan,(224,255,255)).
  colour(extended,dark_turquoise,(0,206,209)).
  colour(extended,turquoise,(64,224,208)).
  colour(extended,medium_turquoise,(72,209,204)).
  colour(extended,pale_turquoise,(175,238,238)).
  colour(extended,aqua_marine,(127,255,212)).
  colour(extended,powder_blue,(176,224,230)).
  colour(extended,cadet_blue,(95,158,160)).
  colour(extended,steel_blue,(70,130,180)).
  colour(extended,corn_flower_blue,(100,149,237)).
  colour(extended,deep_sky_blue,(0,191,255)).
  colour(extended,dodger_blue,(30,144,255)).
  colour(extended,light_blue ,(173,216,230)).
  colour(extended,sky_blue,(135,206,235)).
  colour(extended,light_sky_blue,(135,206,250)).
  colour(extended,midnight_blue,(25,25,112)).
  colour(extended,navy,(0,0,128)).
  colour(extended,dark_blue,(0,0,139)).
  colour(extended,medium_blue,(0,0,205)).
  colour(basic,blue,(0,0,255)).
  colour(extended,royal_blue,(65,105,225)).
  colour(extended,blue_violet,(138,43,226)).
  colour(basic,indigo,(75,0,130)).
  colour(extended,dark_slate_blue,(72,61,139)).
  colour(extended,slate_blue,(106,90,205)).
  colour(extended,medium_slate_blue,(123,104,238)).
  colour(extended,medium_purple,(147,112,219)).
  colour(extended,dark_magenta,(139,0,139)).
  colour(extended,dark_violet,(148,0,211)).
  colour(extended,dark_orchid,(153,50,204)).
  colour(extended,medium_orchid,(186,85,211)).
  colour(basic,purple,(128,0,128)).
  colour(extended,thistle ,(216,191,216)).
  colour(extended,plum,(221,160,221)).
  colour(basic,violet,(238,130,238)).
  colour(basic,magenta,(255,0,255)).
  colour(extended,orchid ,(218,112,214)).
  colour(extended,medium_violet_red,(199,21,133)).
  colour(extended,pale_violet_red,(219,112,147)).
  colour(extended,deep_pink,(255,20,147)).
  colour(extended,hot_pink,(255,105,180)).
  colour(extended,light_pink,(255,182,193)).
  colour(basic,pink,(255,192,203)).
  colour(extended,antique_white,(250,235,215)).
  colour(extended,beige,(245,245,220)).
  colour(extended,bisque,(255,228,196)).
  colour(extended,blanched_almond ,(255,235,205)).
  colour(extended,wheat,(245,222,179)).
  colour(extended,corn_silk ,(255,248,220)).
  colour(extended,lemon_chiffon,	(255,250,205)).
  colour(extended,light_golden,(250,250,210)).
  colour(extended,light_yellow,(255,255,224)).
  colour(extended,saddle_brown,(139,69,19)).
  colour(extended,sienna,(160,82,45)).
  colour(extended,chocolate,(210,105,30)).
  colour(extended,peru ,(205,133,63)).
  colour(extended,sandy_brown,(244,164,96)).
  colour(extended,burly_wood ,(222,184,135)).
  colour(extended,tan ,(210,180,140)).
  colour(extended,rosy_brown,(188,143,143)).
  colour(extended,moccasin,(255,228,181)).
  colour(extended,navajo_white,	(255,222,173)).
  colour(extended,peach_puff,(255,218,185)).
  colour(extended,misty_rose,(255,228,225)).
  colour(extended,lavender_blush,(255,240,245)).
  colour(extended,linen,(250,240,230)).
  colour(extended,old_lace,(253,245,230)).
  colour(extended,papaya_whip ,(255,239,213)).
  colour(extended,sea_shell,(255,245,238)).
  colour(extended,mint_cream,(245,255,250)).
  colour(extended,slate_gray,(112,128,144)).
  colour(extended,light_slate_gray,(119,136,153)).
  colour(extended,light_steel_blue,(176,196,222)).
  colour(extended,lavender,(230,230,250)).
  colour(extended,floral_white,(255,250,240)).
  colour(extended,alice_blue,(240,248,255)).
  colour(extended,ghost_white,(248,248,255)).
  colour(extended,honeydew ,(240,255,240)).
  colour(extended,ivory ,(255,255,240)).
  colour(extended,azure,(240,255,255)).
  colour(extended,snow,(255,250,250)).
  colour(basic,black,(0,0,0)).
  colour(extended,dim_gray,(105,105,105)).
  colour(basic,gray,(128,128,128)).
  colour(extended,dark_gray,(169,169,169)).
  colour(basic,silver,(192,192,192)).
  colour(extended,light_gray,(211,211,211)).
  colour(extended,gainsboro,(220,220,220)).
  colour(extended,white_smoke,(245,245,245)).
  colour(basic,white,(255,255,255)).

numberExtended(3).
numberBasic(2).

closestBasicColour([R,G,B],Col) :-
	findall((D2-C),(colour(basic,C,(X1,Y1,Z1)),distance((X1,Y1,Z1),(R,G,B),D),D2 is 360-D),Pr),
	keysort(Pr,Sorted),
	reverse(Sorted,S2),
	nth1(1,S2,(Dmax-Col)).

closestExtColour([R,G,B],Col) :-
	findall((D2-C),(colour(extended,C,(X1,Y1,Z1)),distance((X1,Y1,Z1),(R,G,B),D),D2 is 360-D),Pr),
	keysort(Pr,Sorted),
	reverse(Sorted,S2),
	nth1(1,S2,(Dmax-Col)).

colourProbability((X2,Y2,Z2),Pend) :-
	findall((D2-C),(colour(basic,C,(X1,Y1,Z1)),distance((X1,Y1,Z1),(X2,Y2,Z2),D),D2 is 360-D),Pr),
	keysort(Pr,Sorted),
	reverse(Sorted,S2),
	numberBasic(NB),
	getPreList(S2,NB,S3),
	%writeln(S3),
	findall((D22-C2),(colour(extended,C2,(X12,Y12,Z12)),distance((X12,Y12,Z12),(X2,Y2,Z2),D2),D22 is 360-D2),Pr2),
	keysort(Pr2,Sorted2),
	reverse(Sorted2,S22),
	numberExtended(N),
	getPreList(S22,N,S32),
	%writeln(S32),
	append(S3,S32,P2),
	keysort(P2,Sorted22),
	reverse(Sorted22,P),
	changelist3(P,P1),
	probabilities_sum(P1,Sum2),
	relative_probs(P1,Sum2,Pend).

