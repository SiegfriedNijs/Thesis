%%% -*- Mode: Prolog; -*-

/*
Categorie statische hiërarchie
*/
category(anything_ca).

%
category(spatialobjects_ca).
subcategory(spatialobjects_ca,anything_ca).
%%
category(things_ca).
subcategory(things_ca,spatialobjects_ca).
%%%
category(moveablethings_ca).
subcategory(moveablethings_ca,things_ca).
category(fluidholders_ca).

subcategory(tools_ca,moveablethings_ca).
subcategory(containers_ca,moveablethings_ca).
subcategory(hobbythings_ca,moveablethings_ca).
subcategory(shapethings_ca,moveablethings_ca).
subcategory(phones_ca,moveablethings_ca).
subcategory(eatingtools_ca,moveablethings_ca).
subcategory(personaltools_ca,moveablethings_ca).
subcategory(healthtools_ca,moveablethings_ca).

category(tools_ca).
category(containers_ca).
category(hobbythings_ca).
category(shapethings_ca).
category(phones_ca).
category(eatingtools_ca).
category(personaltools_ca).
category(healthtools_ca).

subcategory(fluidholders_ca,containers_ca).
category(jug).
subcategory(jug,fluidholders_ca).

category(cup).
subcategory(cup,fluidholders_ca).
subcategory(cup,eatingtools_ca).

/*
Connectie van de 3D CAD trainingset modellen met hun representatieve categorie.
*/ 
numbermodels(cup,4).
model(cup,18645,ikea_cup).%9284
model(cup,18704,plastic_ikea_cup).%9346
model(cup,18748,red_van_pak_cup).%9391
model(cup,18951,plastic_standard_cup).%9605

category(can).
subcategory(can,fluidholders_ca).
subcategory(can,eatingtools_ca).

numbermodels(can,4).
model(can,18783,orangejuice_can).%9428
model(can,18797,tea_can_loose_leaf).%9445
model(can,18807,campbell_soup_can).%9455
model(can,18744,cola_can).%9387

category(mug).
subcategory(mug,fluidholders_ca).
subcategory(mug,eatingtools_ca).
numbermodels(mug,4).
model(mug,18646,mug_beer).%9285
model(mug,18652,mug_standard).%9291
model(mug,18658,mug_drom).%9297
model(mug,18659,mug_tenoz).%9298

category(nondrinking_bottle).
subcategory(nondrinking_bottle,tools_ca).
numbermodels(nondrinking_bottle,5).
model(nondrinking_bottle,18751,bottle_spray1qt).%9394
model(nondrinking_bottle,18760,bottle_laundrydetergent).%9403
model(nondrinking_bottle,18794,bottle_allpurposecleaner).%9439
model(nondrinking_bottle,18802,bottle_peroxide).%9450

category(bowl).
subcategory(bowl,fluidholders_ca).

category(mug).
subcategory(mug,fluidholders_ca).

category(glass).
subcategory(glass,fluidholders_ca).

category(spoon).
subcategory(spoon,eatingtools_ca).

category(straw).
subcategory(straw,eatingtools_ca).

category(fork).
subcategory(fork,eatingtools_ca).

category(drill).
subcategory(drill,tools_ca).

category(prescription_bottle).
subcategory(prescription_bottle,healthtools_ca).
subcategory(prescription_bottle,containers_ca).

category(wrist_watch).
subcategory(wrist_watch,personaltools_ca).

category(wallet).
subcategory(wallet,personaltools_ca).

category(eyeglasses).
subcategory(eyeglasses,personaltools_ca).

category(walking_cane).
subcategory(walking_cane,personaltools_ca).

category(medicine_pill).
subcategory(medicine_pill,healthtools_ca).

category(soap).
subcategory(soap,healthtools_ca).

category(toothbrush).
subcategory(toothbrush,healthtools_ca).

category(toothpaste).
subcategory(toothpaste,healthtools_ca).

category(jar).
subcategory(jar,containers_ca).

category(pitcher).
subcategory(pitcher,fluidholders_ca).

category(saucer).
subcategory(saucer,tools_ca).

category(tumbler).
subcategory(tumbler,fluidholders_ca).



category(tub).
subcategory(tub,fluidholders_ca).

category(plate).
subcategory(plate,eatingtools_ca).

category(pen).
subcategory(pen,tools_ca).

category(lighter).
subcategory(lighter,tools_ca).

category(funnel).
subcategory(funnel,tools_ca).

category(toy).
subcategory(toy,hobbythings_ca).

category(tv_remote).
subcategory(tv_remote,hobbythings_ca).

category(book).
subcategory(book,hobbythings_ca).

category(box).
subcategory(box,containers_ca).

category(container).
subcategory(container,containers_ca).

category(carton).
subcategory(carton,containers_ca).

category(cylinder).
subcategory(cylinder,shapethings_ca).

category(cell_phone).
subcategory(cell_phone,phones_ca).

category(cordless_phone).
subcategory(cordless_phone,phones_ca).

category(robotarms_ca).
subcategory(robotarms_ca,moveablethings_ca).

category(graspablethings_ca).
subcategory(graspablethings_ca,things_ca).
subcategory(cups_ca, graspablethings_ca).

%%%
category(immoveablethings_ca).
subcategory(immoveablethings_ca,things_ca).
category(surfaces_ca).
subcategory(surfaces_ca,immoveablethings_ca).
category(tables_ca).
subcategory(tables_ca,surfaces_ca).

category(rooms_ca).
subcategory(rooms_ca,immoveablethings_ca).
category(kitchen_ca).
subcategory(kitchen_ca,rooms_ca).
category(bedroom_ca).
subcategory(bedroom_ca,rooms_ca).
%%
category(stuff_ca).
subcategory(stuff_ca,spatialobjects_ca).

subcategory(drinkablefluids_ca,fluids_ca).
category(drinkablefluids_ca).
subcategory(nondrinkablefluids_ca,fluids_ca).
category(nondrinkablefluids_ca).

category(fluids_ca).
subcategory(fluids_ca,stuff_ca).
category(water_ca).
subcategory(water_ca,drinkablefluids_ca).
category(milk_ca).
subcategory(milk_ca,drinkablefluids_ca).
category(cola_ca).
subcategory(cola_ca,drinkablefluids_ca).
category(acid_ca).
subcategory(acid_ca,nondrinkablefluids_ca).
%%
category(places_ca).
subcategory(places_ca,spatialobjects_ca).
%%
category(actionperformers_ca).
subcategory(actionperformers_ca,spatialobjects_ca).
category(robots_ca).
subcategory(robots_ca,actionperformers_ca).
category(pr2_ca).
subcategory(pr2_ca, robots_ca).
category(humans_ca).
subcategory(humans_ca,actionperformers_ca).

%
category(temporalobjects_ca).
subcategory(temporalobjects_ca,anything_ca).
%%
category(processes_ca).
subcategory(processes_ca,temporalobjects_ca).
%%%
category(actions_ca).
subcategory(actions_ca,processes_ca).
%%%%


category(performableactions_ca).
subcategory(performableactions_ca,actions_ca).
subcategory(pickandplace_ca,performableactions_ca).
subcategory(pickup_ca,performableactions_ca).
subcategory(answerquestionactions_ca,performableactions_ca).
subcategory(place_ca,performableactions_ca).

category(movesomethings_ca).
subcategory(movesomethings_ca,actions_ca).
subcategory(pickandplace_ca,movesomethings_ca).
subcategory(pickup_ca,movesomethings_ca).
subcategory(place_ca,movesomethings_ca).

category(physicalactions_ca).
subcategory(physicalactions_ca, actions_ca).
%%%%%
category(pickandplace_ca).
subcategory(pickandplace_ca,physicalactions_ca).
%%%%%
category(pickup_ca).
subcategory(pickup_ca,physicalactions_ca).
%%%%%

/*
category(armmovements_ca).
subcategory(armmovements_ca,physicalactions_ca).
%%%%%
category(objectgrasps_ca).
subcategory(objectgrasps_ca,physicalactions_ca).
%%%%
category(nonphysicalactions_ca).
subcategory(nonphysicalactions_ca, actions_ca).
%%%%%
category(answerquestionactions_ca).
subcategory(answerquestionactions_ca,nonphysicalactions_ca).
%%
category(events_ca).
subcategory(events_ca,temporalobjects_ca).
*/
%
category(abstractobjects_ca).
subcategory(abstractobjects_ca,anything_ca).
%%
category(representationalobjects_ca).
subcategory(representationalobjects_ca,abstractobjects_ca).
%%%
category(communicationconcepts_ca).
subcategory(communicationconcepts_ca,representationalobjects_ca).
category(cravings_ca).
subcategory(cravings_ca,representationalobjects_ca).
category(thirst_ca).
subcategory(thirst_ca,cravings_ca).
category(hunger_ca).
subcategory(hunger_ca,cravings_ca).


%%%%
category(commands_ca).
subcategory(commands_ca,communicationconcepts_ca).

/*
category(clearcommands_ca).
subcategory(clearcommands_ca,commands_ca).
category(vaguecommands_ca).
subcategory(vaguecommands_ca,commands_ca).
category(explicitactioncommands_ca).
subcategory(explicitactioncommands_ca,commands_ca).
*/
/*
category(answerquestioncommands_ca).
subcategory(answerquestioncommands_ca,commands_ca).
category(performphysicalactioncommands_ca).
subcategory(performphysicalactioncommands_ca,commands_ca).
category(evidencecommands_ca).
subcategory(evidencecommands_ca,commands_ca).
category(feedbackcommands_ca).
subcategory(feedbackcommands_ca,commands_ca).
%%%%
category(questions_ca).
subcategory(questions_ca,communicationconcepts_ca).
%%%%
category(answers_ca).
subcategory(answers_ca,communicationconcepts_ca).
%%%%
*/

/*
category(commandclues_ca).
subcategory(commandclues_ca,communicationconcepts_ca).
category(actionclues_ca).
subcategory(actionclues_ca,commandclues_ca).
category(explicitactionmentions_ca).
subcategory(explicitactionmentions_ca,actionclues_ca).
category(parameterdescriptions_ca).
subcategory(parameterdescriptions_ca,actionclues_ca).
category(performerdescriptions_ca).
subcategory(performerdescriptions_ca,parameterdescriptions_ca).
category(objectactedondescriptions_ca).
subcategory(objectactedondescriptions_ca,parameterdescriptions_ca).
category(objectclues_ca).
subcategory(objectclues_ca,commandclues_ca).
category(objectcategorydefinitions_ca).
subcategory(objectcategorydefinitions_ca,objectclues_ca).
category(objectpropertydefinitions_ca).
subcategory(objectpropertydefinitions_ca,objectclues_ca).
category(relatedobjectmentions_ca).
subcategory(relatedobjectmentions_ca,objectclues_ca).
category(effectclues_ca).
subcategory(effectclues_ca,commandclues_ca).
category(spatialclues_ca).
subcategory(spatialclues_ca,effectclues_ca).
*/
%%%%
category(missinginformation_ca).
subcategory(missinginformation_ca,communicationconcepts_ca).
%%%
/*
category(properties_ca).
subcategory(properties_ca,representationalobjects_ca).
category(objectproperties_ca).
subcategory(objectproperties_ca,properties_ca).
category(colour_ca).
subcategory(colour_ca,objectproperties_ca).
category(material_ca).
subcategory(material_ca,objectproperties_ca).
category(weight_ca).
subcategory(weight_ca,objectproperties_ca).
category(dimensions_ca).
subcategory(dimensions_ca,objectproperties_ca).
category(actionparameters_ca).
subcategory(actionparameters_ca,properties_ca).
*/
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
subcategory(moveablethings_ca, actionparameters_ca).
category(actionperformers_ca).
subcategory(actionperformers_ca, actionparameters_ca).
%%%
category(contexts_ca).
subcategory(contexts_ca,representationalobjects_ca).
%%
category(mathematicalobjects_ca).
subcategory(mathematicalobjects_ca,representationalobjects_ca).
