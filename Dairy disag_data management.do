
*********************************************************************************************
*Manuscript: Disaggregation of dairy in composite dishes in the UK

*Data management file for estimating dairy consumption with and without dairy disaggregation
*********************************************************************************************


****************
*Clear settings
****************
clear matrix
macro drop _all
graph drop _all


*************************************************************
*Assign values using global macros for file location and date
*************************************************************
global location "K:\DrJaacksGroup\FSS - Dietary Monitoring\SHeS\SHeS 2021\Dairy disag" 
global data `"$location\Data"'
global date "20231512"
global code `"$location\Code"'

*Demographic data
global dems `"$data\shes21i_eul"'
*Intake24 diet data with dairy disag values 
global diet `"$data\diet_dairydisag.dta_$date"'


*****************
*Merging datasets
*****************
use "$dems", clear
keep Cpseriala psu Strata InIntake24 SHeS_Intake24_wt_sc NumberOfRecalls
sort Cpseriala

merge 1:m Cpseriala using "$diet"
	*check merge - OK
	*tab _merge InIntake24
	drop _merge
	*drop dairy disag info per 100g that aren't needed
	drop Dairy_Ingredients - Butter SubGroupDesc
	*drop nutrients not used 
	drop Energykcal- OtherCheeseg
	
*Drop intake from supplements
drop if RecipeMainFoodGroupCode==54 /*n=3,589*/


****************************************
*Create subpop variable for analysis
****************************************
*Completed at least 1 recall
gen intake24=0
replace intake24=1 if InIntake24==1


**********************************************************************************
*Tag each unique recall within the food-level dataset for subsequent calculations
**********************************************************************************
bysort Cpseriala RecallNo: gen n=_n==1
replace n=. if RecallNo==.


/************************************************************************************
Some dairy product re-categorisation is required:

1) Re-categorising dairy-free items out of dairy and into new dairy-free food groups
2) Re-categorising hot chocolates made with water out of dairy and into 'misc'
3) Re-categorising milky coffees (e.g. lattes, cappucinos) into dairy from 'coffee'
4) Re-categorising two ice lollies (without ice cream) incorrectly categorised into 
'ice cream' instead of 'sugar confectionery'
************************************************************************************/

***1) Recategorising dairy-free items

*Sub food group level
replace RecipeSubFoodGroupCode="13R_DF" if RecipeSubFoodGroupCode=="13R" & (strpos(FoodDescription, "Almond") | strpos(FoodDescription, "Alpro") | strpos(FoodDescription, "soya") | strpos(FoodDescription, "Soya") | strpos(FoodDescription, "Hemp") | strpos(FoodDescription, "Oat") | strpos(FoodDescription, "Rice"))
replace RecipeSubFoodGroupCode="13B_DF" if RecipeSubFoodGroupCode=="13B" & strpos(FoodDescription, "Alpro") 
replace RecipeSubFoodGroupCode="14R_DF" if RecipeSubFoodGroupCode=="14R" & strpos(FoodDescription, "Tofu") 
replace RecipeSubFoodGroupCode="15B_DF" if RecipeSubFoodGroupCode=="15B" & strpos(FoodDescription, "Soya") 
replace RecipeSubFoodGroupCode="15C_DF" if RecipeSubFoodGroupCode=="15C" & strpos(FoodDescription, "Soya") 
replace RecipeSubFoodGroupCode="53R_DF" if RecipeSubFoodGroupCode=="53R" & strpos(FoodDescription, "Dairy free")
*Main food group level
replace RecipeMainFoodGroupCode=63 if RecipeSubFoodGroupCode=="13R_DF" | RecipeSubFoodGroupCode=="13B_DF"
replace RecipeMainFoodGroupCode=64 if RecipeSubFoodGroupCode=="14R_DF"
replace RecipeMainFoodGroupCode=65 if RecipeSubFoodGroupCode=="15B_DF" | RecipeSubFoodGroupCode=="15C_DF"
replace RecipeMainFoodGroupCode=66 if RecipeSubFoodGroupCode=="53R_DF"

***2) Re-categorising hot chocolates made with water

*Sub food group level
replace RecipeSubFoodGroupCode="50A" if RecipeSubFoodGroupCode=="13R" & strpos(FoodDescription, "made with water") 
*Main food group level 
replace RecipeMainFoodGroupCode=50 if RecipeSubFoodGroupCode=="50A"


***3) Re-categorising milky coffees (lattes/cappuccinos/mochas) into 'other milk'

*First, re-categorise dairy-free coffees
replace RecipeSubFoodGroupCode="13R_DF" if RecipeSubFoodGroupCode=="51A" & (strpos(FoodDescription, "soya milk"))

*Sub food group level
replace RecipeSubFoodGroupCode="13R" if RecipeSubFoodGroupCode=="51A" & (strpos(FoodDescription, "Cappuccino") | strpos(FoodDescription, "latte") | strpos(FoodDescription, "cappuccino")| strpos(FoodDescription, "Flat white") | strpos(FoodDescription, "Latte") | strpos(FoodDescription, "Mocha")) 
*Main food group level
replace RecipeMainFoodGroupCode=13 if RecipeSubFoodGroupCode=="13R"


***4) Re-categorising two incorrectly categorised ice lollies

*Sub food group level
replace RecipeSubFoodGroupCode="43R" if RecipeSubFoodGroupCode=="53R" & (strpos(FoodDescription, "Sorbet") | strpos(FoodDescription, "Twister")) 

*Main food group level
replace RecipeMainFoodGroupCode=43 if RecipeSubFoodGroupCode=="43R"


***5) Update main and sub food group description variables based on updated categories

*Main food groups
replace RecipeMainFoodGroupDesc="OTHER MILK AND CREAM (DAIRY FREE)" if RecipeMainFoodGroupCode==63
replace RecipeMainFoodGroupDesc="CHEESE (DAIRY FREE)" if RecipeMainFoodGroupCode==64
replace RecipeMainFoodGroupDesc="YOGURT, FROMAGE FRAIS & DAIRY DESSERTS (DAIRY FREE)" if RecipeMainFoodGroupCode==65
replace RecipeMainFoodGroupDesc="ICE CREAM (DAIRY-FREE)" if RecipeMainFoodGroupCode==66
*Sub food group
replace RecipeSubFoodGroupDesc="OTHER MILK (DAIRY-FREE)" if RecipeSubFoodGroupCode=="13R_DF"
replace RecipeSubFoodGroupDesc="CREAM (DAIRY-FREE)" if RecipeSubFoodGroupCode=="13B_DF"
replace RecipeSubFoodGroupDesc="OTHER CHEESE (DAIRY-FREE)" if RecipeSubFoodGroupCode=="14R_DF" 
replace RecipeSubFoodGroupDesc="YOGURT (DAIRY-FREE)" if RecipeSubFoodGroupCode=="15B_DF"
replace RecipeSubFoodGroupDesc="FROMAGE FRAIS AND DAIRY DESSERTS (DAIRY-FREE)" if RecipeSubFoodGroupCode=="15C_DF"
replace RecipeSubFoodGroupDesc="ICE CREAM (DAIRY-FREE)" if RecipeSubFoodGroupCode=="53R_DF"


/***************************************************************************************************
Create high level food categories (that reflect NDNS food categories)
Here, we re-categorise butter (main food group 17) from 'fat spreads' into 'milk and milk products'
***************************************************************************************************/

***Create 'Food Category Code' and 'Food Category Description' variables

**Food category code
gen FoodCategoryCode=.
replace FoodCategoryCode=1 if RecipeMainFoodGroupCode>=1 & RecipeMainFoodGroupCode<=9 |RecipeMainFoodGroupCode==59 
replace FoodCategoryCode=2 if RecipeMainFoodGroupCode>=10 & RecipeMainFoodGroupCode<=15 | RecipeMainFoodGroupCode==17 | RecipeMainFoodGroupCode==60 | RecipeMainFoodGroupCode==53 
replace FoodCategoryCode=3 if RecipeMainFoodGroupCode==16
replace FoodCategoryCode=4 if RecipeMainFoodGroupCode>=18 & RecipeMainFoodGroupCode<=21
replace FoodCategoryCode=5 if RecipeMainFoodGroupCode>=22 & RecipeMainFoodGroupCode<=32
replace FoodCategoryCode=6 if RecipeMainFoodGroupCode>=33 & RecipeMainFoodGroupCode<=35
replace FoodCategoryCode=7 if RecipeMainFoodGroupCode==62
replace FoodCategoryCode=8 if RecipeMainFoodGroupCode>=36 & RecipeMainFoodGroupCode<=39
replace FoodCategoryCode=9 if RecipeMainFoodGroupCode==40
replace FoodCategoryCode=10 if RecipeMainFoodGroupCode==41 | RecipeMainFoodGroupCode==43 | RecipeMainFoodGroupCode==44
replace FoodCategoryCode=11 if RecipeMainFoodGroupCode==42
replace FoodCategoryCode=12 if RecipeMainFoodGroupCode==56 
replace FoodCategoryCode=13 if RecipeMainFoodGroupCode==45 | RecipeMainFoodGroupCode==61 | RecipeMainFoodGroupCode==57 | RecipeMainFoodGroupCode==58 | RecipeMainFoodGroupCode==51
replace FoodCategoryCode=14 if RecipeMainFoodGroupCode>=47 & RecipeMainFoodGroupCode<=49
replace FoodCategoryCode=15 if RecipeMainFoodGroupCode==50
replace FoodCategoryCode=16 if RecipeMainFoodGroupCode==52
replace FoodCategoryCode=17 if RecipeMainFoodGroupCode==55
replace FoodCategoryCode=18	if strpos(RecipeSubFoodGroupCode, "_DF") /* dairy-free items*/
replace FoodCategoryCode=. if RecipeMainFoodGroupCode==.

**Food category description
gen FoodCategoryDesc=""
replace FoodCategoryDesc="Cereals and Cereal Products" if FoodCategoryCode==1
replace FoodCategoryDesc="Milk and Milk Products" if FoodCategoryCode==2
replace FoodCategoryDesc="Eggs and Egg Dishes" if FoodCategoryCode==3
replace FoodCategoryDesc="Fat Spreads" if FoodCategoryCode==4
replace FoodCategoryDesc="Meat and Meat Products" if FoodCategoryCode==5
replace FoodCategoryDesc="Fish and Fish Dishes" if FoodCategoryCode==6
replace FoodCategoryDesc="Sandwiches" if FoodCategoryCode==7
replace FoodCategoryDesc="Vegetables, potatoes" if FoodCategoryCode==8
replace FoodCategoryDesc="Fruit" if FoodCategoryCode==9
replace FoodCategoryDesc="Sugar, Preserves and Confectionery" if FoodCategoryCode==10
replace FoodCategoryDesc="Savoury Snacks" if FoodCategoryCode==11
replace FoodCategoryDesc="Nuts and Seeds" if FoodCategoryCode==12
replace FoodCategoryDesc="Non-alcoholic beverages" if FoodCategoryCode==13
replace FoodCategoryDesc="Alcoholic beverages" if FoodCategoryCode==14
replace FoodCategoryDesc="Misc" if FoodCategoryCode==15
replace FoodCategoryDesc="Toddler foods" if FoodCategoryCode==16
replace FoodCategoryDesc="Artificial sweeteners" if FoodCategoryCode==17
replace FoodCategoryDesc="Milk and Milk Products (dairy-free)" if FoodCategoryCode==18

order FoodCategoryCode FoodCategoryDesc, after(FoodDescription)

/********************************************************
Calculate daily intakes (g) of dairy with disaggregation
*********************************************************/

bysort Cpseriala RecallNo: egen Day_Dairy=sum(Dairyg)
bysort Cpseriala RecallNo: egen Day_Milk=sum(Milkg)
bysort Cpseriala RecallNo: egen Day_Cheese=sum(Cheeseg)
bysort Cpseriala RecallNo: egen Day_Yogurt=sum(Yogurtg)
bysort Cpseriala RecallNo: egen Day_Cream=sum(Creamg)
bysort Cpseriala RecallNo: egen Day_Butter=sum(Butterg)

bysort Cpseriala RecallNo: egen Day_Milk_Skimmed=sum(Milk_Skimmedg)
bysort Cpseriala RecallNo: egen Day_Milk_SemiSkimmed=sum(Milk_SemiSkimmedg)
bysort Cpseriala RecallNo: egen Day_Milk_Whole=sum(Milk_Wholeg)

bysort Cpseriala RecallNo: egen Day_Cheese_Cheddar=sum(Cheese_Cheddarg)
bysort Cpseriala RecallNo: egen Day_Cheese_Cottage=sum(Cheese_Cottageg)
bysort Cpseriala RecallNo: egen Day_Cheese_Other=sum(Cheese_Otherg)

bysort Cpseriala RecallNo: egen Day_Cheese_Skimmed=sum(Cheese_Skimmedg)
bysort Cpseriala RecallNo: egen Day_Cheese_SemiSkimmed=sum(Cheese_SemiSkimmedg)
bysort Cpseriala RecallNo: egen Day_Cheese_Whole=sum(Cheese_Wholeg)

bysort Cpseriala RecallNo: egen Day_Yogurt_Skimmed=sum(Yogurt_Skimmedg)
bysort Cpseriala RecallNo: egen Day_Yogurt_SemiSkimmed=sum(Yogurt_SemiSkimmedg)
bysort Cpseriala RecallNo: egen Day_Yogurt_Whole=sum(Yogurt_Wholeg)

bysort Cpseriala RecallNo: egen Day_Cream_SemiSkimmed=sum(Cream_SemiSkimmedg)
bysort Cpseriala RecallNo: egen Day_Cream_Whole=sum(Cream_Wholeg)


/**********************************************************
Calculate daily intakes (g) of dairy without disaggregation
***********************************************************/

	/*Quark was categorised into fromage frais and other desserts but also disaggregated into 100% 'other cheese'. 
	Re-categorising quark into 'other cheese' so it will not be counted twice*/
	replace RecipeMainFoodGroupCode=14 if FoodDescription=="Quark"
	replace RecipeMainFoodGroupDesc="Cheese" if FoodDescription=="Quark"

	replace RecipeSubFoodGroupCode="14R" if FoodDescription=="Quark"
	replace RecipeSubFoodGroupDesc="Other Cheese" if FoodDescription=="Quark"

*First calculate intakes at food level
egen totcheese=rowtotal(TotalGrams) if RecipeMainFoodGroupCode==14
egen totmilk=rowtotal(TotalGrams) if (RecipeSubFoodGroupCode=="10R" | RecipeSubFoodGroupCode=="11R" | RecipeSubFoodGroupCode=="60R" | RecipeSubFoodGroupCode=="12R" | RecipeSubFoodGroupCode=="13R")
egen totyogurt=rowtotal(TotalGrams) if RecipeSubFoodGroupCode=="15B"
egen totbutter=rowtotal(TotalGrams) if RecipeSubFoodGroupCode=="17R"
egen totcreamdesserts=rowtotal(TotalGrams) if (RecipeSubFoodGroupCode=="15C" | RecipeSubFoodGroupCode=="15D"| RecipeSubFoodGroupCode=="13B" | RecipeSubFoodGroupCode=="53R")
egen totdairy=rowtotal(totcheese totmilk totyogurt totbutter totcreamdesserts)

*Day level
bysort Cpseriala RecallNo: egen Day_Dairy_Indiv =sum(totdairy)
bysort Cpseriala RecallNo: egen Day_Milk_Indiv =sum(totmilk)
bysort Cpseriala RecallNo: egen Day_Cheese_Indiv =sum(totcheese)
bysort Cpseriala RecallNo: egen Day_Yogurt_Indiv =sum(totyogurt)
bysort Cpseriala RecallNo: egen Day_CreamDesserts_Indiv =sum(totcreamdesserts)
bysort Cpseriala RecallNo: egen Day_Butter_Indiv=sum(totbutter)

*Replace values of those without recalls to missing
foreach var of varlist Dairyg Milkg Milk_Skimmedg Milk_SemiSkimmedg Milk_Wholeg Cheeseg Cheese_Cheddarg Cheese_Cottageg Cheese_Otherg Cheese_Skimmedg Cheese_SemiSkimmedg Cheese_Wholeg Yogurtg Yogurt_Skimmedg Yogurt_SemiSkimmedg Yogurt_Wholeg Creamg Cream_SemiSkimmedg Cream_Wholeg Butterg Day_Dairy Day_Milk Day_Cheese Day_Yogurt Day_Cream Day_Butter Day_Milk_Skimmed Day_Milk_SemiSkimmed Day_Milk_Whole Day_Cheese_Cheddar Day_Cheese_Cottage Day_Cheese_Other Day_Cheese_Skimmed Day_Cheese_SemiSkimmed Day_Cheese_Whole Day_Yogurt_Skimmed Day_Yogurt_SemiSkimmed Day_Yogurt_Whole Day_Cream_SemiSkimmed Day_Cream_Whole totcheese totmilk totyogurt totbutter totcreamdesserts totdairy Day_Dairy_Indiv Day_Milk_Indiv Day_Cheese_Indiv Day_Yogurt_Indiv Day_CreamDesserts_Indiv Day_Butter_Indiv{
	replace `var' =. if RecallNo==. 
}


/**********************************************************************
 Calculate mean daily intakes of dairy with and without disaggregation
***********************************************************************/

*Set local macro
ds Day_* 
local dayvalues `r(varlist)'

*Loop through each daily value
foreach var of varlist `dayvalues' {
	bysort Cpseriala RecallNo: egen DayMax_`var' =max(`var') /*daily intake*/
    bysort Cpseriala: egen Wk_`var' = total(DayMax_`var') if n==1 /*total intake across all days*/
	bysort Cpseriala: egen WkMax_`var' = max(Wk_`var') /*filling in total intake across all days across all observations*/
	bysort Cpseriala: gen Avg_`var' = (WkMax_`var'/NumberOfRecalls) /*mean daily intake*/
	drop DayMax_`var' Wk_`var' WkMax_`var'
}


/*********************************************************************
Calculate disaggregated dairy intake from HIGH LEVEL FOOD CATEGORIES
*********************************************************************/
local nutrients "Dairyg" 
levelsof FoodCategoryCode, local(FoodCategoryCode) 

foreach var of varlist `nutrients' {
	foreach 1 of local FoodCategoryCode {
	bysort Cpseriala RecallNo: egen D_`var'_FC`1' = sum(`var') if FoodCategoryCode==`1' /*daily nutrient intake by food group*/
	bysort Cpseriala RecallNo: egen DMax_`var'_FC`1' = max(D_`var'_FC`1') /*filling in daily nutrient intake by food group across all observations*/
	bysort Cpseriala: egen Wk_`var'_FC`1' = total(DMax_`var'_FC`1') if n==1 /*total nutrient intake by food group across all days*/
	bysort Cpseriala: egen WkMax_`var'_FC`1' = max(Wk_`var'_FC`1') /*filling in total intake across all days across all observations*/
	bysort Cpseriala: gen Avg_`var'_FC`1'= (WkMax_`var'_FC`1'/NumberOfRecalls) /*mean daily nutrient intake by food group*/
	drop D_* DMax_* Wk_* WkMax*
	}
}	


/***********************************************************
Calculate disaggregated dairy intake from MAIN FOOD GROUPS
***********************************************************/
local nutrients "Dairyg" 
levelsof RecipeMainFoodGroupCode, local(MainFoodGroup) 

foreach var of varlist `nutrients' {
	foreach 1 of local MainFoodGroup {
	bysort Cpseriala RecallNo: egen D_`var'_`1' = sum(`var') if RecipeMainFoodGroupCode==`1' 
	bysort Cpseriala RecallNo: egen DMax_`var'_`1' = max(D_`var'_`1')
	bysort Cpseriala: egen Wk_`var'_`1' = total(DMax_`var'_`1') if n==1
	bysort Cpseriala: egen WkMax_`var'_`1' = max(Wk_`var'_`1')
	bysort Cpseriala: gen Avg_`var'_`1'= (WkMax_`var'_`1'/NumberOfRecalls)
	drop D_* DMax_* Wk_* WkMax*
	}
}	


/***********************************************************
Calculate disaggregated dairy intake from SUB FOOD GROUPS
***********************************************************/
local nutrients "Dairyg" 
levelsof RecipeSubFoodGroupCode, local(SubFoodGroup)

foreach var of varlist `nutrients' {
	foreach 1 of local SubFoodGroup {
	bysort Cpseriala RecallNo: egen D_`var'_`1' = sum(`var') if RecipeSubFoodGroupCode=="`1'"
	bysort Cpseriala RecallNo: egen DMax_`var'_`1' = max(D_`var'_`1')
	drop D_*
	bysort Cpseriala: egen Wk_`var'_`1' = total(DMax_`var'_`1') if n==1
	drop DMax_*
	bysort Cpseriala: egen WkMax_`var'_`1' = max(Wk_`var'_`1')
	drop Wk_*
	bysort Cpseriala: gen Avg_`var'_`1'= (WkMax_`var'_`1'/NumberOfRecalls)
	drop WkMax*
	}
}

/******************************************************************************
Create variables for proportion of dairy subtypes to disaggregated dairy intake
******************************************************************************/
ds Avg_Day_Milk Avg_Day_Cheese Avg_Day_Yogurt Avg_Day_Cream Avg_Day_Butter
local dailyaverage `r(varlist)'
foreach var of varlist `dailyaverage' {
    bysort Cpseriala: gen Prop_`var'=(`var'/Avg_Day_Dairy)*100
}


/***********************************************************************************
Create variables for proportion of dairy subtypes to non-disaggregated dairy intake
************************************************************************************/
ds Avg_Day_Milk_Indiv Avg_Day_Cheese_Indiv Avg_Day_Yogurt_Indiv Avg_Day_CreamDesserts_Indiv Avg_Day_Butter_Indiv
local dailyaverage `r(varlist)'
foreach var of varlist `dailyaverage' {
    bysort Cpseriala: gen Prop_`var'=(`var'/Avg_Day_Dairy_Indiv)*100
}


/***************************************************************************
 Create variables for food group contributions to disaggregated dairy intake
****************************************************************************/
ds Avg_Dairyg_*
local dailyaverage `r(varlist)'
foreach var of varlist `dailyaverage' {
    bysort Cpseriala: gen Prop_`var'=(`var'/Avg_Day_Dairy)*100 if Avg_Day_Dairy>0
	replace Prop_`var'=0 if Avg_Day_Dairy==0
}

*********************************
 *Create dairy consumer variable
*********************************
gen DairyConsumer=0
replace DairyConsumer=1 if Avg_Day_Dairy>0 & Avg_Day_Dairy!=.


*****************
*Label variables
*****************
do "$code\Dairy disag_label variables.do"


*********************************
*Drop  varibles no longer needed
*********************************
drop n Day_Dairy- Day_Butter_Indiv


*************
*Save dataset
*************

*Food level dataset
save "$data\SHeS 2021_foodlevel_dairydisag_$date.dta", replace

*Participant level dataset for analysis (drop duplicates and unecessary food level variables)
duplicates drop Cpseriala, force
drop SubDay-Butterg 

save "$data\SHeS 2021_participantlevel_dairydisag_$date.dta", replace


