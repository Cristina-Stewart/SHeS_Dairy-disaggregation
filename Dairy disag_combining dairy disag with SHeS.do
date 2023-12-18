
******************************************************************
*Manuscript: Disaggregation of dairy in composite dishes in the UK

*Do file for merging dairy disag file with SHeS 2021 diet data
*****************************************************************


****************
*Clear settings
****************
clear matrix
macro drop _all
graph drop _all


*************************************************************
*Assign values using global macros for file location and date
*************************************************************
global location "K:\DrJaacksGroup\FSS - Dietary Monitoring\SHeS\SHeS 2021" 
global data `"$location\Data"'
global date "20231512"

*Intake24 diet data (multiple obeservations per participant, each observation = food item reported)
global diet `"$data\shes21_intake24_food-level_dietary_data_eul"'
*Set maximum number of variables to 15,000
set maxvar 15000


***********************
*Import SHeS diet data
**********************
use "$diet", clear

	*Two items have two different food numbers - ensure the food numbers match dairy disag file
	replace FoodNumber=10159 if FoodDescription=="Oat milk" & FoodNumber==10966 
	replace FoodNumber=821 if FoodDescription=="Savoury pastry (e.g. cheese pastry)" & FoodNumber==356
	
	*Explore 3 food codes which correspond to two FoodDescription_SHeS items in dairy disag file	
		ta FoodDescription if FoodNumber==5607 /*5 items*/
		ta FoodDescription if FoodNumber==2711 /*1 item*/
		ta FoodDescription if FoodNumber==1921 /*3 items*/

*Save as separate diet data
save "$data\diet_dairydisag_$date.dta", replace


******************************
*Import dairy disag excel file
******************************
clear all
import excel using "K:\DrJaacksGroup\FSS - Dietary Monitoring\SHeS\SHeS 2021\Data\NDB_SHeS_Disag_Dairy_16122023", firstrow	

*Explore these 3 food codes that match two SheS items further

	**5607**

		/*Correspondents to:
			2 items in dairy disag file: butternut squash soup and celery soup
			4 items in SHeS: celery (n=8), parsnip (n=10), pumpkin (n=2) and vegetable soup (n=252)

			Butternut is the only FSA soup with dairy
			
			Decision:
				Drop butternut soup from disag file to merge dataset with SHeS (ensuring no duplicate food codes)
				Manually add back in butternut dairy disag info (3.99g dairy, 3.99g butter) to SHeS*/

		*Drop butternut squash soup
		drop if FoodNumber==5607 & FoodDescription_FSA=="BUTTERNUT SQUASH SOUP"

	**2711**

		/* Corresponds to:
		2 items in dairy disag file: gooseburry and tangarines
		1 item in SHeS: gooseburry
		
		In SHeS, tangarines have the food code 2157
		
		Decision:
			Change the food code of tangarines in disag file to match SHeS*/
	
	*Change the food code for tangerines
	replace FoodNumber=2157 if FoodNumber==2711 & FoodDescription_SHeS=="Tangerines / mandarins / clementines/ satsumas"
	drop if FoodNumber==2157 & FoodDescription_FSA=="" /*Both Gooseberries and tangerines now have this food code - no dairy disag in either so dropping one*/
	
	
	**1921**
		/*Corresponds to:
			2 items in dairy disag file: carrot & swede mash, swede
			2 items in SHeS: carrot & swede mash (n=8), swede (n=7), swede mashed (n=3), 
			
			Decision:
				Drop swede (boiled) in disag file to merge dataset with SHeS (ensuring no duplicate food codes)
				Manually remove dairy from unmashed swede in SHeS*/
		
		*Drop boiled swete
		drop if FoodNumber==1921 & FoodDescription_FSA=="SWEDE BOILED"

	
***************************
*Check for duplicate items*
***************************
	
*Check for duplicates with same dairy disag values
bysort FoodNumber Dairy_Ingredients Dairy Milk Cheese Yogurt Cream Butter: gen n=_n
ta n

	*Dropping duplicate items
	drop if n>1 /*511*/
	drop n
		
*Check for any remaining duplicate FoodNumbers
bysort FoodNumber: gen n=_n
ta n /*1 duplicate*/
ta FoodNumber if n==2 /*FoodNumber 303 - chealsea bun/belgian bun*/

	*Dropping code for chelsea bun as this isn't consumed in SHeS
	drop if FoodNumber==303 & FoodDescription_NDB=="Chelsea bun"
	drop n

***************************************
*Merge dairy disag file with diet data*
***************************************

*Drop variables that are already in SHeS so they don't overwrite SHeS variables
drop FoodDescription RecipeSubFoodGroupCode

*Drop other food description variables not needed
drop FoodDescription_NDB FoodDescription_SHeS FoodDescription_FSA

*Save as new disag file
save "$data\NDB_SHeS_Disag_Dairy_$date.dta", replace

	*Merge disag dataset with Intake24 data
	sort FoodNumber
	merge 1:m FoodNumber using "$data\diet_dairydisag_$date.dta"
	
		*Drop items not in SHeS
		drop if _merge==1 /*466 items*/
		
		*Check items in SHeS that didn't match
		ta FoodDescription if _merge==2 /*198 all supplements - okay*/
		drop _merge
	
	*Check dairy is fully disaggregated into dairy subtypes
	gen dairytotal=Milk + Cheese + Yogurt + Cream + Butter
	gen dairydiff=Dairy-dairytotal
	ta dairydiff  /*All fine*/
		
	drop dairytotal dairydiff


*************************************
*Manually ammend necessary foodcodes*
*************************************

*Add dairy back into butternut squash soup
replace Dairy=3.99 if FoodNumber==5607 & FoodDescription=="Butternut squash soup"
replace Butter=3.99 if FoodNumber==5607 & FoodDescription=="Butternut squash soup"

*Remove dairy from unmashed swede
replace Dairy=0 if FoodNumber==1921 & FoodDescription=="Swede"
replace Butter=0 if FoodNumber==1921 & FoodDescription=="Swede"

	
	
/********************************
Estimate g of dairy in each item
*********************************/
foreach var of varlist Dairy-Butter {
	gen `var'g=(`var'/100)*TotalGrams	
}

*Label variables
foreach var of varlist Dairyg-Butterg {
		label variable `var' "g per portion"
	}

*Save as new diet dataset inclusive of dairy disag data
save "$data\diet_dairydisag.dta_$date", replace




