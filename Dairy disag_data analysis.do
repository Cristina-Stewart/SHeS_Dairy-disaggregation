

*******************************************************************************************
*Manuscript: Disaggregation of dairy in composite dishes in the UK

*Data analysis file for estimating dairy consumption with and without dairy disaggregation
*******************************************************************************************


****************
*Clear settings
****************
clear all
clear matrix
macro drop _all
graph drop _all

*************************************************************
*Assign values using global macros for file location and date
*************************************************************

global location "K:\DrJaacksGroup\FSS - Dietary Monitoring\SHeS\SHeS 2021\Dairy disag" 
global data `"$location\Data"'
global output `"$location\Output"'
global date "20231512"

*Read in data
use "$data\SHeS 2021_participantlevel_dairydisag_$date.dta", clear
*Assign survey sample variables
svyset [pweight=SHeS_Intake24_wt_sc], psu(psu) strata(Strata)


**********************************************************************
*Compare estimates of dairy intake with and without disaggregation
**********************************************************************

*Check individual subtype varieties (e.g. skimmed/whole) add up to total intake of dairy subtypes - all okay
svy, subpop(intake24): mean Avg_Day_Milk Avg_Day_Milk_Skimmed Avg_Day_Milk_SemiSkimmed Avg_Day_Milk_Whole 
svy, subpop(intake24): mean Avg_Day_Cheese Avg_Day_Cheese_Skimmed Avg_Day_Cheese_SemiSkimmed Avg_Day_Cheese_Whole
svy, subpop(intake24): mean Avg_Day_Yogurt Avg_Day_Yogurt_Skimmed Avg_Day_Yogurt_SemiSkimmed Avg_Day_Yogurt_Whole
svy, subpop(intake24): mean Avg_Day_Cream Avg_Day_Cream_SemiSkimmed Avg_Day_Cream_Whole
svy, subpop(intake24): mean Avg_Day_Cheese Avg_Day_Cheese_Cheddar Avg_Day_Cheese_Cottage Avg_Day_Cheese_Other


*With disaggregation
	*Absolute intakes
	svy, subpop(intake24): mean Avg_Day_Dairy Avg_Day_Milk Avg_Day_Cheese Avg_Day_Yogurt Avg_Day_Cream Avg_Day_Butter
	estat sd
	
	*Percent contributions of dairy subtypes
	svy, subpop(DairyConsumer): mean Prop_Avg_Day_Milk Prop_Avg_Day_Cheese Prop_Avg_Day_Yogurt Prop_Avg_Day_Cream Prop_Avg_Day_Butter
	estat sd

*Without disaggregation
	*Absolute intakes -
	svy, subpop(intake24): mean Avg_Day_Dairy_Indiv Avg_Day_Milk_Indiv Avg_Day_Cheese_Indiv Avg_Day_Yogurt_Indiv Avg_Day_CreamDesserts_Indiv Avg_Day_Butter_Indiv
	estat sd

	*Percent contributions of dairy subtypes
	svy, subpop(DairyConsumer): mean Prop_Avg_Day_Milk_Indiv Prop_Avg_Day_Cheese_Indiv Prop_Avg_Day_Yogurt_Indiv Prop_Avg_Day_CreamDesserts_Indiv Prop_Avg_Day_Butter_Indiv
	estat sd


***********************************************
*Food category % contributions to dairy intake
***********************************************	
	matrix avgdairy = J(20, 4, .)
	local r=2

quietly foreach var of varlist Prop_Avg_Dairyg_FC1- Prop_Avg_Dairyg_FC18 {
			
		*overall		
		sum `var' if DairyConsumer==1
		matrix avgdairy[`r',1]=r(N)
		
		svy, subpop(DairyConsumer): mean `var'
		estat sd
		matrix avgdairy[`r',2]=r(mean)
		matrix avgdairy[`r',3]=r(sd)

		local r=`r'+1
}	

*Export to Excel
	putexcel set "$output\SHeS Dairy Intakes_disag vs non disag.xlsx", sheet("Food category % cont") modify
	putexcel B2=matrix(avgdairy)
	putexcel B1="N" 
	putexcel C1="Mean"
	putexcel D1="SD"


***********************************************
*Main food group % contributions to dairy intake
***********************************************
	matrix avgdairy = J(70, 4, .)
	local r=2

	quietly foreach var of varlist Prop_Avg_Dairyg_1- Prop_Avg_Dairyg_66 {
			
		sum `var' if DairyConsumer==1
		matrix avgdairy[`r',1]=r(N)
		
		svy, subpop(DairyConsumer): mean `var'
		estat sd
		matrix avgdairy[`r',2]=r(mean)
		matrix avgdairy[`r',3]=r(sd)

		local r=`r'+1
}	

*Export to Excel
	putexcel set "$output\SHeS Dairy Intakes_disag vs non disag.xlsx", sheet("Main food group % cont") modify
	putexcel B2=matrix(avgdairy)
	putexcel B1="N" 
	putexcel C1="Mean"
	putexcel D1="SD"

	
***********************************************
*Sub food group % contributions to dairy intake
***********************************************
	matrix avgdairy = J(150, 4, .)
	local r=2

	quietly foreach var of varlist Prop_Avg_Dairyg_10R- Prop_Avg_Dairyg_9H {
			
		*overall		
		sum `var' if DairyConsumer==1
		matrix avgdairy[`r',1]=r(N)
		
		svy, subpop(DairyConsumer): mean `var'
		estat sd
		matrix avgdairy[`r',2]=r(mean)
		matrix avgdairy[`r',3]=r(sd)		

		local r=`r'+1
}	

*Export to Excel
	putexcel set "$output\SHeS Dairy Intakes_disag vs non disag.xlsx", sheet("Sub food group % cont") modify
	putexcel B2=matrix(avgdairy)
	putexcel B1="N" 
	putexcel C1="Mean"
	putexcel D1="SD"
