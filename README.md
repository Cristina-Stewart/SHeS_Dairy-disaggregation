# SHeS_Dairy-disaggregation
Estimating dairy consumption among adults in the 2021 Scottish Health Survey with and without dairy disaggregation of composite dishes

## Data Management Files

### Dairy disag_combining dairy disag with SHeS.do
This do file combines our dairy disaggregation dataset (containing the quantity of dairy ingredients (g) per 100g of recipes) with dietary data from SHeS. Here, we calculate the total amount (g) of dairy (total and subtypes - milk, cheese, yogurt, cream and butter) for each food item based on the portion consumed. 

Files needed to run this do-file:
#### Data files
Two data files are needed to run this do-file
- NDB_SHeS_Disag_Dairy_05122023 - dairy disaggregation dataset containing the quantity of dairy ingredients (g) per 100g of recipes. Provided within this GitHub project.
- shes21_intake24_food-level_dietary_data_eul - Intake 24 diet data. There are multiple observations per participant, each observation corresponds to a food item reported. This can be downloaded from the UK Data Archive.

#### Output
- diet_dairydisag.dta_20231512.dta - this is intake24 diet data from SHeS containing disaggregated quantities of dairy per portion of food consumed. This dataset has multiple observations for each participant, corresponding to each food item reported.

### Dairy disag_data management.do
This stata do-file creates a food-level and participant-level dataset for SHeS. It combines survey weights with dietary data (inclusive of dairy disaggregation estimates). At the participant-level mean daily intakes of dairy (total and dairy subtype) are calculated both with and without dairy disaggregation. Mean per cent contributions of dairy subtype (i.e. milk, cheese, yogurt, cream and butter) to dairy intake are calculated, both with and without dairy disaggregation.

Mean per cent contributions of all food categories and food groups to disaggregated dairy are calculated. 

Files needed to run this do-file:
#### Data files
Two data files are needed to run this do-file
- diet_dairydisag.dta_20231512 - Intake 24 diet data from SHeS containing disaggregated estimates of dairy, created from the previous do file.
- shes21i_eul - participant demographic survey data containing survey weights. 

#### Do files
- Dairy disag_label variables - labels all the variables in the data management do-file.

#### Output
- SHeS 2021_foodlevel_dairydisag_20231512.dta - this dataset has multiple observations for each participant, corresponding to each food item reported
- SHeS 2021_participantlevel_dairydisag_20231512.dta - this dataset has one observation for each participant

## Data Analysis Files
### Dairy disag_data analysis.do
This do-file contains descriptive analyses of dairy intake both with and without disaggregation for the manuscript "Disaggregation of dairy in composite dishes in the UK". This do-file also contains code which exports the following data into Excel tables:
1) Per cent contributions of high level food categories to dairy intake
2) Per cent contributions of main food groups to dairy intake
3) Per cent contributions of sub food groups to dairy intake
