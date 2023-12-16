

# Project: FSS Dairy Disaggregation 
# Programmer: Lindsay Jaacks
# Date created: 6 October 2023 



# Prepare R Environment ####

# Clear R Environment
rm(list=ls())

# Check if the packages that we need are installed
want = c("tidyverse", "haven", "writexl","readxl","labelled","gtsummary","flextable","janitor","magrittr","xtable")
have = want %in% rownames(installed.packages())

# Install any missing packages
if ( any(!have) ) { install.packages( want[!have] ) }

# Load the packages
junk <- lapply(want, library, character.only = T)

# Remove the temporary objects we created
rm(have, want, junk)

# Assign directory
setwd("UPDATE with file directory")



# Import and prepare data ####

  ## DISH NDB ####
  ndb <- read.csv("Data/NDB_intake24-nutrient-mapping-UK_V2_2022-18-10-2022.csv")

    # Re-name variables to match SHeS 
    ndb <- ndb %>% select(Local.description,FCT.record.ID,Sub.group.code) %>% 
                   mutate(FoodDescription=Local.description,
                          FoodNumber=FCT.record.ID,
                          RecipeSubFoodGroupCode=Sub.group.code) %>%
                   select(FoodDescription,FoodNumber,RecipeSubFoodGroupCode)
  
    n_distinct(ndb$FoodDescription) # n=3,073 unique food descriptions
    n_distinct(ndb$FoodNumber)      # n=2,417 unique food codes -> several distinct food descriptions have the same food codes
    
    # Change FoodNumber for items with mismatch between SHeS, NDB and FSA database 
    # See Excel "NDB_vs_SHeS_vs_FSA_Mismatches" for details
    ndb <- ndb %>% mutate(FoodNumber = case_when(FoodDescription=="Sandwich thins, wholemeal (brown)" ~ 11282,
                                                 FoodDescription=="Fruit fritters" ~ 577,
                                                 FoodDescription=="Kumquats" ~ 2157,
                                                 FoodDescription=="Butternut squash soup" ~ 3955,
                                                 FoodDescription=="Challah bread" ~ 9129,
                                                 FoodDescription!="Challah bread" ~ FoodNumber))
    
    # Drop duplicate food descriptions, keep only first food code when sorted in ascending order
    ndb <- ndb[order(ndb$FoodNumber, ndb$FoodDescription, na.last=TRUE), ]  # na.last=TRUE puts any missing values at the end when sorting
    ndb.nodup <- ndb[!duplicated(ndb$FoodNumber), ]
  
    
  ## SHeS 2021 food list ####
  shes <- read_excel("Data/SHeS food list.xlsx")
  shes <- shes %>% select(FoodDescription,FoodNumber,RecipeSubFoodGroupCode)
  
    n_distinct(shes$FoodDescription) # n=2,395 unique food descriptions
    n_distinct(shes$FoodNumber)      # n=1,884 unique food codes -> several distinct food descriptions have the same food codes
    
    # Change FoodNumber for items with mismatch between SHeS, NDB and FSA database 
    # See Excel "NDB_vs_SHeS_vs_FSA_Mismatches" for details
    shes <- shes %>% mutate(FoodNumber = case_when(FoodDescription=="Belgian bun" ~ 8125,
                                                   FoodDescription=="Carrot and swede mash" ~ 11602,
                                                   FoodDescription=="Butternut squash soup" ~ 3955,
                                                   FoodDescription!="Butternut squash soup" ~ FoodNumber))
    
    # Drop duplicate food descriptions, keep only first food code when sorted in ascending order
    shes <- shes[order(shes$FoodNumber, shes$FoodDescription, na.last=TRUE), ]  
    shes.nodup <- shes[!duplicated(shes$FoodNumber), ]
  
    
  ## FSA recipe database ####
  fsa <- read_excel("Data/Recipes_Yr15_Hierarchy_2023.09.21.xlsx", guess_max = 100000)
  fsa <- fsa %>% mutate(FoodNumber=L0FoodCode)
  
    # Change FoodNumber for items with mismatch between SHeS, NDB and FSA database 
    # See Excel "NDB_vs_SHeS_vs_FSA_Mismatches" for details
    fsa <- fsa %>% mutate(L0FoodCode = case_when(L0FoodCode==6919 ~ 5592,
                                                 L0FoodName=="LAMB CURRY ( NO POTATOES ) WITH ONIONS & CURRY PAS" ~ 6919,
                                                 L0FoodCode!=6919 & L0FoodCode!=5592 ~ L0FoodCode))
    
    # Drop duplicate food descriptions (these are food items with multiple ingredients)
    fsa.nodup <- fsa[!duplicated(fsa$L0FoodCode), ]
    n_distinct(fsa.nodup$L0FoodCode) # confirm no duplicate food numbers

    # Re-name variables to match SHeS 
    fsa.nodup <- fsa.nodup %>% select(L0FoodCode,L0FoodName) %>% 
                               mutate(FoodDescription=L0FoodName,
                                      FoodNumber=L0FoodCode) %>%
                               select(FoodDescription,FoodNumber)
    
    ## Keep only recipes that match NDB or SHeS ####
    join <- full_join(ndb.nodup,shes.nodup,by="FoodNumber",multiple="all")
    shes.nodup %>% filter(!FoodNumber %in% ndb.nodup$FoodNumber) # n=17 food codes in SHeS that are not in NDB
    join <- join %>% mutate(FoodDescription_NDB=FoodDescription.x,
                            FoodDescription_SHeS=FoodDescription.y) %>%
                    select(FoodNumber,FoodDescription_NDB,FoodDescription_SHeS)
    
      # Change FoodNumber for items missing in FSA database to nearest match 
      # Chocolate covered fingers (e.g. Cadbury milk chocolate fingers) 11416 -> Twix (chocolate & caramel biscuit fingers) 2276
      # Mcvities digestive 11417 -> DIGESTIVE PLAIN 259
      # Mcvities chocolate digestive 11418 -> Chocolate digestive, biscuit 260
      # Oreos, including supermarket brands 11419 -> Biscuits filled with cream (e.g. bourbons, Fox's crunch creams) 268
      # Mcvities rich tea 11420 -> Semi-sweet biscuit (e.g. Rich Tea, Morning Coffee) 269
      # Toblerone 11422 -> Milk chocolate bar (e.g. Dairy Milk) 2254
      join <- join %>% mutate(FoodNumber = case_when(FoodNumber==11416 ~ 2276,
                                                     FoodNumber==11417 ~ 259,
                                                     FoodNumber==11418 ~ 260,
                                                     FoodNumber==11419 ~ 268,
                                                     FoodNumber==11420 ~ 269,
                                                     FoodNumber==11422 ~ 2254,
                                                     FoodNumber!=11416 & FoodNumber!=11417 & FoodNumber!=11418 & FoodNumber!=11419 & FoodNumber!=11420 & FoodNumber!=11422 & FoodNumber!=11784 ~ FoodNumber))
    
      # Change FoodNumber for items with mismatch between SHeS, NDB and FSA database 
      # See Excel "NDB_vs_SHeS_vs_FSA_Mismatches" for details
      join <- join %>% mutate(FoodNumber = case_when(FoodNumber==262 ~ 330,
                                                     FoodNumber==574 ~ 6964,
                                                     FoodNumber==2445 ~ 2438,
                                                     FoodNumber==3797 ~ 10284,
                                                     FoodNumber==5201 ~ 8606,
                                                     FoodNumber==8136 ~ 10187,
                                                     FoodNumber==8445 ~ 7902,
                                                     FoodNumber!=11784 ~ FoodNumber))
    
    join <- full_join(join,fsa.nodup,by="FoodNumber",multiple="all") %>% 
            mutate(FoodDescription_FSA=FoodDescription) %>% 
            select(FoodNumber,FoodDescription_NDB,FoodDescription_SHeS,FoodDescription_FSA) %>% 
            filter((!is.na(FoodDescription_NDB)) |(!is.na(FoodDescription_SHeS)))
    #write_xlsx(join,paste0("Data/","NDB_vs_SHeS_vs_FSA_",format(Sys.time(),"%d%m%Y"),".xlsx"))
    
    # Identify and drop duplicated food items (from changes to food codes above)
    dupl <- join[duplicated(join$FoodNumber),]
    join <- join[!duplicated(join$FoodNumber), ]
    
    disag <- join %>% select(FoodNumber)
    disag <- left_join(disag,fsa,by="FoodNumber")
    n_distinct(disag$FoodNumber) # n=2,427 foods for disaggregation


# Identify dairy ingredients ####

    ## Create smallest ingredient in hierarchy variables ####
    disag <- disag %>% 
      mutate(IFoodCode = case_when(!is.na(L6FoodCode) ~ L6FoodCode,
                                   !is.na(L5FoodCode) ~ L5FoodCode,
                                   !is.na(L4FoodCode) ~ L4FoodCode,
                                   !is.na(L3FoodCode) ~ L3FoodCode,
                                   !is.na(L2FoodCode) ~ L2FoodCode,
                                   !is.na(L1FoodCode) ~ L1FoodCode,
                                   !is.na(L0FoodCode) ~ L0FoodCode)) %>% 
      mutate(IFoodName = case_when(!is.na(L6FoodName) ~ L6FoodName,
                                   !is.na(L5FoodName) ~ L5FoodName,
                                   !is.na(L4FoodName) ~ L4FoodName,
                                   !is.na(L3FoodName) ~ L3FoodName,
                                   !is.na(L2FoodName) ~ L2FoodName,
                                   !is.na(L1FoodName) ~ L1FoodName,
                                   !is.na(L0FoodName) ~ L0FoodName)) %>% 
      mutate(IFoodCategory = case_when(!is.na(L6FoodCategory) ~ L6FoodCategory,
                                       !is.na(L5FoodCategory) ~ L5FoodCategory,
                                       !is.na(L4FoodCategory) ~ L4FoodCategory,
                                       !is.na(L3FoodCategory) ~ L3FoodCategory,
                                       !is.na(L2FoodCategory) ~ L2FoodCategory,
                                       !is.na(L1FoodCategory) ~ L1FoodCategory,
                                       !is.na(L0FoodCategory) ~ L0FoodCategory)) %>% 
      mutate(IBaseValue = case_when(!is.na(L6BaseValue) ~ L6BaseValue,
                                    !is.na(L5BaseValue) ~ L5BaseValue,
                                    !is.na(L4BaseValue) ~ L4BaseValue,
                                    !is.na(L3BaseValue) ~ L3BaseValue,
                                    !is.na(L2BaseValue) ~ L2BaseValue,
                                    !is.na(L1BaseValue) ~ L1BaseValue,
                                    !is.na(L0BaseValue) ~ L0BaseValue)) %>% 
      mutate(IUnits = case_when(!is.na(L6Units) ~ L6Units,
                                !is.na(L5Units) ~ L5Units,
                                !is.na(L4Units) ~ L4Units,
                                !is.na(L3Units) ~ L3Units,
                                !is.na(L2Units) ~ L2Units,
                                !is.na(L1Units) ~ L1Units,
                                !is.na(L0Units) ~ L0Units)) %>% 
      mutate(IComponentAmount = case_when(!is.na(L6ComponentAmount) ~ L6ComponentAmount,
                                          !is.na(L5ComponentAmount) ~ L5ComponentAmount,
                                          !is.na(L4ComponentAmount) ~ L4ComponentAmount,
                                          !is.na(L3ComponentAmount) ~ L3ComponentAmount,
                                          !is.na(L2ComponentAmount) ~ L2ComponentAmount,
                                          !is.na(L1ComponentAmount) ~ L1ComponentAmount)) %>% 
      mutate(IComponentPercentage = case_when(!is.na(L6ComponentPercentage) ~ L6ComponentPercentage,
                                              !is.na(L5ComponentPercentage) ~ L5ComponentPercentage,
                                              !is.na(L4ComponentPercentage) ~ L4ComponentPercentage,
                                              !is.na(L3ComponentPercentage) ~ L3ComponentPercentage,
                                              !is.na(L2ComponentPercentage) ~ L2ComponentPercentage,
                                              !is.na(L1ComponentPercentage) ~ L1ComponentPercentage)) %>% 
      mutate(IComponentPercentOfL1 = case_when(!is.na(L6ComponentPercentOfL1) ~ L6ComponentPercentOfL1,
                                               !is.na(L5ComponentPercentOfL1) ~ L5ComponentPercentOfL1,
                                               !is.na(L4ComponentPercentOfL1) ~ L4ComponentPercentOfL1,
                                               !is.na(L3ComponentPercentOfL1) ~ L3ComponentPercentOfL1,
                                               !is.na(L2ComponentPercentOfL1) ~ L2ComponentPercentOfL1)) 
    
    # Drop duplicate ingredients
    fsa.ingredients <- disag[order(disag$IFoodCode, disag$IFoodName, na.last=TRUE), ]  
    fsa.ingredients <- fsa.ingredients[!duplicated(fsa.ingredients$IFoodCode), ] %>% 
                      select(IFoodCode,IFoodName) %>% 
                      filter(!is.na(IFoodCode))

    ## In Excel: n=1,097 ingredients to classify as dairy or not ####
    # write_xlsx(fsa.ingredients,paste0("Data/","FSA_Ingredients_",format(Sys.time(),"%d%m%Y"),".xlsx"))
  
    # Merge dairy variables created in Excel manually with recipe database for NDB and SHeS
    fsa.ingredients <- read_excel("Data/FSA_Ingredients_26102023_Final Dairy.xlsx")
    fsa.ingredients <- fsa.ingredients %>% 
                        select(IFoodCode,Dairy,Milk,Milk_Skimmed,Milk_SemiSkimmed,Milk_Whole,Cheese,Cheese_Cheddar,Cheese_Cottage,Cheese_Other,
                               Cheese_Skimmed,Cheese_SemiSkimmed,Cheese_Whole,Yogurt,Yogurt_Skimmed,Yogurt_SemiSkimmed,Yogurt_Whole,Cream,
                               Cream_SemiSkimmed,Cream_Whole,Butter)  
    disag <- left_join(disag,fsa.ingredients,by="IFoodCode") 

  
# Count number of dairy ingredients in each recipe ####
dairy.ingredients <- disag %>%
                      group_by(L0FoodCode) %>%
                      summarise(Dairy_Ingredients = sum(Dairy)) %>%
                      ungroup() %>%
                      mutate(FoodNumber=L0FoodCode) %>%
                      select(FoodNumber,Dairy_Ingredients)
join <- left_join(join,dairy.ingredients,by="FoodNumber")


# Calculate amount of dairy in each ingredient ####

  ## Confirm units are all grams and base values are all 100 ####
  #table(disag$IUnits) # Confirmed all other units (capsule,drop,tablet,teaspoon) are for supplements/vitamins
  #table(disag$IBaseValue) # Confirmed all other base values (1,5) are for supplements/vitamins

  
  ## Determine amount (grams) ####
  
  # If Dairy==0 then it is not a dairy ingredient and the value should be 0g
  # If L0FoodCategory=="F" then it is 100% that ingredient, i.e. no recipe required (example: yogurt) and the value should be 100g (because the base is 100g)
  # Otherwise, for dairy ingredients, the amount is the percent of L1 (because L1 is 100g, percent of ingredient=amount of ingredient in grams)
    
disag <- disag %>% 
  mutate(Dairy_g = case_when(Dairy==0 ~ 0,
                            (L0FoodCategory=="F" & Dairy==1) ~ 100,
                            (!is.na(L6FoodName) & Dairy==1) ~ L6ComponentPercentOfL1,
                            (!is.na(L5FoodName) & Dairy==1) ~ L5ComponentPercentOfL1,
                            (!is.na(L4FoodName) & Dairy==1) ~ L4ComponentPercentOfL1,
                            (!is.na(L3FoodName) & Dairy==1) ~ L3ComponentPercentOfL1,
                            (!is.na(L2FoodName) & Dairy==1) ~ L2ComponentPercentOfL1,
                            (!is.na(L1FoodName) & Dairy==1) ~ L1ComponentPercentage)) %>%
  mutate(Milk_g = case_when(is.na(Milk) ~ 0,
                            (L0FoodCategory=="F"& Milk==1) ~ 100,
                            (!is.na(L6FoodName) & Milk==1) ~ L6ComponentPercentOfL1,
                            (!is.na(L5FoodName) & Milk==1) ~ L5ComponentPercentOfL1,
                            (!is.na(L4FoodName) & Milk==1) ~ L4ComponentPercentOfL1,
                            (!is.na(L3FoodName) & Milk==1) ~ L3ComponentPercentOfL1,
                            (!is.na(L2FoodName) & Milk==1) ~ L2ComponentPercentOfL1,
                            (!is.na(L1FoodName) & Milk==1) ~ L1ComponentPercentage)) %>%
  mutate(Milk_Skimmed_g = case_when(is.na(Milk_Skimmed) ~ 0,
                            (L0FoodCategory=="F" & Milk_Skimmed==1) ~ 100,
                            (!is.na(L6FoodName) & Milk_Skimmed==1) ~ L6ComponentPercentOfL1,
                            (!is.na(L5FoodName) & Milk_Skimmed==1) ~ L5ComponentPercentOfL1,
                            (!is.na(L4FoodName) & Milk_Skimmed==1) ~ L4ComponentPercentOfL1,
                            (!is.na(L3FoodName) & Milk_Skimmed==1) ~ L3ComponentPercentOfL1,
                            (!is.na(L2FoodName) & Milk_Skimmed==1) ~ L2ComponentPercentOfL1,
                            (!is.na(L1FoodName) & Milk_Skimmed==1) ~ L1ComponentPercentage)) %>%
  mutate(Milk_SemiSkimmed_g = case_when(is.na(Milk_SemiSkimmed) ~ 0,
                            (L0FoodCategory=="F" & Milk_SemiSkimmed==1) ~ 100,
                            (!is.na(L6FoodName) & Milk_SemiSkimmed==1) ~ L6ComponentPercentOfL1,
                            (!is.na(L5FoodName) & Milk_SemiSkimmed==1) ~ L5ComponentPercentOfL1,
                            (!is.na(L4FoodName) & Milk_SemiSkimmed==1) ~ L4ComponentPercentOfL1,
                            (!is.na(L3FoodName) & Milk_SemiSkimmed==1) ~ L3ComponentPercentOfL1,
                            (!is.na(L2FoodName) & Milk_SemiSkimmed==1) ~ L2ComponentPercentOfL1,
                            (!is.na(L1FoodName) & Milk_SemiSkimmed==1) ~ L1ComponentPercentage)) %>%
  mutate(Milk_Whole_g = case_when(is.na(Milk_Whole) ~ 0,
                            (L0FoodCategory=="F" & Milk_Whole==1) ~ 100,
                            (!is.na(L6FoodName) & Milk_Whole==1) ~ L6ComponentPercentOfL1,
                            (!is.na(L5FoodName) & Milk_Whole==1) ~ L5ComponentPercentOfL1,
                            (!is.na(L4FoodName) & Milk_Whole==1) ~ L4ComponentPercentOfL1,
                            (!is.na(L3FoodName) & Milk_Whole==1) ~ L3ComponentPercentOfL1,
                            (!is.na(L2FoodName) & Milk_Whole==1) ~ L2ComponentPercentOfL1,
                            (!is.na(L1FoodName) & Milk_Whole==1) ~ L1ComponentPercentage)) %>% 
  mutate(Cheese_g = case_when(is.na(Cheese) ~ 0,
                            (L0FoodCategory=="F" & Cheese==1) ~ 100,
                            (!is.na(L6FoodName) & Cheese==1) ~ L6ComponentPercentOfL1,
                            (!is.na(L5FoodName) & Cheese==1) ~ L5ComponentPercentOfL1,
                            (!is.na(L4FoodName) & Cheese==1) ~ L4ComponentPercentOfL1,
                            (!is.na(L3FoodName) & Cheese==1) ~ L3ComponentPercentOfL1,
                            (!is.na(L2FoodName) & Cheese==1) ~ L2ComponentPercentOfL1,
                            (!is.na(L1FoodName) & Cheese==1) ~ L1ComponentPercentage)) %>%      
  mutate(Cheese_Cheddar_g = case_when(is.na(Cheese_Cheddar) ~ 0,
                            (L0FoodCategory=="F" & Cheese_Cheddar==1) ~ 100,
                            (!is.na(L6FoodName) & Cheese_Cheddar==1) ~ L6ComponentPercentOfL1,
                            (!is.na(L5FoodName) & Cheese_Cheddar==1) ~ L5ComponentPercentOfL1,
                            (!is.na(L4FoodName) & Cheese_Cheddar==1) ~ L4ComponentPercentOfL1,
                            (!is.na(L3FoodName) & Cheese_Cheddar==1) ~ L3ComponentPercentOfL1,
                            (!is.na(L2FoodName) & Cheese_Cheddar==1) ~ L2ComponentPercentOfL1,
                            (!is.na(L1FoodName) & Cheese_Cheddar==1) ~ L1ComponentPercentage)) %>%      
  mutate(Cheese_Cottage_g = case_when(is.na(Cheese_Cottage) ~ 0,
                            (L0FoodCategory=="F" & Cheese_Cottage==1) ~ 100,
                            (!is.na(L6FoodName) & Cheese_Cottage==1) ~ L6ComponentPercentOfL1,
                            (!is.na(L5FoodName) & Cheese_Cottage==1) ~ L5ComponentPercentOfL1,
                            (!is.na(L4FoodName) & Cheese_Cottage==1) ~ L4ComponentPercentOfL1,
                            (!is.na(L3FoodName) & Cheese_Cottage==1) ~ L3ComponentPercentOfL1,
                            (!is.na(L2FoodName) & Cheese_Cottage==1) ~ L2ComponentPercentOfL1,
                            (!is.na(L1FoodName) & Cheese_Cottage==1) ~ L1ComponentPercentage)) %>%          
  mutate(Cheese_Other_g = case_when(is.na(Cheese_Other) ~ 0,
                            (L0FoodCategory=="F" & Cheese_Other==1) ~ 100,
                            (!is.na(L6FoodName) & Cheese_Other==1) ~ L6ComponentPercentOfL1,
                            (!is.na(L5FoodName) & Cheese_Other==1) ~ L5ComponentPercentOfL1,
                            (!is.na(L4FoodName) & Cheese_Other==1) ~ L4ComponentPercentOfL1,
                            (!is.na(L3FoodName) & Cheese_Other==1) ~ L3ComponentPercentOfL1,
                            (!is.na(L2FoodName) & Cheese_Other==1) ~ L2ComponentPercentOfL1,
                            (!is.na(L1FoodName) & Cheese_Other==1) ~ L1ComponentPercentage)) %>%        
  mutate(Cheese_Skimmed_g = case_when(is.na(Cheese_Skimmed) ~ 0,
                            (L0FoodCategory=="F" & Cheese_Skimmed==1) ~ 100,
                            (!is.na(L6FoodName) & Cheese_Skimmed==1) ~ L6ComponentPercentOfL1,
                            (!is.na(L5FoodName) & Cheese_Skimmed==1) ~ L5ComponentPercentOfL1,
                            (!is.na(L4FoodName) & Cheese_Skimmed==1) ~ L4ComponentPercentOfL1,
                            (!is.na(L3FoodName) & Cheese_Skimmed==1) ~ L3ComponentPercentOfL1,
                            (!is.na(L2FoodName) & Cheese_Skimmed==1) ~ L2ComponentPercentOfL1,
                            (!is.na(L1FoodName) & Cheese_Skimmed==1) ~ L1ComponentPercentage)) %>%   
  mutate(Cheese_SemiSkimmed_g = case_when(is.na(Cheese_SemiSkimmed) ~ 0,
                            (L0FoodCategory=="F" & Cheese_SemiSkimmed==1) ~ 100,
                            (!is.na(L6FoodName) & Cheese_SemiSkimmed==1) ~ L6ComponentPercentOfL1,
                            (!is.na(L5FoodName) & Cheese_SemiSkimmed==1) ~ L5ComponentPercentOfL1,
                            (!is.na(L4FoodName) & Cheese_SemiSkimmed==1) ~ L4ComponentPercentOfL1,
                            (!is.na(L3FoodName) & Cheese_SemiSkimmed==1) ~ L3ComponentPercentOfL1,
                            (!is.na(L2FoodName) & Cheese_SemiSkimmed==1) ~ L2ComponentPercentOfL1,
                            (!is.na(L1FoodName) & Cheese_SemiSkimmed==1) ~ L1ComponentPercentage)) %>%      
  mutate(Cheese_Whole_g = case_when(is.na(Cheese_Whole) ~ 0,
                            (L0FoodCategory=="F" & Cheese_Whole==1) ~ 100,
                            (!is.na(L6FoodName) & Cheese_Whole==1) ~ L6ComponentPercentOfL1,
                            (!is.na(L5FoodName) & Cheese_Whole==1) ~ L5ComponentPercentOfL1,
                            (!is.na(L4FoodName) & Cheese_Whole==1) ~ L4ComponentPercentOfL1,
                            (!is.na(L3FoodName) & Cheese_Whole==1) ~ L3ComponentPercentOfL1,
                            (!is.na(L2FoodName) & Cheese_Whole==1) ~ L2ComponentPercentOfL1,
                            (!is.na(L1FoodName) & Cheese_Whole==1) ~ L1ComponentPercentage)) %>%          
  mutate(Yogurt_g = case_when(is.na(Yogurt) ~ 0,
                            (L0FoodCategory=="F" & Yogurt==1) ~ 100,
                            (!is.na(L6FoodName) & Yogurt==1) ~ L6ComponentPercentOfL1,
                            (!is.na(L5FoodName) & Yogurt==1) ~ L5ComponentPercentOfL1,
                            (!is.na(L4FoodName) & Yogurt==1) ~ L4ComponentPercentOfL1,
                            (!is.na(L3FoodName) & Yogurt==1) ~ L3ComponentPercentOfL1,
                            (!is.na(L2FoodName) & Yogurt==1) ~ L2ComponentPercentOfL1,
                            (!is.na(L1FoodName) & Yogurt==1) ~ L1ComponentPercentage)) %>%        
  mutate(Yogurt_Skimmed_g = case_when(is.na(Yogurt_Skimmed) ~ 0,
                            (L0FoodCategory=="F" & Yogurt_Skimmed==1) ~ 100,
                            (!is.na(L6FoodName) & Yogurt_Skimmed==1) ~ L6ComponentPercentOfL1,
                            (!is.na(L5FoodName) & Yogurt_Skimmed==1) ~ L5ComponentPercentOfL1,
                            (!is.na(L4FoodName) & Yogurt_Skimmed==1) ~ L4ComponentPercentOfL1,
                            (!is.na(L3FoodName) & Yogurt_Skimmed==1) ~ L3ComponentPercentOfL1,
                            (!is.na(L2FoodName) & Yogurt_Skimmed==1) ~ L2ComponentPercentOfL1,
                            (!is.na(L1FoodName) & Yogurt_Skimmed==1) ~ L1ComponentPercentage)) %>%          
  mutate(Yogurt_SemiSkimmed_g = case_when(is.na(Yogurt_SemiSkimmed) ~ 0,
                            (L0FoodCategory=="F" & Yogurt_SemiSkimmed==1) ~ 100,
                            (!is.na(L6FoodName) & Yogurt_SemiSkimmed==1) ~ L6ComponentPercentOfL1,
                            (!is.na(L5FoodName) & Yogurt_SemiSkimmed==1) ~ L5ComponentPercentOfL1,
                            (!is.na(L4FoodName) & Yogurt_SemiSkimmed==1) ~ L4ComponentPercentOfL1,
                            (!is.na(L3FoodName) & Yogurt_SemiSkimmed==1) ~ L3ComponentPercentOfL1,
                            (!is.na(L2FoodName) & Yogurt_SemiSkimmed==1) ~ L2ComponentPercentOfL1,
                            (!is.na(L1FoodName) & Yogurt_SemiSkimmed==1) ~ L1ComponentPercentage)) %>%           
  mutate(Yogurt_Whole_g = case_when(is.na(Yogurt_Whole) ~ 0,
                            (L0FoodCategory=="F" & Yogurt_Whole==1) ~ 100,
                            (!is.na(L6FoodName) & Yogurt_Whole==1) ~ L6ComponentPercentOfL1,
                            (!is.na(L5FoodName) & Yogurt_Whole==1) ~ L5ComponentPercentOfL1,
                            (!is.na(L4FoodName) & Yogurt_Whole==1) ~ L4ComponentPercentOfL1,
                            (!is.na(L3FoodName) & Yogurt_Whole==1) ~ L3ComponentPercentOfL1,
                            (!is.na(L2FoodName) & Yogurt_Whole==1) ~ L2ComponentPercentOfL1,
                            (!is.na(L1FoodName) & Yogurt_Whole==1) ~ L1ComponentPercentage)) %>%     
  mutate(Cream_g = case_when(is.na(Cream) ~ 0,
                            (L0FoodCategory=="F" & Cream==1) ~ 100,
                            (!is.na(L6FoodName) & Cream==1) ~ L6ComponentPercentOfL1,
                            (!is.na(L5FoodName) & Cream==1) ~ L5ComponentPercentOfL1,
                            (!is.na(L4FoodName) & Cream==1) ~ L4ComponentPercentOfL1,
                            (!is.na(L3FoodName) & Cream==1) ~ L3ComponentPercentOfL1,
                            (!is.na(L2FoodName) & Cream==1) ~ L2ComponentPercentOfL1,
                            (!is.na(L1FoodName) & Cream==1) ~ L1ComponentPercentage)) %>%   
  mutate(Cream_SemiSkimmed_g = case_when(is.na(Cream_SemiSkimmed) ~ 0,
                            (L0FoodCategory=="F" & Cream_SemiSkimmed==1) ~ 100,
                            (!is.na(L6FoodName) & Cream_SemiSkimmed==1) ~ L6ComponentPercentOfL1,
                            (!is.na(L5FoodName) & Cream_SemiSkimmed==1) ~ L5ComponentPercentOfL1,
                            (!is.na(L4FoodName) & Cream_SemiSkimmed==1) ~ L4ComponentPercentOfL1,
                            (!is.na(L3FoodName) & Cream_SemiSkimmed==1) ~ L3ComponentPercentOfL1,
                            (!is.na(L2FoodName) & Cream_SemiSkimmed==1) ~ L2ComponentPercentOfL1,
                            (!is.na(L1FoodName) & Cream_SemiSkimmed==1) ~ L1ComponentPercentage)) %>%         
  mutate(Cream_Whole_g = case_when(is.na(Cream_Whole) ~ 0,
                            (L0FoodCategory=="F" & Cream_Whole==1) ~ 100,
                            (!is.na(L6FoodName) & Cream_Whole==1) ~ L6ComponentPercentOfL1,
                            (!is.na(L5FoodName) & Cream_Whole==1) ~ L5ComponentPercentOfL1,
                            (!is.na(L4FoodName) & Cream_Whole==1) ~ L4ComponentPercentOfL1,
                            (!is.na(L3FoodName) & Cream_Whole==1) ~ L3ComponentPercentOfL1,
                            (!is.na(L2FoodName) & Cream_Whole==1) ~ L2ComponentPercentOfL1,
                            (!is.na(L1FoodName) & Cream_Whole==1) ~ L1ComponentPercentage)) %>%  
  mutate(Butter_g = case_when(is.na(Butter) ~ 0,
                            (L0FoodCategory=="F" & Butter==1) ~ 100,
                            (!is.na(L6FoodName) & Butter==1) ~ L6ComponentPercentOfL1,
                            (!is.na(L5FoodName) & Butter==1) ~ L5ComponentPercentOfL1,
                            (!is.na(L4FoodName) & Butter==1) ~ L4ComponentPercentOfL1,
                            (!is.na(L3FoodName) & Butter==1) ~ L3ComponentPercentOfL1,
                            (!is.na(L2FoodName) & Butter==1) ~ L2ComponentPercentOfL1,
                            (!is.na(L1FoodName) & Butter==1) ~ L1ComponentPercentage)) 

  
  
# Calculate amount of dairy in each recipe ####

# Sum value within a recipe code then merge new variable onto master dataset, fill in missing 0s, repeat for all dairy categories 
Dairy.g <- disag %>%
            group_by(L0FoodCode) %>%
            summarise(Dairy = sum(Dairy_g)) %>%
            ungroup() %>%
            mutate(FoodNumber=L0FoodCode)
join <- left_join(join,Dairy.g,by="FoodNumber")
join <- join %>% mutate(Dairy = case_when(is.na(Dairy) ~ 0,
                                          !is.na(Dairy) ~ Dairy)) %>%
          select(FoodNumber,FoodDescription_NDB,FoodDescription_SHeS,FoodDescription_FSA,Dairy_Ingredients,Dairy)

Milk.g <- disag %>%
            group_by(L0FoodCode) %>%
            summarise(Milk = sum(Milk_g)) %>%
            ungroup() %>%
            mutate(FoodNumber=L0FoodCode)
join <- left_join(join,Milk.g,by="FoodNumber")
join <- join %>% mutate(Milk = case_when(is.na(Milk) ~ 0,
                                         !is.na(Milk) ~ Milk)) %>%
          select(FoodNumber,FoodDescription_NDB,FoodDescription_SHeS,FoodDescription_FSA,Dairy_Ingredients,Dairy,Milk)

Milk_Skimmed.g <- disag %>%
            group_by(L0FoodCode) %>%
            summarise(Milk_Skimmed = sum(Milk_Skimmed_g)) %>%
            ungroup() %>%
            mutate(FoodNumber=L0FoodCode)
join <- left_join(join,Milk_Skimmed.g,by="FoodNumber")
join <- join %>% mutate(Milk_Skimmed = case_when(is.na(Milk_Skimmed) ~ 0,
                                                 !is.na(Milk_Skimmed) ~ Milk_Skimmed)) %>%
          select(FoodNumber,FoodDescription_NDB,FoodDescription_SHeS,FoodDescription_FSA,Dairy_Ingredients,Dairy,Milk,Milk_Skimmed)

Milk_SemiSkimmed.g <- disag %>%
            group_by(L0FoodCode) %>%
            summarise(Milk_SemiSkimmed = sum(Milk_SemiSkimmed_g)) %>%
            ungroup() %>%
            mutate(FoodNumber=L0FoodCode)
join <- left_join(join,Milk_SemiSkimmed.g,by="FoodNumber")
join <- join %>% mutate(Milk_SemiSkimmed = case_when(is.na(Milk_SemiSkimmed) ~ 0,
                                                     !is.na(Milk_SemiSkimmed) ~ Milk_SemiSkimmed)) %>%
          select(FoodNumber,FoodDescription_NDB,FoodDescription_SHeS,FoodDescription_FSA,Dairy_Ingredients,Dairy,Milk,Milk_Skimmed,Milk_SemiSkimmed)

Milk_Whole.g <- disag %>%
            group_by(L0FoodCode) %>%
            summarise(Milk_Whole = sum(Milk_Whole_g)) %>%
            ungroup() %>%
            mutate(FoodNumber=L0FoodCode)
join <- left_join(join,Milk_Whole.g,by="FoodNumber")
join <- join %>% mutate(Milk_Whole = case_when(is.na(Milk_Whole) ~ 0,
                                               !is.na(Milk_Whole) ~ Milk_Whole)) %>%
          select(FoodNumber,FoodDescription_NDB,FoodDescription_SHeS,FoodDescription_FSA,Dairy_Ingredients,Dairy,Milk,Milk_Skimmed,Milk_SemiSkimmed,
                 Milk_Whole)

Cheese.g <- disag %>%
            group_by(L0FoodCode) %>%
            summarise(Cheese = sum(Cheese_g)) %>%
            ungroup() %>%
            mutate(FoodNumber=L0FoodCode)
join <- left_join(join,Cheese.g,by="FoodNumber")
join <- join %>% mutate(Cheese = case_when(is.na(Cheese) ~ 0,
                                           !is.na(Cheese) ~ Cheese)) %>%
          select(FoodNumber,FoodDescription_NDB,FoodDescription_SHeS,FoodDescription_FSA,Dairy_Ingredients,Dairy,Milk,Milk_Skimmed,Milk_SemiSkimmed,
                 Milk_Whole,Cheese)

Cheese_Cheddar.g <- disag %>%
            group_by(L0FoodCode) %>%
            summarise(Cheese_Cheddar = sum(Cheese_Cheddar_g)) %>%
            ungroup() %>%
            mutate(FoodNumber=L0FoodCode)
join <- left_join(join,Cheese_Cheddar.g,by="FoodNumber")
join <- join %>% mutate(Cheese_Cheddar = case_when(is.na(Cheese_Cheddar) ~ 0,
                                                   !is.na(Cheese_Cheddar) ~ Cheese_Cheddar)) %>%
        select(FoodNumber,FoodDescription_NDB,FoodDescription_SHeS,FoodDescription_FSA,Dairy_Ingredients,Dairy,Milk,Milk_Skimmed,Milk_SemiSkimmed,
               Milk_Whole,Cheese,Cheese_Cheddar)

Cheese_Cottage.g <- disag %>%
            group_by(L0FoodCode) %>%
            summarise(Cheese_Cottage = sum(Cheese_Cottage_g)) %>%
            ungroup() %>%
            mutate(FoodNumber=L0FoodCode)
join <- left_join(join,Cheese_Cottage.g,by="FoodNumber")
join <- join %>% mutate(Cheese_Cottage = case_when(is.na(Cheese_Cottage) ~ 0,
                                                   !is.na(Cheese_Cottage) ~ Cheese_Cottage)) %>%
        select(FoodNumber,FoodDescription_NDB,FoodDescription_SHeS,FoodDescription_FSA,Dairy_Ingredients,Dairy,Milk,Milk_Skimmed,Milk_SemiSkimmed,
               Milk_Whole,Cheese,Cheese_Cheddar,Cheese_Cottage)

Cheese_Other.g <- disag %>%
            group_by(L0FoodCode) %>%
            summarise(Cheese_Other = sum(Cheese_Other_g)) %>%
            ungroup() %>%
            mutate(FoodNumber=L0FoodCode)
join <- left_join(join,Cheese_Other.g,by="FoodNumber")
join <- join %>% mutate(Cheese_Other = case_when(is.na(Cheese_Other) ~ 0,
                                                 !is.na(Cheese_Other) ~ Cheese_Other)) %>%
        select(FoodNumber,FoodDescription_NDB,FoodDescription_SHeS,FoodDescription_FSA,Dairy_Ingredients,Dairy,Milk,Milk_Skimmed,Milk_SemiSkimmed,
               Milk_Whole,Cheese,Cheese_Cheddar,Cheese_Cottage,Cheese_Other)

Cheese_Skimmed.g <- disag %>%
            group_by(L0FoodCode) %>%
            summarise(Cheese_Skimmed = sum(Cheese_Skimmed_g)) %>%
            ungroup() %>%
            mutate(FoodNumber=L0FoodCode)
join <- left_join(join,Cheese_Skimmed.g,by="FoodNumber")
join <- join %>% mutate(Cheese_Skimmed = case_when(is.na(Cheese_Skimmed) ~ 0,
                                                  !is.na(Cheese_Skimmed) ~ Cheese_Skimmed)) %>%
        select(FoodNumber,FoodDescription_NDB,FoodDescription_SHeS,FoodDescription_FSA,Dairy_Ingredients,Dairy,Milk,Milk_Skimmed,Milk_SemiSkimmed,
               Milk_Whole,Cheese,Cheese_Cheddar,Cheese_Cottage,Cheese_Other,Cheese_Skimmed)

Cheese_SemiSkimmed.g <- disag %>%
            group_by(L0FoodCode) %>%
            summarise(Cheese_SemiSkimmed = sum(Cheese_SemiSkimmed_g)) %>%
            ungroup() %>%
            mutate(FoodNumber=L0FoodCode)
join <- left_join(join,Cheese_SemiSkimmed.g,by="FoodNumber")
join <- join %>% mutate(Cheese_SemiSkimmed = case_when(is.na(Cheese_SemiSkimmed) ~ 0,
                                                       !is.na(Cheese_SemiSkimmed) ~ Cheese_SemiSkimmed)) %>%
            select(FoodNumber,FoodDescription_NDB,FoodDescription_SHeS,FoodDescription_FSA,Dairy_Ingredients,Dairy,Milk,Milk_Skimmed,Milk_SemiSkimmed,
                   Milk_Whole,Cheese,Cheese_Cheddar,Cheese_Cottage,Cheese_Other,Cheese_Skimmed,Cheese_SemiSkimmed)

Cheese_Whole.g <- disag %>%
            group_by(L0FoodCode) %>%
            summarise(Cheese_Whole = sum(Cheese_Whole_g)) %>%
            ungroup() %>%
            mutate(FoodNumber=L0FoodCode)
join <- left_join(join,Cheese_Whole.g,by="FoodNumber")
join <- join %>% mutate(Cheese_Whole = case_when(is.na(Cheese_Whole) ~ 0,
                                                       !is.na(Cheese_Whole) ~ Cheese_Whole)) %>%
          select(FoodNumber,FoodDescription_NDB,FoodDescription_SHeS,FoodDescription_FSA,Dairy_Ingredients,Dairy,Milk,Milk_Skimmed,Milk_SemiSkimmed,
                 Milk_Whole,Cheese,Cheese_Cheddar,Cheese_Cottage,Cheese_Other,Cheese_Skimmed,Cheese_SemiSkimmed,Cheese_Whole)

Yogurt.g <- disag %>%
            group_by(L0FoodCode) %>%
            summarise(Yogurt = sum(Yogurt_g)) %>%
            ungroup() %>%
            mutate(FoodNumber=L0FoodCode)
join <- left_join(join,Yogurt.g,by="FoodNumber")
join <- join %>% mutate(Yogurt = case_when(is.na(Yogurt) ~ 0,
                                                 !is.na(Yogurt) ~ Yogurt)) %>%
          select(FoodNumber,FoodDescription_NDB,FoodDescription_SHeS,FoodDescription_FSA,Dairy_Ingredients,Dairy,Milk,Milk_Skimmed,Milk_SemiSkimmed,
                 Milk_Whole,Cheese,Cheese_Cheddar,Cheese_Cottage,Cheese_Other,Cheese_Skimmed,Cheese_SemiSkimmed,Cheese_Whole,Yogurt)

Yogurt_Skimmed.g <- disag %>%
            group_by(L0FoodCode) %>%
            summarise(Yogurt_Skimmed = sum(Yogurt_Skimmed_g)) %>%
            ungroup() %>%
            mutate(FoodNumber=L0FoodCode)
join <- left_join(join,Yogurt_Skimmed.g,by="FoodNumber")
join <- join %>% mutate(Yogurt_Skimmed = case_when(is.na(Yogurt_Skimmed) ~ 0,
                                                   !is.na(Yogurt_Skimmed) ~ Yogurt_Skimmed)) %>%
          select(FoodNumber,FoodDescription_NDB,FoodDescription_SHeS,FoodDescription_FSA,Dairy_Ingredients,Dairy,Milk,Milk_Skimmed,Milk_SemiSkimmed,
                 Milk_Whole,Cheese,Cheese_Cheddar,Cheese_Cottage,Cheese_Other,Cheese_Skimmed,Cheese_SemiSkimmed,Cheese_Whole,Yogurt,Yogurt_Skimmed)

Yogurt_SemiSkimmed.g <- disag %>%
            group_by(L0FoodCode) %>%
            summarise(Yogurt_SemiSkimmed = sum(Yogurt_SemiSkimmed_g)) %>%
            ungroup() %>%
            mutate(FoodNumber=L0FoodCode)
join <- left_join(join,Yogurt_SemiSkimmed.g,by="FoodNumber")
join <- join %>% mutate(Yogurt_SemiSkimmed = case_when(is.na(Yogurt_SemiSkimmed) ~ 0,
                                                             !is.na(Yogurt_SemiSkimmed) ~ Yogurt_SemiSkimmed)) %>%
          select(FoodNumber,FoodDescription_NDB,FoodDescription_SHeS,FoodDescription_FSA,Dairy_Ingredients,Dairy,Milk,Milk_Skimmed,Milk_SemiSkimmed,
                 Milk_Whole,Cheese,Cheese_Cheddar,Cheese_Cottage,Cheese_Other,Cheese_Skimmed,Cheese_SemiSkimmed,Cheese_Whole,Yogurt,Yogurt_Skimmed,
                 Yogurt_SemiSkimmed)

Yogurt_Whole.g <- disag %>%
            group_by(L0FoodCode) %>%
            summarise(Yogurt_Whole = sum(Yogurt_Whole_g)) %>%
            ungroup() %>%
            mutate(FoodNumber=L0FoodCode)
join <- left_join(join,Yogurt_Whole.g,by="FoodNumber")
join <- join %>% mutate(Yogurt_Whole = case_when(is.na(Yogurt_Whole) ~ 0,
                                                       !is.na(Yogurt_Whole) ~ Yogurt_Whole)) %>%
          select(FoodNumber,FoodDescription_NDB,FoodDescription_SHeS,FoodDescription_FSA,Dairy_Ingredients,Dairy,Milk,Milk_Skimmed,Milk_SemiSkimmed,
                 Milk_Whole,Cheese,Cheese_Cheddar,Cheese_Cottage,Cheese_Other,Cheese_Skimmed,Cheese_SemiSkimmed,Cheese_Whole,Yogurt,Yogurt_Skimmed,
                 Yogurt_SemiSkimmed,Yogurt_Whole)

Cream.g <- disag %>%
            group_by(L0FoodCode) %>%
            summarise(Cream = sum(Cream_g)) %>%
            ungroup() %>%
            mutate(FoodNumber=L0FoodCode)
join <- left_join(join,Cream.g,by="FoodNumber")
join <- join %>% mutate(Cream = case_when(is.na(Cream) ~ 0,
                                          !is.na(Cream) ~ Cream)) %>%
          select(FoodNumber,FoodDescription_NDB,FoodDescription_SHeS,FoodDescription_FSA,Dairy_Ingredients,Dairy,Milk,Milk_Skimmed,Milk_SemiSkimmed,
                 Milk_Whole,Cheese,Cheese_Cheddar,Cheese_Cottage,Cheese_Other,Cheese_Skimmed,Cheese_SemiSkimmed,Cheese_Whole,Yogurt,Yogurt_Skimmed,
                 Yogurt_SemiSkimmed,Yogurt_Whole,Cream)

Cream_SemiSkimmed.g <- disag %>%
            group_by(L0FoodCode) %>%
            summarise(Cream_SemiSkimmed = sum(Cream_SemiSkimmed_g)) %>%
            ungroup() %>%
            mutate(FoodNumber=L0FoodCode)
join <- left_join(join,Cream_SemiSkimmed.g,by="FoodNumber")
join <- join %>% mutate(Cream_SemiSkimmed = case_when(is.na(Cream_SemiSkimmed) ~ 0,
                                                      !is.na(Cream_SemiSkimmed) ~ Cream_SemiSkimmed)) %>%
          select(FoodNumber,FoodDescription_NDB,FoodDescription_SHeS,FoodDescription_FSA,Dairy_Ingredients,Dairy,Milk,Milk_Skimmed,Milk_SemiSkimmed,
                 Milk_Whole,Cheese,Cheese_Cheddar,Cheese_Cottage,Cheese_Other,Cheese_Skimmed,Cheese_SemiSkimmed,Cheese_Whole,Yogurt,Yogurt_Skimmed,
                 Yogurt_SemiSkimmed,Yogurt_Whole,Cream,Cream_SemiSkimmed)

Cream_Whole.g <- disag %>%
            group_by(L0FoodCode) %>%
            summarise(Cream_Whole = sum(Cream_Whole_g)) %>%
            ungroup() %>%
            mutate(FoodNumber=L0FoodCode)
join <- left_join(join,Cream_Whole.g,by="FoodNumber")
join <- join %>% mutate(Cream_Whole = case_when(is.na(Cream_Whole) ~ 0,
                                                !is.na(Cream_Whole) ~ Cream_Whole)) %>%
          select(FoodNumber,FoodDescription_NDB,FoodDescription_SHeS,FoodDescription_FSA,Dairy_Ingredients,Dairy,Milk,Milk_Skimmed,Milk_SemiSkimmed,
                 Milk_Whole,Cheese,Cheese_Cheddar,Cheese_Cottage,Cheese_Other,Cheese_Skimmed,Cheese_SemiSkimmed,Cheese_Whole,Yogurt,Yogurt_Skimmed,
                 Yogurt_SemiSkimmed,Yogurt_Whole,Cream,Cream_SemiSkimmed,Cream_Whole)

Butter.g <- disag %>%
            group_by(L0FoodCode) %>%
            summarise(Butter = sum(Butter_g)) %>%
            ungroup() %>%
            mutate(FoodNumber=L0FoodCode)
join <- left_join(join,Butter.g,by="FoodNumber")
join <- join %>% mutate(Butter = case_when(is.na(Butter) ~ 0,
                                           !is.na(Butter) ~ Butter)) %>%
          select(FoodNumber,FoodDescription_NDB,FoodDescription_SHeS,FoodDescription_FSA,Dairy_Ingredients,Dairy,Milk,Milk_Skimmed,Milk_SemiSkimmed,
                 Milk_Whole,Cheese,Cheese_Cheddar,Cheese_Cottage,Cheese_Other,Cheese_Skimmed,Cheese_SemiSkimmed,Cheese_Whole,Yogurt,Yogurt_Skimmed,
                 Yogurt_SemiSkimmed,Yogurt_Whole,Cream,Cream_SemiSkimmed,Cream_Whole,Butter)



# Investigate missing data ####
missing <- join %>% select(FoodNumber,FoodDescription_NDB,FoodDescription_SHeS,FoodDescription_FSA,Dairy_Ingredients) %>% 
                    filter(is.na(Dairy_Ingredients))
#write_xlsx(missing,paste0("Data/","Missing_FSA_",format(Sys.time(),"%d%m%Y"),".xlsx"))



# Save dataset ####

  ## Change FoodNumbers back to original ####
  # See Excel "NDB_vs_SHeS_vs_FSA_Mismatches" for details
  join <- join %>% mutate(FoodNumber = case_when(FoodNumber==2276 ~ 11416,
                                                 FoodNumber==2254 ~ 11422,
                                                 FoodNumber==268 ~ 11419,
                                                 FoodNumber==259 ~ 11417,
                                                 FoodNumber==260 ~ 11418,
                                                 FoodNumber==269 ~ 11420,
                                                 FoodDescription_NDB=="Sandwich thins, wholemeal (brown)" ~ 11784,
                                                 FoodNumber==330 ~ 262,
                                                 FoodDescription_SHeS=="Belgian bun" ~ 303,
                                                 FoodNumber==6964 ~ 574,
                                                 FoodDescription_NDB=="Fruit fritters" ~ 586,
                                                 FoodDescription_SHeS=="Carrot and swede mash" ~ 1921,
                                                 FoodNumber==2438 ~ 2445,
                                                 FoodDescription_NDB=="Kumquats" ~ 2711,
                                                 FoodNumber==10284 ~ 3797,
                                                 FoodNumber==8606 ~ 5201,
                                                 FoodDescription_SHeS=="Butternut squash soup" ~ 5607,
                                                 FoodDescription_NDB=="Butternut squash soup" ~ 6736,
                                                 FoodNumber==10187 ~ 8136,
                                                 FoodNumber==7902 ~ 8445,
                                                 FoodDescription_NDB=="Challah bread" ~ 9372,
                                                 FoodNumber!=11784 ~ FoodNumber))

  ## Fill in missing dairy for duplicated items ####
  test <- full_join(shes,join,by="FoodNumber")

    # Chocolate covered fingers (e.g. Cadbury milk chocolate fingers) 11416 -> Twix (chocolate & caramel biscuit fingers) 2276
    twix <- test %>% filter(FoodNumber==11416 | FoodNumber==2276)                                       
      # Index of the observation you want to fill in
      miss <- c(1,2,3)
      # Index of the observation whose values you want to use
      fill <- 4
      # Columns to fill (you can specify multiple columns)
      columns_to_fill <- c("Dairy_Ingredients", "Dairy", "Milk", "Milk_Skimmed", "Milk_SemiSkimmed", "Milk_Whole", "Cheese", "Cheese_Cheddar", "Cheese_Cottage", "Cheese_Other", "Cheese_Skimmed",
                            "Cheese_SemiSkimmed", "Cheese_Whole", "Yogurt", "Yogurt_Skimmed", "Yogurt_SemiSkimmed", "Yogurt_Whole", "Cream", "Cream_SemiSkimmed", "Cream_Whole", "Butter")
      # Fill in values for the specified observation
      for (obs in miss) {
        twix[obs, columns_to_fill] <- twix[fill, columns_to_fill]
      }
      
      # Mcvities digestive 11417 -> DIGESTIVE PLAIN 259
      mcvities <- test %>% filter(FoodNumber==11417 | FoodNumber==259)                                       
      # Index of the observation you want to fill in
      miss <- 1
      # Index of the observation whose values you want to use
      fill <- 2
      # Fill in values for the specified observation
      mcvities[miss,columns_to_fill] <- mcvities[fill,columns_to_fill]  
      
      # Mcvities chocolate digestive 11418 -> Chocolate digestive, biscuit 260
      mcvitieschoc <- test %>% filter(FoodNumber==11418 | FoodNumber==260)                                       
      # Index of the observation you want to fill in
      miss <- 1
      # Index of the observation whose values you want to use
      fill <- 2
      # Fill in values for the specified observation
      mcvitieschoc[miss,columns_to_fill] <- mcvitieschoc[fill,columns_to_fill] 
      
      # Mcvities rich tea 11420 -> Semi-sweet biscuit (e.g. Rich Tea, Morning Coffee) 269
      mcvitiestea <- test %>% filter(FoodNumber==11420 | FoodNumber==269)                                       
      # Index of the observation you want to fill in
      miss <- 1
      # Index of the observation whose values you want to use
      fill <- 2
      # Fill in values for the specified observation
      mcvitiestea[miss,columns_to_fill] <- mcvitiestea[fill,columns_to_fill] 
      
      # Oreos, including supermarket brands 11419 -> Biscuits filled with cream (e.g. bourbons, Fox's crunch creams) 268
      oreo <- test %>% filter(FoodNumber==11419 | FoodNumber==268)                                       
      # Index of the observation you want to fill in
      miss <- 1
      # Index of the observation whose values you want to use
      fill <- 2
      # Fill in values for the specified observation
      oreo[miss,columns_to_fill] <- oreo[fill,columns_to_fill]  
      
      # Toblerone 11422 -> Milk chocolate bar (e.g. Dairy Milk) 2254
      toblerone <- test %>% filter(FoodNumber==11422 | FoodNumber==2254)                                       
      # Index of the observation you want to fill in
      miss <- c(1,2,3,4)
      # Index of the observation whose values you want to use
      fill <- 5
      # Fill in values for the specified observation
      for (obs in miss) {
        toblerone[obs, columns_to_fill] <- toblerone[fill, columns_to_fill]
      }
      
      # Fish sauce 2445 -> Oyster sauce 2438
      fish <- test %>% filter(FoodNumber==2445 | FoodNumber==2438)                                       
      # Index of the observation you want to fill in
      miss <- 1
      # Index of the observation whose values you want to use
      fill <- 2
      # Fill in values for the specified observation
      fish[miss,columns_to_fill] <- fish[fill,columns_to_fill]       
     
      # Butternut squash soup 5607 -> Butternut squash soup 3955
      squash <- test %>% filter(FoodNumber==5607 | FoodNumber==3955 | FoodNumber==6736)                                       
      # Index of the observation you want to fill in
      miss <- 1
      # Index of the observation whose values you want to use
      fill <- 2
      # Fill in values for the specified observation
      squash[miss,columns_to_fill] <- squash[fill,columns_to_fill]
      
      # Belgian bun 303 -> Iced bun 8125
      bun <- test %>% filter(FoodNumber==303 | FoodNumber==8125)                                       
      # Index of the observation you want to fill in
      miss <- c(1,2)
      # Index of the observation whose values you want to use
      fill <- 4
      # Fill in values for the specified observation
      for (obs in miss) {
        bun[obs, columns_to_fill] <- bun[fill, columns_to_fill]
      }
      
      # Porridge, made with water 10284 -> Instant porridge pot, plain (e.g. Oat So Simple original), made up with water 3797
      porridge <- test %>% filter(FoodNumber==10284 | FoodNumber==3797)                                       
      # Index of the observation you want to fill in
      miss <- 4
      # Index of the observation whose values you want to use
      fill <- 1
      # Fill in values for the specified observation
      porridge[miss,columns_to_fill] <- porridge[fill,columns_to_fill]
      
      # Sandwich thins, wholemeal (brown) 11784 -> Sandwich thins, wholemeal (brown) 11282
      thins <- test %>% filter(FoodNumber==11784 | FoodNumber==11282)                                       
      # Index of the observation you want to fill in
      miss <- 1
      # Index of the observation whose values you want to use
      fill <- 2
      # Fill in values for the specified observation
      thins[miss,columns_to_fill] <- thins[fill,columns_to_fill]    
     
      # Carrot and swede mash 1921 -> Carrot and swede mash 11602
      swede <- test %>% filter(FoodNumber==1921 | FoodNumber==11602)                                       
      # Index of the observation you want to fill in
      miss <- 5
      # Index of the observation whose values you want to use
      fill <- 2
      # Fill in values for the specified observation
      swede[miss,columns_to_fill] <- swede[fill,columns_to_fill]      
       
      # Tangerines / mandarins / clementines/ satsumas 2157 -> Kumquats 2711
      kumquat <- test %>% filter(FoodNumber==2157 | FoodNumber==2711)                                       
      # Index of the observation you want to fill in
      miss <- 1
      # Index of the observation whose values you want to use
      fill <- 2
      # Fill in values for the specified observation
      kumquat[miss,columns_to_fill] <- kumquat[fill,columns_to_fill]      
      
    # Append
    test <- test %>% filter(FoodNumber!=11416 & FoodNumber!=2276 & FoodNumber!=11417 & FoodNumber!=259 & FoodNumber!=11418 & FoodNumber!=260 & FoodNumber!=11420 & FoodNumber!=269
                            & FoodNumber!=11419 & FoodNumber!=268 & FoodNumber!=11422 & FoodNumber!=2254 & FoodNumber!=2445 & FoodNumber!=2438 & FoodNumber!=5607 & FoodNumber!=3955 & FoodNumber!=6736
                            & FoodNumber!=303 & FoodNumber!=8125 & FoodNumber!=10284 & FoodNumber!=3797 & FoodNumber!=11784 & FoodNumber!=11282 & FoodNumber!=1921 & FoodNumber!=11602
                            & FoodNumber!=2157 & FoodNumber!=2711)  
    test <- rbind(test,twix,mcvities,mcvitieschoc,mcvitiestea,oreo,toblerone,fish,squash,bun,porridge,thins,swede,kumquat)

                                        
  ## Fix dairy-free hot chocolate ####
  test <- test %>% mutate(Dairy_Ingredients = case_when(FoodDescription_SHeS=="Horlicks/Ovaltine, made with alternative milks (e.g. soya milk)" ~ 0,
                                                         FoodDescription_SHeS!="Horlicks/Ovaltine, made with alternative milks (e.g. soya milk)" ~ Dairy_Ingredients,
                                                         is.na(FoodDescription_SHeS) ~ Dairy_Ingredients),
                          Dairy = case_when(FoodDescription_SHeS=="Horlicks/Ovaltine, made with alternative milks (e.g. soya milk)" ~ 0,
                                            FoodDescription_SHeS!="Horlicks/Ovaltine, made with alternative milks (e.g. soya milk)" ~ Dairy,
                                            is.na(FoodDescription_SHeS) ~ Dairy),
                          Milk = case_when(FoodDescription_SHeS=="Horlicks/Ovaltine, made with alternative milks (e.g. soya milk)" ~ 0,
                                           FoodDescription_SHeS!="Horlicks/Ovaltine, made with alternative milks (e.g. soya milk)" ~ Milk,
                                           is.na(FoodDescription_SHeS) ~ Milk),
                          Milk_SemiSkimmed = case_when(FoodDescription_SHeS=="Horlicks/Ovaltine, made with alternative milks (e.g. soya milk)" ~ 0,
                                                       FoodDescription_SHeS!="Horlicks/Ovaltine, made with alternative milks (e.g. soya milk)" ~ Milk_SemiSkimmed,
                                                       is.na(FoodDescription_SHeS) ~ Milk_SemiSkimmed),
                          Milk_Skimmed = case_when(FoodDescription_SHeS=="Horlicks/Ovaltine, made with alternative milks (e.g. soya milk)" ~ 0,
                                                   FoodDescription_SHeS!="Horlicks/Ovaltine, made with alternative milks (e.g. soya milk)" ~ Milk_Skimmed,
                                                   is.na(FoodDescription_SHeS) ~ Milk_Skimmed),
                          Milk_Whole = case_when(FoodDescription_SHeS=="Horlicks/Ovaltine, made with alternative milks (e.g. soya milk)" ~ 0,
                                                 FoodDescription_SHeS!="Horlicks/Ovaltine, made with alternative milks (e.g. soya milk)" ~ Milk_Whole,
                                                 is.na(FoodDescription_SHeS) ~ Milk_Whole))
  
  ## Set nutritional supplement drinks (e.g. Slimfast, Protein shake) to 0 (not counted as dairy)
  test <- test %>% mutate(Dairy_Ingredients = case_when(RecipeSubFoodGroupCode=="50E" ~ 0,
                                                        RecipeSubFoodGroupCode!="50E" ~ Dairy_Ingredients,
                                                        is.na(RecipeSubFoodGroupCode) ~ 0),
                            Dairy = case_when(RecipeSubFoodGroupCode=="50E" ~ 0,
                                              RecipeSubFoodGroupCode!="50E" ~ Dairy,
                                              is.na(RecipeSubFoodGroupCode) ~ 0),
                            Milk = case_when(RecipeSubFoodGroupCode=="50E" ~ 0,
                                             RecipeSubFoodGroupCode!="50E" ~ Milk,
                                             is.na(RecipeSubFoodGroupCode) ~ 0),
                            Milk_SemiSkimmed = case_when(RecipeSubFoodGroupCode=="50E" ~ 0,
                                                         RecipeSubFoodGroupCode!="50E" ~ Milk_SemiSkimmed,
                                                         is.na(RecipeSubFoodGroupCode) ~ 0),
                            Milk_Skimmed = case_when(RecipeSubFoodGroupCode=="50E" ~ 0,
                                                     RecipeSubFoodGroupCode!="50E" ~ Milk_Skimmed,
                                                     is.na(RecipeSubFoodGroupCode) ~ 0),
                            Milk_Whole = case_when(RecipeSubFoodGroupCode=="50E" ~ 0,
                                                   RecipeSubFoodGroupCode!="50E" ~ Milk_Whole,
                                                   is.na(RecipeSubFoodGroupCode) ~ 0))

  ## Merge in food group labels ####
  labels <- read_excel("Data/Sub_Food_Group_Labels.xlsx")
  labels <- labels %>% mutate(RecipeSubFoodGroupCode=SubGroupCode) %>%
                       select(RecipeSubFoodGroupCode,SubGroupDesc)
  test <- left_join(test,labels,by="RecipeSubFoodGroupCode")
 
  
  ## Export ####
  write_xlsx(test,paste0("Data/","NDB_SHeS_Disag_Dairy_",format(Sys.time(),"%d%m%Y"),".xlsx"))
  
  
  
# Figures for paper ####
  
## Figure 1 ####
table(fsa.ingredients$Dairy) # 89 dairy ingredients
table(join$Dairy)

group_mean <- test %>% group_by(SubGroupDesc) %>%
                       summarise_at(vars(Dairy),
                       list(Mean_Dairy = mean))

fig1.df <- group_mean %>% mutate(Mean_Dairy = as.integer(Mean_Dairy)) %>%
                          filter(Mean_Dairy>20 & Mean_Dairy!=100 & !is.na(SubGroupDesc))

fig1.df<- fig1.df  %>%
  mutate(SubGroupDesc_lab = str_replace(SubGroupDesc," - ","\n")) %>%
  mutate(SubGroupDesc_lab = str_replace(SubGroupDesc_lab," \\(","\n\\(")) %>%
  mutate(SubGroupDesc_lab = str_replace(SubGroupDesc_lab," INC","\nINC")) %>%
  mutate(SubGroupDesc_lab = str_replace(SubGroupDesc_lab," MAN","\nMAN")) %>%
  mutate(SubGroupDesc_lab = reorder(SubGroupDesc_lab, Mean_Dairy))


fig1.fig <- ggplot(fig1.df, aes(x = SubGroupDesc_lab, y = Mean_Dairy)) +
        geom_bar(position=position_dodge(), stat="identity") +
        theme_minimal() +
        labs(x = "", y="Mean Dairy (g dairy / 100 g food)") +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5,hjust = 1)) +
  lims(y=c(0,100))

ggsave(fig1.fig, filename = "Output/Figure1.jpeg",,width = 7, height = 5)


## Figure 2 ####

# Read in the data frame
fig2.df <- data.frame(Quality=as.factor(c("Without Disaggregation","Without Disaggregation","Without Disaggregation",
                                          "Without Disaggregation","Without Disaggregation","Without Disaggregation",
                                          "With Disaggregation","With Disaggregation","With Disaggregation",
                                          "With Disaggregation","With Disaggregation","With Disaggregation")),
                        Category=as.factor(c("Total dairy","Milk","Cheese","Yogurt","Cream","Butter",
                                             "Total dairy","Milk","Cheese","Yogurt","Cream","Butter")),
                        Mean=c(218.4,171.5,10.7,25.7,6.7,3.8,
                               248.3,193.2,21.5,22.4,3.7,7.5),
                        sd=c(218.0,202.4,22.4,59.4,19.2,6.6,
                             213.9,200.0,30.5,49.6,9.5,8.7)) 
# Plot 
fig2.fig <- ggplot(fig2.df, aes(x=reorder(Category,desc(Mean)), y=Mean, fill=factor(Quality, levels = c("Without Disaggregation", "With Disaggregation")))) +  
       geom_bar(position=position_dodge(), stat="identity") +
       labs(x="",
            y="Mean (g/day)",
            fill="") +
       theme_minimal() +
  theme(legend.position = "top") +
  scale_fill_grey()

ggsave(fig2.fig, filename = "Output/Figure2.jpeg",width = 4.5, height = 3)



# Explore data ####
butter <- test %>% filter(RecipeSubFoodGroupCode=="17R")
cream <- test  %>%  filter(Cream>0)
