library(tidyr) 
library(dplyr) # the main data manipulation library (dplyr)
library(haven) # to read stata

# Note: this thesis is on household level #

############################################# Consumptions########################################################################################################################

#----------- Consumption (b1_ks1) -----------#

#-#-# Selecting Data #-#-#
# [PC] 
b1_ks1 = read_dta("D:/Kuliah/Playing With Data/IFLS 5/b1_ks1.dta",
#                        
col_select = c(hhid14, ks1type, ks02))
#[LAPTOP] b1_ks1 = read_dta("D:/College/IFLS 5/b1_ks1.dta",
#                  col_select = c(hhid14, ks1type, ks02))
b1_ks1 = as_tibble(b1_ks1)



#-#-# Reshaping #-#-#
b1_ks1 = b1_ks1 %>%
  pivot_wider(names_from = ks1type, values_from = ks02, values_fn = sum)



#-#-# Classifying Foods: Protein, Fiber #-#-#
b1_ks1_A1_pf = b1_ks1 %>%
  mutate(
    total_food = rowSums(across(-hhid14), na.rm = TRUE),  # Total Food
    protein_food = rowSums(across(c('K', 'L', 'M', 'N', 'OA', 'OB', 'P', 'Q')), na.rm = TRUE),  # Protein
    fiber_food = rowSums(across(c('F', 'G', 'H')), na.rm = TRUE)  # Fiber
  )



#-#-# Classifying Foods: Healthy Eating Index (idea: certain foods are set for moderation (<q3) and adequacy (>q1)) #-#-#
# Additional Foods Variable for Moderation & Adequacy 
b1_ks1_B1_hei = b1_ks1_A1_pf %>%
  mutate(
    fish_meat_food = rowSums(across(c('K', 'L', 'M', 'N', 'OA')), na.rm = TRUE),  # Fish and Meat (for 'adequacy')
    sugar_food = rowSums(across(c('AA', 'W')), na.rm = TRUE),  # Sugar and Food (for 'moderation')
    sat_fat_food = rowSums(across(c('K', 'Y', 'X', 'Q')), na.rm = TRUE)  # Saturated Fat (for 'moderation')
  ) %>%

# Food Proportion 
  mutate(
    across(c(-hhid14, -total_food, -protein_food, -fiber_food),
           ~ . / total_food,
           .names = "{.col}_proportion")
  ) %>%

# Quantile Tagging
  mutate(
    across(c('F_proportion', 'H_proportion', 'Q_proportion', 'fish_meat_food_proportion', 'G_proportion', 'Y_proportion'),
            ~ ifelse(. > quantile(., 0.25, na.rm=TRUE), 1, 0),
            .names = "{.col}_adequacy"),
    across(c('EA_proportion', 'FA_proportion', 'sugar_food_proportion', 'S_proportion', 'sat_fat_food_proportion'),
           ~ ifelse(. < quantile(., 0.75, na.rm=TRUE), 1, 0),
           .names = "{.col}_moderation")
  ) %>%

# HEI summation (total point of _adequacy and _moderation variables)
  mutate(
    HEI = rowSums(across(c(ends_with("_adequacy"), ends_with("_moderation"))), na.rm = TRUE)
  )

# Finalize Data Set
b1_ks1_final = b1_ks1_B1_hei %>%
  select(hhid14, protein_food, fiber_food, HEI)


############################################# Educations #################################################################################################################################

#----------- Education (b3a_dl1) -----------#

#-#-# Selecting Data #-#-#
# [PC] 
b3a_dl1 = read_dta("D:/Kuliah/Playing With Data/IFLS 5/b3a_dl1.dta") 
# [LAPTOP] b3a_dl1 = read_dta("D:/College/IFLS 5/b3a_dl1.dta")
b3a_dl1 = as_tibble(b3a_dl1)
b3a_dl1 = b3a_dl1 %>%
  select(hhid14, pidlink, dl06, dl07, everything()) %>%
  arrange(hhid14)



#-#-# Making Year of Education #-#-#
# Base Educ (if they're X graduate, then they're AT LEAST Y year of education)
b3a_dl1_A1_base = b3a_dl1 %>%
  mutate(base_educ = case_when(
    dl06 %in% c(2, 11, 72) ~ 0, # If elementary, then at least 0 year
    dl06 %in% c(3, 4, 12, 73) ~ 6, # If primary, then at least 6 year (they graduated 6 years of elementary), and so on..
    dl06 %in% c(5, 6, 15, 74) ~ 9,
    dl06 %in% c(13, 60, 61) ~ 12,
    dl06 == 62 ~ 16,
    dl06 == 63 ~ 18,
    
    TRUE ~ NA_real_ # for catching NA, basically
  ))
# Checking
b3a_dl1_A1_base %>% 
  filter(dl06==74) %>% ### FILTER first
  select(hhid14, pidlink, dl06, base_educ) %>%
  print()



#-#-# Making Year of Education: base educ + dl07 #-#-#
b3a_dl1_B1_educ = b3a_dl1_A1_base %>%
  mutate(educ = case_when(
    dl07 %in% c(0, 1, 2, 3, 4, 5, 6) ~ rowSums(across(c('base_educ', 'dl07'))), # if not graduated, then add to base_educ
    dl07 == 7 & dl06 %in% c(2, 11, 72) ~ 6, # if graduated (from elementary), then 6 years educ
    dl07 == 7 & dl06 %in% c(3, 4, 12, 73) ~ 9, # if graduated (from middle school), then 9 years educ
    dl07 == 7 & dl06 %in% c(5, 6, 15, 74) ~ 12, # if graduated (from high school), then 12 years educ
    dl07 == 7 & dl06 %in% c(13, 60, 61) ~ 16, # if graduated (from bachelors), then 16 years educ
    dl07 == 7 & dl06 == 62 ~ 18, # if graduated (from master), then 18 years educ
    dl07 == 7 & dl06 == 62 ~ 20, # if graduated (from doctorate), then 18 years educ
    TRUE ~ NA_real_ 
  ))
# Checking
b3a_dl1_B1_educ %>% 
  select(hhid14, pidlink, dl06, dl07, base_educ, educ) %>% 
  print(n = 25)



#-#-# Making Highest Child's Year of Education: base educ + dl07 #-#-#
# NOT POSSIBLE, require bk_ar1 merging #


#-#-# Finalize b3a_dl1 #-#-#
b3a_dl1_final = b3a_dl1_B1_educ %>%
  select(hhid14, pidlink, educ)

############################################# Socio Status ################################################################################################################################

#----------- Socio Status (bk_ar1) -----------#

#-#-# Amazing function for summing #-#-#
sum_na_handle = function(x) {
  if (all(is.na(x))) { # ... if all variables summed are na, the sum would yield NA as well ...
    return(NA_real_)
  } else {            # ... but if some are not NA, then summation with na.rm=T is EXECUTED
    return(sum(x, na.rm=T))
  }
}



#-#-# Selecting Data #-#-#
# [PC] 
bk_ar1 = read_dta("D:/Kuliah/Playing With Data/IFLS 5/bk_ar1.dta")
# [LAPTOP]
#bk_ar1 = read_dta("D:/College/IFLS 5/bk_ar1.dta")
bk_ar1 = as_tibble(bk_ar1)
bk_ar1 = bk_ar1 %>%
  select(hhid14, pidlink, everything(), -hhid14_9, -pid14, hhid14_9, pid14) %>%  # hhid14_9 and pid14 would be in the rightmost position
  arrange(hhid14)



#-#-# Making household size (N of HHID) #-#-#
bk_ar1_A1_hhsize = bk_ar1 %>%
  group_by(hhid14) %>%
  mutate(hhsize = n())
# Checking
bk_ar1_A1_hhsize %>%
  select(hhid14, pidlink, hhsize, everything(), -hhid14_9, -pid14, hhid14_9, pid14) %>%
  print()



#-#-# Modifying gender variable #-#-#
bk_ar1_B1_gender = bk_ar1_A1_hhsize %>%
  mutate( 
    gender_male = if_else(ar07==1, 1, 0, missing = NA_real_),  # this is just like case_when, although for binary (more efficient for binaries/dummies)
    gender_fem = if_else(ar07==3, 1, 0, missing = NA_real_)
    )
# Checking
bk_ar1_B1_gender %>%
  select(hhid14, pidlink, ar07, starts_with("gender")) %>%
  print(n=25)



#-#-# Generating Income Variable #-#-#
bk_ar1_C1_income = bk_ar1_B1_gender %>%
  select(hhid14, pidlink, hhsize, starts_with("gender"), ar15a, ar15b, ar15bx, ar02b, everything()) %>%
  arrange(hhid14)

# Per Capita
bk_ar1_C1_income = bk_ar1_C1_income %>%
  mutate(incomepc = case_when(
    ar15a == 3 & !is.na(ar15b) ~ 0,
    ar15bx == 6 & !is.na(ar15b) ~ 0,
    ar15b > 999999990 ~ NA_real_,
    TRUE ~ ar15b # this is like "else:" in python
  ))
## Check (should be none)
bk_ar1_C1_income %>% 
  filter(!is.na(ar15b) & is.na(incomepc) & ar15b<99999999) %>% 
  print()
  
# Household Average Income & Parents Income
bk_ar1_C2_income = bk_ar1_C1_income %>%
  group_by(hhid14) %>%
  mutate(
    incomeavg = sum_na_handle(incomepc)/first(hhsize), # first = take the first value in each grouping
    incomeparents = sum_na_handle(if_else(ar02b %in% c(1,2), incomepc, NA_real_))
    )

## Check
bk_ar1_C2_income %>% 
  select(hhid14, pidlink, ar15b, ar02b, incomepc, incomeavg, incomeparents) %>% 
  print(n=250)



#-#-# Live in the same house #-#-#
# Pre-Checking
bk_ar1 %>% select(hhid14, pidlink, ar01a, ar18b, ar18d) %>% print(n=25) # better to use ar18b
# Generating variable
bk_ar1_D1_live = bk_ar1_C2_income %>%
  group_by(hhid14) %>%
  mutate(
    live_house = sum_na_handle(if_else(ar18b==1, ar18b, NA_real_))
    )

# Checking
bk_ar1_D1_live %>% select(hhid14, pidlink, hhsize, ar18b, live_house) %>% print(n=50)



#-#-# Final Socio Status #-#-#
bk_ar1_final = bk_ar1_D1_live %>%
  select(hhid14, pidlink, hhsize, incomeavg, incomeparents, live_house, ar02b)


############################################# Finalize Data #####################################################################################################################

#----------- Final Data Cleaning -----------#
#-#-# Remove unused things in environment #-#-#
suffix_keywords = "_final"
keep = ls()[endsWith(ls(), suffix_keywords)]
rm(list = setdiff(ls(), keep))



#-#-# Merging for highest education variable #-#-#
merged_ar1_dl1 = left_join(bk_ar1_final, b3a_dl1_final, by = c("hhid14", "pidlink"))



#-#-# Generating the variable #-#-#
# NA handling function
max_na_handle = function(x) {
  if (all(is.na(x))) { # ... if all variables summed are na, the sum would yield NA as well ...
    return(NA_real_)
  } else {            # ... but if some are not NA, then summation with na.rm=T is EXECUTED
    return(max(x, na.rm=T))
  }
}
merged_ar1_dl1 = merged_ar1_dl1 %>%
  group_by(hhid14) %>%
  mutate(highest_educ = max_na_handle(if_else(ar02b %in% c(3, 4), educ, NA_real_)))



#-#-# Last Merging #-#-#
# remove unimportant variables
merged_ar1_dl1_final = merged_ar1_dl1 %>%
  select(everything(), -ar02b, -educ, -pidlink)
# unpivot the merged data
merged_ar1_dl1_final_collapsed = merged_ar1_dl1_final %>%
  distinct(hhid14, .keep_all = TRUE)
# Merge final
family_food = left_join(b1_ks1_final, merged_ar1_dl1_final_collapsed, by = "hhid14")
# keep only the last data
keep = "family_food"
rm(list = setdiff(ls(), keep))



#-#-# Saving #-#-#
write_dta(family_food, "family_food.dta")






############################################# Regression #############################################
# Open Data
family_food = read_dta("G:/Other computers/My Laptop/Script/R/Thesis/family_food.dta")
attach(family_food)

# Regression
HEI_MLR = lm(HEI ~ incomeparents + live_house + highest_educ) 
summary(HEI_MLR)

protein_MLR = lm(protein_food ~ incomeparents + live_house + highest_educ) 
summary(protein_MLR)

fiber_MLR = lm(fiber_food ~ incomeparents + live_house + highest_educ) 
summary(fiber_MLR)




