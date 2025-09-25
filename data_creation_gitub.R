rm(list=ls())
options(scipen=999)

library(survey)
library(dplyr)
library(data.table)
library(haven)
library(lubridate)
library(tidyr)
library(gtsummary)
library(ggplot2)
library(marginaleffects)
library(margins)
library(sjPlot)
library(sandwich)
library(lme4)
library(tibble)
library(caret)
library(pROC)
library(MLmetrics)
library(missRanger)
library(tableone)

# Full scale data
df_full <- read.csv('yrbs2023_full_scale.csv') %>%  filter(year == "2023")

outcomes <- c('q27', 'q29')

ace_variables <- c('qbasicneedsace','qemoabuseace', 
                   'qincarparentace','qintviolenceace',
                   'qlivedwabuseace','qlivedwillace', 
                   'qphyabuseace', 'qsexabuseace') 

dgh <- c('grade', 'sex', 'race4')

confounders <- c( "q14", "q23","q24","q25","q26","q31", "q35", "q41",
                  "q46", "q52", "q53", "q49","q76", "q77", "q78")

id <- 'PSU'
sry_weight <- "weight"
time <- 'year'
cluster <- "stratum"

df_full <- df_full %>%
  select(
    id, time,cluster, outcomes,ace_variables, dgh, 
    confounders, sry_weight
  )

# Exposure 
df_full <- df_full %>% 
  mutate(
    qbasicneedsace_rc = case_when(
      qbasicneedsace %in% c(1,2)  ~  1, 
      qbasicneedsace %in% c(3,4,5) ~ 0
    ), 
    
    qemoabuseace_rc = case_when(
      qemoabuseace %in% c(2,3,4,5) ~ 1,
      qemoabuseace %in% c(1) ~ 0,
    ),
    
    qincarparentace_rc = case_when(
      qincarparentace == 1 ~ 1, 
      qincarparentace == 2 ~ 0
    ), 
    
    qintviolenceace_rc = case_when(
      qintviolenceace %in% c(2,3,4,5) ~ 1,
      qintviolenceace %in% c(1) ~ 0,
    ), 
    
    qlivedwabuseace_rc = case_when(
      qlivedwabuseace == 1 ~ 1, 
      qlivedwabuseace == 2 ~ 0
    ), 
    
    qlivedwillace_rc = case_when(
      qlivedwillace == 1 ~ 1, 
      qlivedwillace == 2 ~ 0
    ),
    
    qphyabuseace_rc = case_when(
      qphyabuseace %in% c(2,3,4,5) ~ 1,
      qphyabuseace %in% c(1) ~ 0,
    ), 
    
    qsexabuseace_rc = case_when(
      qsexabuseace == 1 ~ 1, 
      qsexabuseace == 2 ~ 0
    )
    
  )  %>% 
  mutate(num_non_missing = rowSums(!is.na(select(.,qbasicneedsace_rc, qemoabuseace_rc,
                                                 qincarparentace_rc, qintviolenceace_rc, 
                                                 qlivedwabuseace_rc,  qlivedwillace_rc,
                                                 qphyabuseace_rc, qsexabuseace_rc )))) %>%
  mutate(
    ace_count = case_when(
      num_non_missing >= 5 ~ rowSums(select(.,qbasicneedsace_rc, qemoabuseace_rc,
                                            qincarparentace_rc, qintviolenceace_rc, 
                                            qlivedwabuseace_rc,  qlivedwillace_rc,
                                            qphyabuseace_rc, qsexabuseace_rc), na.rm = T)
      
    ),
    
    ace_count_cdc = case_when(
      ace_count == 0 ~ "0 ACEs", 
      ace_count == 1 ~ "1 ACE", 
      ace_count %in% c(2,3) ~ "2 or 3 ACEs", 
      ace_count >= 4 ~ "4+ ACEs", 
    ),
    
    ace_count = case_when(
      ace_count == 0 ~ "0 ACEs", 
      ace_count == 1 ~ "1 ACE", 
      ace_count == 2  ~ "2 ACEs",
      ace_count == 3 ~  "3 ACEs", 
      ace_count >= 4 ~ "4+ ACEs", 
    )
    
    
  ) 


df_full <-  df_full %>% 
  mutate(
    # Outcomes 
    thinksuicide_qn27  = case_when(
      q27 == 1 ~ 1, 
      q27 == 2 ~ 0, 
    ), 
    
    suicideatt_qn29 = case_when(
      q29 %in% c(2,3,4,5) ~ 1, 
      q29 == 1 ~ 0, 
    ),
    
    # Demographics 
    grade_rc = case_when(
      
      grade == 1 ~  "9th grade", 
      grade == 2 ~  "10th grade",
      grade == 3 ~  "11th grade",
      grade == 4 ~  "12th grade"
      
    ), 
    
    sex_rc = case_when(
      sex == 1 ~ "Female", 
      sex == 2 ~ "Male", 
    ),
    
    race_eth = case_when(
      
      race4 == 1 ~ "White", 
      race4 == 2  ~ "Black or African American", 
      race4 == 3 ~ "Hispanic/Latino", 
      race4 == 4  ~ "All Other Races"
      
    ), 
    
    # Confounders
    safetyconcernschool_qn14 = case_when(
      q14 == 1 ~ 0, 
      q14 %in% c(2,3,4,5) ~ 1, 
    ), 
    
    racism_qn23 = case_when(
      q23 == 1 ~ 0, 
      q23 %in% c(2,3,4,5) ~ 1, 
    ), 
    
    bulliedatschool_qn24 = case_when(
      q24 == 1 ~ 1, 
      q24 == 2 ~ 0, 
    ), 
    
    ebullied_qn25 =  case_when(
      q25 == 1 ~ 1, 
      q25 == 2 ~ 0, 
    ), 
    
    sad_qn26 = case_when(
      q26 == 1 ~ 1, 
      q26 == 2 ~ 0, 
    ), 
    
    smokeever_qn31 = case_when(
      q31 == 1 ~ 1, 
      q31 == 2 ~ 0,
    ), 
    
    vapeever_qn35 = case_when(
      q35 == 1 ~ 1, 
      q35 == 2 ~ 0,
    ), 
    
    agefirstdrink_qn41 =  case_when(
      q41 %in% c(2,3,4) ~ 1,
      q41 %in% c(1,5,6,7) ~ 0
      
    ), 
    
    weedever_qn46 =  case_when(
      q46 %in%  c(2,3,4,5,6,7) ~ 1,
      q46 == 1 ~ 0  
    ), 
    
    heroinever_qn52 = case_when(
      q52 %in% c(2,3,4,5,6) ~ 1,
      q52 == 1 ~ 0
    ), 
    
    methever_qn53 = case_when(
      q52 %in% c(2,3,4,5,6) ~ 1, 
      q53 == 1 ~ 0
    ),
    
    painmeds_qn49 = case_when(
      q49 %in% c(2,3,4,5,6) ~ 1, 
      q49 == 1 ~ 0
    ), 
    
    physicallyactive_qn76 =  case_when(
      q76 %in% c(6,7,8) ~ 1, 
      q76 %in% c(1,2,3,4,5) ~ 0 
    ), 
    
    PEattendance_qn77 = case_when(
      q77 %in% c(2,3,4,5,6) ~ 1, 
      q77 == 1 ~ 0, 
      
    ), 
    
    playsports_qn78 = case_when(
      q78 %in% c(2,3,4) ~ 1,
      q78 == 1 ~ 0 
    )
    
    
  )

df_full <- df_full %>%
  mutate(
    
    ideation = case_when(
      
      thinksuicide_qn27 == 1 & suicideatt_qn29 == 0 ~ 1,
      
      thinksuicide_qn27 == 1 & suicideatt_qn29 == 1 ~ 0,
      
      thinksuicide_qn27 == 0 ~ 0
    ),
    
    any_substance_abuse  = case_when(
      
      heroinever_qn52 == 1 ~ 1, 
      
      methever_qn53 == 1 ~ 1, 
      
      painmeds_qn49 == 1 ~ 1,
      
      heroinever_qn52 == 0 ~ 0, 
      
      methever_qn53 == 0 ~ 0, 
      
      painmeds_qn49 == 0 ~ 0
      
    ), 
    
    any_act = case_when(
      
      physicallyactive_qn76 == 1 ~ 1, 
      
      playsports_qn78 == 1 ~ 1, 
      
      PEattendance_qn77 == 1 ~ 1, 
      
      physicallyactive_qn76 == 0 ~ 0, 
      
      playsports_qn78 == 0 ~ 0, 
      
      PEattendance_qn77 == 0 ~ 0
    )
    
  ) %>%
  select(
    -thinksuicide_qn27, -heroinever_qn52, - methever_qn53,-painmeds_qn49, 
    -physicallyactive_qn76, -playsports_qn78, - PEattendance_qn77
  )

df_full %>% glimpse()
write.csv(df_full, 'Data/pre_balanced_data_national_full_yrbs2023.csv')
