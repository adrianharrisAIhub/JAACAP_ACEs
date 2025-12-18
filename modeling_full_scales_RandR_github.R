rm(list=ls())
options(scipen=999)

library(survey)
library(dplyr)
library(data.table)
library(tidyr)
library(gtsummary)
library(ggplot2)
library(marginaleffects)
library(tibble)
library(missRanger)
library(ggpubr)
library(tipr)
library(xgboost)
library(nnet)
library(twang)
library(gridExtra)
library(mice)
library(naniar)
library(VIM)
library(patchwork)
library(pROC)
library(MLmetrics)
library(qwraps2) 
library(janitor)
library(fastDummies)

set.seed(1234) 
n_imps <- 20

setwd("YOUR WD")

# Preprocessing 
df <-  read.csv('Data/pre_balanced_data_national_full_yrbs2023.csv') %>% select(-X)

outcomes <- c('ideation','suicideatt_qn29')

ace_variables <- c('qbasicneedsace_rc','qemoabuseace_rc', 
                   'qincarparentace_rc','qintviolenceace_rc',
                   'qlivedwabuseace_rc','qlivedwillace_rc', 
                   'qphyabuseace_rc', 'qsexabuseace_rc') 

treatment <- 'ace_count'

dgh <- c('grade_rc', 'sex_rc', 'race_eth')

confounders <- c( "safetyconcernschool_qn14", "racism_qn23","bulliedatschool_qn24",
                  "ebullied_qn25","smokeever_qn31", "vapeever_qn35", "agefirstdrink_qn41",
                  "weedever_qn46", "any_substance_abuse", "any_act")

id <- 'PSU'
sry_weight <- "weight"
time <- 'year'
cluster <- "stratum"

variable_list <- c(treatment, dgh, confounders, cluster, id, time, sry_weight )
table_list <- c(treatment, dgh, confounders)

df <- df %>% 
  mutate(
    sex_rc = as.factor(sex_rc), 
    sex_rc = relevel(sex_rc, "Male"), 
    
    race_eth = as.factor(race_eth), 
    race_eth = relevel(race_eth, "White"), 
    
    grade_rc = as.factor(grade_rc), 
    grade_rc = relevel(grade_rc, "9th grade"), 
    
    ace_count = as.factor(ace_count), 
    ace_count = relevel(ace_count, "0 ACEs")
  ) 

df <- df %>%
  select(
    outcomes,ace_count,ace_variables,  variable_list, race_eth_7
  ) 

# Missing test 
df %>%
  select(
    outcomes,ace_count,variable_list
  ) %>% 
  select(
    -year
  ) %>%
  mcar_test(.)

# Imputation of confounders  
to_imp <- df %>% select(confounders)
keep <- df %>% select(-confounders)

imp <- mice(to_imp, m = n_imps,  maxit=5)

# We can pick at random a version of the imputed dataset 
random_number <- sample(c(1:n_imps), 1)
df_imputed  <- complete(imp, random_number)

df_imputed  <- keep %>% # Same order so we can just bind the cols 
  bind_cols(
    df_imputed 
  ) 

# Table 1 
# Table by 4+ Aces on outcome, demographics and initial confounders w/ weight
weighted_design_table_overall <- svydesign(ids = ~PSU, data = df_imputed, strata = ~stratum, weights = ~weight, nest=TRUE) 
weighted_design_table_race <- svydesign(ids = ~PSU, data = df_imputed %>% filter(complete.cases(race_eth)), strata = ~stratum, weights = ~weight, nest=TRUE) 
weighted_design_table_sex <- svydesign(ids = ~PSU, data = df_imputed %>% filter(complete.cases(sex_rc)), strata = ~stratum, weights = ~weight, nest=TRUE) 

overall_table <- weighted_design_table_overall  %>%
  tbl_svysummary(
    include = c(outcomes,treatment, dgh, confounders), 
    statistic = list(
      all_continuous() ~ c("{mean} ({sd})"),
      all_categorical() ~ "{n_unweighted} ({p}%)"
    ),
    digits = list(all_continuous() ~ 1,
                  all_categorical() ~ c(0,1)),
    
    missing = "no")  %>% 
  modify_header(all_stat_cols() ~ "**{level} N = {n_unweighted}**") %>%
  add_stat_label(
    label = all_categorical() ~ "No. (%)"
  ) %>%
  add_n("{N_nonmiss_unweighted}") %>%
  add_ci(pattern = "{stat} ({ci})") 

table_1_weighted_by_race <- weighted_design_table_race  %>%
  tbl_svysummary(by = "race_eth",
                 include = c(outcomes,treatment,dgh, confounders), 
                 statistic = list(
                   all_continuous() ~ c("{mean} ({sd})"),
                   all_categorical() ~ "{n_unweighted} ({p}%)"
                 ),
                 digits = list(all_continuous() ~ 1,
                               all_categorical() ~ c(0,1)),
                 
                 missing = "no")  %>% 
  add_stat_label(
    label = all_categorical() ~ "No. (%)"
  ) %>%
  add_n("{N_nonmiss_unweighted}") %>%
  modify_header(all_stat_cols() ~ "**{level} N = {n_unweighted}**") %>%
  add_p(
    pvalue_fun = ~ style_pvalue(.x, digits = 2)
  ) %>%
  bold_p(t = 0.05) %>%
  add_ci(pattern = "{stat} ({ci})") 

table_1_weighted_by_sex <- weighted_design_table_sex %>%
  tbl_svysummary(by = "sex_rc",
                 include = c(outcomes,treatment,dgh, confounders), 
                 statistic = list(
                   all_continuous() ~ c("{mean} ({sd})"),
                   all_categorical() ~ "{n_unweighted} ({p}%)"
                 ),
                 digits = list(all_continuous() ~ 1,
                               all_categorical() ~ c(0,1)),
                 
                 missing = "no")  %>% 
  add_stat_label(
    label = all_categorical() ~ "No. (%)"
  ) %>%
  add_n("{N_nonmiss_unweighted}") %>%
  modify_header(all_stat_cols() ~ "**{level} N = {n_unweighted}**") %>%
  add_p(
    pvalue_fun = ~ style_pvalue(.x, digits = 2)
  ) %>%
  bold_p(t = 0.05) %>%
  add_ci(pattern = "{stat} ({ci})") 

combinded_weighted_table_one <- tbl_merge(tbls = list(overall_table, table_1_weighted_by_race, table_1_weighted_by_sex ))
#Overall sample by exposure on Confounders
combinded_weighted_table_one 
write.csv(combinded_weighted_table_one  %>% as.tibble() %>% replace(is.na(.), ""), "Tables/table_1_weighted_full.csv")

# Table 2 by race on outcome and all ACEs separately w/ weights 
overall_table_two <- weighted_design_table_overall  %>%
  tbl_svysummary(
    include = c(ace_variables),
    statistic = list(
      all_continuous() ~ c("{mean} ({sd})"),
      all_categorical() ~ "{n_unweighted} ({p}%)"
    ),
    digits = list(all_continuous() ~ 1,
                  all_categorical() ~ c(0,1)),
    
    missing = "no")  %>% 
  modify_header(all_stat_cols() ~ "**{level} N = {n_unweighted}**") %>%
  add_stat_label(
    label = all_categorical() ~ "No. (%)"
  ) %>%
  add_n("{N_nonmiss_unweighted}")  %>%
  add_ci(pattern = "{stat} ({ci})") 

table_2_weighted_by_race <- weighted_design_table_race  %>%
  tbl_svysummary(by = "race_eth",
                 include = c(ace_variables), 
                 statistic = list(
                   all_continuous() ~ c("{mean} ({sd})"),
                   all_categorical() ~ "{n_unweighted} ({p}%)"
                 ),
                 digits = list(all_continuous() ~ 1,
                               all_categorical() ~ c(0,1)),
                 
                 missing = "no")  %>% 
  add_stat_label(
    label = all_categorical() ~ "No. (%)"
  ) %>%
  add_n("{N_nonmiss_unweighted}") %>%
  modify_header(all_stat_cols() ~ "**{level} N = {n_unweighted}**") %>%
  add_p(
    pvalue_fun = ~ style_pvalue(.x, digits = 2)
  ) %>%
  bold_p(t = 0.05) %>%
  add_ci(pattern = "{stat} ({ci})") 

table_2_weighted_by_sex <- weighted_design_table_sex %>%
  tbl_svysummary(by = "sex_rc",
                 include = c(ace_variables),
                 statistic = list(
                   all_continuous() ~ c("{mean} ({sd})"),
                   all_categorical() ~ "{n_unweighted} ({p}%)"
                 ),
                 digits = list(all_continuous() ~ 1,
                               all_categorical() ~ c(0,1)),
                 
                 missing = "no")  %>% 
  add_stat_label(
    label = all_categorical() ~ "No. (%)"
  ) %>%
  add_n("{N_nonmiss_unweighted}") %>%
  modify_header(all_stat_cols() ~ "**{level} N = {n_unweighted}**") %>%
  add_p(
    pvalue_fun = ~ style_pvalue(.x, digits = 2)
  ) %>%
  bold_p(t = 0.05)  %>%
  add_ci(pattern = "{stat} ({ci})") 

combinded_weighted_table_two <- tbl_merge(tbls = list(overall_table_two, table_2_weighted_by_race, table_2_weighted_by_sex))

#Overall sample by race on ACE count and single ACEs and Overall sample by sex on ACE count and single ACEs
combinded_weighted_table_two
write.csv(combinded_weighted_table_two  %>% as.tibble() %>% replace(is.na(.), ""), "Tables/table_2_weighted_full.csv")

weighted_design_table_ideation <- svydesign(ids = ~PSU, data = df_imputed %>% filter(complete.cases(ideation)), strata = ~stratum, weights = ~weight, nest=TRUE) 
weighted_design_table_attempt <- svydesign(ids = ~PSU, data = df_imputed %>% filter(complete.cases(suicideatt_qn29)), strata = ~stratum, weights = ~weight, nest=TRUE) 
weighted_design_table_exposure <- svydesign(ids = ~PSU, data = df_imputed %>% filter(complete.cases(ace_count)), strata = ~stratum, weights = ~weight, nest=TRUE) 

# Raceeth by outcome
sup_dist_one <- weighted_design_table_ideation %>%
  tbl_svysummary(by = "ideation",
                 include = race_eth_7, 
                 statistic = list(
                   all_continuous() ~ c("{mean} ({sd})"),
                   all_categorical() ~ "{n_unweighted} ({p}%)"
                 ),
                 digits = list(all_continuous() ~ 1,
                               all_categorical() ~ c(0,1)),
                 
                 missing = "no")  %>% 
  add_stat_label(
    label = all_categorical() ~ "No. (%)"
  ) %>%
  add_n("{N_nonmiss_unweighted}") %>%
  modify_header(all_stat_cols() ~ "**{level} N = {n_unweighted}**") %>%
  add_p(
    pvalue_fun = ~ style_pvalue(.x, digits = 2)
  ) %>%
  bold_p(t = 0.05) %>%
  add_ci(pattern = "{stat} ({ci})") 

sup_dist_two <- weighted_design_table_attempt  %>%
  tbl_svysummary(by = "suicideatt_qn29",
                 include = race_eth_7, 
                 statistic = list(
                   all_continuous() ~ c("{mean} ({sd})"),
                   all_categorical() ~ "{n_unweighted} ({p}%)"
                 ),
                 digits = list(all_continuous() ~ 1,
                               all_categorical() ~ c(0,1)),
                 
                 missing = "no")  %>% 
  add_stat_label(
    label = all_categorical() ~ "No. (%)"
  ) %>%
  add_n("{N_nonmiss_unweighted}") %>%
  modify_header(all_stat_cols() ~ "**{level} N = {n_unweighted}**") %>%
  add_p(
    pvalue_fun = ~ style_pvalue(.x, digits = 2)
  ) %>%
  bold_p(t = 0.05) %>%
  add_ci(pattern = "{stat} ({ci})") 

# Raceeth by exposure
options(survey.lonely.psu = "adjust")

sup_dist_three <- weighted_design_table_exposure  %>%
  tbl_svysummary(by = "ace_count",
                 include = race_eth_7, 
                 statistic = list(
                   all_continuous() ~ c("{mean} ({sd})"),
                   all_categorical() ~ "{n_unweighted} ({p}%)"
                 ),
                 digits = list(all_continuous() ~ 1,
                               all_categorical() ~ c(0,1)),
                 
                 missing = "no")  %>% 
  add_stat_label(
    label = all_categorical() ~ "No. (%)"
  ) %>%
  add_n("{N_nonmiss_unweighted}") %>%
  modify_header(all_stat_cols() ~ "**{level} N = {n_unweighted}**") %>%
  add_p(
    pvalue_fun = ~ style_pvalue(.x, digits = 2)
  ) %>%
  bold_p(t = 0.05) %>%
  add_ci(pattern = "{stat} ({ci})") 

combinded_sup <- tbl_merge(tbls = list(sup_dist_one, sup_dist_two, sup_dist_three))
combinded_sup 
write.csv(combinded_sup  %>% as.tibble() %>% replace(is.na(.), ""), "Tables/combinded_sup_full.csv")

balance_tables <- full_model <- race_model <- sex_model <-  overall_rr  <- race_rr <- sex_rr <- race_compare <- sex_compare <- p_score_df <- data.frame()

for(i in 1:n_imps){
  for(j in 1:length(outcomes)){
    
    df_imputed  <- complete(imp, i)
    
    df_imputed_combined  <- keep %>%
      bind_cols(
        df_imputed 
      ) 
    
    cat("Imputed Confounders Dataset Size: ",df_imputed %>% nrow(.), "\n")
    cat("Combined Dataset Size: ",df_imputed_combined %>% nrow(.), "\n")
    
    
    tmp <- df_imputed_combined  %>%
      mutate(
        
        sex_rc = as.factor(sex_rc), 
        sex_rc = relevel(sex_rc, "Male"), 
        
        race_eth = as.factor(race_eth), 
        race_eth = relevel(race_eth, "White"), 
        
        grade_rc = as.factor(grade_rc), 
        grade_rc = relevel(grade_rc, "9th grade"), 
        
        ace_count = as.factor(ace_count), 
        ace_count = relevel(ace_count, "0 ACEs")
        
      ) %>%
      mutate(
        row_id = 1:nrow(.)
      )
    
    tmp_iptw <- tmp %>% select(row_id,treatment,sex_rc, race_eth, cluster, id, weight) %>% filter(complete.cases(.))
    
    # IPTW 
    fmla_ipw  <- paste0(treatment, "~", paste0(c("sex_rc", "race_eth"),collapse = "+"),"+", cluster, "+",  id)
    cat('Formula for IPTW Stage for outcome:', as.character(outcomes[j]),"at imp", i,  '\n')
    print(fmla_ipw)
    
    multiple_treatment_ipw  <- mnps(as.formula(fmla_ipw),
                                    data =   tmp_iptw ,
                                    estimand = "ATE",
                                    verbose = FALSE,
                                    stop.method = c("es.mean"),
                                    sampw =   tmp_iptw$weight, 
                                    version = "xgboost", 
                                    n.trees = 300)
    
    # Getting P scores from the IPTW stage in the twang package 
    # Both outcomes share the same p score model so the values will be the same at each imp and outcome combination
    
    p_score_df <-  p_score_df %>%
      bind_rows(
        
        tibble(
          ACE_0  = multiple_treatment_ipw$psList$`0 ACEs`$ps, 
          ACE_1  = multiple_treatment_ipw$psList$`1 ACE`$ps,
          ACE_2  = multiple_treatment_ipw$psList$`2 ACEs`$ps,
          ACE_3  = multiple_treatment_ipw$psList$`3 ACEs`$ps,
          ACE_4  = multiple_treatment_ipw$psList$`4+ ACEs`$ps
        ) %>%
          bind_cols(
            tmp_iptw 
          ) %>%
          dummy_cols(., select_columns = "ace_count") %>%
          clean_names() %>%
          mutate(
            
            ace_0_ipw  = (ace_count_0_ac_es / ace_0) + ((1 - ace_count_0_ac_es) / (1 - ace_0)), 
            ace_1_ipw  = (ace_count_1_ace / ace_1) + ((1 - ace_count_1_ace) / (1 - ace_1)), 
            ace_2_ipw  = (ace_count_2_ac_es / ace_2) + ((1 - ace_count_2_ac_es) / (1 - ace_2)), 
            ace_3_ipw  = (ace_count_3_ac_es / ace_3) + ((1 - ace_count_3_ac_es) / (1 - ace_3)), 
            ace_4_ipw  = (ace_count_4_ac_es / ace_4) + ((1 - ace_count_4_ac_es) / (1 - ace_4)), 
            
            ipw_twang = case_when( 
              
              ace_count == "0 ACEs" ~  ace_0_ipw, 
              ace_count == "1 ACE" ~ ace_1_ipw, 
              ace_count == "2 ACEs" ~  ace_2_ipw, 
              ace_count == "3 ACEs" ~ ace_3_ipw, 
              ace_count == "4+ ACEs" ~  ace_4_ipw, 
              
            )
            
          ) %>%
          mutate(
            
            outcome = outcomes[j], 
            imputation = i
            
          )
        
      )
    
    
    balance_tables  <- balance_tables %>%
      bind_rows(
        
        bal.table(multiple_treatment_ipw, collapse.to = 'covariate', digits = 2, subset.stop.method ="es.mean") %>% as.data.frame() %>% 
          mutate(
            outcome  = outcomes[j], 
            imputation = i 
          )
        
      )
    
    tmp_iptw$weighting <- get.weights(multiple_treatment_ipw, stop.method = "es.mean", withSampW = T) # IPW * survey weight 
    
    model_df <-  tmp %>% 
      left_join(
        tmp_iptw %>% select(row_id,weighting)
      ) %>%
      select(
        outcomes[j],treatment, dgh,confounders,PSU,stratum,weighting
      ) %>%
      filter(complete.cases(.))
    
    cat("Model Dataset Size: ",model_df %>% nrow(.), "\n")
    
    weighted_design <- svydesign(ids = ~PSU, data =  model_df, strata = ~stratum, weights = ~weighting, nest=TRUE) 
    options(survey.lonely.psu = "adjust")
    
    outcome_model_fmla <- paste0(outcomes[j],'~', 
                                 treatment, "+", 
                                 paste0(dgh,collapse = "+"), "+", 
                                 paste0(confounders,collapse = "+"))
    
    outcome_model_fmla_raceeth <- paste0(outcomes[j],'~',
                                         paste0(treatment, "*",  "race_eth" ), "+", 
                                         paste0(c("grade_rc",  "sex_rc"),collapse = "+"), "+",
                                         paste0(confounders,collapse = "+"))
    
    outcome_model_fmla_sex <- paste0(outcomes[j],'~',
                                     paste0(treatment, "*",    "sex_rc"), "+", 
                                     paste0(c("grade_rc", "race_eth"),collapse = "+"), "+",
                                     paste0(confounders,collapse = "+"))
    
    
    cat("Outcome Model for Overall-ACEs:",  outcome_model_fmla ,"at imp", i, '\n')
    cat("Outcome Model for Raceeth-ACEs:",  outcome_model_fmla_raceeth ,"at imp", i, '\n')
    cat("Outcome Model for Sex-ACEs:",  outcome_model_fmla_sex, "at imp", i,'\n')
    
    model_svy <-  svyglm(outcome_model_fmla, family = quasibinomial(link = 'logit'), design = weighted_design)
    model_svy_raceeth <- svyglm(outcome_model_fmla_raceeth, family = quasibinomial(link = 'logit'), design = weighted_design)
    model_svy_sex <-  svyglm(outcome_model_fmla_sex, family = quasibinomial(link = 'logit'), design = weighted_design)
    
    
    full_model <- full_model %>%
      bind_rows(
        
        model_svy %>%
          tbl_regression(exponentiate = T,
                         estimate_fun = purrr::partial(style_ratio, digits = 2),
                         pvalue_fun = purrr::partial(style_sigfig, digits = 2)) %>%
          add_significance_stars(
            pattern = "{estimate} ({conf.low}, {conf.high}){stars}",
            hide_se = TRUE
          ) %>%
          as.data.frame() %>%
          mutate(
            outcome = outcomes[j], 
            imputation = i
          )
        
      )
    
    race_model <- race_model %>%
      bind_rows(
        
        model_svy_raceeth %>%
          tbl_regression(exponentiate = T,
                         estimate_fun = purrr::partial(style_ratio, digits = 2),
                         pvalue_fun = purrr::partial(style_sigfig, digits = 2)) %>%
          add_significance_stars(
            pattern = "{estimate} ({conf.low}, {conf.high}){stars}",
            hide_se = TRUE
          ) %>%
          as.data.frame() %>%
          mutate(
            outcome = outcomes[j], 
            imputation = i
          )
        
      )
    
    sex_model <- sex_model %>%
      bind_rows(
        
        model_svy_sex %>%
          tbl_regression(exponentiate = T,
                         estimate_fun = purrr::partial(style_ratio, digits = 2),
                         pvalue_fun = purrr::partial(style_sigfig, digits = 2)) %>%
          add_significance_stars(
            pattern = "{estimate} ({conf.low}, {conf.high}){stars}",
            hide_se = TRUE
          ) %>%
          as.data.frame() %>%
          mutate(
            outcome = outcomes[j], 
            imputation = i
          )
        
      )
    
    race_eth_anova <- anova(model_svy, model_svy_raceeth, method = "Wald")
    sex_anova <-  anova(model_svy, model_svy_sex, method = "Wald") 
    
    
    race_compare <- race_compare %>%
      bind_rows(
        
        tibble(
          outcome = outcomes[j], 
          imputation = i, 
          test_term =  race_eth_anova$test.terms %>% as.character(), 
          p_value =  race_eth_anova$p %>% as.numeric(.)
          
        )
        
      )
    
    sex_compare <- sex_compare %>%
      bind_rows(
        
        tibble(
          outcome = outcomes[j], 
          imputation = i, 
          test_term =  sex_anova$test.terms %>% as.character(), 
          p_value =  sex_anova$p %>% as.numeric(.)
        )
        
      )
    
    overall_rr <-   overall_rr %>%
      bind_rows(
        
        avg_comparisons(model_svy,
                        variables = treatment,
                        wt = weighted_design$allprob$weight, 
                        comparison = "ratioavg",
                        hypothesis =  1) %>%
          as.data.frame() %>%
          select(
            term,contrast,std.error,  estimate, conf.low,  conf.high, p.value 
          ) %>%
          mutate(
            
            outcome = outcomes[j], 
            imputation = i, 
            n_obs  = model_df %>% nrow(.)
            
          )
        
        
      )
    
    race_rr <-   race_rr %>%
      bind_rows(
        
        avg_comparisons(model_svy, 
                        variables = treatment,
                        by = 'race_eth',
                        wt = weighted_design$allprob$weight,
                        comparison = "ratioavg", 
                        hypothesis =  1) %>%
          as.data.frame() %>% 
          select(
            term,contrast,race_eth,std.error,  estimate, conf.low,  conf.high, p.value 
          ) %>%
          mutate(
            outcome = outcomes[j], 
            imputation = i,
            n_obs  = model_df %>% nrow(.)
          )
        
        
      )
    
    sex_rr <- sex_rr %>%
      bind_rows(
        
        avg_comparisons(model_svy, 
                        variables = treatment,
                        by = 'sex_rc', 
                        wt = weighted_design$allprob$weight, 
                        comparison = "ratioavg", 
                        hypothesis =  1)  %>%
          as.data.frame() %>% 
          select(
            term,contrast,sex_rc, std.error,  estimate, conf.low,  conf.high, p.value 
          ) %>%
          mutate(
            outcome = outcomes[j], 
            imputation = i,
            n_obs  = model_df %>% nrow(.)
          )
        
        
      )
    
    
  }
}


# Balance Table - Same across  all models due to the exposure, demographics and sampling design being the same
balance_table <- balance_tables  %>% 
  group_by(var, outcome) %>%
  summarise(
    avg_max_std = mean(max.std.eff.sz)
  ) %>%
  ungroup() %>%
  arrange(outcome)

write.csv(balance_table, "Tables/balance_table_full.csv")

# Overlap Plot 
overlap_plot <- p_score_df %>%
  select(
    ace_0, ace_1,ace_2,ace_3,ace_4,  ace_count, outcome, imputation) %>%
  gather(., "var", "val",-c(outcome,  ace_count, imputation)) %>% 
  ungroup()  %>% 
  mutate(
    
    var = case_when(
      var == "ace_0" ~ "0 ACEs", 
      
      var == "ace_1" ~ "1 ACE", 
      
      
      var == "ace_2" ~ "2 ACEs", 
      
      var == "ace_3" ~ "3 ACEs", 
      
      
      var == "ace_4" ~ "4+ ACEs", 
      
    )
  ) %>% 
  filter(outcome == "ideation") %>%
  filter(imputation == random_number) %>%
  ggplot(.) +
  aes(ace_count, val) +
  geom_boxplot() +
  ylim(0,0.6) +
  theme_minimal() +
  facet_wrap(~var) +
  labs(x = "Exposure", 
       y = paste('Propensity Score'), 
       title = "Overlap of Propensity Scores") +
  theme(plot.title = element_text(hjust = 0.5,size=20),
        axis.text.x = element_text(color = "grey20", size = 10, angle = 45, hjust = 1),
        axis.text.y = element_text(color = "grey20", size = 10, angle = 0, hjust = 1 ),  
        axis.title.x = element_text(color = "grey20", size = 10, angle = 0, hjust = .5),
        axis.title.y = element_text(color = "grey20", size = 10, angle = 90, hjust = .5),
        strip.text = element_text(size = 10)) 

overlap_plot 
ggsave("Figures/overlap_plot.pdf",
       width = 15, height = 15)

# Per outcome there are 20 imputations meaning there are 20 different contrasts 
# So we pool across those imputations to the the pooled aRRs
# The number of parameters are what was observed in the full model. 
parameters <- length(model_svy$coefficients)

# Using the Rubins rules that are stated here: https://bookdown.org/mwheymans/bookmi/rubins-rules.html and here: https://stefvanbuuren.name/fimd/sec-whyandwhen.html 
# We pooled our adjusted risk ratios 

# Figure 1 
overall_rr_dataset <- overall_rr %>% 
  group_by(
    contrast, outcome 
  ) %>%
  summarise(
    # https://bookdown.org/mwheymans/bookmi/rubins-rules.html 
    # 9.1 Pooled aRRs, Rubin (1987)
    est_mean = mean(estimate), 
    
    # 9.2 Within imputation variance, Rubin (1987)
    v_w  = mean(std.error^2), 
    
    # 9.2 Between imputation variance , Rubin (1987)
    v_b = var(estimate), 
    
    # 9.2 Total variance, Rubin (1987)
    v_total = v_w + v_b + (v_b/n_imps), 
    
    # 9.2 Pooled standard error, Rubin (1987)
    se = sqrt(v_total), 
    
    # 9.3 Wald pooled - testing the hypothesis of does the estimates differ from 1 (no difference in the adjusted risk ratios), 
    # Rubin (1987), Van Buuren (2018), Marshall et al. (2009))
    w_p =  (est_mean - 1)/se, 
    
    # 10.1 Fraction of Missing Information - Lambda, Van Buuren (2018) and Enders (2010) 
    lambda = (v_b +  (v_b/n_imps))/v_total, 
    
    # 9.4 
    df_old = (n_imps - 1)/(lambda^2), 
    
    # 9.4 
    df_obs = (((n_obs - parameters ) + 1) / ((n_obs  - parameters ) + 3)) * ((n_obs  - parameters) * (1 - lambda)),  
    
    # 9.4 
    df_adj =  (df_old * df_obs)/(df_old + df_obs), 
    
    # P value calculation  
    p_val = 2 * pt(abs(w_p), df = df_adj, lower.tail = FALSE),
    
    # Finding the T value for the 95% CI, sample is large enough where the t and z dist. will be similar 
    t_value =  qt(p = 0.05/2, df = df_adj, lower.tail = FALSE), 
    
    # 9.5
    lower_bound = est_mean - t_value*se, 
    upper_bound = est_mean +  t_value*se,
  ) %>%
  ungroup() %>%
  distinct(contrast, outcome, .keep_all = T) %>%
  ungroup() %>%
  mutate(
    
    outcome = case_when(
      outcome == "ideation" ~ "Ideation", 
      outcome == "suicideatt_qn29" ~ "Attempt"
    ), 
    
    contrast = case_when(
      
      contrast ==  "mean(1 ACE) / mean(0 ACEs)" ~ "1 ACE",
      contrast ==  "mean(2 ACEs) / mean(0 ACEs)" ~ "2 ACEs",
      contrast ==  "mean(3 ACEs) / mean(0 ACEs)" ~ "3 ACEs",
      contrast ==  "mean(4+ ACEs) / mean(0 ACEs)" ~ "4+ ACEs",
      
    ), 
    outcome = factor(outcome, levels = c("Ideation","Attempt"))
  ) 

# Figure 1 
plot_overall_rr <- overall_rr_dataset  %>% 
  ggplot(.) +
  aes(contrast,  est_mean, color = outcome) + 
  theme_minimal() + 
  geom_point(position=position_dodge(width=0.5)) +
  geom_errorbar(aes(ymin= lower_bound, ymax=upper_bound),
                width = 0.2,
                position=position_dodge(width=0.5)) +
  labs(x = "Adverse Childhood Experience Catogery Comparision", 
       y = paste('Adjusted Risk Ratios'), 
       title = "Adjusted Risk Ratios of Suicidality by Adverse Childhood Experience Catogery") +
  theme(plot.title = element_text(hjust = 0.5, size = 20), 
        axis.text.x = element_text(size = 20), 
        axis.text.y = element_text(size = 20), 
        axis.title = element_text(size = 20), 
        legend.text = element_text(size = 20), 
        legend.title = element_text(size = 20))+ 
  geom_hline(yintercept=1,linetype="dashed",color = "red", size=0.3) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(color='Group') 

plot_overall_rr 
ggsave("Figures/plot_overall_rr_full.pdf",
       width = 15, height = 15)

dispar <- race_compare %>%
  group_by(
    outcome 
  ) %>%
  summarise(
    p_val_count = sum(p_value < 0.05), 
    avg_p_value = mean(p_value)
  ) %>%
  mutate(
    observed_difference = ifelse(p_val_count > 0, "Yes", "No"), 
    other_label = "Race/Ethnicity"
  )  %>%
  bind_rows(
    
    sex_compare %>%
      group_by(
        outcome 
      ) %>%
      summarise(
        p_val_count = sum(p_value < 0.05), 
        avg_p_value = mean(p_value)
      ) %>%
      mutate(
        observed_difference = ifelse(p_val_count > 0, "Yes", "No"), 
        other_label = "Sex"
      ) 
    
  ) %>%
  arrange(outcome)

# Figure 2 
race_rr_dataset <- race_rr %>%  
  group_by(
    contrast, outcome, race_eth
  ) %>%
  summarise(
    # https://bookdown.org/mwheymans/bookmi/rubins-rules.html
    # 9.1 Pooled aRRs, Rubin (1987)
    est_mean = mean(estimate), 
    
    # 9.2 Within imputation variance, Rubin (1987)
    v_w  = mean(std.error^2), 
    
    # 9.2 Between imputation variance , Rubin (1987)
    v_b = var(estimate), 
    
    # 9.2 Total variance, Rubin (1987)
    v_total = v_w + v_b + (v_b/n_imps), 
    
    # 9.2 Pooled standard error, Rubin (1987)
    se = sqrt(v_total), 
    
    # 9.3 Wald pooled - testing the hypothesis of does the estimates differ from 1 (no difference in the adjusted risk ratios), 
    # Rubin (1987), Van Buuren (2018), Marshall et al. (2009))
    w_p =  (est_mean - 1)/se, 
    
    # 10.1 Fraction of Missing Information - Lambda, Van Buuren (2018) and Enders (2010) 
    lambda = (v_b +  (v_b/n_imps))/v_total, 
    
    # 9.4 
    df_old = (n_imps - 1)/(lambda^2), 
    
    # 9.4 
    df_obs = (((n_obs - parameters) + 1) / ((n_obs  - parameters) + 3)) * ((n_obs  - parameters) * (1 - lambda)),  
    
    # 9.4 
    df_adj =  (df_old * df_obs)/(df_old + df_obs), 
    
    # P value calculation  
    p_val = 2 * pt(abs(w_p), df = df_adj, lower.tail = FALSE),
    
    # Finding the T value for the 95% CI, sample is large enough where the t and z dist. will be similar 
    t_value =  qt(p = 0.05/2, df = df_adj, lower.tail = FALSE), 
    
    # 9.5
    lower_bound = est_mean - t_value*se, 
    upper_bound = est_mean +  t_value*se,
  ) %>%
  ungroup() %>%
  distinct(contrast, outcome, race_eth, .keep_all = T)  %>%
  left_join( 
    dispar %>% filter(other_label == "Race/Ethnicity"), 
    
  ) %>% 
  mutate(
    
    outcome = case_when(
      outcome == "ideation" ~ "Ideation", 
      outcome == "suicideatt_qn29" ~ "Attempt"
    ), 
    
    contrast = case_when(
      
      contrast ==  "mean(1 ACE) / mean(0 ACEs)" ~ "1 ACE",
      contrast ==  "mean(2 ACEs) / mean(0 ACEs)" ~ "2 ACEs",
      contrast ==  "mean(3 ACEs) / mean(0 ACEs)" ~ "3 ACEs",
      contrast ==  "mean(4+ ACEs) / mean(0 ACEs)" ~ "4+ ACEs",
      
    ), 
    outcome = factor(outcome, levels = c("Ideation","Attempt"))
  )

plot_race_eth_rr <- race_rr_dataset %>% 
  ggplot(.) +
  aes(contrast, est_mean, color =  race_eth) +
  theme_minimal() + 
  geom_point(position=position_dodge(width=0.8),aes(shape = observed_difference)) +
  geom_errorbar(aes(ymin=lower_bound, ymax= upper_bound ),
                width = 0.8,
                position=position_dodge(width=0.8)) +
  labs(x = NULL, 
       y = paste('Adjusted Risk Ratios'), 
       title = "Adjusted Risk Ratios of Suicidality by Adverse Childhood Experience Catogery") +
  facet_wrap(~outcome) +
  theme(plot.title = element_text(hjust = 0.5, size = 20), 
        axis.text.x = element_text(size = 20), 
        axis.text.y = element_text(size = 20), 
        axis.title = element_text(size = 20), 
        legend.text = element_text(size = 20), 
        legend.title = element_text(size = 20), 
        strip.text = element_text(size = 20)) + 
  geom_hline(yintercept=1,linetype="dashed",color = "red", size=0.3) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(color='Group',  shape = "Moderation Effect Observed") 


# Figure 3 
sex_rr_dataset <- sex_rr %>%  
  group_by(
    contrast, outcome ,sex_rc
  ) %>%
  summarise(
    # https://bookdown.org/mwheymans/bookmi/rubins-rules.html
    # 9.1 Pooled aRRs, Rubin (1987)
    est_mean = mean(estimate), 
    
    # 9.2 Within imputation variance, Rubin (1987)
    v_w  = mean(std.error^2), 
    
    # 9.2 Between imputation variance , Rubin (1987)
    v_b = var(estimate), 
    
    # 9.2 Total variance, Rubin (1987)
    v_total = v_w + v_b + (v_b/n_imps), 
    
    # 9.2 Pooled standard error, Rubin (1987)
    se = sqrt(v_total), 
    
    # 9.3 Wald pooled - testing the hypothesis of does the estimates differ from 1 (no difference in the adjusted risk ratios), 
    # Rubin (1987), Van Buuren (2018), Marshall et al. (2009))
    w_p =  (est_mean - 1)/se, 
    
    # 10.1 Fraction of Missing Information - Lambda, Van Buuren (2018) and Enders (2010) 
    lambda = (v_b +  (v_b/n_imps))/v_total, 
    
    # 9.4 
    df_old = (n_imps - 1)/(lambda^2), 
    
    # 9.4 
    df_obs = (((n_obs - parameters) + 1) / ((n_obs  - parameters) + 3)) * ((n_obs  - parameters) * (1 - lambda)),  
    
    # 9.4 
    df_adj =  (df_old * df_obs)/(df_old + df_obs), 
    
    # P value calculation  
    p_val = 2 * pt(abs(w_p), df = df_adj, lower.tail = FALSE),
    
    # Finding the T value for the 95% CI, sample is large enough where the t and z dist. will be similar 
    t_value =  qt(p = 0.05/2, df = df_adj, lower.tail = FALSE), 
    
    # 9.5
    lower_bound = est_mean - t_value*se, 
    upper_bound = est_mean +  t_value*se,
  ) %>%
  ungroup() %>%
  distinct(contrast, outcome, sex_rc, .keep_all = T)  %>%
  ungroup() %>%
  left_join( 
    dispar %>% filter(other_label == "Sex"),  
    
  ) %>%
  mutate(
    
    outcome = case_when(
      outcome == "ideation" ~ "Ideation", 
      outcome == "suicideatt_qn29" ~ "Attempt"
    ), 
    
    contrast = case_when(
      
      contrast ==  "mean(1 ACE) / mean(0 ACEs)" ~ "1 ACE",
      contrast ==  "mean(2 ACEs) / mean(0 ACEs)" ~ "2 ACEs",
      contrast ==  "mean(3 ACEs) / mean(0 ACEs)" ~ "3 ACEs",
      contrast ==  "mean(4+ ACEs) / mean(0 ACEs)" ~ "4+ ACEs",
      
    ), 
    
    outcome = factor(outcome, levels = c("Ideation","Attempt"))
  ) 

plot_sex_rr <- sex_rr_dataset %>% 
  ggplot(.) +
  aes(contrast,  est_mean, color =  sex_rc) +
  theme_minimal() + 
  geom_point(position=position_dodge(width=0.8),aes( shape = observed_difference)) +
  geom_errorbar(aes(ymin=lower_bound , ymax= upper_bound),
                width = 0.8,
                position=position_dodge(width=0.8)) +
  labs(x = NULL, 
       y = paste('Adjusted Risk Ratios'), 
       title = "Adjusted Risk Ratios of Suicidality by Adverse Childhood Experience Catogery") +
  facet_wrap(~outcome) +
  theme(plot.title = element_text(hjust = 0.5, size = 20), 
        axis.text.x = element_text(size = 20), 
        axis.text.y = element_text(size = 20), 
        axis.title = element_text(size = 20), 
        legend.text = element_text(size = 20), 
        legend.title = element_text(size = 20), 
        strip.text = element_text(size = 20)) + 
  geom_hline(yintercept=1,linetype="dashed",color = "red", size=0.3) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(color='Group',  shape = "Moderation Effect Observed") 

# plot_overall_rr 
plot_demo <- (plot_race_eth_rr / plot_sex_rr)
plot_demo

ggsave("Figures/plot_demo_full.pdf",
       width = 15, height = 15)

# ARR - Overall 
arr_table <- overall_rr_dataset %>%
  mutate(
    est_mean = format(round(est_mean, 2), nsmall  = 2), 
    lower_bound = format(round(lower_bound, 2), nsmall  = 2), 
    upper_bound =  format(round(upper_bound, 2), nsmall  = 2), 
    combine_est =  paste(est_mean, paste0("(", lower_bound, ",", upper_bound, ")") )
  ) %>%
  select(
    contrast, outcome, combine_est 
  ) %>%
  mutate(
    type = "Overall"
  ) %>%
  bind_rows(
    
    race_rr_dataset %>%
      mutate(
        est_mean = format(round(est_mean, 2), nsmall  = 2), 
        lower_bound = format(round(lower_bound, 2), nsmall  = 2), 
        upper_bound =  format(round(upper_bound, 2), nsmall  = 2), 
        combine_est =  paste(est_mean, paste0("(", lower_bound, ",", upper_bound, ")") )
      ) %>%
      select(
        contrast, outcome, type = race_eth, combine_est 
      )
    
  ) %>%
  bind_rows(
    sex_rr_dataset %>%
      mutate(
        est_mean = format(round(est_mean, 2), nsmall  = 2), 
        lower_bound = format(round(lower_bound, 2), nsmall  = 2), 
        upper_bound =  format(round(upper_bound, 2), nsmall  = 2), 
        combine_est =  paste(est_mean, paste0("(", lower_bound, ",", upper_bound, ")") )
      ) %>%
      select(
        contrast, outcome, type = sex_rc, combine_est 
      )
    
  ) %>%
  pivot_wider(id_cols = c("outcome", "type"), 
              names_from = contrast, 
              values_from = c(combine_est)) %>%
  arrange(outcome)

arr_table
write.csv(arr_table, "Tables/arr_table_full.csv")

dis_table <- dispar %>%
  mutate(
    outcome = case_when(
      outcome == "suicideatt_qn29" ~ "Attempt", 
      outcome == "ideation" ~ "Ideation"
    ), 
    outcome = factor(outcome, levels = c("Ideation","Attempt")),
    avg_p_value  = format(round( avg_p_value , 2), nsmall = 2), 
    
  ) %>%
  arrange(outcome)

write.csv(dis_table , "Tables/dis_table.csv")
