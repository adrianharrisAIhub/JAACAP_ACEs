rm(list=ls())
options(scipen=999)

scaleFUN <- function(x) sprintf("%.2f", x)

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
library(SuperLearner)
library(janitor)
library(fastDummies)
library(WeightIt)

set.seed(1234) 
n_imps <- 10
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
                  "ebullied_qn25","sad_qn26","smokeever_qn31", "vapeever_qn35", "agefirstdrink_qn41",
                  "weedever_qn46", "any_substance_abuse", "any_act")

id <- 'PSU'
sry_weight <- "weight"
time <- 'year'
cluster <- "stratum"

variable_list <- c(treatment, dgh , confounders,cluster, id, time, sry_weight )
table_list <- c(treatment, dgh , confounders)

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

variable_list <- c(treatment, dgh , confounders,cluster, id, time, sry_weight )
table_list <- c(treatment, dgh , confounders)

df <- df %>%
  select(
    outcomes,ace_count,ace_variables,  variable_list
  ) 

# Imputation of Confounders  
to_imp <- df %>% select(confounders)
keep <- df %>% select(-confounders)

imp <- mice(to_imp, m = n_imps,  maxit=5)

# We can pick at random for a version of the imputed dataset 
random_number <- sample(c(1:n_imps), 1)

df_imputed  <- complete(imp, random_number)

df_imputed  <- keep %>%
  bind_cols(
    df_imputed 
  ) 

boots <- 1000
frac <- 400

paper_rr <- paper_rr_glm <-  boot_aRRs  <- model_eval <-   package_diff <-   balance_table  <-  balance_table_glm <-  balance_table_gbm <- data.frame()

for(i in 1:length(outcomes)){
  
  tmp <- df_imputed %>%
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
  
  tmp_iptw <- tmp %>% 
    select(row_id,treatment,sex_rc, 
           race_eth, cluster, 
           id, weight) %>% 
    filter(complete.cases(.)) %>%
    mutate(stratum = as.factor(stratum), 
           PSU = as.factor(PSU)) 
  
  # IPTW  - Formula 
  fmla_ipw  <- paste0(treatment, "~", paste0(c("sex_rc", "race_eth"),collapse = "+"),"+", cluster, "+",  id)
  cat('Formula for IPTW Stage for outcome:', as.character(outcomes[i]), '\n')
  print(fmla_ipw)
  
  # IPTW  - XGBoost 
  multiple_treatment_ipw  <- mnps(as.formula(fmla_ipw),
                                  data =   tmp_iptw ,
                                  estimand = "ATE",
                                  verbose = FALSE,
                                  stop.method = c("es.mean"),
                                  sampw =   tmp_iptw$weight, 
                                  version = "xgboost", 
                                  n.trees = 300)
  
  # Getting P scores from the IPTW stage in the twang package 
  # Keeping this static since both outcomes share the same p score model
  p_score_df <- tibble(
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
    clean_names()
  
  # GLM predictions - Using a simpler model 
  p_score_df <-  p_score_df %>%
    bind_cols(
      ace_0_glm = predict(glm(ace_count_0_ac_es ~ sex_rc + race_eth + stratum  + psu , family = "binomial", data = p_score_df), type = "response"),
      ace_1_glm = predict(glm(ace_count_1_ace ~ sex_rc + race_eth + stratum  + psu ,family = "binomial", data = p_score_df), type = "response"),
      ace_2_glm = predict(glm(ace_count_2_ac_es ~ sex_rc + race_eth + stratum  + psu ,family = "binomial", data = p_score_df), type = "response"),
      ace_3_glm = predict(glm(ace_count_3_ac_es ~ sex_rc + race_eth + stratum  + psu ,family = "binomial", data = p_score_df), type = "response"),
      ace_4_glm = predict(glm(ace_count_4_ac_es ~ sex_rc + race_eth + stratum  + psu , family = "binomial",data = p_score_df), type = "response")
    )
  
  p_score_df <-  p_score_df %>%
    mutate(
      
      ace_0_ipw  = (ace_count_0_ac_es / ace_0) + ((1 - ace_count_0_ac_es) / (1 - ace_0)), 
      ace_1_ipw  = (ace_count_1_ace / ace_1) + ((1 - ace_count_1_ace) / (1 - ace_1)), 
      ace_2_ipw  = (ace_count_2_ac_es / ace_2) + ((1 - ace_count_2_ac_es) / (1 - ace_2)), 
      ace_3_ipw  = (ace_count_3_ac_es / ace_3) + ((1 - ace_count_3_ac_es) / (1 - ace_3)), 
      ace_4_ipw  = (ace_count_4_ac_es / ace_4) + ((1 - ace_count_4_ac_es) / (1 - ace_4)), 
      
      ace_0_ipw_glm  = (ace_count_0_ac_es / ace_0_glm) + ((1 - ace_count_0_ac_es) / (1 - ace_0_glm)), 
      ace_1_ipw_glm  = (ace_count_1_ace / ace_1_glm) + ((1 - ace_count_1_ace) / (1 - ace_1_glm)), 
      ace_2_ipw_glm  = (ace_count_2_ac_es / ace_2_glm) + ((1 - ace_count_2_ac_es) / (1 - ace_2_glm)), 
      ace_3_ipw_glm  = (ace_count_3_ac_es / ace_3_glm) + ((1 - ace_count_3_ac_es) / (1 - ace_3_glm)), 
      ace_4_ipw_glm  = (ace_count_4_ac_es / ace_4_glm) + ((1 - ace_count_4_ac_es) / (1 - ace_4_glm)), 
      
      ipw_twang = case_when( # Way to double check math from the values in the package 
        
        ace_count == "0 ACEs" ~  ace_0_ipw, 
        ace_count == "1 ACE" ~ ace_1_ipw, 
        ace_count == "2 ACEs" ~  ace_2_ipw, 
        ace_count == "3 ACEs" ~ ace_3_ipw, 
        ace_count == "4+ ACEs" ~  ace_4_ipw, 
        
      ), 
      
      ipw_glm = case_when(
        
        ace_count == "0 ACEs" ~ ace_0_ipw_glm, 
        ace_count == "1 ACE" ~ ace_1_ipw_glm, 
        ace_count == "2 ACEs" ~ ace_2_ipw_glm, 
        ace_count == "3 ACEs" ~  ace_3_ipw_glm, 
        ace_count == "4+ ACEs" ~  ace_4_ipw_glm, 
        
      )
      
    )
  
  
  tmp_iptw$weighting <- get.weights(multiple_treatment_ipw, stop.method = "es.mean", withSampW = T) # Output: IPW * survey weight 
  tmp_iptw$weighting_glm <- p_score_df$ipw_glm * p_score_df$weight # Different dataframe but same people
  
  model_df <-  tmp %>% 
    left_join(
      tmp_iptw %>% select(row_id,weighting, weighting_glm)
    ) %>%
    select(
      outcomes[i],treatment, dgh,confounders,PSU,stratum,weighting, weighting_glm
    ) %>%
    filter(complete.cases(.))
  
  weighted_design <- svydesign(ids = ~PSU, data =  model_df, strata = ~stratum, weights = ~weighting, nest=TRUE) 
  weighted_design_glm  <- svydesign(ids = ~PSU, data =  model_df, strata = ~stratum, weights = ~weighting_glm, nest=TRUE) 
  
  options(survey.lonely.psu = "adjust")
  
  # Counterfactual Grid - This expands the dataset where people have their counterfactual selfs 
  org <- weighted_design$variables %>% 
    mutate(
      row_id = 1:nrow(.)
    )
  
  exposures <- org %>% pull(ace_count) %>% unique(.) 
  
  grid_data <- cbind(org , repped = rep(row.names(org), each = exposures %>% length()))  %>%
    arrange(row_id) %>%
    group_by(row_id) %>%
    mutate(
      ace_count = exposures
    ) %>%
    ungroup()
  
  weighted_grid <- svydesign(ids = ~PSU, data = grid_data, strata = ~stratum, weights = ~weighting, nest=TRUE) 
  weighted_grid_glm <- svydesign(ids = ~PSU, data = grid_data, strata = ~stratum, weights = ~weighting_glm, nest=TRUE) # should be the same people so just using the glm weights for this weighted obj.
  
  options(survey.lonely.psu = "adjust")
  
  outcome_model_fmla <- paste0(outcomes[i],'~', 
                               treatment, "+", 
                               paste0(dgh,collapse = "+"), "+", 
                               paste0(confounders,collapse = "+"))
  
  cat("Outcome Model for Overall-ACEs:",  outcome_model_fmla , '\n')
  
  Y <- model_df[, outcomes[i]]
  X <- model_df[, c( treatment, dgh,confounders,  "stratum", "PSU")]
  X <- X %>% dummy_cols(., remove_first_dummy = TRUE)  %>% clean_names() %>% select(-ace_count, -grade_rc, -sex_rc, -race_eth, -stratum, -psu) 
  wt <-  model_df[, "weighting"]
  
  model_svy <-  svyglm(outcome_model_fmla, family = quasibinomial(link = 'logit'), design = weighted_design)
  model_svy_glm <-  svyglm(outcome_model_fmla, family = quasibinomial(link = 'logit'), design = weighted_design_glm)
  
  paper_rr <-  paper_rr %>%
    bind_rows(
      avg_comparisons(model_svy,
                      variables = treatment,
                      wt = weighted_design$allprob$weight, 
                      comparison = "lnratioavg", 
                      transform = "exp") %>%
        as.data.frame() %>% 
        select(
          term,contrast, estimate, conf.low,  conf.high, p.value 
        ) %>%
        mutate(
          outcome = outcomes[i]
        )
      
    )
  
  paper_rr_glm <- paper_rr_glm %>%
    bind_rows(
      avg_comparisons(model_svy_glm, 
                      variables = treatment,
                      wt = weighted_design$allprob$weight, 
                      comparison = "lnratioavg", 
                      transform = "exp") %>%
        as.data.frame() %>% 
        select(
          term,contrast, estimate, conf.low,  conf.high, p.value 
        ) %>%
        mutate(
          outcome = outcomes[i]
        )
      
    )
  
  # XgBoost 
  xgb <- xgb.DMatrix(data = as.matrix(X), label = Y)
  
  xgb_params <- list(
    booster = "gbtree",
    eta = 0.01,
    max_depth = 8,
    gamma = 4,
    subsample = 0.75,
    colsample_bytree = 1,
    objective = "binary:logistic"
    
  )
  
  xgboost_model_fitted <- xgboost(params = xgb_params,
                                  data =  xgb,
                                  nrounds = 1000, 
                                  verbose = 0, 
                                  weight =  wt)
  
  xgb_preds <- predict(xgboost_model_fitted, as.matrix(X), reshape = TRUE)
  
  X_grid <- grid_data[, c( treatment, dgh,confounders,  "stratum", "PSU")]
  X_grid <- X_grid %>% dummy_cols(., remove_first_dummy = TRUE)  %>% clean_names() %>% select(-ace_count, -grade_rc, -sex_rc, -race_eth, -stratum, -psu) 
  
  # Approach 3 - Grid model
  fitted_vector_xgboost_grid <-  predict(xgboost_model_fitted, as.matrix(X_grid), reshape = TRUE)
  
  # Saving fitted values
  # Base/Approach 1 - Fitted model
  outcome_vector <- model_svy$model[outcomes[i]] %>% unlist() %>% as.vector() 
  fitted_vector <-  model_svy$fitted.values %>% as.vector()
  
  # Approach 2 - Fitted model
  outcome_vector_glm <- model_svy_glm$model[outcomes[i]] %>% unlist() %>% as.vector() 
  fitted_vector_glm <-  model_svy_glm$fitted.values %>% as.vector()
  
  # Approach 1 - Grid model
  outcome_vector_grid  <-  grid_data[outcomes[i]] %>% unlist() %>% as.vector() 
  fitted_vector_grid <-  predict(model_svy, newdata = weighted_grid, type = "response") %>%
    as.data.frame() %>%
    pull(response)
  
  # Approach 2 - Grid Model 
  outcome_vector_grid_glm  <-  grid_data[outcomes[i]] %>% unlist() %>% as.vector() 
  fitted_vector_grid_glm <-  predict(model_svy_glm, newdata = weighted_grid, type = "response") %>%
    as.data.frame() %>%
    pull(response)
  
  # Base/Approach 1: Model Metrics 
  fitted_model_matrix <- confusion_matrix(truth = outcome_vector, predicted = fitted_vector, boot = TRUE, boot_samples = 1000L, alpha = 0.05)
  grid_model_matrix <- confusion_matrix(truth = outcome_vector_grid, predicted = fitted_vector_grid, boot = TRUE, boot_samples = 1000L, alpha = 0.05)
  
  # Approach 2: Model Metrics 
  fitted_model_matrix_glm <- confusion_matrix(truth = outcome_vector_glm, predicted = fitted_vector_glm, boot = TRUE, boot_samples = 1000L, alpha = 0.05)
  grid_model_matrix_glm <- confusion_matrix(truth = outcome_vector_grid_glm, predicted = fitted_vector_grid_glm, boot = TRUE, boot_samples = 1000L, alpha = 0.05)
  
  # Approach 3: Model Metrics 
  fitted_model_xgb_matrix <- confusion_matrix(truth = outcome_vector, predicted = xgb_preds, boot = TRUE, boot_samples = 1000L, alpha = 0.05)
  grid_model_xgb_matrix <- confusion_matrix(truth = outcome_vector_grid, predicted = fitted_vector_xgboost_grid, boot = TRUE, boot_samples = 1000L, alpha = 0.05)
  
  model_eval <- model_eval %>%
    bind_rows(
      
      tibble(
        type = 'Approach 1: Fitted Model', 
        roc =  fitted_model_matrix$auroc, 
        roc_lower =   fitted_model_matrix$auroc_ci[1],
        roc_upper =   fitted_model_matrix$auroc_ci[2],
        pr =  fitted_model_matrix$auprc, 
        pr_lower =   fitted_model_matrix$auprc_ci[1],
        pr_upper =   fitted_model_matrix$auprc_ci[2],
        outcome = outcomes[i]
      )  %>% 
        bind_rows(
          
          tibble(
            type = 'Approach 1: Grid Model', 
            roc =  grid_model_matrix$auroc, 
            roc_lower =  grid_model_matrix$auroc_ci[1],
            roc_upper =   grid_model_matrix$auroc_ci[2],
            pr =  grid_model_matrix$auprc, 
            pr_lower =  grid_model_matrix$auprc_ci[1],
            pr_upper =   grid_model_matrix$auprc_ci[2],
            outcome = outcomes[i]
          ) 
          
        )  %>% 
        bind_rows(
          
          tibble(
            type = 'Approach 2: Fitted Model', 
            roc =  fitted_model_matrix_glm$auroc, 
            roc_lower =   fitted_model_matrix_glm$auroc_ci[1],
            roc_upper =   fitted_model_matrix_glm$auroc_ci[2],
            pr =  fitted_model_matrix_glm$auprc, 
            pr_lower =   fitted_model_matrix_glm$auprc_ci[1],
            pr_upper =   fitted_model_matrix_glm$auprc_ci[2],
            outcome = outcomes[i]
          )
          
        ) %>%
        bind_rows(
          
          tibble(
            type = 'Approach 2: Grid Model', 
            roc =  grid_model_matrix_glm$auroc, 
            roc_lower =  grid_model_matrix_glm$auroc_ci[1],
            roc_upper =   grid_model_matrix_glm$auroc_ci[2],
            pr =  grid_model_matrix_glm$auprc, 
            pr_lower =  grid_model_matrix_glm$auprc_ci[1],
            pr_upper =   grid_model_matrix_glm$auprc_ci[2],
            outcome = outcomes[i]
          ) 
          
        ) %>%
        bind_rows(
          
          tibble(
            type = 'Approach 3: Fitted Model', 
            roc =    fitted_model_xgb_matrix$auroc, 
            roc_lower =   fitted_model_xgb_matrix$auroc_ci[1],
            roc_upper =    fitted_model_xgb_matrix$auroc_ci[2],
            pr =    fitted_model_xgb_matrix$auprc, 
            pr_lower =    fitted_model_xgb_matrix$auprc_ci[1],
            pr_upper =     fitted_model_xgb_matrix$auprc_ci[2],
            outcome = outcomes[i]
          ) 
          
        ) %>% bind_rows(
          
          tibble(
            type = 'Approach 3: Grid Model', 
            roc =    grid_model_xgb_matrix$auroc, 
            roc_lower =   grid_model_xgb_matrix$auroc_ci[1],
            roc_upper =    grid_model_xgb_matrix$auroc_ci[2],
            pr =    grid_model_xgb_matrix$auprc, 
            pr_lower =    grid_model_xgb_matrix$auprc_ci[1],
            pr_upper =     grid_model_xgb_matrix$auprc_ci[2],
            outcome = outcomes[i]
          ) 
          
        )
      
      
    )
  
  intial_model_output <- tibble(
    outcome_name = outcomes[i], 
    exposure = model_df$ace_count, 
    outcome = outcome_vector, 
    prob_svy = fitted_vector, 
    prob_xgboost = xgb_preds, 
    race =  model_df$race_eth, 
    sex =  model_df$sex_rc
  )
  
  grid_prediction_output <- tibble(
    outcome_name = outcomes[i], 
    exposure = grid_data$ace_count, 
    outcome = outcome_vector_grid, 
    prob_svy = fitted_vector_grid, 
    prob_xgboost = fitted_vector_xgboost_grid, 
    race = grid_data$race_eth, 
    sex = grid_data$sex_rc
  )
  
  for(j in 1:boots){
    
    grid_prediction_output_resample <-  grid_prediction_output %>% sample_n(.,  round(grid_prediction_output %>% nrow(.)/frac), replace = T)
    
    boot_aRRs <- boot_aRRs %>%
      bind_rows(
        grid_prediction_output_resample %>% 
          gather(., "var", "val", -c(outcome_name, exposure, outcome, race, sex)) %>% 
          group_by(exposure, var) %>% 
          summarise(
            mean_prob = mean(val)
          ) %>%
          ungroup() %>%
          pivot_wider(names_from = exposure, values_from = mean_prob) %>%
          summarise(
            var = var,
            four_vs_zero =  `4+ ACEs`/ `0 ACEs`,
            three_vs_zero  = `3 ACEs`/ `0 ACEs`,
            two_vs_zero  = `2 ACEs`/ `0 ACEs`,
            one_vs_zero =  `1 ACE`/ `0 ACEs`
            
          ) %>%
          mutate(
            outcome = outcomes[i], 
            boot_strap = j, 
          )
        
      )
    
    
    
  }
  
}

# Math Checks 
p_score_df %>%
  bind_cols(
    con_twang =  get.weights(multiple_treatment_ipw, stop.method = "es.mean", withSampW = T)
  ) %>%
  summarise(
    correct = mean((ipw_twang * weight) ==  con_twang), # Raw calc is the same as the package output
    correct_two = mean(abs((ipw_twang * weight) - con_twang )), # No difference between the raw calc and the package output
    lr_df = mean(abs((ipw_glm * weight) - con_twang ))) # Since we verified our calcs we can see the  average absolute difference  between Lr and Xgboost

range(p_score_df$ipw_twang * p_score_df$weight)
range(tmp_iptw$weighting)
range(tmp_iptw$weighting_glm) 

# Balance of P model for XgBoost 
bal.table(multiple_treatment_ipw,
          collapse.to = 'covariate',
          digits = 2, 
          subset.stop.method ="es.mean") %>% 
  as.data.frame()


library(cobalt)
#  Balance of P model for XgBoost and GLM - Raw - WIP
exposure_vector <- c('ace_count_0_ac_es','ace_count_0_ac_es',
                     'ace_count_1_ace', 'ace_count_1_ace',
                     'ace_count_2_ac_es', 'ace_count_2_ac_es',
                     'ace_count_3_ac_es', 'ace_count_3_ac_es', 
                     'ace_count_4_ac_es','ace_count_4_ac_es')

p_score_vector <- c("ace_0",'ace_0_glm',
                    "ace_1", "ace_1_glm", 
                    "ace_2", "ace_2_glm", 
                    "ace_3", "ace_3_glm",
                    "ace_4", "ace_4_glm")

bal_loop <- data.frame()

for(i in 1:length(exposure_vector)){
  
  balance_obj <- cobalt::bal.tab(p_score_df %>% select(sex_rc,race_eth, stratum, psu),
                                 treat = p_score_df %>% pull(exposure_vector[i]), 
                                 weights = p_score_df %>% pull(p_score_vector[i]), 
                                 binary = "std", 
                                 estimand = "ATE",
                                 thresholds = .1) 
  bal_loop  <- bal_loop %>%
    bind_rows(
      
      balance_obj$Balance %>%
        as.data.frame() %>%
        mutate(
          exposure_group = exposure_vector[i], 
          p_score_group  = p_score_vector[i]
        ) %>%
        rownames_to_column("variable")
    )
}

bal_loop %>% 
  mutate(
    
    Method =  case_when(
      grepl("_glm",  p_score_group) ~ "GLM", 
      
      TRUE ~ "Xgboost"
    ), 
    
    Method = factor(Method, levels = c('GLM', "Xgboost")), 
    
    new_variable = case_when(
      grepl("sex",  variable) ~ "sex", 
      grepl("race",  variable) ~ "race_eth", 
      grepl("stratum_",  variable) ~ "stratum", 
      grepl("psu_",  variable) ~ "psu"
    )
    
  ) %>%
  mutate(
    abs_diff = abs(Diff.Adj)
  ) %>%
  select(
    - variable 
  ) %>%
  group_by(
    new_variable, Method
  ) %>%
  dplyr::slice(which.max(abs_diff)) %>%
  ungroup() %>%
  select(
    new_variable,  abs_diff,  Method,
  )

# P Score dist. 
p_score_df %>%
  select(
    ace_0, ace_1,ace_2,ace_3,ace_4,
    ace_0_glm, ace_1_glm,ace_2_glm,ace_3_glm,ace_4_glm) %>%
  gather(., "var", "val") %>%
  group_by(
    var
  ) %>%
  summarise(
    min = min(val),
    q1 = quantile(val, 0.25),
    q2 = quantile(val, 0.50),
    q3 = quantile(val, 0.75),
    max = max(val)
  )

# Overlap Plot of P model - There seems to be some set seed issues between this script at the modeling_full_scale at this time
raw_p_score_plot <- p_score_df %>%
  select(
    ace_0, ace_1,ace_2,ace_3,ace_4,
    ace_0_glm, ace_1_glm,ace_2_glm,ace_3_glm,ace_4_glm, 
    ace_count
  ) %>%
  gather(., "var", "val", -ace_count) %>%
  mutate(
    
    Method =  case_when(
      grepl("_glm",  var) ~ "GLM", 
      
      TRUE ~ "Xgboost"
    ), 
    
    Method = factor(Method, levels = c('GLM', "Xgboost"))
    
    
  ) %>% 
  mutate(
    
    var = case_when(
      var == "ace_0" ~ "0 ACEs", 
      var == "ace_0_glm" ~ "0 ACEs", 
      
      var == "ace_1" ~ "1 ACE", 
      var == "ace_1_glm" ~ "1 ACE", 
      
      var == "ace_2" ~ "2 ACEs", 
      var == "ace_2_glm" ~ "2 ACEs", 
      
      var == "ace_3" ~ "3 ACEs", 
      var == "ace_3_glm" ~ "3 ACEs", 
      
      var == "ace_4" ~ "4+ ACEs", 
      var == "ace_4_glm" ~ "4+ ACEs", 
      
    )
  ) %>% 
  ggplot(.) +
  aes(ace_count, val, color = Method) +
  geom_boxplot() +
  ylim(0,0.8) +
  theme_minimal() +
  facet_wrap(~var) +
  labs(x = "Exposure", 
       y = paste('Propensity Score'), 
       title = "") +
  theme(plot.title = element_text(hjust = 0.5,size=20),
        axis.text.x = element_text(color = "grey20", size = 10, angle = 45, hjust = 1),
        axis.text.y = element_text(color = "grey20", size = 10, angle = 0, hjust = 1 ),  
        axis.title.x = element_text(color = "grey20", size = 10, angle = 0, hjust = .5),
        axis.title.y = element_text(color = "grey20", size = 10, angle = 90, hjust = .5),
        strip.text = element_text(size = 10)) 

raw_p_score_plot
ggsave("Figures/raw_p_score_plot.pdf",
       width = 15, height = 15)


# XgBoost 
ace0_matrix <- confusion_matrix(truth = p_score_df$ace_count_0_ac_es,
                                predicted = p_score_df$ace_0, 
                                boot = TRUE, boot_samples = 1000L, alpha = 0.05)

ace1_matrix <- confusion_matrix(truth = p_score_df$ace_count_1_ace,
                                predicted = p_score_df$ace_1, 
                                boot = TRUE, boot_samples = 1000L, alpha = 0.05)

ace2_matrix <- confusion_matrix(truth = p_score_df$ace_count_2_ac_es,
                                predicted = p_score_df$ace_2, 
                                boot = TRUE, boot_samples = 1000L, alpha = 0.05)

ace3_matrix <- confusion_matrix(truth = p_score_df$ace_count_3_ac_es,
                                predicted = p_score_df$ace_3, 
                                boot = TRUE, boot_samples = 1000L, alpha = 0.05)

ace4_matrix <- confusion_matrix(truth = p_score_df$ace_count_4_ac_es,
                                predicted = p_score_df$ace_4, 
                                boot = TRUE, boot_samples = 1000L, alpha = 0.05)

# GLM 
ace0_matrix_glm <- confusion_matrix(truth = p_score_df$ace_count_0_ac_es,
                                    predicted = p_score_df$ace_0_glm, 
                                    boot = TRUE, boot_samples = 1000L, alpha = 0.05)

ace1_matrix_glm <- confusion_matrix(truth = p_score_df$ace_count_1_ace,
                                    predicted = p_score_df$ace_1_glm, 
                                    boot = TRUE, boot_samples = 1000L, alpha = 0.05)

ace2_matrix_glm <- confusion_matrix(truth = p_score_df$ace_count_2_ac_es,
                                    predicted = p_score_df$ace_2_glm, 
                                    boot = TRUE, boot_samples = 1000L, alpha = 0.05)

ace3_matrix_glm <- confusion_matrix(truth = p_score_df$ace_count_3_ac_es,
                                    predicted = p_score_df$ace_3_glm, 
                                    boot = TRUE, boot_samples = 1000L, alpha = 0.05)

ace4_matrix_glm <- confusion_matrix(truth = p_score_df$ace_count_4_ac_es,
                                    predicted = p_score_df$ace_4_glm, 
                                    boot = TRUE, boot_samples = 1000L, alpha = 0.05)

p_model_metrics <- tibble(
  exposure = '0 ACEs', 
  roc = ace0_matrix$auroc, 
  roc_lower = ace0_matrix$auroc_ci[1], 
  roc_upper =   ace0_matrix$auroc_ci[2], 
  pr = ace0_matrix$auprc, 
  pr_lower = ace0_matrix$auprc_ci[1], 
  pr_upper = ace0_matrix$auprc_ci[2]
  
) %>%
  bind_rows(
    
    tibble(
      exposure = '1 ACE', 
      roc = ace1_matrix$auroc, 
      roc_lower = ace1_matrix$auroc_ci[1], 
      roc_upper =   ace1_matrix$auroc_ci[2], 
      pr = ace1_matrix$auprc, 
      pr_lower = ace1_matrix$auprc_ci[1], 
      pr_upper = ace1_matrix$auprc_ci[2]
      
      
    ) 
    
  ) %>%
  bind_rows(
    
    tibble(
      exposure = '2 ACEs', 
      roc = ace2_matrix$auroc, 
      roc_lower = ace2_matrix$auroc_ci[1], 
      roc_upper =   ace2_matrix$auroc_ci[2], 
      pr = ace2_matrix$auprc, 
      pr_lower = ace2_matrix$auprc_ci[1], 
      pr_upper = ace2_matrix$auprc_ci[2]
      
    ) 
    
  ) %>%
  bind_rows(
    tibble(
      exposure = '3 ACEs', 
      roc = ace3_matrix$auroc, 
      roc_lower = ace3_matrix$auroc_ci[1], 
      roc_upper =   ace3_matrix$auroc_ci[2], 
      pr = ace3_matrix$auprc, 
      pr_lower = ace3_matrix$auprc_ci[1], 
      pr_upper = ace3_matrix$auprc_ci[2]
      
    ) 
  ) %>%
  bind_rows(
    
    tibble(
      exposure = '4+ ACEs', 
      roc = ace4_matrix$auroc, 
      roc_lower = ace4_matrix$auroc_ci[1], 
      roc_upper =   ace4_matrix$auroc_ci[2], 
      pr = ace4_matrix$auprc, 
      pr_lower = ace4_matrix$auprc_ci[1], 
      pr_upper = ace4_matrix$auprc_ci[2]
      
      
    ) 
    
  )

p_model_metrics_glm <- tibble(
  exposure = '0 ACEs', 
  roc = ace0_matrix_glm$auroc, 
  roc_lower = ace0_matrix_glm$auroc_ci[1], 
  roc_upper =   ace0_matrix_glm$auroc_ci[2], 
  pr = ace0_matrix_glm$auprc, 
  pr_lower = ace0_matrix_glm$auprc_ci[1], 
  pr_upper =   ace0_matrix_glm$auprc_ci[2]
  
) %>%
  bind_rows(
    
    tibble(
      exposure = '1 ACE', 
      roc = ace1_matrix_glm$auroc, 
      roc_lower = ace1_matrix_glm$auroc_ci[1], 
      roc_upper =   ace1_matrix_glm$auroc_ci[2],
      pr = ace1_matrix_glm$auprc, 
      pr_lower = ace1_matrix_glm$auprc_ci[1], 
      pr_upper =   ace1_matrix_glm$auprc_ci[2]
    ) 
    
  ) %>%
  bind_rows(
    
    tibble(
      exposure = '2 ACEs', 
      roc = ace2_matrix_glm$auroc, 
      roc_lower = ace2_matrix_glm$auroc_ci[1], 
      roc_upper =   ace2_matrix_glm$auroc_ci[2],
      pr = ace2_matrix_glm$auprc, 
      pr_lower = ace2_matrix_glm$auprc_ci[1], 
      pr_upper =   ace2_matrix_glm$auprc_ci[2]
    ) 
    
  ) %>%
  bind_rows(
    tibble(
      exposure = '3 ACEs', 
      roc = ace3_matrix_glm$auroc, 
      roc_lower = ace3_matrix_glm$auroc_ci[1], 
      roc_upper =   ace3_matrix_glm$auroc_ci[2],
      pr = ace3_matrix_glm$auprc, 
      pr_lower = ace3_matrix_glm$auprc_ci[1], 
      pr_upper =   ace3_matrix_glm$auprc_ci[2]
    ) 
  ) %>%
  bind_rows(
    
    tibble(
      exposure = '4+ ACEs', 
      roc = ace4_matrix_glm$auroc, 
      roc_lower = ace4_matrix_glm$auroc_ci[1], 
      roc_upper =   ace4_matrix_glm$auroc_ci[2],
      pr = ace4_matrix_glm$auprc, 
      pr_lower = ace4_matrix_glm$auprc_ci[1], 
      pr_upper =   ace4_matrix_glm$auprc_ci[2]
      
    ) 
    
  )

# Model Metrics for P Model
table_p_metrics <- p_model_metrics %>%
  mutate(
    method = "XgBoost"
  ) %>%
  bind_rows(
    p_model_metrics_glm %>%
      mutate(
        method = "GLM"
      )
  ) %>%
  mutate(
    
    roc = format(round(roc, 2),  nsmall = 2), 
    roc_lower = format(round(roc_lower, 2), nsmall = 2), 
    roc_upper  = format(round(roc_upper, 2), nsmall = 2), 
    pr = format(round(pr, 2),  nsmall = 2), 
    pr_lower = format(round(pr_lower, 2), nsmall = 2), 
    pr_upper  = format(round(pr_upper, 2), nsmall = 2), 
    combine_est_roc =  paste(roc, paste0("(", roc_lower, ",",  roc_upper, ")") ),
    combine_est_pr =  paste(pr, paste0("(", pr_lower, ",",  pr_upper, ")") )
    
  ) %>%
  select(
    exposure , method, combine_est_roc,combine_est_pr
  )  %>%
  pivot_wider(
    names_from = c(method),
    values_from = c(combine_est_roc,combine_est_pr) 
  ) 

table_p_metrics
write.csv(table_p_metrics, "Tables/table_p_metrics.csv")

plot_df <-  paper_rr %>%
  mutate(
    contrast = case_when(
      
      contrast ==  "ln(mean(1 ACE) / mean(0 ACEs))" ~ "1 ACE",
      contrast ==  "ln(mean(2 ACEs) / mean(0 ACEs))" ~ "2 ACEs",
      contrast ==  "ln(mean(3 ACEs) / mean(0 ACEs))" ~ "3 ACEs",
      contrast ==  "ln(mean(4+ ACEs) / mean(0 ACEs))" ~ "4+ ACEs",
      
    ), 
    
    method = "Base"
  ) %>%
  select(
    contrast, estimate,  
    new_conf.low = conf.low, new_conf.high =  conf.high,
    outcome, method
  ) %>%
  bind_rows(
    
    paper_rr_glm %>%
      mutate(
        contrast = case_when(
          
          contrast ==  "ln(mean(1 ACE) / mean(0 ACEs))" ~ "1 ACE",
          contrast ==  "ln(mean(2 ACEs) / mean(0 ACEs))" ~ "2 ACEs",
          contrast ==  "ln(mean(3 ACEs) / mean(0 ACEs))" ~ "3 ACEs",
          contrast ==  "ln(mean(4+ ACEs) / mean(0 ACEs))" ~ "4+ ACEs",
          
        ), 
        
        method = "Approach 2"
      ) %>%
      select(
        contrast, estimate,  
        new_conf.low = conf.low, new_conf.high =  conf.high,
        outcome, method
      )
    
  ) %>%
  bind_rows(
    boot_aRRs %>%
      select(
        -boot_strap
      ) %>% 
      gather(., "other", "val", -c(var, outcome)) %>%
      group_by(
        var, outcome, other
      ) %>%
      summarise(
        point  = mean(val), 
        new_conf.low  = quantile(val, 0.025), 
        new_conf.high = quantile(val, 0.975), 
        
        
      ) %>%
      ungroup() %>%
      mutate(
        contrast = case_when(
          other ==  "four_vs_zero" ~  "4+ ACEs", 
          other ==  "three_vs_zero" ~ "3 ACEs", 
          other ==  "two_vs_zero" ~  "2 ACEs", 
          other ==  "one_vs_zero" ~   "1 ACE"
        )
      ) %>%
      rename(
        estimate = point,
        method = var
      ) %>%
      select(
        contrast, estimate,  
        new_conf.low, new_conf.high,
        outcome,  method
      )
  )

# Outcome Model - aRRs Main Slides and appendix 
diff_method_arr <- plot_df %>% 
  mutate(
    estimate = round( estimate, 2), 
    new_conf.low  = round(new_conf.low, 2), 
    new_conf.high = round(new_conf.high, 2 )
  ) %>%
  mutate(
    outcome = case_when(
      outcome == "ideation" ~ "Ideation", 
      outcome == "suicideatt_qn29" ~ "Attempt"
    ),
    
    method = case_when(
      method == "prob_svy" ~ "Approach 1", 
      method == "prob_xgboost" ~ "Approach 3", 
      method == "Base" ~ "Base", 
      method == "Approach 2" ~ "Approach 2"
      
    ),
    outcome = factor(outcome, levels = c("Ideation","Attempt")),
    method = factor(method, levels = c("Base", "Approach 1", "Approach 2", "Approach 3")),
    
    estimate = format(round(estimate, 2),  nsmall = 2), 
    new_conf.low = format(round(new_conf.low, 2), nsmall = 2), 
    new_conf.high = format(round(new_conf.high, 2), nsmall = 2), 
    combine_est =  paste(estimate, paste0("(", new_conf.low, ",", new_conf.high, ")") )
    
  ) %>%
  select(
    -estimate, - new_conf.low, -new_conf.high
  ) %>%
  pivot_wider(
    names_from = c(contrast),
    values_from = combine_est
  ) %>%
  arrange(method)

diff_method_arr
write.csv(diff_method_arr, "Tables/diff_method_arr.csv")

# Outcome Model Model Metrics 
outcome_model_model_metrics <- model_eval %>%
  mutate(
    outcome = case_when(
      outcome == "ideation" ~ "Ideation", 
      outcome == "suicideatt_qn29" ~ "Attempt"
    ),
    outcome = factor(outcome, levels = c("Ideation","Attempt")),
    
  ) %>%
  pivot_longer(
    cols = -c(type, outcome),
    names_to = c("Metric", "CI"),
    names_pattern = "(.*?)(_lower|_upper)?$",
    values_to = "Value"
  ) %>%
  mutate(
    Metric = case_when(
      Metric == "roc" ~ "AUCROC", 
      Metric == "pr" ~ "AUCPRC"
    ),
    
    CI = case_when(
      CI == "" ~ "Point", 
      CI == "_lower" ~ "Lower", 
      CI == "_upper" ~ "Upper"
    )
    
  ) %>%
  pivot_wider(
    names_from = CI,
    values_from = Value
  )  %>%
  separate(type, into = c("Approach", "type"), sep = ":\\s*") %>%
  mutate(
    Point = format(round(Point, 2),  nsmall = 2), 
    Lower =  format(round(Lower, 2),  nsmall = 2), 
    Upper =  format(round(Upper, 2),  nsmall = 2), 
    combine_est =  paste(Point, paste0("(",  Lower, ",", Upper, ")") )
  ) %>%
  select(
    Approach, type, outcome, Metric, combine_est
  ) %>%
  pivot_wider(
    names_from = c(type, Approach), 
    values_from = combine_est
  ) 

outcome_model_model_metrics
write.csv(outcome_model_model_metrics, "Tables/outcome_model_model_metrics.csv")

