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

set.seed(1234) 
n_imps <- 10


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

# Table S2: Little’s Missing Completely at Random Test
df %>%
  select(
    outcomes,ace_count,variable_list
  ) %>%
  select(
    -year
  ) %>%
  mcar_test(.)

# Imputation of Confounders  
to_imp <- df %>% select(confounders)
keep <- df %>% select(-confounders)

imp <- mice(to_imp, m = n_imps,  maxit=5)

# We picked at random a version of the imputed dataset 
random_number <- sample(c(1:n_imps), 1)

df_imputed  <- complete(imp, random_number)

df_imputed  <- keep %>%
  bind_cols(
    df_imputed 
  ) 


# Table 1: Descriptives of Analytical Sample of 2023 National Youth Risk Behavioral Study Overall and by Demographics
# Table S1: Imputed Confounders of the Study Sample 
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
combinded_weighted_table_one 

write.csv(combinded_weighted_table_one  %>% as.tibble() %>% replace(is.na(.), ""), "Tables/table_1_weighted_full.csv")

# Table 2: Descriptives of the Adverse Childhood Experiences in the Analytical Sample of 2023 National Youth Risk Behavioral Study Overall and by Demographics  
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
combinded_weighted_table_two

write.csv(combinded_weighted_table_two  %>% as.tibble() %>% replace(is.na(.), ""), "Tables/table_2_weighted_full.csv")

overlap_plot <- before_and_after_weighting_plot <- balance_table  <- risk_ratio <- contrast_marg <- model_tables <-  model_tables_raceeth <- model_tables_sex <- vector("list", length(outcomes)) 
names(overlap_plot) <- names(before_and_after_weighting_plot) <- names(balance_table)  <- names(risk_ratio) <-  names(contrast_marg) <- names(model_tables)  <- names(model_tables_raceeth) <-  names(model_tables_sex) <-  outcomes 
race_compare <- sex_compare <- data.frame()

for(i in 1:length(outcomes)) { 
  
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
  
  tmp_iptw <- tmp %>% select(row_id,treatment,sex_rc, race_eth, cluster,  id, weight) %>% filter(complete.cases(.))
  
  # IPTW 
  fmla_ipw  <- paste0(treatment, "~", paste0(c("sex_rc", "race_eth"),collapse = "+"),"+", cluster, "+",  id)
  cat('Formula for IPTW Stage for outcome:', as.character(outcomes[i]), '\n')
  print(fmla_ipw)
  
  multiple_treatment_ipw  <- mnps(as.formula(fmla_ipw),
                                  data =   tmp_iptw ,
                                  estimand = "ATE",
                                  verbose = FALSE,
                                  stop.method = c("es.mean"),
                                  sampw =   tmp_iptw$weight, 
                                  version = "xgboost", 
                                  n.trees = 300)
  
  overlap_plot[[i]] <- plot(multiple_treatment_ipw, plots = 2, subset = "es.mean") 
  
  before_and_after_weighting_plot[[i]] <-  plot(multiple_treatment_ipw, plots = 3)
  
  balance_table[[i]] <- bal.table(multiple_treatment_ipw, collapse.to = 'covariate', digits = 2, subset.stop.method ="es.mean") %>% as.data.frame() %>% 
    mutate(
      outcome  = outcomes[i]
    )
  
  tmp_iptw$weighting <- get.weights(multiple_treatment_ipw, stop.method = "es.mean", withSampW = T) # IPW * survey weight 
  tmp_iptw$ipw_check <- get.weights(multiple_treatment_ipw,, stop.method = "es.mean", withSampW = F)
  
  tmp_iptw <-  tmp_iptw %>%
    mutate(
      weighting_check = ipw_check * weight
    )
  
  passed_weights <-  tmp_iptw %>%
    summarise(
      acc =  mean(weighting == weighting_check )
    ) %>%
    pull(acc)
  
  if(passed_weights != 1) {
    print("weights werent passed throught the ipw stage")
  } 
  
  model_df <-  tmp %>% 
    left_join(
      tmp_iptw %>% select(row_id,weighting)
    ) %>%
    select(
      outcomes[i],treatment, dgh,confounders,PSU,stratum,weighting
    ) %>%
    filter(complete.cases(.))
  
  weighted_design <- svydesign(ids = ~PSU, data =  model_df, strata = ~stratum, weights = ~weighting, nest=TRUE) 
  options(survey.lonely.psu = "adjust")
  
  outcome_model_fmla <- paste0(outcomes[i],'~', 
                               treatment, "+", 
                               paste0(dgh,collapse = "+"), "+", 
                               paste0(confounders,collapse = "+"))
  
  outcome_model_fmla_raceeth <- paste0(outcomes[i],'~',
                                       paste0(treatment, "*",  "race_eth" ), "+", 
                                       paste0(c("grade_rc",  "sex_rc"),collapse = "+"), "+",
                                       paste0(confounders,collapse = "+"))
  
  outcome_model_fmla_sex <- paste0(outcomes[i],'~',
                                   paste0(treatment, "*",    "sex_rc"), "+", 
                                   paste0(c("grade_rc", "race_eth"),collapse = "+"), "+",
                                   paste0(confounders,collapse = "+"))
  
  cat("Outcome Model for Overall-ACEs:",  outcome_model_fmla , '\n')
  cat("Outcome Model for Raceeth-ACEs:",  outcome_model_fmla_raceeth , '\n')
  cat("Outcome Model for Sex-ACEs:",  outcome_model_fmla_sex, '\n')
  
  model_svy <-  svyglm(outcome_model_fmla, family = quasibinomial(link = 'logit'), design = weighted_design)
  model_svy_raceeth <-  svyglm(outcome_model_fmla_raceeth, family = quasibinomial(link = 'logit'), design = weighted_design)
  model_svy_sex <-  svyglm(outcome_model_fmla_sex, family = quasibinomial(link = 'logit'), design = weighted_design)
  
  model_tables[[i]]  <- model_svy %>%
    tbl_regression(exponentiate = T,
                   estimate_fun = purrr::partial(style_ratio, digits = 2),
                   pvalue_fun = purrr::partial(style_sigfig, digits = 2)) %>%
    add_significance_stars(
      pattern = "{estimate} ({conf.low}, {conf.high}){stars}",
      hide_se = TRUE
    )
  
  model_tables_raceeth[[i]]  <-   model_svy_raceeth %>%
    tbl_regression(exponentiate = T,
                   estimate_fun = purrr::partial(style_ratio, digits = 2),
                   pvalue_fun = purrr::partial(style_sigfig, digits = 2)) %>%
    add_significance_stars(
      pattern = "{estimate} ({conf.low}, {conf.high}){stars}",
      hide_se = TRUE
    )
  
  model_tables_sex[[i]]  <-   model_svy_sex %>%
    tbl_regression(exponentiate = T,
                   estimate_fun = purrr::partial(style_ratio, digits = 2),
                   pvalue_fun = purrr::partial(style_sigfig, digits = 2)) %>%
    add_significance_stars(
      pattern = "{estimate} ({conf.low}, {conf.high}){stars}",
      hide_se = TRUE
    )
  
  race_eth_anova <- anova(model_svy, model_svy_raceeth, method = "Wald")
  sex_anova <-  anova(model_svy, model_svy_sex, method = "Wald") 

  race_compare <- race_compare %>%
    bind_rows(
      
      tibble(
        outcome = outcomes[i], 
        test_term =  race_eth_anova$test.terms %>% as.character(), 
        p_value =  race_eth_anova$p %>% as.numeric(.)
      )
      
    )
  
  sex_compare <- sex_compare %>%
    bind_rows(
      
      tibble(
        outcome = outcomes[i], 
        test_term =  sex_anova$test.terms %>% as.character(), 
        p_value =  sex_anova$p %>% as.numeric(.)
      )
      
    )
  
  
  overall_rr <- avg_comparisons(model_svy,
                                variables = treatment,
                                wt = weighted_design$allprob$weight, 
                                comparison = "lnratioavg", 
                                transform = "exp")
  
  by_sex_rr <- avg_comparisons(model_svy, 
                               variables = treatment,
                               by = 'sex_rc', 
                               wt = weighted_design$allprob$weight, 
                               comparison = "lnratioavg", 
                               transform = "exp")
  
  by_race_rr <- avg_comparisons(model_svy, 
                                variables = treatment,
                                by = 'race_eth',
                                wt = weighted_design$allprob$weight,
                                comparison = "lnratioavg", 
                                transform = "exp")
  
  effects_df_rr <- overall_rr %>%
    as.data.frame() %>% 
    select(
      term,contrast, estimate, conf.low,  conf.high, p.value 
    )  %>%
    bind_rows(
      by_sex_rr %>%
        as.data.frame() %>%
        select(
          term,contrast, sex_rc, estimate, conf.low,  conf.high, p.value 
        ) 
    ) %>%
    bind_rows(
      by_race_rr %>%
        as.data.frame() %>%
        select(
          term,contrast, race_eth, estimate, conf.low,  conf.high, p.value 
        ) 
    ) %>%
    mutate(
      p.value =   format(round(p.value, 2), nsmall = 2), 
      estimate  = format(round(estimate, 2), nsmall = 2), 
      conf.low =  format(round(conf.low, 2),nsmall = 2), 
      conf.high =  format(round(conf.high, 2),nsmall = 2), 
      
      common_name = case_when(
        is.na(sex_rc) & is.na(race_eth) ~ "Overall", 
        is.na(sex_rc) & !is.na(race_eth) ~ race_eth, 
        !is.na(sex_rc) & is.na(race_eth) ~ sex_rc
        
      )
    ) 
  
  risk_ratio[[i]] <- effects_df_rr  %>%
    mutate(
      combine_est =  paste(estimate, paste0("(", conf.low, ",", conf.high, ")") )
    ) %>%
    select(
      contrast,  common_name,  combine_est, p.value ,estimate,  conf.low , conf.high
    ) %>%
    mutate(
      outcome_varialbe = outcomes[i]
    )
  
}
# Both outcomes share the same p model 
# Table S3: Balance Table
balance_table <- do.call('rbind', balance_table) %>% 
  select(
    var,max.std.eff.sz, outcome
  ) %>%
  mutate(
    balance = ifelse(max.std.eff.sz < 0.25, "Balance", "Not Balance")
  )

write.csv(balance_table, "Tables/balance_table_full.csv")

# Figure S1 (A to E): Overlap Plots of Propensity Scores
pdf(file = 'Figures/overlap_ideation1_full.pdf')
overlap_plot$ideation[[1]]
dev.off()

pdf(file = 'Figures/overlap_ideation2_full.pdf')
overlap_plot$ideation[[2]]
dev.off()

pdf(file = 'Figures/overlap_ideation3_full.pdf')
overlap_plot$ideation[[3]]
dev.off()

pdf(file = 'Figures/overlap_ideation4_full.pdf')
overlap_plot$ideation[[4]]
dev.off()

pdf(file = 'Figures/overlap_ideation5_full.pdf')
overlap_plot$ideation[[5]]
dev.off()


#pdf(file = 'Figures/overlap_attempt1_full.pdf')
#overlap_plot$suicideatt_qn29[[1]]
#dev.off()

#pdf(file = 'Figures/overlap_attempt2_full.pdf')
#overlap_plot$suicideatt_qn29[[2]]
#dev.off()

#pdf(file = 'Figures/overlap_attempt3_full.pdf')
#overlap_plot$suicideatt_qn29[[3]]
#dev.off()

#pdf(file = 'Figures/overlap_attempt4_full.pdf')
#overlap_plot$suicideatt_qn29[[4]]
#dev.off()

#pdf(file = 'Figures/overlap_attempt5_full.pdf')
#overlap_plot$suicideatt_qn29[[5]]
#dev.off()

# Adjusted Odd Ratios
outcome_modeling <- tbl_merge(
  tbls = list(
    
    model_tables$ideation, 
    model_tables$suicideatt_qn29, 
    model_tables_raceeth$ideation, 
    model_tables_raceeth$suicideatt_qn29, 
    model_tables_sex$ideation, 
    model_tables_sex$suicideatt_qn29
    
  ), 
  
  tab_spanner = c("Suicide Ideation", "Suicide Attempt", 
                  "Suicide Ideation - Raceeth", "Suicide Attempt - Raceeth",
                  "Suicide Ideation - Sex", "Suicide Attempt - Sex")
) 

outcome_modeling 

write.csv(outcome_modeling  %>% as.tibble() %>% replace(is.na(.), ""), "Tables/outcome_models_full.csv")

dispar <- race_compare %>%
  mutate(
    observed_difference = ifelse(p_value < 0.05, "Yes", "No"), 
    other_label = "Race/Ethnicity"
  )  %>%
  bind_rows(
    
    sex_compare %>%
      mutate(
        observed_difference = ifelse(p_value < 0.05, "Yes", "No"), 
        other_label = "Sex"
      ) 
    
  )

plot_df <- do.call('rbind', risk_ratio) %>% 
  mutate(
    estimate = as.numeric(estimate), 
    conf.low  = as.numeric(conf.low), 
    conf.high = as.numeric(conf.high), 
    
    other_label = case_when(
      common_name %in% c('Female', "Male") ~ "Sex", 
      common_name  == "Overall" ~ "Overall", 
      TRUE ~ "Race/Ethnicity"
    )
  ) %>% 
  rename( outcome = outcome_varialbe ) %>%
  mutate(
    outcome = case_when(
      outcome == "ideation" ~ "Ideation", 
      outcome == "suicideatt_qn29" ~ "Attempt"
    ) 
    
  ) %>% 
  mutate(
    outcome = factor(outcome, levels = c("Ideation","Attempt"))
  ) 

# Figure 1: Adverse Childhood Experience Dose Response Adjusted Risk Ratios on Each Outcome (Overall) 
plot_overall_rr <- plot_df %>%
  filter(common_name  == "Overall" ) %>%
  mutate(
    contrast = case_when(
      
      contrast ==  "ln(mean(1 ACE) / mean(0 ACEs))" ~ "1 ACE",
      contrast ==  "ln(mean(2 ACEs) / mean(0 ACEs))" ~ "2 ACEs",
      contrast ==  "ln(mean(3 ACEs) / mean(0 ACEs))" ~ "3 ACEs",
      contrast ==  "ln(mean(4+ ACEs) / mean(0 ACEs))" ~ "4+ ACEs",
      
    )
  ) %>%
  ggplot(.) +
  aes(contrast,  estimate, color= outcome) + 
  theme_minimal() + 
  geom_point(position=position_dodge(width=0.5)) +
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high),
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

# Figure 2: Adverse Childhood Experience Dose Response Adjusted Risk Ratios on Each Outcome (By Race/ethnicity and Sex) 
plot_race_eth_rr <- plot_df %>% 
  left_join(
    dispar %>% 
      mutate(
        outcome = case_when(
          outcome == "ideation" ~ "Ideation", 
          outcome == "suicideatt_qn29" ~ "Attempt"
        ) 
      ) %>%
      select(other_label, observed_difference, outcome)
    
  ) %>%
  filter(other_label == "Race/Ethnicity" ) %>% 
  mutate(
    contrast = case_when(
      
      contrast ==  "ln(mean(1 ACE) / mean(0 ACEs))" ~ "1 ACE",
      contrast ==  "ln(mean(2 ACEs) / mean(0 ACEs))" ~ "2 ACEs",
      contrast ==  "ln(mean(3 ACEs) / mean(0 ACEs))" ~ "3 ACEs",
      contrast ==  "ln(mean(4+ ACEs) / mean(0 ACEs))" ~ "4+ ACEs",
      
    ),
    
    outcome = factor(outcome, levels = c("Ideation","Attempt")) 
  ) %>%
  ggplot(.) +
  aes(contrast,  estimate, color = common_name) + 
  theme_minimal() + 
  geom_point(position=position_dodge(width=0.8),aes( shape = observed_difference)) +
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high),
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


# By sex 
plot_sex_rr <- plot_df %>% 
  left_join(
    dispar %>% 
      mutate(
        outcome = case_when(
          outcome == "ideation" ~ "Ideation", 
          outcome == "suicideatt_qn29" ~ "Attempt"
        ) 
      ) %>%
      select(other_label, observed_difference, outcome)
    
  ) %>%
  filter(other_label == "Sex" ) %>% 
  mutate(
    
    contrast = case_when(
      
      contrast ==  "ln(mean(1 ACE) / mean(0 ACEs))" ~ "1 ACE",
      contrast ==  "ln(mean(2 ACEs) / mean(0 ACEs))" ~ "2 ACEs",
      contrast ==  "ln(mean(3 ACEs) / mean(0 ACEs))" ~ "3 ACEs",
      contrast ==  "ln(mean(4+ ACEs) / mean(0 ACEs))" ~ "4+ ACEs",
      
    ),
    
    outcome = factor(outcome, levels = c("Ideation","Attempt")) 
  ) %>%
  ggplot(.) +
  aes(contrast,  estimate, color = common_name) + 
  theme_minimal() + 
  geom_point(position=position_dodge(width=0.8),aes( shape = observed_difference)) +
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high),
                width = 0.8,
                position=position_dodge(width=0.8)) +
  labs(x = "Adverse Childhood Experience Catogery Comparision", 
       y = paste('Adjusted Risk Ratios'), 
       title = NULL) +
  facet_wrap(~outcome) +
  theme(plot.title = element_text(hjust = 0.5, size = 20), 
        axis.text.x = element_text(size = 20), 
        axis.text.y = element_text(size = 20), 
        axis.title = element_text(size = 20), 
        legend.text = element_text(size = 20), 
        legend.title = element_text(size = 20), 
        strip.text = element_text(size = 20)) + 
  geom_hline(yintercept=1,linetype="dashed",color = "red", size=0.3) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(color='Group',  shape = "Moderation Effect Observed")

plot_demo <- (plot_race_eth_rr / plot_sex_rr)
plot_demo

ggsave("Figures/plot_demo_full.pdf",
       width = 15, height = 15)

# Table S4: Adjusted Risk Ratios (aRR) for Dose Response Plots
arr_table <- do.call('rbind', risk_ratio) %>% 
  mutate(
    other_label = case_when(
      common_name %in% c('Female', "Male") ~ "Sex", 
      common_name  == "Overall" ~ "Overall", 
      TRUE ~ "Race/Ethnicity"
    )
  ) %>% 
  rename( outcome = outcome_varialbe ) %>%
  mutate(
    outcome = case_when(
      outcome == "ideation" ~ "Ideation", 
      outcome == "suicideatt_qn29" ~ "Attempt"
    ) 
    
  ) %>% 
  mutate(
    outcome = factor(outcome, levels = c("Ideation","Attempt"))
  )  %>%
  select(
    contrast,  common_name, combine_est,  outcome
  ) %>%
  pivot_wider(id_cols = c("common_name", "outcome"), 
              names_from = contrast, 
              values_from = c(combine_est)) 
arr_table
write.csv(arr_table, "Tables/arr_table_full.csv")

# Table S5: Moderation Analysis
dis_table <- dispar %>%
  mutate(
    outcome = case_when(
      outcome == "suicideatt_qn29" ~ "Attempt", 
      outcome == "ideation" ~ "Ideation"
    ), 
    
    outcome = factor(outcome, levels = c("Ideation","Attempt")),
    p_value  = format(round( p_value , 2), nsmall = 2), 
    
  ) %>%
  arrange(outcome)

dis_table
write.csv(dis_table , "Tables/dis_table.csv")
