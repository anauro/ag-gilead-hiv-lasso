##################################################################
# Case:   041749 - Gilead HIV Retention in Care                       
# Code:   4.0 LASSO Model            			               
# Date:   12.13.2023 (FINAL MODEL)
# Objective: Prediction of retention in care 
# Author: Ana Urosevic
##################################################################

rm(list=ls())

#Install and load packages
library(data.table)
library(dplyr)
library(openxlsx)
library(fst)
library(glmnet)
library(gglasso)
library(grpreg)
library(caret)
library(tibble)
library(MLmetrics)
library(predtools)
library(cutpointr)
library(mosaic)
library(tidyr)

#Set folders
proj_dir <- file.path("//ace/data/health3/gilead_hiv_retention_in_care_041749")
out <- file.path(proj_dir, "data", "r_datasets","out")
sp <- file.path(proj_dir,"/data/r_datasets/study_pop")


#=======================================================#
# 0. Data import and preparation                        #
#=======================================================#

#We need to grab both retention in care and all predictors - i.e., baseline covariates

#COMBINE BASELINE DATASETS
#The following are relevant:
#Demo
#Drug and Labs
#HRU and Costs
cohorts <- read_fst(paste0(sp,"/cohorts.fst"))

demo <- read_fst(paste0(out,"/demo.fst"))
drug_labs <- read_fst(paste0(out,"/drug_labs.fst"))
hru_cost <- read_fst(paste0(out,"/hru_cost.fst"))
comorbs <- read_fst(paste0(out,"/comorbs.fst"))

covariates <- cohorts %>%
  left_join(demo) %>%
  left_join(drug_labs) %>%
  left_join(hru_cost) %>%
  left_join(comorbs) %>%
  #Create outcome flag
  mutate(flag_fooc=as.numeric(cohort=="fooc"))


#=======================================================#
# Model version                                         #
#=======================================================#

# Set seed 
set.seed(1234567)


#=======================================================#
# (1) Training and test set (80/20 split)               #
#=======================================================#

#DATA PREPARATION

#We need to first make sure no variables have missing values!
for_logit <- covariates
any(is.na(for_logit))
names(which(colSums(is.na(for_logit))>0))
#Number of IP, ER , and OP visits
#We will just need to impute 0s for these values
for_logit[is.na(for_logit)] <- 0
names(which(colSums(is.na(for_logit))>0))

#We also need to standardize all continuous variables (use z-score)
#What are the continuous variables? 
x_cont <- c("age","cci",colnames(for_logit)[grepl("^num|^cost",colnames(for_logit))])
for_logit_standard <- cbind(for_logit[!colnames(for_logit) %in% x_cont], apply(for_logit[x_cont],2, function(x){zscore(x)}))
mean(for_logit_standard$age)
sd(for_logit_standard$age)

#We could potentially try a 30/70 split if needed
training.samples <- for_logit_standard$flag_fooc %>%
  createDataPartition(p=0.8, list=FALSE)
train.data <- for_logit_standard[training.samples, ]
test.data <- for_logit_standard[-training.samples, ]

# Check distribution of outcome (should be ~45% in both the train and test sets)
summary(train.data$flag_fooc)
summary(test.data$flag_fooc)
#Great, looks good! :) 


#=======================================================#
# (2) Specify list of predictors                        #
#=======================================================#

x_list <- c(

  ##### GROUPED VARIABLES #####
  #Region = 4
  #"reg_s",
  "reg_mw","reg_e","reg_w","reg_o",
  
  #Insurance = 4
  #"plan_p",
  "plan_h","plan_s","plan_d","plan_i",
  
  #Specialty = 4
  #"spec_pc",
  "spec_inf_dis","spec_obgyn","spec_oth","spec_unk",
  
  ###### UNGROUPED VARIABLES #####
  
  #Age
  "age",
  
  #Sex - female
  "sex_f",
  
  #Concomitant medication (overall use and by agent)
#  "med_cat_concom",
  "med_antihypertensives",
  "med_antihyperlipidemics",
  "med_antidiabetics",
  #<5% & <2%
#  "med_antineoplastics",
#  "med_contraceptives",
#  "med_bisphosphonates",
  
  #ART
  #MTR
#  "med_cat_mtr",
  "med_insti",
  "med_pi",
  #<5%
  "med_nnrti",
  "med_nrti",
  #<2%
#  "med_ccr5_antagonist",
#  "med_fusion_inhibitor",
#  "med_post_attachment_inhibitor",
  #STR
#  "med_cat_str",
  "med_bic_taf_ftc",
  "med_evg_taf_ftc_cobi",
  "med_dtg_abc_3tc",
  "med_rpv_ftc_taf",
  "med_efv_tdf_ftc",
  #<5%
  "med_dtg_rpv",
  "med_evg_tdf_ftc_cobi",
  "med_drv_cobi_ftc_taf",
  #<2%
#  "med_rpv_tdf_ftc",
#  "med_dtg_3tc",
#  "med_dor_3tc_tdf",
#  "med_efv_3tc_tdf",
  
  #Lab tests
  "lab_hiv",
#  "num_lab_hiv",
  "lab_cd4",
# "num_lab_cd4",
  #"lab_covid",
  #"num_lab_covid",
  
  #HRU
  #All-cause
#  "visit_ip_all",
#  "num_ip_all",
#  "los_all",
#  "visit_er_all",
#  "num_er_all",
#  "visit_op_all",
#  "num_op_all", 
#  "visit_op_pc_all",
#  "num_op_pc_all",
#  "visit_op_inf_all",
#  "num_op_inf_all",
#  "visit_op_oth_all",
#  "num_op_oth_all",
  
  #HIV-related
  "visit_ip_hiv",
#  "num_ip_hiv",
#  "los_hiv",
  "visit_er_hiv",
#  "num_er_hiv_2p",
#  "num_er_hiv",
#  "visit_op_hiv",
#  "num_op_hiv", 
#  "visit_op_pc_hiv",
#  "num_op_pc_hiv",
#  "visit_op_inf_hiv",
#  "num_op_inf_hiv",
#  "visit_op_oth_hiv",
#  "num_op_oth_hiv",
  
  #COMORBIDITIES
  #CCI
#  "cci",
  
  #HIV-related
  "dx_aids",
  "dx_hsv",
#  "dx_kaposi_sarc",
#  "dx_pneumon_recurr",
#  "dx_waste_synd",
#  "dx_encephal",
#  "dx_cmv",
#  "dx_candid_esoph",
#  "dx_toxoplas",
#  "dx_lymph_burk",
#  "dx_pneumocys",
#  "dx_histoplas",
#  "dx_myco_avium_kans",
#  "dx_resp_tub",
#  "dx_candid_bronc",
#  "dx_myco_other",
#  "dx_cryptococc",
#  "dx_cryptospor",
#  "dx_cerv_canc",
#  "dx_lymph_brain",
#  "dx_pml",
#  "dx_coccidio", #Ignore because 0 in both cohorts
#  "dx_salmonella_sept", #Ignore because 0 in both cohorts
#  "dx_isopor", #Ignore because 0 in both cohorts
#  "dx_lymph_immuno", #Ignore because 0 in both cohorts
  "dx_hepc",
  
  #Elixhauser
  "elix_hyp",
# "elix_hypunc", #Ignore because we have the overall hypertension category
# "elix_hypc", #Ignore because we have the overall hypertension category
  "elix_depre",
  "elix_obes",
  "elix_diab",
# "elix_diabc", #Ignore because we have the overall diabetes category
# "elix_diabunc", #Ignore because we have the overall diabetes category
  "elix_cpd",
  "elix_rf",
  "elix_ld",
  #<5%
  "elix_fed",
  "elix_carit",
  "elix_drug",
  "elix_hypothy",
  "elix_alcohol",
  "elix_solidtum",
  "elix_dane",
  "elix_pvd",
  "elix_coag",
  "elix_chf",
  #<2%
#  "elix_ond",
#  "elix_wloss",
#  "elix_valv",
#  "elix_rheumd",
#  "elix_lymph",
#  "elix_pcd",
#  "elix_psycho",
#  "elix_pud",
#  "elix_blane",
#  "elix_para",
#  "elix_metacanc",
  
  #DSM-5
  "dsm_anxiety_dis",
  "dsm_depressive_dis",
  "dsm_sub_addi_dis",
  "dsm_sleep_wake_dis",
  "dsm_other_cond",
  #<5%
  "dsm_trauma_dis",
  "dsm_neurodev_dis",
  "dsm_bipolar_dis",
  #<2%
#  "dsm_sex_dysfunction",
#  "dsm_neurocog_dis",
#  "dsm_schizo_dis",
#  "dsm_elim_dis",
#  "dsm_med_induced_mov",
#  "dsm_other_mental_dis",
#  "dsm_personality_dis",
#  "dsm_gender_dysphoria",
#  "dsm_obs_comp_dis",
#  "dsm_somatic_dis",
#  "dsm_eating_dis",
#  "dsm_imp_conduct_dis",
#  "dsm_dissoc_dis",
#  "dsm_paraphilic_dis",
  
  ##### OUTCOME #####
  "flag_fooc")

# Create a vector of identical numbers for each variable 
# (e.g., Region has four variables: 1,1,1,1 ; Specialist has 4 variables: 2,2,2,2; etc.)
grouped <- c(rep(1,4), # Region
             rep(2,4), # Insurance
             rep(3,4) # Specialist
             )

# For all variables that are not grouped, assign a unique number 
groups <- c(grouped,c(max(grouped)+1:(length(x_list)-length(grouped)-1)))

#Split predictors and outcome 
x_train <- model.matrix(flag_fooc~., train.data[x_list],)[,-1] # Remove intercept
y_train <- train.data$flag_fooc


#=======================================================#
# (3) Fit model to training set                         #
#=======================================================#

#Find the best lambda using cross-validation
#Default alpha=1 which is what we want for logistic regression
cv.lasso <- cv.grpreg(X=x_train, y=y_train, penalty="grLasso", family="binomial",
                      group=groups, nfolds=5, lambda=seq(0.001,10,by=0.001))

# Optimal lambda
cv.lasso$lambda.min # <- lambda we keep 0.003
#cv.lasso$lambda.1se

# See coefficients 
coef(cv.lasso, s="lambda.min")


#=======================================================#
# (4) Model coefficients                                #
#=======================================================#

# Fit the model with the best lambda to the whole training data set
model <- grpreg(X=x_train, y=y_train, penalty="grLasso", family="binomial", group = groups, lambda=cv.lasso$lambda.min)

# Save coefficients
coefs_chosen <- as.matrix(coef(model)) 

# Keep non-null coefficients only
non_zero <- which(abs(coefs_chosen[,1])>0) 
coefficients_chosen <- rownames_to_column(data.frame(coefs_chosen[non_zero,1]))

# Rename columns
names(coefficients_chosen) <- c('variable','coefficient') 

### Since we want to get ORs, CIs and p-values, we will re-run a regular logistic regression ###
#Grab all coefficients
coefficients <- unique(coefficients_chosen$variable)[!unique(coefficients_chosen$variable)=="(Intercept)"]

#Regular logistic regression
regular_logit <- glm(reformulate(coefficients, "flag_fooc"), data = for_logit_standard, family = "binomial")

##### CLEAN UP #####

#(1) Get coefficients and p-values
coef_final <- rownames_to_column(data.frame(coefficient = summary(regular_logit)$coefficients[,1], pval = summary(regular_logit)$coefficients[,4])) %>%
  rename(variable=rowname)

#(2) Get confidence intervals 
cis <- data.table(confint(regular_logit)) 
names(cis) <- c("lower_ci","upper_ci")

#(3) Combine all info
ors <- cbind(coef_final,cis)

#(4) Exponentiate
cols <- c("coefficient","lower_ci","upper_ci")
ors[cols] <- format(round(exp(ors[cols]),2),nsmall=2,big.mark=",")
ors$value <- paste0(ors$coefficient," (",ors$lower_ci,", ",ors$upper_ci,")")
ors <- ors %>%
  mutate(pval_format= case_when(pval<0.001 ~ "<0.001",
                                TRUE ~ format(round(pval,3),nsmall=3,big.mark=",")))

#Save
today <- Sys.Date()
today_format <- toupper(format(today,format="%d%b%y"))
write.xlsx(ors,paste0(proj_dir,"/r_output/lasso_",today_format,".xlsx"),sheetName="predictors")

#Save as dataset as well - for forest plot
write_fst(ors,path=paste0(out,"/lasso_ors.fst"))

#=======================================================#
# (5) Model performance on training data                #
#=======================================================#

#Create metrics summary table to append to
metrics_summary <- data.table(matrix(ncol=2,nrow=0))
colnames(metrics_summary) <- c("metric","score")

#We only need the AUC calculated on the training set!
#Get prediction measures on training set
preds_internal = predict(model, X=x_train, s='lambda.min', type='response')

#Get optimal cutoff point 
metrics.internal <- cutpointr(x=preds_internal, class=y_train, pos_class=1, method=maximize_metric, metric=youden, direction=">=") %>%
  add_metric(list(npv,ppv))

#Keep only AUC (train)
auc_train <- metrics.internal[1,"AUC"][[1]]
y_train <- train.data$flag_fooc
logloss_train <- LogLoss(y_pred = preds_internal, y_true = y_train)

#Keep Youden (train)
youden_train <- metrics.internal[1,"youden"][[1]]

#Create empty dataframe with list of metrics of interest
summ_train <- data.table("metric"=c("auc_train","logloss_train","youden"),"score"=c(auc_train,logloss_train,youden_train))

#Add lambda 
summ_lambda <- data.table("metric"="lambda","score"=cv.lasso$lambda.min)


#=======================================================#
# (6) Validation on holdout test set                    #
#=======================================================#

# Prepare holdout test set 
x_test <- model.matrix(flag_fooc~., test.data[x_list],)[,-1]
y_test <- test.data$flag_fooc

#Predict values
preds_external <- predict(model, X=x_test, s='lambda.min', type='response')

#Assess model performance
metrics.external <- cutpointr(x=preds_external, class=y_test, pos_class=1, method=oc_manual, cutpoint=metrics.internal$optimal_cutpoint, direction=">=") %>%
  add_metric(list(npv,ppv,youden)) 

#Add log-loss
metrics.external$logloss <- LogLoss(y_pred = preds_external, y_true = y_test)
metrics.external$predicted_rate <-  sum(preds_external > metrics.external$optimal_cutpoint)/length(preds_external)

#List metrics of interest
metric_list <- c("AUC","acc","sensitivity","specificity","optimal_cutpoint","npv","ppv","logloss")

#Combine metrics from internal and holdout
summ_test <- gather(metrics.external[,metric_list], metric, score)
summ_test$metric[summ_test$metric=="AUC"] <- "auc_test"
summ_test$metric[summ_test$metric=="logloss"] <-"logloss_test"


#=======================================================#
# (7) Combine performance metrics                        #
#=======================================================#

metrics_summary <- rbind(summ_train,summ_test,summ_lambda)

metrics_summary$score_rounded <- round(metrics_summary$score, 3)
metrics_summary$score_format <- format(metrics_summary$score_rounded,nsmall=2,big.mark=",")

#Final
final_metrics <- metrics_summary[,c("metric","score_format")]  %>%
  rename(score=score_format)

#Save
today <- Sys.Date()
today_format <- toupper(format(today,format="%d%b%y"))
write.xlsx(final_metrics,paste0(proj_dir,"/r_output/performance_",today_format,".xlsx"),sheetName="metrics")
