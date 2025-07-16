##################################################################
# Case:   041749 - Gilead HIV Retention in Care                       
# Code:   2.2 Comparison of retention in care            			               
# Date:   07.17.2023
# Objective: Comparison of retention in care 
# Author: Ana Urosevic
##################################################################

rm(list=ls())

#Install and load packages
library(data.table)
library(dplyr)
library(haven)
library(openxlsx)
library(fst)
library(gee)

#Set folders
proj_dir <- file.path("//ace/data/health3/gilead_hiv_retention_in_care_041749")
out <- file.path(proj_dir, "data", "r_datasets","out")
study_pop <- data.table(read_sas(paste0(proj_dir,"/data/sas_datasets/final/study_pop.sas7bdat")))

##### LOGISTIC REGRESSION #####

retention <- read_fst(paste0(out,"/retention.fst")) 

logistic_regression <- function(var_label) {
  
  #Grab variables of interest
  if (var_label=="ret_med") {
    grab <- retention %>%
      filter(meds_hiv_id==1) %>%
      select(pat_id, starts_with(var_label))
  } else {
    grab <- retention %>%
      select(pat_id,starts_with(var_label))
  }
  
  #Reshape to long format
  reshape <- melt(setDT(grab), id.vars = c("pat_id"), variable.name = "label", value.name=var_label) %>%
    mutate(period = substring(label,9)) %>%
    mutate(period=factor(period))
  
  #Re-order so that periodpre is the reference category
  logit_input <- within(reshape, period <- relevel(period,ref=2)) %>%
    mutate(pat_id=as.factor(pat_id)) %>%
    #VERY IMPORTANT! must be arranged in order of patient ID otherwise it won't work!!
    arrange(pat_id)
  
  #Run model
  gee_logit <- gee(get(var_label) ~ period,
                   data = logit_input, 
                   id = pat_id, 
                   family = binomial,
                   corstr = "exchangeable")
  #Corstr="independence" is a regular logistic model - i.e., no corr between responses for the same patient
  
  #Grab summary
  # (1) Odds Ratio
  coef <- gee_logit$coefficients["periodpost"]
  odds_ratio <- exp(coef)
  
  #(2) Confidence Interval
  #Grab robust standard error
  coefficients <-summary(gee_logit)["coefficients"]
  rob.se <- coefficients[[1]]["periodpost","Robust S.E."]
  CIs <- exp(coef(gee_logit)["periodpost"]+c(-1,1)*rob.se*qnorm(0.975))
  ci_2.5 <- CIs[1]
  ci_97.5 <- CIs[2]
  
  #(3) P-value
  pval <-  2 * pnorm(abs(coef(summary(gee_logit))["periodpost","Robust z"]), lower.tail = FALSE)

  #Format
  format_or <- format(round(odds_ratio,digits=2),nsmall=2,big.mark=",")
  format_ci <- paste0("(",format(round(ci_2.5,digits=2),nsmall=2),", ",format(round(ci_97.5,digits=2),nsmall=2),")")
  format_or_ci <- paste0(format_or," ", format_ci)
  
  if (pval<0.001) {
    format_pval <- "<0.001"
  } else {
    format_pval <- format(round(pval,digits=3),nsmall=3)
  }
  
  #Create summary
  #Empty dataset to append to
  summary <- data.table(matrix(ncol=3,nrow=0))
  colnames(summary) <- c("variable","odds_ratio","pval")
  summ <- data.table("variable"=var_label, "odds_ratio"=format_or_ci,"pval"=format_pval)
  
return(summ)  
}
logit_hiv <- logistic_regression("ret_hiv")
logit_lab <- logistic_regression("ret_lab")
logit_med <- logistic_regression("ret_med")

final_summary <- rbind(logit_hiv,logit_lab,logit_med)

#Save
today <- Sys.Date()
today_format <- toupper(format(today,format="%d%b%y"))
write.xlsx(final_summary,paste0(proj_dir,"/r_output/retention_comparison_",today_format,".xlsx"),sheetName="retention_comparison")
