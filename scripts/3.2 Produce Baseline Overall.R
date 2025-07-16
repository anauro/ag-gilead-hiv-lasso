##################################################################
# Case:   041749 - Gilead HIV Retention in Care                       
# Code:   3.2 Produce baseline characteristics            			               
# Date:   07.31.2023
# Objective: Overall cohort for LASSO
# Author: Ana Urosevic
##################################################################

rm(list=ls())

#Install and load packages
library(data.table)
library(dplyr)
library(openxlsx)
library(fst)
library(tidyr)

#Set folders
proj_dir <- file.path("//ace/data/health3/gilead_hiv_retention_in_care_041749")
out <- file.path(proj_dir, "data", "r_datasets","out")
sp <- file.path(proj_dir, "data", "r_datasets","study_pop")

#Study pop
study_pop <- read.fst(paste0(sp,"/cohorts.fst"))

##### COMBINE BASELINE DATASETS #####
#The following are relevant:
#Demo
#Drug and Labs
#HRU and Costs
demo <- read_fst(paste0(out,"/demo.fst"))
drug_labs <- read_fst(paste0(out,"/drug_labs.fst"))
hru_cost <- read_fst(paste0(out,"/hru_cost.fst"))
comorbs <- read_fst(paste0(out,"/comorbs.fst"))

#Import
baseline <- study_pop %>%
  left_join(demo) %>%
  left_join(drug_labs) %>%
  left_join(hru_cost) %>%
  left_join(comorbs)


##### PRODUCE OUTPUT #####
#Specify numeric columns 
all_cols <- colnames(baseline)[!colnames(baseline) %in% colnames(study_pop)]
cont_vars <- all_cols[grepl("^num_|^cost_|^los|^cci",all_cols)]
cont_vars <- append(cont_vars,"age")
dich_vars <- all_cols[!all_cols %in% cont_vars]
cost_vars <- cont_vars[grepl("^cost_",cont_vars)]
round0 <- cost_vars
round2 <- cont_vars[grepl("^num_|cci",cont_vars)]
round2 <- round2[!round2 %in% c("num_lab_hiv","num_lab_cd4","num_lab_covid")]

#Create summary
#Empty dataset to append to
summary <- data.table(matrix(ncol=2,nrow=0))
colnames(summary) <- c("variable","value")

#NUM PTS
num_pts <- data.table("variable"="num_pts","value"=format(length(unique(baseline$pat_id)),big.mark=","))

#DICHOTOMOUS VARIABLES
for(var in dich_vars) {
  freq <- sum(select(baseline,var))
  total <- sum(! is.na(select(baseline,var)))
  prop <- format(round(freq/total*100,1),nsmall=1)
  #Combine
  value <- paste0(format(freq,big.mark=",")," (",prop, ")")
  #Summary
  summ_dich <- data.table("variable"=var,"value"=value)

  #Append 
  summary <- rbind(summary,summ_dich)
}

#ALTERNATIVE LAPPLY APPROACH
# calculate_dichotomous <- function(x) {
#   freq <- sum(x)
#   total <-sum(!is.na(x))
#   prop <- format(round(freq/total*100,1),nsmall=1)
#   value <- paste0(format(freq,big.mark=",")," (",prop, ")")
#   
#   return(value)
# }
# test <- baseline[,dich_vars]
# results <- lapply(test, calculate_dichotomous)
# summary <- data.table("variable"=names(results),"value"=unlist(results))

#CONTINUOUS VARIABLES
#Note: Round to 1 decimal is the default
for(var in cont_vars) {
  
  if(var %in% round0) {
    len=0
  } else if(var %in% round2){
    len=2
  } else{
    len=1
  }
  
  mean <- format(round(mean(baseline[,var],na.rm=TRUE),digits=len),nsmall=len,big.mark=",")
  median <- format(round(median(baseline[,var],na.rm=TRUE),digits=0),nsmall=0,big.mark=",")
  sd <- format(round(sd(baseline[,var],na.rm=TRUE),digits=len),nsmall=len,big.mark=",")

  if(var %in% cost_vars) {
    formatted <- paste0("$",mean," ± ",sd," [",median,"]")
  } else {
    formatted <- paste0(mean," ± ",sd," [",median,"]")
  }
  summ_cont <- data.table("variable" = var, "value" = formatted)
  
  #Append 
  summary <- rbind(summary,summ_cont)
}

#Final
final_summary <- rbind(num_pts,summary)

#Save
today <- Sys.Date()
today_format <- toupper(format(today,format="%d%b%y"))
write.xlsx(final_summary,paste0(proj_dir,"/r_output/baseline_overall_",today_format,".xlsx"),sheetName="baseline")
