##################################################################
# Case:   041749 - Gilead HIV Retention in Care                       
# Code:   2.2 Produce retention in care            			               
# Date:   05.31.2023
# Objective: Produce retention in care
# Author: Ana Urosevic
##################################################################

rm(list=ls())

#Install and load packages
library(data.table)
library(dplyr)
library(haven)
library(openxlsx)
library(fastDummies)
library(fst)

#Set folders
proj_dir <- file.path("//ace/data/health3/gilead_hiv_retention_in_care_041749")
out <- file.path(proj_dir, "data", "r_datasets","out")
study_pop <- data.table(read_sas(paste0(proj_dir,"/data/sas_datasets/final/study_pop.sas7bdat")))

##### PRODUCE OUTPUT #####

retention <- read_fst(paste0(out,"/retention.fst"))

#Specify numeric columns 
all_cols <- colnames(retention)[!colnames(retention) %in% colnames(study_pop)]
cont_vars <- c("none")
dich_vars <- all_cols[!all_cols %in% cont_vars]
cost_vars <- c("none")
round0 <- cost_vars

#Create summary
#Empty dataset to append to
summary <- data.table(matrix(ncol=2,nrow=0))
colnames(summary) <- c("variable","value")


#NUM PTS
num_pts <- data.table("variable"="num_pts","value"=format(length(unique(retention$pat_id)),big.mark=","))
  
  #DICHOTOMOUS VARIABLES
  for(var in dich_vars) {

    freq <- sum(select(retention,var),na.rm=TRUE)
    total <- sum(!is.na(select(retention,var)))
    prop <- format(round(freq/total*100,1),nsmall=1)
    #Combine
    value <- paste0(format(freq,big.mark=",")," (",prop, ")")
    #Summary
    summ_dich <- data.table("variable"=var,"value"=value)
  
    #Append 
    summary <- rbind(summary,summ_dich)
  }
  
  #CONTINUOUS VARIABLES
  #Note: Round to 1 decimal is the default
  for(var in cont_vars) {
    if(var %in% round0) {
      len=0
    } else {
      len=1
    }
    
    mean <- format(round(mean(retention[,get(var)],na.rm=TRUE),digits=len),nsmall=len,big.mark=",")
    median <- format(round(median(retention[,get(var)],na.rm=TRUE),digits=len),nsmall=len,big.mark=",")
    sd <- format(round(sd(retention[,get(var)],na.rm=TRUE),digits=len),nsmall=len,big.mark=",")
  
    if(var %in% cost_vars) {
      #We don't report median for cost variables
      formatted <- paste0("$",mean," ± ",sd)
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
write.xlsx(final_summary,paste0(proj_dir,"/r_output/retention_",today_format,".xlsx"),sheetName="retention")
