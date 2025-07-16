##################################################################
# Case:   041749 - Gilead HIV Retention in Care                       
# Code:   3.3 Produce baseline characteristics            			               
# Date:   07.31.2023
# Objective: By cohort for LASSO
# Author: Ana Urosevic
##################################################################

rm(list=ls())

#Install and load packages
library(data.table)
library(dplyr)
library(openxlsx)
library(fst)
library(stddiff)
library(tidyr)
library(stringr)

#Set folders
proj_dir <- file.path("//ace/data/health3/gilead_hiv_retention_in_care_041749")
out <- file.path(proj_dir, "data", "r_datasets","out")
sp <- file.path(proj_dir, "data", "r_datasets","study_pop")

#Study pop
cohorts <- read.fst(paste0(sp,"/cohorts.fst"))

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
baseline <- cohorts %>%
  left_join(demo) %>%
  left_join(drug_labs) %>%
  left_join(hru_cost) %>%
  left_join(comorbs)


##### PRODUCE OUTPUT #####
#Specify numeric columns 
all_cols <- colnames(baseline)[!colnames(baseline) %in% colnames(cohorts)]
cont_vars <- all_cols[grepl("^num_|^cost_|^los|^cci",all_cols)]
#Remove COVID test, since this is zero among all patients!
cont_vars <- cont_vars[!cont_vars=="num_lab_covid"]
cont_vars <- append(cont_vars,"age")
dich_vars <- all_cols[!all_cols %in% cont_vars]
cost_vars <- cont_vars[grepl("^cost_",cont_vars)]
round0 <- cost_vars
round2 <- cont_vars[grepl("^num_|cci",cont_vars)]
round2 <- round2[!round2 %in% c("num_lab_hiv","num_lab_cd4","num_lab_covid")]

#Create summary
#Empty dataset to append to
summary <- data.table(matrix(ncol=3,nrow=0))
colnames(summary) <- c("ric","fooc","std_diff")

#NUM PTS
num_pts <- baseline %>% 
  count(cohort) %>%
  rename(group=cohort,value=n) %>%
  mutate(value=format(value,big.mark=",")) %>%
  spread(key="group", value="value") %>%
  mutate(variable="num_pts")


#DICHOTOMOUS VARIABLES
for(var in dich_vars) {
  
  by_cohort <- baseline %>%
    group_by(cohort) %>%
    summarize(freq=sum(get(var),na.rm=TRUE),
              total=sum(!is.na(get(var)),na.rm=TRUE)) %>%
    ungroup() %>%
    mutate(prop=format(round(freq/total*100,1),nsmall=1)) %>%
    mutate(value=paste0(format(freq,big.mark=",")," (",prop, ")")) %>%
    select(cohort,value) %>%
    spread(key = "cohort", value="value") %>%
    mutate(variable=var)
  
  var_num <- match(var,names(baseline))
  std_diff <- stddiff.binary(baseline,gcol=match("cohort",names(baseline)),vcol=c(var_num,var_num))[1,"stddiff"]*100
  
  #Add std. diff. 
  summ_dich <- by_cohort %>%
    mutate(std_diff=std_diff)
  
  #Append 
  summary <- rbind(summary,summ_dich,fill=TRUE)
}

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
  
  by_cohort <- baseline %>%
    group_by(cohort) %>%
    summarize(mean=format(round(mean(get(var),na.rm=TRUE),digits=len),nsmall=len,big.mark=","),
           median=format(round(median(get(var),na.rm=TRUE),digits=0),nsmall=0,big.mark=","),
           sd=format(round(sd(get(var),na.rm=TRUE),digits=len),nsmall=len,big.mark=",")) %>%
    mutate(value=case_when(var %in% cost_vars ~  paste0("$",mean," ± ",sd," [",median,"]"),
                           TRUE ~ paste0(mean," ± ",sd," [",median,"]"))) %>%
    select(cohort,value) %>%
    spread(key = "cohort", value="value") %>%
    mutate(variable=var)
  
  var_num <- match(var,names(baseline))
  std_diff <- stddiff.numeric(baseline,gcol=match("cohort",names(baseline)),vcol=c(var_num,var_num))[1,"stddiff"]*100
  
  #Add std. diff. 
  summ_cont <- by_cohort %>%
    mutate(std_diff=std_diff)
  
  #Append 
  summary <- rbind(summary,summ_cont)
}

#Final
final_summary <- rbind(num_pts,summary,fill=TRUE)

#Formatting for excel
to_excel <- final_summary[,c("variable","ric","fooc","std_diff")]
to_excel$std_diff <- str_trim(format(round(to_excel$std_diff,digits=1),nsmall=1,big.mark=","))
to_excel$ric <- str_trim(to_excel$ric)
to_excel$fooc <- str_trim(to_excel$fooc)
to_excel$std_diff[to_excel$std_diff=="NaN"] <- "0.0"
to_excel$std_diff[to_excel$std_diff=="NA"] <-""

#Save
today <- Sys.Date()
today_format <- toupper(format(today,format="%d%b%y"))
write.xlsx(to_excel,paste0(proj_dir,"/r_output/baseline_cohorts_",today_format,".xlsx"),sheetName="baseline")
