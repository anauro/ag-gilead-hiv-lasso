##################################################################
# Case:   041749 - Gilead HIV Retention in Care                       
# Code:   2.1 Retention in Care            			               
# Date:   09.22.2023
# Objective: Retention in Care
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
library(tidyr)

#Set folders
proj_dir <- file.path("//ace/data/health3/gilead_hiv_retention_in_care_041749")
f <- file.path(proj_dir, "data", "r_datasets","final")
out <- file.path(proj_dir, "data", "r_datasets","out")
codes <- file.path(proj_dir,"data","codes")
study_pop <- data.table(read_sas(paste0(proj_dir,"/data/sas_datasets/final/study_pop.sas7bdat")))

#This program will create retention in care for the pre AND post periods using 3 definitions:
#>=2 HIV OP visits >=90 days apart
#>=2 CD4/viral load counts >=90 days apart 
#ART/STR PDC >=90%


###### PREPARE DATA #####

#IMPORT MEDICAL CLAIMS
med_clms <- data.table(read.csv(paste0(f,"/med_claims.csv")))
#Note - import takes a while

#Convert svcdate into date
med_clms$svcdate <- as.Date(med_clms$svcdate,"%d%B%Y")
#Keep only pre and post periods
#Label pre- and post-pandemic periods
med_clms_period <- med_clms %>%
  filter(as.Date("2019-03-01")<=svcdate & svcdate<as.Date("2021-03-01")) %>%
  mutate(period = case_when(svcdate<as.Date("2020-03-01") ~ "pre",
                            TRUE ~ "post"))

#IMPORT MTR AND STR CLAIMS
mtr_clms <- read_fst(paste0(out,"/mtr_adjusted.fst"))
str_clms <- read_fst(paste0(out,"/str_adjusted.fst"))

##### HIV DEFINITION #####

#Grab all OP visits with a primary diagnosis of HIV (i.e., diag_admit or diag1)
#Anything that isn't IP or ER is OP
#We will also exclude OP visits for which the primary diagnosis is COVID-19

hiv1_codes <- paste0(codes,"/hiv1.sas7bdat")
covid19_codes <- c("U00", "U071", "U49")

hiv1 <- read_sas(hiv1_codes)
diagnoses <- med_clms_period %>%
  #If record type=A, then this could be a lab test to rule out a given condition
  filter(!rectype=="A") %>%
  #Keep only OP visits 
  filter(!type %in% c("IP","ER")) %>%
  #Since we are only interested in primary diagnoses, focus on diag_admit and diag1
  select(pat_id,svcdate,type,period,diag_admit,diag1) %>%
  #Exclude all claims where COVID is the primary diagnosis
  mutate(covid19=as.numeric(diag_admit %in% covid19_codes | diag1 %in% covid19_codes)) %>%
  #Drop all claims where COVID is primary diag
  filter(covid19==0)
table(diagnoses$type)

long_dx <- melt(setDT(diagnoses), id.vars = c("pat_id","svcdate","type","period"), variable.name = "Position",value.name="Diagnosis")

#Keep unique HIV OP dx dates
hiv_dx <- long_dx %>%
  inner_join(hiv1,by=c("Diagnosis"="Code")) %>%
  #Select unique dates
  select(pat_id,svcdate,period) %>%
  unique()

retention_hiv <- function(period) {
  
  if (period=="pre") {
    period_hiv <- filter(hiv_dx, period=="pre")
  }
  else {
    period_hiv <- filter(hiv_dx, period=="post")
  }

  #Grab first HIV-1 date for each patient
  first_hiv <- period_hiv[ , .SD[which.min(svcdate)], by = pat_id] %>%
    rename(first_date=svcdate)
  second_hiv <- first_hiv %>%
    inner_join(period_hiv) %>%
    filter(first_date+90<=svcdate)
  
  #Create variable
  ret_hiv <- second_hiv %>%
    select(pat_id) %>%
    unique() %>%
    mutate(ret_hiv=1)
  
  #Rename variables
  new_name <- paste0("ret_hiv_",period)
  setnames(ret_hiv,old="ret_hiv",new=new_name)
  
  return(ret_hiv)
}
ret_hiv_pre <- retention_hiv("pre")
ret_hiv_post <- retention_hiv("post")

#Combine 
retention_hiv <- study_pop %>%
  left_join(ret_hiv_pre) %>%
  left_join(ret_hiv_post) %>%
  replace(is.na(.),0)
table(retention_hiv$ret_hiv_pre)
table(retention_hiv$ret_hiv_post)


##### LAB DEFINITION #####

#Grab all procedures
procedures <- med_clms_period %>%
  select(pat_id,svcdate,period,starts_with("proc_cpt_hcpc"))
#Convert to long format
long_px <- melt(setDT(procedures), id.vars = c("pat_id","svcdate","period"), variable.name = "position", value.name="procedure")
#Remove NAs
long_px <- long_px %>%
  drop_na(procedure)

#Grab relevant flags
#Lab CD4 is 86361, lab HIV viral load is 87536
lab_codes <- c("86361","87536")
lab_px <- long_px %>%
  filter(procedure %in% lab_codes) %>%
  select(pat_id,svcdate,period)

retention_lab <- function(period) {
  
  if (period=="pre") {
    period_lab <- filter(lab_px, period=="pre")
  }
  else {
    period_lab <- filter(lab_px, period=="post")
  }
 
  #Grab first lab date for each patient
  first_lab <- period_lab[ , .SD[which.min(svcdate)], by = pat_id] %>%
    rename(first_date=svcdate)
  second_lab <- first_lab %>%
    inner_join(period_lab) %>%
    filter(first_date+90<=svcdate)
  
  #Create variable
  ret_lab <- second_lab %>%
    select(pat_id) %>%
    unique() %>%
    mutate(ret_lab=1)
  
  #Rename variables
  new_name <- paste0("ret_lab_",period)
  setnames(ret_lab,old="ret_lab",new=new_name)
  
  return(ret_lab)
}
ret_lab_pre <- retention_lab("pre")
ret_lab_post <- retention_lab("post")

#Combine 
retention_lab <- study_pop %>%
  left_join(ret_lab_pre) %>%
  left_join(ret_lab_post) %>%
  replace(is.na(.),0)


##### ART DEFINITION #####

#Expand dataset to create one row for each date that a patient had MTR/STR
#Use adjusted start and end!
#STACK MTR/STR
stack <- rbind(data.table(mtr_clms),data.table(str_clms))
#expand_meds <- data.table(stack)[, .(date = seq(from_dt, end_dt, 'day')), by = .(pat_id, from_dt, end_dt)]
expand_meds <- data.table(stack)[, .(date = seq(adj_start, adj_end, 'day')), by = .(pat_id, adj_start, adj_end)]
#Keep only unique days
expand_unique <- expand_meds %>%
  select(pat_id,date) %>%
  unique()

retention_med <- function(period) {

  if (period=="pre") {
    med_period <- expand_unique %>%
      #Truncate to period if days of supply extends beyond the period of interest
      filter(as.Date("2019-03-01")<=date & date<as.Date("2020-03-01"))
    period_length <- as.numeric(difftime(as.Date("2020-03-01"), as.Date("2019-03-01"), units = "days"))
  }
  else {
    med_period <- expand_unique %>%
      #Truncate to period if days of supply extends beyond the period of interest
      filter(as.Date("2020-03-01")<=date & date<=as.Date("2021-03-01"))
    period_length <- as.numeric(difftime(as.Date("2021-03-01"), as.Date("2020-03-01"), units = "days"))+1
  }

  pdc <- count(med_period,pat_id) %>%
    rename(days_covered=n) %>%
    mutate(pdc=days_covered/period_length) %>%
    mutate(ret_med=case_when(0.9<=pdc ~1,
                             TRUE ~0))
  hist(pdc$pdc)
  
  ret_med <- pdc %>%
    select(pat_id,ret_med)
  
  #Rename variables
  new_name <- paste0("ret_med_",period)
  setnames(ret_med,old="ret_med",new=new_name)
  
  return(ret_med)
}
ret_med_pre <- retention_med("pre")
ret_med_post <- retention_med("post")

#UPDATE: Let's assess this metric only among patients with >=1 STR/ART in the 6-month HIV identification period
#Grab all patients with at least one dispensing during the period between Sept 1, 2018 - March 1, 2019
med_hiv_id <- expand_meds %>%
  filter(as.Date("2018-09-01")<=date & date<as.Date("2019-03-01")) %>%
  mutate(meds_hiv_id = 1) %>%
  select(pat_id,meds_hiv_id) %>%
  unique()
#18,941 patients 

#Combine 
retention_med <- med_hiv_id %>%
  left_join(ret_med_pre) %>%
  left_join(ret_med_post) %>%
  replace(is.na(.),0)

sum(retention_med$ret_med_pre)
sum(retention_med$ret_med_post)

#How many patients have no ART dispensing in the full 18 months pre-index?
pre_index <- expand_meds %>%
  filter(as.Date("2018-09-01")<=date & date<as.Date("2020-03-01")) %>%
  select(pat_id) %>%
  unique()
#19,725
pts_wo_art_str <- nrow(study_pop)-nrow(pre_index)
#507

##### COMBINE #####
retention <- study_pop %>%
  left_join(retention_hiv) %>%
  left_join(retention_lab) %>%
  left_join(retention_med)

#Set to 0
retention$meds_hiv_id[is.na(retention$meds_hiv_id)] <- 0

#Add an ALL category
retention_final <- retention %>%
  mutate(ret_all_pre=case_when(meds_hiv_id==0 ~ NA,
                      TRUE ~ as.numeric(ret_hiv_pre==1 & ret_lab_pre==1 & ret_med_pre==1)),
         ret_all_post=case_when(meds_hiv_id==0 ~ NA,
                       TRUE ~ as.numeric(ret_hiv_post==1 & ret_lab_post==1 & ret_med_post==1)))

table(retention$ret_hiv_pre)
table(retention$ret_hiv_post)
table(retention$ret_lab_pre)
table(retention$ret_lab_post)
table(retention$ret_med_pre)
table(retention$ret_med_post)

###### INVESTIGATION: What if we used a composite measure of retention in care? #####
#Composite measure: 
#(1) Any of the above criteria
#(2) All of the above criteria

retention_composite <- retention %>%
  mutate(ret_pre_any=as.numeric(sum(ret_hiv_pre==1, ret_lab_pre==1, ret_med_pre==1, na.rm=TRUE)>=1),
         ret_post_any=as.numeric(sum(ret_hiv_post==1, ret_lab_post==1, ret_med_post==1, na.rm=TRUE)>=1),
         #For all, we should still evaluate only among all patients with the med use in the period of interest
         ret_pre_all=case_when(meds_hiv_id==0 ~ NA,
                               TRUE ~ as.numeric(ret_hiv_pre==1 & ret_lab_pre==1 & ret_med_pre==1)),
         ret_post_all=case_when(meds_hiv_id==0 ~ NA,
                                 TRUE ~ as.numeric(ret_hiv_post==1 & ret_lab_post==1 & ret_med_post==1)))
table(retention_composite$ret_pre_any)
#100%
table(retention_composite$ret_post_any)
#100%
table(retention_composite$ret_pre_all)
#9,980/18941 (52.9%)
table(retention_composite$ret_post_all)
#7,703/18,941 (40.7%)

#Conclusion: any is too loose, all is an option!
#How about if we just combine the HIV visits and ART adherence?
retention_composite <- retention %>%
  mutate(ret_hiv_art_pre=case_when(meds_hiv_id==0 ~ NA,
                                   TRUE ~ as.numeric(ret_hiv_pre==1 & ret_med_pre==1)),
         ret_hiv_art_post=case_when(meds_hiv_id==0 ~ NA,
                                    TRUE ~ as.numeric(ret_hiv_post==1 & ret_med_post==1)))
table(retention_composite$ret_hiv_art_pre)
#11,872/18941 (62.3%)
table(retention_composite$ret_hiv_art_post)
#10,319/18,941 (54.5%)

#How about if we just combine the lab tests and ART adherence?
retention_composite <- retention %>%
  mutate(ret_lab_art_pre=case_when(meds_hiv_id==0 ~ NA,
                                   TRUE ~ as.numeric(ret_lab_pre==1 & ret_med_pre==1)),
         ret_lab_art_post=case_when(meds_hiv_id==0 ~ NA,
                                    TRUE ~ as.numeric(ret_lab_post==1 & ret_med_post==1)))
table(retention_composite$ret_lab_art_pre)
#10,670/18941 (56.3%)
table(retention_composite$ret_lab_art_post)
#8,551/18,941 (45.0%)

#How about if we just combine the HIV visits and lab tests?
retention_composite <- retention %>%
  mutate(ret_hiv_lab_pre=as.numeric(ret_lab_pre==1 & ret_hiv_pre==1),
         ret_hiv_lab_post=as.numeric(ret_lab_post==1 & ret_hiv_post==1))
table(retention_composite$ret_hiv_lab_pre)
#13,277/19,782 (67.1%)
table(retention_composite$ret_hiv_lab_post)
#8,551/18,941 (45.0%)

##### END INVESTIGATION #####

#Save dataset 
write_fst(retention_final,path=paste0(out,"/retention.fst"))
