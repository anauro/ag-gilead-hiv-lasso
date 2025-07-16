##################################################################
# Case:   041749 - Gilead HIV Retention in Care                       
# Code:   2.0 STR and ART Adjustment            			               
# Date:   06.29.2023
# Objective: Adjust pharmacy claim dates
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

###### PREPARE DATA #####
#The purpose of this program is to adjust the start and end dates for all STR and ART medication

#Import all drug claims
drug_clms <- data.table(read.csv(paste0(f,"/drug_claims.csv")))
#Note - import takes a while

#Convert from_dt into date
drug_clms$from_dt <- as.Date(drug_clms$from_dt,"%d%b%Y")
#Note - do not subset to any period! This will be done in subsequent programs


##### MTR #####

#Grab MTR medication list
mtr_meds <- data.table(read_sas(paste0(codes,"/mtr.sas7bdat"))) %>%
  rename(ndc=NDC)
#Check that all are unique
nrow(mtr_meds)
#BL AUDIT - commented out following line as there is no art_meds dataset and replaced with mtr.
n_distinct(art_meds$ndc)#
n_distinct(mtr_meds$ndc)

#Nice!
drugs_mtr <- inner_join(drug_clms,mtr_meds) %>%
  select(pat_id,from_dt,dayssup, ndc) %>%
  mutate(end_dt=from_dt+dayssup-1)

#Create temporary adjusted start and end
adjust_mtr <- drugs_mtr %>%
  mutate(adj_start=from_dt,
         adj_end=end_dt) %>%
  #Very important to order accordingly!
  #For MTR, we will also only adjust by ndc
  arrange(pat_id, ndc, from_dt)

#Adjust for overlapping pharmacy claims
#If the end of the last dispensing overlaps with the start of the next dispensing, 
#we assume that the patient waiting until they finished their preceding dose before starting the next
start_time <- Sys.time()
#Start from row 2! We won't have any adjustment in the very first row
for(row in 2:nrow(adjust_mtr)) {
  print(row)
  
  #Set current row variables
  pat_id <- adjust_mtr$pat_id[row]
  start <- adjust_mtr$adj_start[row]
  dayssup <- adjust_mtr$dayssup[row]
  ndc <- adjust_mtr$ndc[row]
  
  #Set lag variables
  #This is only for the first time we're defining lag, otherwise, we will just carry over the lag
  if (row==2) {
    lag_pat_id <- adjust_mtr$pat_id[row-1]
    lag_end <- adjust_mtr$adj_end[row-1]
    lag_ndc <- adjust_mtr$ndc[row-1]
  }
  
  #We will only adjust if the following 3 conditions are met:
  #(1) If it's the same patient as the previous row (i.e., the first dispensing for each patient is always unadjusted)
  #(2) If the end date of the previous row overlaps with the start date of the current row
  #(3) If it's the same NDC as the previous row (i.e., the first NDC for each patient is always unadjusted)
  if (pat_id==lag_pat_id & lag_end>=start & ndc==lag_ndc) {
    new_start <- lag_end +1
    new_end <- new_start+dayssup-1
  }  else {
    new_start <- start
    new_end <- start+dayssup-1
  }
  
  #Update adjusted start and adjusted end
  adjust_mtr[row,"adj_start"] <- new_start
  adjust_mtr[row,"adj_end"] <- new_end
  
  #Update lagged variables
  lag_pat_id <- pat_id
  lag_end <- new_start+dayssup-1
  lagndc <- ndc
}
end_time <- Sys.time()
print(paste0("This step took ",end_time-start_time," minutes to run!"))
#Note - this step takes a long time to run! ~1 hour

#Sort
mtr_adjust_sorted <- adjust_mtr %>%
  arrange(pat_id,adj_start)

#Save dataset 
write_fst(mtr_adjust_sorted,path=paste0(out,"/mtr_adjusted.fst"))


##### STR DEFINITION #####

#Grab STR medication list
str_meds <- data.table(read_sas(paste0(codes,"/str.sas7bdat"))) %>%
  rename(ndc=NDC)
#Check that all are unique
nrow(str_meds)
n_distinct(str_meds$ndc)
#Nice!
drugs_str <- inner_join(drug_clms,str_meds) %>%
  select(pat_id,from_dt,dayssup, ndc) %>%
  mutate(end_dt=from_dt+dayssup-1)

#Create temporary adjusted start and end
adjust_str <- drugs_str %>%
  mutate(adj_start=from_dt,
         adj_end=end_dt) %>%
  #Very important to order accordingly!
  arrange(pat_id, from_dt)

#Adjust for overlapping pharmacy claims
#If the end of the last dispensing overlaps with the start of the next dispensing, 
#we assume that the patient waiting until they finished their preceding dose before starting the next
start_time <- Sys.time()
#Start from row 2! We won't have any adjustment in the very first row
for(row in 2:nrow(adjust_str)) {
  print(row)
  
  #Set current row variables
  pat_id <- adjust_str$pat_id[row]
  start <- adjust_str$adj_start[row]
  dayssup <- adjust_str$dayssup[row]

  #Set lag variables
  #This is only for the first time we're defining lag, otherwise, we will just carry over the lag
  if (row==2) {
    lag_pat_id <- adjust_str$pat_id[row-1]
    lag_end <- adjust_str$adj_end[row-1]
  }
  
  #We will only adjust if the following 2 conditions are met:
  #(1) If it's the same patient as the previous row (i.e., the first dispensing for each patient is always unadjusted)
  #(2) If the end date of the previous row overlaps with the start date of the current row
  if (pat_id==lag_pat_id & lag_end>=start) {
    new_start <- lag_end +1
    new_end <- new_start+dayssup-1
  }  else {
    new_start <- start
    new_end <- start+dayssup-1
  }

  #Update adjusted start and adjusted end
  adjust_str[row,"adj_start"] <- new_start
  adjust_str[row,"adj_end"] <- new_end
  
  #Update lagged variables
  lag_pat_id <- pat_id
  lag_end <- new_start+dayssup-1
}
end_time <- Sys.time()
print(paste0("This step took ",end_time-start_time," minutes to run!"))
#Note - this step takes a long time to run!

#Save dataset 
write_fst(adjust_str,path=paste0(out,"/str_adjusted.fst"))
