##################################################################
# Case:   041749 - Gilead HIV Retention in Care                       
# Code:   3.0 Cohort Selection          			               
# Date:   07.31.2023
# Objective: Prediction of retention in care 
# Author: Ana Urosevic
##################################################################

rm(list=ls())

#Install and load packages
library(data.table)
library(dplyr)
library(haven)
library(openxlsx)
library(fst)

#Set folders
proj_dir <- file.path("//ace/data/health3/gilead_hiv_retention_in_care_041749")
out <- file.path(proj_dir, "data", "r_datasets","out")
sp <- file.path(proj_dir, "data", "r_datasets", "study_pop")
study_pop <- data.table(read_sas(paste0(proj_dir,"/data/sas_datasets/final/study_pop.sas7bdat")))

#=======================================================#
# 1. Study Pop from Phase I                             #
#=======================================================#

box1 <- study_pop
#Save dataset 
write_fst(box1,path=paste0(sp,"/box1.fst"))

#=======================================================#
# 2. Retained in care in pre-pandemic                   #
#=======================================================#

#Grab all retention in care metrics defined in the previous program
retention <- read_fst(paste0(out,"/retention.fst")) 
#We will use retention in care defined by the HIV metric

merge <- box1 %>%
  inner_join(retention) 

box2 <- merge %>%
  filter(ret_med_pre==1)

#Save dataset
write_fst(box2,path=paste0(sp,"/box2.fst"))

#=======================================================#
# 3. Retained in Care/Falling Out of Care Cohorts       #
#=======================================================#

#Retained in care: ret_hiv_post=1
#Falling out of care: ret_hiv_post=0

define_cohorts <- box2 %>%
  mutate(cohort=case_when(ret_med_pre==1 & ret_med_post==1 ~ "ric",
                          TRUE ~ "fooc"))
table(define_cohorts$cohort)

cohorts <- define_cohorts[,c("pat_id","index_dt","start","end","cohort")] 

#Save dataset
write_fst(cohorts,path=paste0(sp,"/cohorts.fst"))
