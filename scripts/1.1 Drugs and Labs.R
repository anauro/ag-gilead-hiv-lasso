##################################################################
# Case:   041749 - Gilead HIV Retention in Care                       
# Code:   1.1 Drugs and Labs            			               
# Date:   05.20.2023
# Objective: Drugs and Labs
# Author: Ana Urosevic
##################################################################

rm(list=ls())

#Install and load packages
library(data.table)
library(tidyverse)
library(dplyr)
library(haven)
library(openxlsx)
library(fastDummies)
library(fst)

#Set folders
proj_dir <- file.path("//ace/data/health3/gilead_hiv_retention_in_care_041749")
f <- file.path(proj_dir, "data", "r_datasets","final")
out <- file.path(proj_dir, "data", "r_datasets","out")
codes <- file.path(proj_dir,"data","codes")
study_pop <- data.table(read_sas(paste0(proj_dir,"/data/sas_datasets/final/study_pop.sas7bdat")))

#Variables to create in this program:
#Medication use: Concomitant, ATR, STR
#Laboratory tests: CD4, HIV, COVID

###### MEDICATION USE #####

#(1) IMPORT MEDICATION LISTS
concom_meds <- data.table(read_sas(paste0(codes,"/concomitant_ndcclass.sas7bdat"))) %>%
  rename("ndc"="Code","flag"="Class") %>%
  select(ndc,flag) %>%
  mutate(med_cat="concom")
mtr_meds <- data.table(read_sas(paste0(codes,"/mtr.sas7bdat"))) %>%
  rename("ndc"="NDC","flag"="Flag") %>%
  select(ndc,flag) %>%
  mutate(med_cat="mtr")
str_meds <- data.table(read_sas(paste0(codes,"/str.sas7bdat"))) %>%
  rename("ndc"="NDC","flag"="Flag") %>%
  select(ndc,flag) %>%
  mutate(med_cat="str")
#MDDB contains all GPI/NDC combinations, and will be used to identify pill burden
all_ndc_gpi <- data.table(read_sas(paste0(codes,"/mddb.sas7bdat"))) %>%
  select(NDC,GPI) %>%
  rename("ndc"="NDC","gpi"="GPI") %>%
  unique()

#Combine
meds_list <- rbind(concom_meds,mtr_meds,str_meds)

#(2) IMPORT DRUG CLAIMS
drug_clms <- data.table(read.csv(paste0(f,"/drug_claims.csv")))
#Note - import takes a while

#Convert from_dt into date
drug_clms$from_dt <- as.Date(drug_clms$from_dt,"%d%b%Y")
#Grab only during baseline
bl_drug_clms <- drug_clms %>%
  filter(as.Date("2019-03-01")<=from_dt & from_dt<as.Date("2020-03-01")) %>%
  select(pat_id,from_dt,ndc)

#Merge on medication list
bl_meds <- inner_join(bl_drug_clms,meds_list)
unique(bl_meds$flag)

#(3) CREATE MEDICATION FLAGS
#Update flags so that you can easily create dummy variables
#All we need to do is convert everything to lowercase and replace spaces and / with underscores
bl_meds$med <- bl_meds$flag
bl_meds$med <- gsub(" ","_",bl_meds$med)
bl_meds$med <- gsub("/","_",bl_meds$med)
bl_meds$med <- gsub("-","_",bl_meds$med)
#Convert all to lowercase 
bl_meds$med <- tolower(bl_meds$med)
unique(bl_meds$med)
med_flags <- dummy_cols(bl_meds, select_columns = c("med","med_cat")) %>% 
  select(pat_id,starts_with("med_")) %>%
  select(-med_cat)
#Grab max by patient
med_flags_max <- med_flags %>%
  group_by(pat_id) %>%
  select(starts_with("med_")) %>%
  summarise_all(max,na.rm=TRUE)

#(4) DEFINE PILL BURDEN
#Merge all GPI codes onto all NDCs during baseline
gpi <- bl_drug_clms %>%
  select(pat_id,from_dt,ndc) %>%
  inner_join(all_ndc_gpi)
#If this worked as expected, we should have 1 GPI code for each row
test <- gpi %>%
  filter(is.na(gpi))
#GPI is missing only for 480 records - we will assume the NDC code is incorrectly recorded (some are dates)
#Therefore, let's use an inner join

#Keep only first two characters of GPI
gpi_2char <- gpi %>%
  mutate(gpi2=substr(gpi,1,2)) %>%
  select(pat_id,gpi2) %>%
  #Keep only unique 
  unique()

#Count by patient
count_gpi_types <- count(gpi_2char,pat_id) %>%
  rename("num_gpi"="n") 
hist(count_gpi_types$num_gpi)
  
#Merge with study pop
x_meds <- study_pop %>%
  select(pat_id,index_dt) %>%
  left_join(med_flags_max) %>%
  left_join(count_gpi_types)
sapply(x_meds, anyNA)
#Replace N/A
x_meds <- x_meds %>%
  mutate_at(vars(starts_with("med_") | starts_with("num_")), ~replace(., is.na(.), 0)) %>%
  mutate(gpi_type=case_when(num_gpi<=5 ~ "5",
                            num_gpi>5 & num_gpi<=10 ~ "10",
                            TRUE ~ "11p")) %>%
  dummy_cols(select_columns="gpi_type") %>%
  select(-gpi_type)
sapply(x_meds, anyNA)
#Nice!


##### LABORATORY TESTS #####
#(1) IMPORT MEDICAL CLAIMS
med_clms <- data.table(read.csv(paste0(f,"/med_claims.csv")))
#Note - import takes a while

#Convert svc into date
med_clms$svcdate <- as.Date(med_clms$svcdate,"%d%b%Y")
#Grab only during baseline
bl_med_clms <- med_clms %>%
  filter(as.Date("2019-03-01")<=svcdate & svcdate<as.Date("2020-03-01")) %>%
  select(pat_id,svcdate,starts_with("proc_cpt_hcpc"))
#Convert to long format
long <- melt(setDT(bl_med_clms), id.vars = c("pat_id","svcdate"), variable.name = "position", value.name="procedure")
#Remove NAs
long <- long %>%
  drop_na(procedure)

#(2) CREATE LAB FLAGS
#Flag relevant procedures
lab_flag <- long %>% 
  mutate(lab_cd4=
           case_when(procedure=="86361" ~1,
         TRUE ~ 0),
         lab_hiv=
           case_when(procedure=="87536" ~1,
                     TRUE ~0),
         lab_covid=
           case_when(procedure %in% c("87635","86328","86769") ~1,
                     TRUE ~0))
#Grab max by patient
lab_flag_max <- lab_flag %>%
  group_by(pat_id) %>%
  select(starts_with("lab_")) %>%
  summarise_all(max,na.rm=TRUE)
#Grab sum by patient
lab_count <- lab_flag %>%
  group_by(pat_id) %>%
  select(starts_with("lab_")) %>%
  summarise_all(sum, na.rm=TRUE) %>%
  rename("num_lab_cd4"="lab_cd4","num_lab_hiv"="lab_hiv","num_lab_covid"="lab_covid")

#Merge
x_labs <- study_pop %>%
  select(pat_id,index_dt) %>%
  left_join(lab_flag_max) %>%
  left_join(lab_count)

#Replace NA
x_labs <- x_labs %>%
  mutate_at(vars(starts_with("lab_")), ~replace(., is.na(.), 0))
sapply(x_labs, anyNA)
#Update to report num of labs among patients with at least one
x_labs$num_lab_hiv[x_labs$num_lab_hiv==0] <-NA
x_labs$num_lab_cd4[x_labs$num_lab_cd4==0] <-NA
x_labs$num_lab_covid[x_labs$num_lab_covid==0] <-NA
#Nice!


##### COMBINE #####
drug_labs <- study_pop %>%
  left_join(x_meds) %>%
  left_join(x_labs)

#Save dataset 
write_fst(drug_labs,path=paste0(out,"/drug_labs.fst"))
