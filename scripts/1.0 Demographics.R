##################################################################
# Case:   041749 - Gilead HIV Retention in Care                       
# Code:   1.0 Demographics            			               
# Date:   05.19.2023 / 09.12.2023 - Add urbanicity
# Objective: Demographics
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
library(stringr)

#Set folders
proj_dir <- file.path("//ace/data/health3/gilead_hiv_retention_in_care_041749")
f <- file.path(proj_dir, "data", "r_datasets","final")
out <- file.path(proj_dir, "data", "r_datasets","out")
codes <- file.path(proj_dir,"data","codes")
study_pop <- data.table(read_sas(paste0(proj_dir,"/data/sas_datasets/final/study_pop.sas7bdat")))

#Variables to create in this program:
#Age
#Sex
#Health plan type
#Geographic region
#Physician specialty

##############################
# Import Rural-Urban Codes          
##############################

rucc <- data.table(read.xlsx(paste0(codes,"/ruralurbancodes2013.xlsx"), sheet="Codes"))

#Categories:
#1 Counties in metro areas of 1 million population or more
#2 Counties in metro areas of 250,000 to 1 million population
#3 Counties in metro areas of fewer than 250,000 population
#4 Urban population of 20,000 or more, adjacent to a metro area
#5 Urban population of 20,000 or more, not adjacent to a metro area
#6 Urban population of 2,500 to 19,999, adjacent to a metro area
#7 Urban population of 2,500 to 19,999, not adjacent to a metro area
#8 Completely rural or less than 2,500 urban population, adjacent to a metro area
#9 Completely rural or less than 2,500 urban population, not adjacent to a metro area

#Since we only have FIPS, we need to convert to ZIP
#Use ZIP-FIPS crosswalk
zip_to_fips <- data.table(read.xlsx(paste0(codes,"/ZIP_FIPS_crosswalk.xlsx"),sheet="ZIP_COUNTY")) %>%
  select(ZIP,COUNTY) %>%
  rename(FIPS=COUNTY)

#Merge on FIPS
rucc_zip <- rucc %>%
  left_join(zip_to_fips) %>%
  select(ZIP,Description,RUCC_2013) %>%
  rename(rucc=RUCC_2013)

#Since we only have ZIP3 instead of ZIP5, we need to calculate average RUCA for each ZIP3
table(rucc_zip$rucc)

rucc_zip3 <- rucc_zip %>%
  mutate(zip3=substring(ZIP,1,3)) %>%
  group_by(zip3) %>%
  summarize(avg_rucc=round(mean(rucc))) %>%
  arrange(zip3)

#Add high-level categories
rucc_categories <- rucc_zip3 %>%
  mutate(urbanicity=case_when(avg_rucc %in% c(1,2,3) ~ "metro",
                              avg_rucc %in% c(4,5,6,7) ~ "urban",
                              avg_rucc %in% c(8,9) ~ "rural")) %>%
  select(-avg_rucc)
table(rucc_categories$urbanicity)

##############################
# (1) Import eligibility data
##############################

elig <- data.table(read.csv(paste0(f,"/elig.csv")))

#Add urbanicity as an additional variable 
elig_urbanicity <- elig %>% 
  #Convert zip3 to character
  mutate(zip3=str_pad(as.character(pat_zip3),3,pad="0")) %>%
  left_join(rucc_categories) %>%
  #If NA urbanicity, then just make it unknown 
  mutate(urbanicity=coalesce(urbanicity,"unk"))
table(elig_urbanicity$urbanicity)

#Great, there is one row for each patient! 
#Select only variables of interest
elig_subset <- elig_urbanicity %>%
  select(pat_id, index_dt, der_yob, der_sex, pat_region, prd_type,urbanicity) %>%
  rename(yob=der_yob, sex=der_sex, reg=pat_region, plan=prd_type,urban=urbanicity)

#Update index date to date variable
elig_subset$index_dt <- as.Date(elig_subset$index_dt,"%d%b%Y")

#Quick test
for(i in 1:length(colnames(elig_subset))) {
  unique_values <- unique(select(elig_subset,colnames(elig_subset)[i]))
  print(unique_values)
}
#Great - no missing values in sex
#In region we have O - this is unknown
#In product type (i.e., insurance plan), U represents unknown


##############################
# (2) Calculate variables
##############################

#Update variables so that the colname is a prefix
elig_vars <- dummy_cols(elig_subset, select_columns = c("sex","reg","plan","urban")) %>%
  select(pat_id, index_dt, yob, starts_with(c("sex_","reg_","plan_","urban_"))) 
#Rename so that they are all lowercase
old_names <- colnames(elig_vars)[grepl("sex_|reg_|plan_|urban_",colnames(elig_vars))]
new_names <- tolower(old_names)
setnames(elig_vars,old=old_names,new=new_names)

#Calculate age
#Set birth date to July 1 of birth year (i.e., middle of year)
elig_vars$bday <- as.Date(paste(elig_vars$yob,"07","01"), "%Y %m %d")
elig_vars[, age:=as.numeric(index_dt-bday)/365]
#Add categories of age: <20, 20-49, >=50
age_cat <- elig_vars %>%
  mutate(age_cat=case_when(age<20 ~ "lt20",
                           age>=20 & age<50 ~ "20_50",
                           age>=50 ~ "50p")) %>%
  dummy_cols(select_columns="age_cat")
table(age_cat$age_cat)
x_elig_vars <- age_cat %>%
  select(-c("index_dt","yob","bday","age_cat"))

#Healthcare professional specialty will be from HIV diagnosis closest to the index date
#Grab all medical claims occurring during the PRE-INDEX period (18 months prior to the index date)
med_clms <- data.table(read.csv(paste0(f,"/med_claims.csv")))
#Note - importing the data takes some time!

#Subset to baseline
#Convert service date to date
med_clms$svcdate <- as.Date(med_clms$svcdate,"%d%b%Y")
pre_index_med_clms <- med_clms %>%
  filter(as.Date("2018-09-01")<=svcdate & svcdate<=as.Date("2020-03-01")) %>%
  #Since we will be looking at diagnoses, only keep records that are not ancillary!
  filter(!rectype=="A")
#Keep only variables of interest - i.e., pat_id, svcdate, rend spec, diag_admit, diag1-12
pre_index_med_clms <- pre_index_med_clms %>%
  select(pat_id,svcdate,rend_spec,starts_with("diag")) %>%
  select(-c("diagprc_ind"))

long <- melt(setDT(pre_index_med_clms), id.vars = c("pat_id","svcdate","rend_spec"), variable.name = "Position",value.name="Diagnosis")
#Identify HIV diagnoses
hiv1_codes <- paste0(codes,"/hiv1.sas7bdat")
hiv1 <- read_sas(hiv1_codes)

pre_index_hiv_clms <- inner_join(long,hiv1,by=c("Diagnosis"="Code"))
#Sort and grab last occurrence for each patient (i.e., closest to the index date)
#Since patients could have multiple specialist types on the same day, then select all entries that occur on the last date
last_hiv <- pre_index_hiv_clms %>% group_by(pat_id) %>% slice_max(svcdate)

#Categories:
#Infectious disease: INF_DIS 
#Primary care: NRS_PRCT, INTERN, GP_FP, PHYS_AST, GERIATRC
#OBGYN: OB_GYN
#Unknown: N/A, UNKNOWN, no CPI
#Other is all remaining categories
spec_types <- last_hiv %>%
  select(pat_id,svcdate,rend_spec) %>%
  mutate(spec = case_when(
    rend_spec=="INF_DIS" ~ "inf_dis",
    rend_spec %in% c("NRS_PRCT","INTERN","GP_FP","PHYS_AST","GERIATRC") ~ "pc",
    rend_spec=="OB_GYN" ~ "obgyn",
    rend_spec %in% c("--no cpi--","N/A","UNKNOWN") ~ "unk",
    TRUE ~ "oth"
  ))

ranking_order <- c("inf_dis","obgyn","pc","oth","unk")
spec_types_order <- spec_types %>% 
  mutate(spec = factor(spec, levels = ranking_order)) %>%
  arrange(pat_id, spec)
#Grab first entry!
specialist <- spec_types_order %>%
  group_by(pat_id) %>%
  arrange(pat_id,spec) %>%
  filter(row_number()==1)
#Check
table(specialist$spec)

#Merge with study_pop
spec_merge <- study_pop[,c('pat_id')] %>%
  left_join(specialist)
sapply(spec_merge, anyNA)
spec_merge$spec[is.na(spec_merge$spec)] <- "unk"
sapply(spec_merge, anyNA)
x_specialist <- dummy_cols(spec_merge, select_columns = "spec") %>%
  select(pat_id, starts_with("spec_"))


##############################
# (3) Combine
##############################
demo <- study_pop %>%
  left_join(x_elig_vars) %>%
  left_join(x_specialist)

#Save dataset :) 
write_fst(demo,path=paste0(out,"/demo.fst"))
