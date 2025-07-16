##################################################################
# Case:   041749 - Gilead HIV Retention in Care                       
# Code:   1.3 Comorbidities            			               
# Date:   05.25.2023
# Objective: Comorbidities
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
library(fuzzyjoin)

#Let's try using the comorbidity package for CCI/Elixhauser
#install.packages("comorbidity")
library(comorbidity)

#Set folders
proj_dir <- file.path("//ace/data/health3/gilead_hiv_retention_in_care_041749")
f <- file.path(proj_dir, "data", "r_datasets","final")
out <- file.path(proj_dir, "data", "r_datasets","out")
codes <- file.path(proj_dir,"data","codes")
study_pop <- data.table(read_sas(paste0(proj_dir,"/data/sas_datasets/final/study_pop.sas7bdat")))

#Comorbidity types to create in this program:
#CCI Score
#Elixhauser comorbidities
#HIV-related comorbidities


#(1) PREPARE DATA
med_clms <- data.table(read.csv(paste0(f,"/med_claims.csv")))
#Note - importing the data takes some time!

#Subset to baseline
#Convert service date to date
med_clms$svcdate <- as.Date(med_clms$svcdate,"%d%b%Y")
bl_med_clms <- med_clms %>%
  #Remove record types that are ancillary!
  filter(!rectype=="A") %>%
  filter(as.Date("2019-03-01")<=svcdate & svcdate<as.Date("2020-03-01")) %>%
  #Keep only variables of interest - i.e., pat_id, svcdate, diagprc_ind, diag_admit, diag1-12
  select(pat_id,svcdate,starts_with("diag")) %>%
  select(-diagprc_ind)

long <- melt(setDT(bl_med_clms), id.vars = c("pat_id","svcdate"), variable.name = "Position",value.name="Diagnosis")
#Drop if diagnosis is blank
long$Diagnosis[long$Diagnosis==""] <- NA
diagnosis_clean <- long[!is.na(long$Diagnosis)] %>%
  arrange(pat_id,svcdate)
length(unique(diagnosis_clean$pat_id))
#Nice! All patients are accounted for :) 


##### CCI AND ELIXHAUSER #####

#Types of mappings present in the comorbidity package:
#Charlson ICD9/ICD10 (SE and AM variations)
#Elixhauser ICD9/ICD10
#For more mappings, run available_algorithms()
#For further details see: https://cran.r-project.org/web/packages/comorbidity/comorbidity.pdf

#QUAN-CCI SCORE
cci_comorb <- comorbidity(x=diagnosis_clean,id="pat_id",code="Diagnosis",map="charlson_icd10_quan", assign0=TRUE) %>%
  arrange(pat_id)
cci <- score(x=cci_comorb,weights="quan",assign0=TRUE)

#ELIXHAUSER
elix_comorb <- comorbidity(x=diagnosis_clean, id="pat_id", code="Diagnosis",map="elixhauser_icd10_quan",assign0=TRUE) %>%
  arrange(pat_id) %>%
  mutate(hyp=pmax(hypunc,hypc),
         diab=pmax(diabunc,diabc))
#Rename the colnames
old_names <- colnames(elix_comorb)[!colnames(elix_comorb)=="pat_id"]
new_names <- paste0("elix_",tolower(old_names))
setnames(elix_comorb,old_names,new_names)

#Ensure study pop is also sorted correctly so that we can cbind
pts <- study_pop %>% 
  select(pat_id) %>%
  arrange(pat_id)
cci_elix <- cbind(pts,cci,elix_comorb[,!names(elix_comorb)=="pat_id"])


##### HIV-RELATED COMORBS #####
#This section will take a look at the comorbidities specific to HIV
comorbs_list <- data.table(read_sas(paste0(codes,"/comorb.sas7bdat")))
#Format comorbs to remove the period
comorbs_list <- comorbs_list %>%
  #fixed=TRUE tells R not to interpret the "." as any character, but as a period specifically
  mutate(code=sub(".","",ICD10CM, fixed=TRUE)) %>%
  rename("category"="Name") %>%
  select("category","code")

#Merge onto long using fuzzy join - i.e., if the diagnosis code in claims STARTS WITH the code in our codes list
same_start <- function(x, y){str_starts(x, y)}
hiv_comorbs_all <- diagnosis_clean %>% 
  fuzzy_inner_join(comorbs_list, by = c("Diagnosis" = "code"), match_fun = same_start)

#Create flags for the different categories
hiv_comorbs <- hiv_comorbs_all %>%
  select(pat_id, category) %>% 
  rename("dx"="category") %>%
  dummy_cols(select_columns = "dx") %>%
  select(-"dx")

#Grab max by patient
hiv_comorbs_max <- hiv_comorbs %>%
  group_by(pat_id) %>%
  select(starts_with("dx_")) %>%
  summarise_all(max,na.rm=TRUE)

#Merge onto all patients 
hiv_comorbs_max <- study_pop %>%
  select(pat_id) %>%
  left_join(hiv_comorbs_max) %>% 
  replace(is.na(.), 0)

#Grab all that were not identified
all <- unique(comorbs_list$category)
present <- unique(hiv_comorbs_all$category)
missing <- all[which(!all %in% present)]

if (length(missing)!=0) {
  rename_miss <- paste0("dx_",missing)
  replace_miss <- data.table(matrix(ncol=3,nrow=n_distinct(study_pop)))
  replace_miss[is.na(replace_miss)] <- 0
  setnames(replace_miss,colnames(replace_miss),rename_miss)
  #Cbind
  hiv_comorbs_max <- cbind(hiv_comorbs_max,replace_miss)
}

#Sum all but cardiovascular, depression, diabetes and hepatitis C for AIDS-defining illnesses
aids_illnesses <- c(colnames(hiv_comorbs_max)[!colnames(hiv_comorbs_max) %in% c("pat_id","dx_cvd","dx_depress","dx_diabetes","dx_hepc")])
hiv_comorbs_max$dx_aids <- apply(hiv_comorbs_max[,..aids_illnesses],1,max)


##### DSM5 #####
dsm5_list <- data.table(read_sas(paste0(codes,"/dsmv.sas7bdat"))) %>%
  select("broad_cat","Code")

#Merge onto long using inner join
dsm_comorbs_all <- diagnosis_clean %>% 
  inner_join(dsm5_list, by = c("Diagnosis" = "Code"))

#Create flags for the different categories
dsm_comorbs <- dsm_comorbs_all %>%
  select(pat_id, broad_cat) %>% 
  rename("dsm"="broad_cat") %>%
  dummy_cols(select_columns = "dsm") %>%
  select(-"dsm")

#Grab max by patient
dsm_comorbs_max <- dsm_comorbs %>%
  group_by(pat_id) %>%
  select(starts_with("dsm_")) %>%
  summarise_all(max,na.rm=TRUE)

#Merge onto all patients 
dsm_comorbs_max <- study_pop %>%
  select(pat_id) %>%
  left_join(dsm_comorbs_max) %>% 
  replace(is.na(.), 0)

#Grab all that were not identified
all <- unique(dsm5_list$broad_cat)
present <- unique(dsm_comorbs_all$broad_cat)
missing <- all[which(!all %in% present)]

if (length(missing)!=0) {
  rename_miss <- paste0("dsm_",missing)
  replace_miss <- data.table(matrix(ncol=length(missing),nrow=n_distinct(study_pop)))
  replace_miss[is.na(replace_miss)] <- 0
  setnames(replace_miss,colnames(replace_miss),rename_miss)
  #Cbind
  dsm_comorbs_max <- cbind(dsm_comorbs_max,replace_miss)
}


#(3) COMBINE
comorbs <- study_pop %>%
  left_join(cci_elix) %>%
  left_join(hiv_comorbs_max) %>%
  left_join(dsm_comorbs_max)

#Save dataset :) 
write_fst(comorbs,path=paste0(out,"/comorbs.fst"))
