##################################################################
# Case:   041749 - Gilead HIV Retention in Care                       
# Code:   1.2 HRU and Costs            			               
# Date:   05.23.2023
# Objective: HRU and Costs
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
f <- file.path(proj_dir, "data", "r_datasets","final")
out <- file.path(proj_dir, "data", "r_datasets","out")
codes <- file.path(proj_dir,"data","codes")
study_pop <- data.table(read_sas(paste0(proj_dir,"/data/sas_datasets/final/study_pop.sas7bdat")))

#This program will create HRU counts and costs for the following:
#All-cause
#HIV-related
#Visit types:
#IP, ER, OP (primary care, specialist, and other)

###### PREPARE DATA #####

#(1) IMPORT MEDICAL CLAIMS
med_clms <- data.table(read.csv(paste0(f,"/med_claims.csv")))
#Note - import takes a while

#Convert svcdate, admdt, disdt into date
med_clms$svcdate <- as.Date(med_clms$svcdate,"%d%B%Y")
med_clms$admdt <- as.Date(med_clms$admdt,"%d%B%Y")
med_clms$disdt <- as.Date(med_clms$disdt,"%d%B%Y")

#Only IP visits have an admittance and discharge date
#For all remaining visit types, let's assign svcdate to both
med_clms$admdt[is.na(med_clms$admdt)] <- med_clms$svcdate[is.na(med_clms$admdt)]
med_clms$disdt[is.na(med_clms$disdt)] <- med_clms$svcdate[is.na(med_clms$disdt)]

#Grab only during baseline
bl_med_clms <- med_clms %>%
  filter(as.Date("2019-03-01")<=svcdate & svcdate<as.Date("2020-03-01")) %>%
  select(pat_id,svcdate,admdt,disdt,type,pos,rectype,rend_spec,paid_plan_2022,paid_pat_2022)


#(2) IDENTIFY VISIT TYPES
#(A) OP primary care (PC): type="OP", pos="11", rectype="M" and rend_spec in c("NRS_PRCT","INTERN","GP_FP","PHYS_AST","GERIATRC)
#(B) OP infection (i.e., specialist [INF]): type="OP" and red_spec="inf_dis"
#(C) OP other: type="OP" (not in 1 or 2) and type in ("DME","DVC")

#Note: case_when is equivalent to a if/elif/else statement 
#As a result, the hierarchy becomes IP>ER>OP_INF>OP_PC>OP_OTH
op_visits <- bl_med_clms %>%
  mutate(type_op=
           case_when(type=="IP" ~ "IP",
                     type=="ER" ~ "ER",
                     type=="OP" & rend_spec=="INF_DIS" ~ "OP_INF",
                     type=="OP" & pos=="11" & rectype=="M" & rend_spec %in% c("NRS_PRCT","INTERN","GP_FP","PHYS_AST","GERIATRC") ~ "OP_PC",
                     TRUE ~ "OP_OTH"))

#Investigation - where are the other OP visits occurring?
test <- data.table(table(op_visits$rend_spec[op_visits$type_op=="OP_OTH"])) %>% arrange(desc(N))
#Top 5 specialty: other facility, unknown, hospital, pathology, internal medicine
sum_total <- sum(test$N)
test <- mutate(test, prob=N/sum_total*100)
test <- data.table(table(op_visits$pos[op_visits$type_op=="OP_OTH"])) %>% arrange(desc(N))
#Top 5 POS: 11-office, 81-independent lab, 22-on-campus op hospital, 65-end-stage renal disease treatment facility, 12-home

op_visits <- op_visits %>% 
  select(pat_id,svcdate,admdt,disdt,type_op,paid_plan_2022,paid_pat_2022) %>%
  rename("type"="type_op")


#(3) FLAG HIV VISITS
#Identify HIV diagnoses
hiv1_codes <- paste0(codes,"/hiv1.sas7bdat")
hiv1 <- read_sas(hiv1_codes)
diagnoses <- med_clms %>%
  #Make sure we do not use ancillary claims!
  filter(!rectype=="A") %>%
  select(pat_id,svcdate,admdt,disdt,starts_with("diag")) %>%
  select(-c("diagprc_ind"))
long <- melt(setDT(diagnoses), id.vars = c("pat_id","svcdate","admdt","disdt"), variable.name = "Position",value.name="Diagnosis")

hiv_dx <- inner_join(long,hiv1,by=c("Diagnosis"="Code"))
#Keep HIV dx at the episode level - i.e., for each admdt/disdt
hiv_dx <- hiv_dx %>%
  select(pat_id,admdt,disdt) %>%
  unique() 

#If one claim during a given episode is associated with HIV, then the whole episode is associated with HIV
#Base med will contain all baseline 
#HIV med will contain subset of HIV-related episodes
med_cost_all <- op_visits 
med_cost_hiv <- inner_join(med_cost_all,hiv_dx)


#(4) HRU EPISODES
#For episodes, collapse to the admdt and disdt level
#Create flags
temp_eps <- dummy_cols(op_visits, select_columns = c("type")) %>% 
  select(pat_id, admdt, disdt,starts_with("type_"))
temp_eps2 <- temp_eps %>%
  group_by(pat_id, admdt,disdt) %>%
  summarise(flag_ip = max(type_IP),
            flag_er=max(type_ER),
            flag_op_inf=max(type_OP_INF),
            flag_op_pc=max(type_OP_PC),
            flag_op_oth=max(type_OP_OTH))
#Re-prioritize: OP INF > OP PC > OP OTH
episodes <- temp_eps2 %>%
  mutate(type=case_when(flag_ip==1 ~"IP",
                        flag_er==1 ~ "ER",
                        flag_op_inf==1 ~"OP_INF",
                        flag_op_pc==1 ~ "OP_PC", 
                        flag_op_oth==1 ~"OP_OTH")) %>%
  select(pat_id,admdt,disdt,type)

med_eps_all <- episodes
med_eps_hiv <- inner_join(med_eps_all,hiv_dx)


#(5) PHARMACY COSTS

#Import med lists
mtr_meds <- data.table(read_sas(paste0(codes,"/mtr.sas7bdat")))%>%
  select(NDC)
str_meds <- data.table(read_sas(paste0(codes,"/str.sas7bdat"))) %>%
  select(NDC)

#Combine
meds_list <- rbind(mtr_meds,str_meds)

drug_clms <- data.table(read.csv(paste0(f,"/drug_claims.csv")))
#Note - import takes a while

#Convert from_dt into date
drug_clms$from_dt <- as.Date(drug_clms$from_dt,"%d%b%Y")
#Grab only during baseline
bl_drug_clms <- drug_clms %>%
  filter(as.Date("2019-03-01")<=from_dt & from_dt<as.Date("2020-03-01")) %>%
  select(pat_id,from_dt,ndc,paid_plan_2022,paid_pat_2022)

base_drug_all <- bl_drug_clms
#Merge on medication list
base_drug_hiv <- inner_join(base_drug_all,meds_list,by=c("ndc"="NDC"))


###### HRU #####

#For each visit type, we want to count the number of occurrences by patient ID at the episode level
calculate_hru <- function(dx) {

  data <- paste0("med_eps_",dx)
  
  #Count for each type
  #Data is already at the episode level - i.e., unique by adm/dis date
  visit_count <- get(data) %>%
    group_by(pat_id,type) %>%
    summarise(count=n(),.groups='drop')
  
  #Reshape
  hru <- dcast(data.table(visit_count), pat_id ~ type, value.var = "count")
  
  #Create total OP
  hru[is.na(hru)] <- 0
  hru$OP <- hru$OP_PC + hru$OP_INF + hru$OP_OTH
  
  #Rename variables
  old_names <- colnames(hru)[! colnames(hru)=="pat_id"]
  new_names <- paste0("num_",old_names)
  setnames(hru,old=old_names,new=new_names)
  
  #Calculate proportion of patients with >=1 visit
  hru_visits <- hru %>%
    mutate(visit_IP=case_when(num_IP==0~0, TRUE~1),
           visit_ER=case_when(num_ER==0~0, TRUE~1),
           visit_OP=case_when(num_OP==0~0, TRUE~1),
           visit_OP_PC=case_when(num_OP_PC==0~0, TRUE~1),
           visit_OP_INF=case_when(num_OP_INF==0~0,TRUE~1), 
           visit_OP_OTH=case_when(num_OP_OTH==0~0,TRUE~1))

  #Rename variables
  old_names <- colnames(hru_visits)[! colnames(hru_visits)=="pat_id"]
  new_names <- paste0(tolower(old_names),"_",dx)
  setnames(hru_visits,old=old_names,new=new_names)
    
  return(hru_visits)
}
base_hru_all <- calculate_hru(dx="all")
base_hru_hiv <- calculate_hru(dx="hiv")

hru <- study_pop %>%
  select(pat_id) %>%
  left_join(base_hru_all) %>%
  left_join(base_hru_hiv)

hru[is.na(hru)] <- 0
#Update - we want to report number of events among patients with at least one event
#Therefore, set as NA if 0 for all cols starting with num_
cols_to_na <- colnames(hru)[grepl("^num_", colnames(hru))]
hru <- hru %>% mutate_at(cols_to_na, ~na_if(., 0))

#Test
mean <- mean(baseline[,num_ip_all],na.rm=TRUE)
median <- median(baseline[,num_ip_all],na.rm=TRUE)
sd <- sd(baseline[,num_ip_all],na.rm=TRUE)
#Looks good! :) 

calculate_los <- function(dx) {
  
  data <- paste0("med_eps_",dx)
  
  #Average hospital LOS among patients with a hospitalization
  los <- get(data) %>%
    filter(type=="IP") %>%
    group_by(pat_id) %>%
    summarise(los=mean(disdt-admdt+1))
  los$los <- as.numeric(los$los)
  
  #Rename
  new_name <- paste0("los_",dx)
  setnames(los,old="los",new=new_name)
  
  return(los)
}
los_all <- calculate_los(dx="all")
los_hiv <- calculate_los(dx="hiv")


##### COSTS #####

#For each visit type, we want to count the sum of costs
calculate_med_costs <- function(dx) {

  data <- paste0("med_cost_",dx)
  
  #Sum for each type
  cost_sum <- get(data) %>%
    group_by(pat_id,type) %>%
    #For medical costs, we will calculate plan paid costs
    summarise(sum=sum(paid_plan_2022),.groups='drop')
  
  #Reshape 
  cost <- dcast(data.table(cost_sum), pat_id ~ type, value.var = "sum")
  
  #Create total OP
  cost[is.na(cost)] <-0
  cost$OP <- cost$OP_PC + cost$OP_INF + cost$OP_OTH
  
  #Rename variables
  old_names <- colnames(cost)[! colnames(cost) == "pat_id"]
  new_names <- paste0("cost_",tolower(old_names),"_",dx)
  setnames(cost,old=old_names,new=new_names)
  
  return(cost)
}
base_cost_all <- calculate_med_costs(dx="all")
base_cost_hiv <- calculate_med_costs(dx="hiv")

calculate_pharm_costs <- function(dx) {

  data <- paste0("base_drug_",dx)
  
  #Sum for each type
  pharm <- get(data) %>%
    group_by(pat_id) %>%
    summarise(plan=sum(paid_plan_2022),
              pat=sum(paid_pat_2022),.groups='drop')
  
  #Rename variables
  old_names <- colnames(pharm)[! colnames(pharm) == "pat_id"]
  new_names <- paste0("cost_pharm_",tolower(old_names),"_",dx)
  setnames(pharm,old=old_names,new=new_names)
  
  return(pharm)
}
base_pharm_all <- calculate_pharm_costs(dx="all")
base_pharm_hiv <- calculate_pharm_costs(dx="hiv")

cost <- study_pop %>%
  select(pat_id) %>%
  left_join(base_cost_all) %>%
  left_join(base_cost_hiv) %>%
  left_join(base_pharm_all) %>%
  left_join(base_pharm_hiv)

cost[is.na(cost)] <- 0

#Create totals
cost <- cost %>%
  mutate(cost_med_all=cost_ip_all + cost_er_all + cost_op_all,
         cost_med_hiv=cost_ip_hiv + cost_er_hiv + cost_op_hiv) %>%
  mutate(cost_total_all=cost_med_all + cost_pharm_plan_all,
         cost_total_hiv=cost_med_hiv + cost_pharm_plan_hiv)


##### COMBINE #####
hru_cost <- study_pop %>%
  left_join(hru) %>%
  left_join(cost) %>%
  left_join(los_all) %>%
  left_join(los_hiv)

#Save dataset 
write_fst(hru_cost,path=paste0(out,"/hru_cost.fst"))
