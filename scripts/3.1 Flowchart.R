##################################################################
# Case:   041749 - Gilead HIV Retention in Care                       
# Code:   3.1 Flowchart          			               
# Date:   07.31.2023
# Objective: Flowchart
# Author: Ana Urosevic
##################################################################

rm(list=ls())

#Install and load packages
library(data.table)
library(dplyr)
library(fst)
library(writexl)

#Set folders
proj_dir <- file.path("//ace/data/health3/gilead_hiv_retention_in_care_041749")
output <- file.path(proj_dir, "r_output")
sp <- file.path(proj_dir, "data", "r_datasets","study_pop")

#IMPORT ALL FILES
file_list <- list.files(sp)
full_directory <- paste0(sp,"/",file_list)

#For each file, import 
myfiles <- lapply(full_directory, function(i){
  read.fst(i)})

#For each file, count number of patients and append
summary_pts <- data.table(matrix(ncol=2,nrow=0))
colnames(summary_pts) <- c("box","num_pts")

for (i in 1:length(file_list)) {
  box <- substr(file_list[i],1,nchar(file_list[i])-4)
  num_pts <- myfiles[[i]] %>%
    n_distinct("PatientID")
  num_pts_format <- paste0(format(num_pts,big.mark=","))
  
  patients <- data.table("box"=box,"num_pts"=num_pts_format)
  summary_pts <- rbind(summary_pts,patients)
}

#ADDITIONAL INFO
#Final study pop (by cohort)
final <- read.fst(paste0(sp,"/cohorts.fst"))
ric_pts <- paste0(format(sum(final$cohort=="ric"),big.mark=","))
fooc_pts <- paste0(format(sum(final$cohort=="fooc"),big.mark=","))

cohorts <- data.table("box"=c("ric","fooc"),"num_pts"=c(ric_pts,fooc_pts))

flowchart <- rbind(summary_pts,cohorts)

#Save
today <- Sys.Date()
today_format <- toupper(format(today,format="%d%b%y"))
filename <- paste0(output,"/flowchart_cohorts_",today_format,".xlsx")
write_xlsx(list(flowchart=flowchart),path=filename)
