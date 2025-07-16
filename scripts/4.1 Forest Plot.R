##################################################################
# Case:   041749 - Gilead HIV Retention in Care                       
# Code:   4.1 LASSO Model Forest Plot            			               
# Date:   12.14.2023 
# Objective: Create forest plot of predictors 
# Author: Ana Urosevic
##################################################################

rm(list=ls())

#Install and load packages
library(data.table)
library(dplyr)
library(openxlsx)
library(fst)
library(tidyr)
library(ggplot2)
library(grid)
library(forestploter)

#Set folders
proj_dir <- file.path("//ace/data/health3/gilead_hiv_retention_in_care_041749")
out <- file.path(proj_dir, "data", "r_datasets","out")
codes <- file.path(proj_dir,"data","codes")

#=======================================================#
# 0. Data import and preparation                        #
#=======================================================#

#We need to grab the LASSO model results
variables <- read_fst(paste0(out,"/lasso_ors.fst"))

#Drop the intercept!
relevant_vars <- variables %>%
  filter(variable!='(Intercept)')

#Dictionary of variable names: variable labels
dictionary <- read.xlsx(paste0(codes,"/variable_names.xlsx"),sheet="A1")

#Merge for variable names and categories 
var_labels <- relevant_vars %>%
  left_join(dictionary, by=c("variable"="variable_name")) %>%
  #Sort in a meaningful way 
  arrange(desc(category_order),coefficient) %>%
  #Add index 
  mutate(index=row_number()) 

#Add an asterisk if the p-value is significant! (i.e., <0.5%)
plot_input <- var_labels %>%
  mutate(final_label=case_when(pval<0.05 ~ paste0(variable_label,"*"),
                               TRUE ~ variable_label)) %>%
  dplyr::select(variable_category,final_label,index,coefficient,lower_ci,upper_ci) %>%
  rename(Category=variable_category,Variable=final_label)

#Convert all to numeric
cols <- c('coefficient','lower_ci','upper_ci')
plot_input[cols] <- sapply(plot_input[cols], as.numeric)


#=======================================================#
# 1. Build plot                                         #
#=======================================================#

#Initial plot
forest_plot <- 
  ggplot(data=plot_input, aes(y=index, x=coefficient, xmin=lower_ci, xmax=upper_ci, color=Category)) +
  geom_point() + 
  geom_errorbarh(height=.1) +
  scale_y_continuous(name = "", breaks=1:nrow(plot_input), labels=plot_input$Variable) +
  labs(x='Odds Ratio') +
  geom_vline(xintercept=1, color='black', linetype='dashed', alpha=.5) +
  scale_x_continuous(breaks = round(seq(0,5.5, by = 0.5),1)) +
  theme_light() +
  theme(text=element_text(size=12,  family="Arial"))


#=======================================================#
# 2. Save plot                                          #
#=======================================================#

#Save
today <- Sys.Date()
today_format <- toupper(format(today,format="%d%b%y"))
png(paste0(proj_dir,"/r_output/forest_plot_",today_format,".png"), width=10, height=20, units="in", res=1200)

print(forest_plot)
dev.off()
