#Master.R
##
#Authors: Marcus J Thomson (IIASA), Hugo Valin (IIASA), David LeClere (IIASA)
#Intenational Institute for Applied Systems Analysis (IIASA), Laxenburg, Austria
#Dependencies: FABLE_plots_data.R, FABLE_plots_group1.R, FABLE_plots_group2.R

#Instructions: See README file.
#(1) Copy the zipped folder and extract its contents into your working directory.
#(2) Run the following as a script.


#libraries
libs <- c("tidyverse", "lattice", "RColorBrewer", "reshape2", "readxl", "grid", "cowplot", "scales")
install_libs <- libs[!libs %in% installed.packages()]
install.packages(install_libs, dependencies = TRUE)
# for(i in install_libs){
#   install.packages(i, dependencies = TRUE)
# }
sapply(libs, require, character = TRUE)

#define local parameters
calc_data_file<- "ind_scen2_to_it50_indicators_04jul19_corr.csv" #edit this yourself
biod_data_file<- "Biodiversity_results.csv"
pathname_wkdr <- "C:/Users/thomson/Documents/IIASA/FABLE/Publications/FABLE Interim Report/Graphs_Interim_Report/" #edit this yourself

ifelse(!dir.exists(pathname_wkdr), dir.create(pathname_wkdr), FALSE)
pathname_data <- paste0(pathname_wkdr, "Data/")
pathname_imag <- paste0(pathname_wkdr, "Output/")
pathname_scrp <- paste0(pathname_wkdr, "Scripts/")
pathname_enve <- paste0(pathname_data, "Envelope/")
pathname_tmpl <- paste0(pathname_data, "Templates/")
pathname_calc <- paste0(pathname_data, "Calculator/")
pathname_biod <- paste0(pathname_data, "Biodiversity/")


#Data
ifelse(!dir.exists(pathname_scrp), dir.create(pathname_scrp), FALSE) #makes appropriate folder if one does not exist
ifelse(!dir.exists(pathname_data), dir.create(pathname_data), FALSE)
ifelse(!dir.exists(pathname_enve), dir.create(pathname_enve), FALSE)
ifelse(!dir.exists(pathname_imag), dir.create(pathname_imag), FALSE)
ifelse(!dir.exists(pathname_tmpl), dir.create(pathname_tmpl), FALSE)
ifelse(!dir.exists(pathname_calc), dir.create(pathname_calc), FALSE)


source(paste0(pathname_scrp, "FABLE_plots_data.R"))

## uncomment to OMIT countries if needed
# r_reg_country_df <- r_reg_country_df %>% filter(!reg_country %in% c("ET", "FI"))
## or
## uncomment to SELECT countries if needed
# r_reg_country_df <- r_reg_country_df %>% filter(reg_country %in% c("FI"))

source(paste0(pathname_scrp, "FABLE_plots_maker1.R"))

source(paste0(pathname_scrp, "FABLE_plots_maker2.R"))