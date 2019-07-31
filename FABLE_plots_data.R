#FABLE_plots_data.R
##
#Authors: Marcus J Thomson (IIASA), Hugo Valin (IIASA), David LeClere (IIASA)
#Intenational Institute for Applied Systems Analysis (IIASA), Laxenburg, Austria
##
#Dependencies: Master.R
#Purpose: Loads and prepares data for plotting in the country chapters. Use Master.R to run.

calculat_file <- read.csv(paste0(pathname_calc, calc_data_file), header = TRUE, stringsAsFactors = FALSE)
envelope_file <- dir(pathname_enve, full.names = TRUE)

#define country short names
country_short <- c("AR","AU","BR","CA", 
                   "CO","CN","ET","GB",
                   "ID","IN","MX","MY",
                   "RU","RW","SE")
region_short  <- c("RAFR","RAME","RASI","RCAS",
                   "RMID","RNEU","ROEU","RPAC")
iso_map <- data.frame(Iso3 = c("ARG","AUS","BRA","CAN",
                             "CHN","COL","ETH","FIN",
                             "IND","IDN","MYS","MEX",
                             "RUS","RWA","SWE","GBR",
                             "USA","ROEU"),
                      Iso2 = c("AR" ,"AU" ,"BR" ,"CA" ,
                             "CN" ,"CO" ,"ET" ,"FI" ,
                             "IN" ,"ID" ,"MY" ,"MX" ,
                             "RU" ,"RW" ,"SE" ,"GB" ,
                             "US" ,"ROEU"))

biodiv_data_new   <- read.csv(paste0(pathname_biod, biod_data_file))

biodiv_data_new2  <- merge(biodiv_data_new, iso_map, by="Iso3")

files.template    <- list.files(pathname_tmpl, pattern = ".xlsx", recursive = F)
mytest <- grep(files.template, pattern = '~'); if(length(mytest) > 0) {files.template <- files.template[-mytest]}
files.reg_country <- unlist(strsplit(files.template, split='_'))[seq(1,length(unlist(strsplit(files.template, split='_'))), 4)]
reg_country_df    <- merge(cbind.data.frame("reg_country"=c(region_short, country_short)),
                        cbind.data.frame("reg_country"=files.reg_country, "file"=files.template),
                        by=c("reg_country"),all.x=T, all.y=T)
reg_country_df    <- merge(reg_country_df, iso_map, by.x = c('reg_country'), by.y=c('Iso2'), all.x=T)

# select cases for which there is a file
r_reg_country_df  <- reg_country_df[which(!is.na(reg_country_df$file)),]

