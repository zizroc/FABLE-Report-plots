Readme associated with Master.R, FABLE_plots_data.R, FABLE_plots_maker1.R and FABLE_plots_maker2.R
by
Marcus J Thomson, IIASA
thomson@iiasa.ac.at
31 July 2019

Instructions:
(1) These scripts run in R, a general use coding language and environment. You should begin by making sure that the version of R on your machine is up to date (version 3.5.2, "Egshell Igloo", or more recent). If you are unfamiliar with R, start here (https://cran.r-project.org/) and install both R and R-Studio (https://www.rstudio.com/products/rstudio/download/) on your computer. Both are free.

(2) Copy the zipped folder "FABLE_report_plots.zip" and extract its contents into the directory of your choice, here called "~/path/" for convenience. (If you are using Windows, this will look like "C://~/path/"; and if you're using a Mac, "User//~/path/".)

(3) Copy the scripts "Master.R", "FABLE_plots_data.R", "FABLE_plots_maker1.R", and "FABLE_plots_maker2.R" from GitHub to "~/path/Scripts/" on your computer.

(4) Open the script "~/path/Scripts/Master.R" in R-Studio and change:
- (a) "pathname_wkdr" to whatever you use for "~/path/". E.g., 
change the following
pathname_wkdr <- "C:/Users/thomson/Documents/IIASA/FABLE/Publications/FABLE Interim Report/Graphs_Interim_Report/" #edit this yourself
to
pathname_wkdr <- "C:/~/path/" #edit this yourself
- (b) if necessary, change "calc_data_file" to whatever version of the Calculator data file you are using (by default, "ind_scen2_to_it50_indicators_04jul19_corr.csv").

(5) Highlight lines 1-45 in "Master.R", down to where you find the line "source(paste0(pathname_scrp, "FABLE_plots_data.R"))" and click "Run" from the header menu. This will load the data necessary to produce the plots. (NB: If you encounter errors, and you have followed the above instructions, open "~/path/Scripts/FABLE_plots_data.R" and check for problems along the lines starting with "calculat_file" and "envelop_file". Confirm that "calc_data_file" is correctly named in "Master.R" and on your computer.)

(6) Optional step: The script was originally built to generate all of the chapter plots one after another in a loop structure. Likely you will only want to run one (i.e., your own) or a few countries. You can do this here by removing the "#" symbols (uncommenting) from one or other (not both) of the lines defining "r_reg_country_df". E.g.,
> filter(!reg_country %in% c("ET", "FI")) means "do not select ET or FI from the data frame reg_country_df" and
> filter(reg_country %in% c("FI")) means "select only FI from reg_country_df".

(7) Highlight "source(paste0(pathname_scrp, "FABLE_plots_maker1.R"))" and click "Run".

(8) Highlight "source(paste0(pathname_scrp, "FABLE_plots_maker2.R"))" and click "Run".


Notes on errors and warnings:
(1) Errors like
New names:
* `` -> `..3`
are caused by incorrectly formatted Excel templates. They are not serious. But you can clear them up by deleting extraneous information (anything outside of the cells we filled or marked for you to edit) from your template.

(2) Warnings like
> Warning message:
Column `variable` joining character vector and factor, coercing into character vector.
are caused by the "merge" function from the "tidyverse" package. They are not serious but can be eliminated by carefully formatting the data frames that are being merged.

> Expected 3 pieces. Additional pieces discarded in 294912 rows [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, ...].
are caused by merging large data frames containing NAs when envelopes are evaluated. R omits these rows altogether, so this does not cause a problem in the analysis. This is due to NAs generated when the envelope data files were produced. Rather than fix these here, the envelope analyses should be re-run with up-to-date Calculators.

> Removed 8 rows containing missing values (geom_point).
are caused when ggplot is not able to find points to plot for certain countries with incomplete historical data, etc. These are not a problem, in general, but can be fixed on a per-country basis.