#FABLE_plots_maker2.R
##
#Authors: Marcus J Thomson (IIASA), Hugo Valin (IIASA), David LeClere (IIASA)
#Intenational Institute for Applied Systems Analysis (IIASA), Laxenburg, Austria
##
#Dependencies: Master.R, FABLE_plots_data.R
#Purpose: Produces the second set of "results" plots in the country chapters. Use Master.R to run.

for(j in seq(1,dim(r_reg_country_df)[1],1)){
  
  # - Define data sources by country
  country_alpha <- as.character(r_reg_country_df$reg_country[j])
  template_name <- as.character(r_reg_country_df$file[j])
  source_id     <- read_xlsx(path = paste0(pathname_tmpl, template_name), progress = readxl_progress(), skip = 1, sheet = "Sources")
  print(paste(j,country_alpha))
  
  # - Create path to output if not existing
  pathname_outp <- paste0(pathname_imag, country_alpha)
  ifelse(!dir.exists(pathname_outp), dir.create(pathname_outp), FALSE)
  
  ## Historical data from Excel
  defor_hist_data <- read_xlsx(path = paste0(pathname_tmpl, template_name), progress = readxl_progress(), skip = 1, sheet = "FOREST_CC_RESULTS")
  defor_hist_data$YEAR_plot <- defor_hist_data$YEAR
  test_lines <- which(nchar(defor_hist_data$YEAR)>4)
  if(length(test_lines) >0) {
    defor_hist_data$YEAR_plot[test_lines] <- paste(substr(defor_hist_data$YEAR,1,4),substr(defor_hist_data$YEAR,6,9),sep="-")[test_lines]
  }
  test_lines_2 <- which(nchar(defor_hist_data$YEAR)<=4)
  if(length(test_lines_2) > 0) {
    defor_hist_data$YEAR_plot[test_lines_2] <- paste(defor_hist_data$YEAR-4,defor_hist_data$YEAR,sep="-")[test_lines_2]
  }
  test_lines_3 <- which(nchar(defor_hist_data$YEAR)>4)
  if(length(test_lines_3) > 0) {
    defor_hist_data$YEAR[test_lines_3] <- as.numeric(substr(defor_hist_data$YEAR,6,9))[test_lines_3]
  }
  defor_hist_data$variable <- "Historical forest cover change"
  
  #Read in calculator data
  calculat_data <- calculat_file %>%
    filter(alpha2 == country_alpha & iteration == 50)
  
  #Read in ensemble envelope data
  country_index <- grep(paste0("_", country_alpha), envelope_file)
  is_envpe_avail <- 1
  if(length(country_index) > 0) {
    envelope_data <- read.csv(envelope_file[country_index], header = TRUE, stringsAsFactors = FALSE)
  } else {
    is_envpe_avail <- 0; print("!! Envelope data not available")
  }
  
  #There is additional data from the ROEU Calculator template
  if(country_alpha == "ROEU"){
    tab_names <- c("BIODIV_GLOBIOM", "FOOD_GLOBIOM", "GHG_GLOBIOM", "LAND_GLOBIOM")
    data_glob_ls <- list()
    for(k in seq_along(tab_names)){
      tmp <- read_xlsx(path = paste0(pathname_tmpl, template_name), progress = readxl_progress(), skip = 1, sheet = tab_names[k])
      if(k == 1){
        data_glob_ls[[k]] <- tmp %>% 
          mutate(Var = rep("GLOBIOM", 6)) %>% 
          rename(Val = GLOBIOM_BiodivInd) %>% 
          select(YEAR, Var, Val)
      }
      if(k == 2){
        data_glob_ls[[k]] <- tmp %>% 
          mutate(CLASS_AGG = rep("GLOBIOM", 6)) %>% 
          mutate(COLOR = rep("#111111", 6)) %>% 
          rename(KCAL = KCAL_GLOBIOM) %>% 
          select(YEAR, CLASS_AGG, KCAL, COLOR)
      }
      if(k == 3){
        data_glob_ls[[k]] <- tmp %>% 
          filter(CLASS == "GLOBIOM agriculture") %>% 
          rename(GLOBIOM = MtCO2e) %>% 
          melt(., id.var = "CLASS") %>% 
          filter(variable == "GLOBIOM") %>% 
          mutate(YEAR = seq(2000, 2050, 10)) %>% 
          select(variable, value, YEAR)
      }
      if(k == 4){
        data_glob_ls[[k]] <- tmp %>% 
          rename(Var = CLASS, Val = AREA_1000ha) %>% 
          select(SCENARIO, YEAR, Var, Val, COLOR)
      }
    }
    names(data_glob_ls) <- tab_names
  }  
  
  if(is_envpe_avail==1) {
    
    #Refine dataset
    name_tmp1 <- names(envelope_data)[grep("Forest_change", names(envelope_data), ignore.case = FALSE)]
    
    if(country_alpha == "ROEU"){
      rm(name_tmp1)
      name_tmp1 <- names(envelope_data)[grep("ForestChange", names(envelope_data), ignore.case = FALSE)]
    }
    
    #Manipulate the envelope data to plot it.
    forest_tmp <- envelope_data %>%
      select(name_tmp1) %>%
      reshape2::melt(., id.var = NULL)
    
    Forest_tmp <- str_split(forest_tmp$variable, "_", simplify = TRUE)
    ForestChange <- data.frame(YEAR = as.numeric(Forest_tmp[,2]),
                               TYPE = Forest_tmp[,3],
                               AREA = forest_tmp$value)
    
    NetForestChange <- ForestChange %>%
      group_by(YEAR, TYPE) %>%
      summarize(MXAREA = max(AREA), MNAREA = min(AREA)) %>%
      group_by(YEAR) %>%
      summarize(NETMAX = sum(MXAREA), NETMIN = sum(MNAREA))
    
    NetForestChange_df <- rbind(NetForestChange %>%
                                  select(NETMIN) %>%
                                  melt(., id.vars = NULL) %>%
                                  mutate(YEAR = seq(2015, 2050, 5)) %>%
                                  arrange(., YEAR),
                                NetForestChange %>%
                                  select(NETMAX) %>%
                                  melt(., id.vars = NULL) %>%
                                  mutate(YEAR = seq(2015, 2050, 5)) %>%
                                  arrange(., desc(YEAR)))
    
    NetForestChange_df$YEAR_plot <- ordered(as.character(NetForestChange_df$YEAR), levels = c(unique(as.character(NetForestChange_df$YEAR))))
    levels(NetForestChange_df$YEAR_plot) <- c("2011-2015","2016-2020","2021-2025","2026-2030","2031-2035","2036-2040","2041-2045","2046-2050")
    
    NetForestChange_df$value <- NetForestChange_df$value / 5000
  }
  
  forest_data <- calculat_data %>%
    select(forest_change, newforest_change, netforest_change) %>%
    melt(., id.vars = NULL) %>%
    mutate(YEAR = rep(seq(2000, 2050, 5), 3))
  
  forest_data$value <- forest_data$value / 5000
  
  level_key <- c(forest_change = "Forest", newforest_change = "Afforested land", netforest_change = "net change")
  forest_data$variable <- recode(forest_data$variable, !!!level_key)
  

  
  #PLOT 1
  
  caption_tmp <- paste("Source:", source_id %>%
                         filter(TAB == "TRADE2") %>%
                         select(SOURCE))
  wrapped_caption <- str_wrap(caption_tmp, width = caption_width)
  
  rp1_col_key <- colorRampPalette(c("forestgreen", "lightgreen"))
  rp1_colours <- c(rp1_col_key(2))
  
  forest_data$YEAR_plot <- ordered(as.character(forest_data$YEAR), levels = c(unique(as.character(forest_data$YEAR))))
  levels(forest_data$YEAR_plot) <- c("1996-2000","2001-2005","2006-2010","2011-2015","2016-2020","2021-2025","2026-2030","2031-2035","2036-2040","2041-2045","2046-2050")
  add_forest_data <- forest_data %>% filter(variable == 'net change'); add_forest_data$variable <- 'FABLE target'; add_forest_data$value <- 0
  forest_data_2 <- rbind.data.frame(forest_data, add_forest_data)
  forest_data_2$variable <- as.character(forest_data_2$variable)
  forest_data_2$variable[which(forest_data_2$variable=='net change')] <- 'net forest cover change'
  
  if(country_alpha == "ID"){
    #Indonesia is a special case wherein peat and non-peat forest are considered
    forest_data_3 <- read.csv(paste0(pathname_data, "Special/ID_forest_change.csv"), 
                              header = TRUE, 
                              stringsAsFactors = FALSE)
    forest_data <- forest_data_3 %>% 
      select(NewForestChange, ForestPeatChange, ForestNonPeatChange, ForestChange) %>% 
      rename("Afforested land" = "NewForestChange", 
             "Peat forest" = "ForestPeatChange", 
             "Non-peat forest" = "ForestNonPeatChange", 
             "net forest cover change" = "ForestChange") %>% 
      mutate("FABLE target" = rep(0, 11)) %>% 
      melt(., id.vars = NULL) %>% 
      mutate(YEAR = rep(seq(2000, 2050, 5), 5)) %>% 
      mutate(value = value/5000)
    
    rp1_col_key <- data.frame(variable = c("Afforested land", 
                                           "Peat forest", 
                                           "Non-peat forest"),
                              COLOURS  = c("lightgreen", 
                                           "darkkhaki",  
                                           "darkolivegreen"), 
                              stringsAsFactors = FALSE)
    
    rp1_colours <- rp1_col_key %>% 
      inner_join(., forest_data, by = "variable") %>% 
      arrange(variable) %>% 
      distinct(COLOURS, .keep_all = TRUE) %>% 
      pull(., COLOURS)
    
    forest_data$YEAR_plot <- ordered(as.character(forest_data$YEAR), levels = c(unique(as.character(forest_data$YEAR))))
    levels(forest_data$YEAR_plot) <- c("1996-2000","2001-2005","2006-2010","2011-2015","2016-2020","2021-2025","2026-2030","2031-2035","2036-2040","2041-2045","2046-2050")
    forest_data$variable  <- as.character(forest_data$variable)
    forest_data$YEAR_plot <- as.character(forest_data$YEAR_plot)
    
    caption_tmp <- paste("Source:", source_id %>%
                           filter(TAB == "TRADE2") %>%
                           select(SOURCE))
    wrapped_caption <- str_wrap(caption_tmp, width = caption_width)
  }
  
  if(is_envpe_avail==1) {
    rp1 <- ggplot(data = forest_data_2 %>% filter(YEAR > 2000), aes(x = YEAR_plot, group='variable')) +
      geom_polygon(data = NetForestChange_df, aes(x = YEAR_plot, y = value), fill= "firebrick", alpha = 0.2) +
      geom_bar(data = forest_data_2 %>%
                 filter(variable %in% c("Forest", "Afforested land") & YEAR > 2000), position="stack", aes(weight = value, fill = variable, group=variable), alpha = 1, width = 0.75) +
      geom_hline(yintercept = 0, size = 0.5) +
      geom_point(data = defor_hist_data, aes(x = YEAR_plot, y = MILLION_HECTARES_PER_YEAR, shape = variable), alpha = 1, size = 1.2) +
      scale_shape_manual(values = 8) +
      scale_fill_manual( values = c("Forest" = rp1_colours[1],"Afforested land" = rp1_colours[2])) +
      geom_line(data = forest_data_2 %>%
                  filter(variable %in% c("net forest cover change","FABLE target") & YEAR > 2000), aes(y = value, x = YEAR_plot, group= variable, linetype=variable), color = "firebrick", alpha = 0.99, size = 1) +
      scale_linetype_manual(values = c('solid','dotted')) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, color = "#666666", size = my_title_text_size),
            plot.subtitle = element_text(hjust = 0.5, color = "#666666", size = my_subtitle_text_size),
            legend.position = "right",
            legend.title = element_blank(),
            legend.key.size = unit(0.75, "line"), 
            legend.spacing.x = unit(0.1, "cm"),
            legend.text = element_text(size = my_legend_text_size),
            plot.caption = element_text(hjust = 0.5, color = "#666666", size = my_caption_text_size),
            axis.text = element_text(size = my_axis_text_size),
            axis.text.x = element_text(angle = 45),
            axis.title = element_text(size = my_axistitle_text_size),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank()) +
      #scale_x_continuous(breaks=seq(2000, 2050, 5)) +
      labs(x = "Year", y = "Million hectares per year", caption = str_wrap(paste("Historical data source:", source_id %>%
                                                                                   filter(TAB == "FOREST_CC_RESULTS") %>%
                                                                                   select(SOURCE)), 100))

    
    if(country_alpha == "ID"){
      rm(rp1)
      rp1 <- ggplot(data = forest_data %>% filter(YEAR > 2000), aes(x = YEAR, group='variable')) +
          geom_polygon(data = NetForestChange_df, aes(x = YEAR, y = value), fill= "firebrick", alpha = 0.2) +
          geom_bar(data = forest_data %>%
                     filter(variable %in% c("Afforested land", "Peat forest", "Non-peat forest") & YEAR > 2000), position="stack", aes(weight = value, fill = variable, group=variable), alpha = 1, width = 3) +
          scale_fill_manual( values = rp1_colours) +
          geom_hline(yintercept = 0, size = 0.5) +
          geom_point(data = defor_hist_data, aes(x = YEAR, y = MILLION_HECTARES_PER_YEAR, shape = variable), alpha = 1, size = 1.2) +
          scale_shape_manual(values = 8) +
          geom_line(data = forest_data %>%
                      filter(variable %in% c("net forest cover change","FABLE target") & YEAR > 2000), aes(y = value, x = YEAR, group= variable, linetype=variable), color = "firebrick", alpha = 0.99, size = 1) +
          scale_linetype_manual(values = c("solid", "dotted")) +
          theme_minimal() +
          theme(plot.title = element_text(hjust = 0.5, color = "#666666", size = my_title_text_size),
                plot.subtitle = element_text(hjust = 0.5, color = "#666666", size = my_subtitle_text_size),
                legend.position = "right",
                legend.title = element_blank(),
                legend.key.size = unit(0.75, "line"), 
                legend.spacing.x = unit(0.1, "cm"),
                legend.text = element_text(size = my_legend_text_size),
                plot.caption = element_text(hjust = 0.5, color = "#666666", size = my_caption_text_size),
                axis.text = element_text(size = my_axis_text_size),
                axis.text.x = element_text(angle = 45),
                axis.title = element_text(size = my_axistitle_text_size),
                panel.grid.major.x = element_blank(),
                panel.grid.minor.x = element_blank()) +
          scale_x_continuous(breaks=seq(2000, 2050, 5), labels = c("1996-2000","2001-2005","2006-2010","2011-2015","2016-2020","2021-2025","2026-2030","2031-2035","2036-2040","2041-2045","2046-2050")) +
          labs(x = "Year", y = "Million hectares per year", caption = str_wrap(paste("Historical data source:", source_id %>%
                                                                                       filter(TAB == "FOREST_CC_RESULTS") %>%
                                                                                       select(SOURCE)), 100))
    }
    ggsave(filename = paste0(country_alpha, "_RESULTS_Fig1_wEV.pdf"), plot = rp1, device = "pdf", path = pathname_outp, scale = 1, width = 14, height = 7, units = "cm", dpi = 320, limitsize = TRUE)
  }
  
  if(is_envpe_avail != 1){
      rp1 <- ggplot(data = forest_data_2 %>% filter(YEAR > 2000), aes(x = YEAR_plot, group='variable')) +
        geom_bar(data = forest_data_2 %>%
                   filter(variable %in% c("Forest","Afforested land") & YEAR > 2000), position="stack", aes(weight = value, fill = variable, group=variable), alpha = 1, width = 0.75) +
        geom_hline(yintercept = 0, size = 0.5) +
        geom_point(data = defor_hist_data, aes(x = YEAR_plot, y = MILLION_HECTARES_PER_YEAR, shape = variable), alpha = 1, size = 1.2) +
        scale_shape_manual(values = 8) +
        scale_fill_manual( values = c("Forest" = rp1_colours[1],"Afforested land" = rp1_colours[2])) +
        geom_line(data = forest_data_2 %>%
                    filter(variable %in% c("net forest cover change","FABLE target") & YEAR > 2000), aes(y = value, x = YEAR_plot, group= variable, linetype=variable), color = "firebrick", alpha = 0.99, size = 1) +
        scale_linetype_manual(values = c('solid','dotted')) +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, color = "#666666", size = my_title_text_size),
              plot.subtitle = element_text(hjust = 0.5, color = "#666666", size = my_subtitle_text_size),
              legend.position = "right",
              legend.title = element_blank(),
              legend.key.size = unit(0.75, "line"), 
              legend.spacing.x = unit(0.1, "cm"),
              legend.text = element_text(size = my_legend_text_size),
              plot.caption = element_text(hjust = 0.5, color = "#666666", size = my_caption_text_size),
              axis.text = element_text(size = my_axis_text_size),
              axis.text.x = element_text(angle = 45),
              axis.title = element_text(size = my_axistitle_text_size),
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank()) +
        #scale_x_continuous(breaks=seq(2000, 2050, 5)) +
        labs(x = "Year", y = "Million hectares per year", caption = str_wrap(paste("Source historical data:", source_id %>%
                                                                                     filter(TAB == "FOREST_CC_RESULTS") %>%
                                                                                     select(SOURCE)), 100))
      if(country_alpha == "ID"){
        rm(rp1)
        rp1 <- ggplot(data = forest_data %>% filter(YEAR > 2000), aes(x = YEAR, group='variable')) +
          geom_bar(data = forest_data %>%
                     filter(variable %in% 
                              c("Afforested land", "Peat forest", "Non-peat forest") & YEAR > 2000), position="stack", aes(weight = value, fill = variable, group=variable), alpha = 1, width = 3) +
          scale_fill_manual( values = rp1_colours) +
          geom_hline(yintercept = 0, size = 0.5) +
          geom_point(data = defor_hist_data, aes(x = YEAR, y = MILLION_HECTARES_PER_YEAR, shape = variable), alpha = 1, size = 1.2) +
          scale_shape_manual(values = 8) +
          geom_line(data = forest_data %>%
                      filter(variable %in% 
                               c("net forest cover change","FABLE target") & YEAR > 2000), aes(y = value, x = YEAR, group= variable, linetype=variable), color = "firebrick", alpha = 0.99, size = 1) +
          scale_linetype_manual(values = c("solid", "dotted")) +
          theme_minimal() +
          theme(plot.title = element_text(hjust = 0.5, color = "#666666", size = my_title_text_size),
                plot.subtitle = element_text(hjust = 0.5, color = "#666666", size = my_subtitle_text_size),
                legend.position = "right",
                legend.title = element_blank(),
                legend.key.size = unit(0.75, "line"), 
                legend.spacing.x = unit(0.1, "cm"),
                legend.text = element_text(size = my_legend_text_size),
                plot.caption = element_text(hjust = 0.5, color = "#666666", size = my_caption_text_size),
                axis.text = element_text(size = my_axis_text_size),
                axis.text.x = element_text(angle = 45),
                axis.title = element_text(size = my_axistitle_text_size),
                panel.grid.major.x = element_blank(),
                panel.grid.minor.x = element_blank()) +
          scale_x_continuous(breaks=seq(2000, 2050, 5), labels = c("1996-2000","2001-2005","2006-2010","2011-2015","2016-2020","2021-2025","2026-2030","2031-2035","2036-2040","2041-2045","2046-2050")) +
          labs(x = "Year", y = "Million hectares per year", 
               caption = str_wrap(paste("Historical data source:", source_id %>%
                                          filter(TAB == "FOREST_CC_RESULTS") %>%
                                          select(SOURCE)), 100))
      }
      ggsave(filename = paste0(country_alpha, "_RESULTS_Fig1_woEV.pdf"), plot = rp1, device = "pdf", path = pathname_outp, scale = 1, width = 14, height = 7, units = "cm", dpi = 320, limitsize = TRUE)
    }
  
  #PLOT2
  
  #BIODIVERSITY
  if (is_envpe_avail==1){
    name_tmp1 <- names(envelope_data)[grep("Biodiv", names(envelope_data), ignore.case = FALSE)]
    biodiv_tmp <- envelope_data %>%
      select(name_tmp1) %>%
      reshape2::melt(., id.vars = NULL)
    
    Biodiv_tmp <- str_split(biodiv_tmp$variable, "_", simplify = TRUE)
    Biodiv     <- data.frame(Year = as.numeric(Biodiv_tmp[,2]), TYPE = Biodiv_tmp[,3], INDEX = biodiv_tmp$value)
    
    BioDivind <- Biodiv %>%
      group_by(Year, TYPE) %>%
      summarize(MXINDEX = max(INDEX), MNINDEX = min(INDEX))
    
    BioDivind_df <- rbind(BioDivind %>%
                            select(Year, MNINDEX) %>%
                            arrange(., Year) %>%
                            rename(INDEX = MNINDEX), 
                          BioDivind %>%
                            select(Year, MXINDEX) %>%
                            arrange(., desc(Year)) %>%
                            rename(INDEX = MXINDEX))
    
    biodiv_data <- rbind(calculat_data %>%
                           select(biodivshareland) %>%
                           melt(., id.vars = NULL),
                         data.frame(target_share = rep(0.5, 11)) %>%
                           melt(., id.vars = NULL)) %>%
      mutate(Year = rep(seq(2000, 2050, 5), 2))
    
    #Historical data
    biodiv_hist <- read_xlsx(path = paste0(pathname_tmpl, template_name), progress = readxl_progress(), skip = 1, sheet = "BIODIV_IND_RESULTS")
    names(biodiv_hist) <- c("YEAR", "historical share")
    years_tmp <- biodiv_hist$YEAR
    
  }
  
  ## NEW BIODIVERSITY DATA
  
  var.sel <- c("ForestLand_%","NewForestLand_%","AbandonedAgri_%","OtherLand_%") ## "BiodivLandCons_%","BiodivLand_%"
  
  biodiv_data <- subset(biodiv_data_new2, Src == "scen" & Iso2==country_alpha & Var %in% var.sel )[c("Year","Var","Val")]
  biodiv_data_hist <- subset(biodiv_data_new2, Src == "FAO" & Iso2==country_alpha & Var %in% var.sel)[c("Year","Var","Val")]
  biodiv_data_targ <- unique(subset(biodiv_data_new2, Iso2==country_alpha & Var %in% var.sel[1] )[c("Year","Var","Val")])
  biodiv_data_targ$Val <- 0.5
  biodiv_data$Src      <- "computed share"
  biodiv_data_hist$Src <- "historical share"
  biodiv_data_targ$Src <- "target share"
  biodiv_data$Src <- factor(biodiv_data$Src)
  
  Biodiv_data_hist <- biodiv_data_hist %>% 
    filter(Year < 2015) %>% 
    spread(., Var, Val)
  Biodiv_data_hist$'NatLand_%' <- Biodiv_data_hist$'ForestLand_%' + Biodiv_data_hist$'OtherLand_%'
  if(country_alpha == "ID"){
    Biodiv_data_hist$`NatLand_%` <- c(0.581785017, 0.571054061, 0.553328348)
  }
  Biodiv_data_hist2 <- gather(Biodiv_data_hist, Var, Val, 'ForestLand_%':'NatLand_%')
  
  Biodiv_data <- full_join(full_join(biodiv_data, Biodiv_data_hist2, by = c("Year", "Var", "Val", "Src")), biodiv_data_targ, by = c("Year", "Var", "Val", "Src"))
  
  level_key <- c("ForestLand_%" = "forest", "NewForestLand_%" = "afforested land", "AbandonedAgri_%" = "abandoned agricultural land", "OtherLand_%" = "other natural land", "NatLand_%" = "natural land (historical)")
  Biodiv_data$Var <- recode(Biodiv_data$Var, !!!level_key)
  
  rp2_col_key <- data.frame(Var = c("abandoned land", 
                                    "abandoned agricultural land", 
                                    "afforested land",
                                    "forest",
                                    "historical natural land",
                                    "other natural land"),
                            COLOURS  = c("#B3A4C8", 
                                         "#DA70D6", 
                                         "#26DF26",
                                         "#228B22",
                                         "#B3A4C8",
                                         "#5B2C6F"), 
                            stringsAsFactors = FALSE)
  
  rp2_colours <- rp2_col_key %>%  
    inner_join(., Biodiv_data, by = "Var") %>% 
    arrange(Var) %>% 
    distinct(COLOURS, .keep_all = TRUE) %>% 
    pull(., COLOURS)
  
  
  if(is_envpe_avail==1) {
    
    rp2 <- ggplot(data = Biodiv_data, aes(x = Year, y = Val * 100)) +
      geom_polygon(data = BioDivind_df, aes(x = Year, y = INDEX*100), fill= "#2936F7", alpha = 0.2) +
      geom_col(data = Biodiv_data %>% filter(Year >= 2000 & ! Src %in% c("target share","historical share")), position="stack", aes(x = Year, y = Val * 100, fill = Var), alpha = 1) + 
      geom_line(data = Biodiv_data %>% filter(Year >= 2000 & Src =="target share"), aes(x = Year, y = Val * 100, linetype=Src, color=Src), color = "firebrick", alpha = 0.99, size = 1.2) + 
      geom_point(data = Biodiv_data %>% filter(Var %in% c("natural land (historical)")), aes(x = Year, y = Val * 100, shape=Var), size=1.2) +
      scale_shape_manual(values = c(8)) +
      scale_linetype_manual(values="solid") +
      scale_fill_manual( values = rp2_colours) +
      geom_hline(yintercept = 0, size = 0.5) +
      ylim(0, 100) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, color = "#666666", size = my_title_text_size),
            plot.subtitle = element_text(hjust = 0.5, color = "#666666", size = my_subtitle_text_size),
            legend.position = "top",
            legend.title = element_blank(),
            legend.key.size = unit(0.6, "line"), 
            legend.spacing.x = unit(0.1, "cm"),
            legend.text = element_text(size = my_legend_text_size*0.9),
            plot.caption = element_text(hjust = 0.5, color = "#666666", size = my_caption_text_size),
            axis.text = element_text(size = my_axis_text_size),
            axis.title = element_text(size = my_axistitle_text_size),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank()) +
      scale_x_continuous(breaks=seq(2000, 2050, 5)) +
      labs(x = "Year", y = "Share of total land (%)", caption = str_wrap(paste("Historical data source:", source_id %>%
                                                                                 filter(TAB == "BIODIV_IND_RESULTS") %>%
                                                                                 select(SOURCE)), 100)) +
      guides(fill=guide_legend(nrow=2))
    
    if(country_alpha == "ROEU"){
      rm(rp2)
      rp2 <- ggplot(data = Biodiv_data, aes(x = Year, y = Val * 100)) + 
        geom_polygon(data = BioDivind_df, aes(x = Year, y = INDEX*100), fill= "#2936F7", alpha = 0.2) +
        geom_col(data = Biodiv_data %>% filter(Year >= 2000 & ! Src %in% c("target share","historical share")), position="stack", aes(x = Year, y = Val * 100, fill = Var), alpha = 1) +
        geom_line(data = Biodiv_data %>% filter(Year >= 2000 & Src =="target share"), aes(x = Year, y = Val * 100, linetype=Src, color=Src), color = "firebrick", alpha = 0.99, size = 1.2) +
        geom_point(data = Biodiv_data %>% filter(Var %in% c("natural land (historical)")), aes(x = Year, y = Val * 100, shape=Var), size=1.2) + 
        geom_point(data = data_glob_ls$BIODIV_GLOBIOM, aes(x = YEAR, y = Val * 100, shape = Var), size = 1.2) + 
        scale_shape_manual(values = c(3, 8)) +
        scale_linetype_manual(values="solid") +
        scale_fill_manual( values = rp2_colours) + 
        geom_hline(yintercept = 0, size = 0.5) +
        ylim(0, 100) +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, color = "#666666", size = my_title_text_size),
              plot.subtitle = element_text(hjust = 0.5, color = "#666666", size = my_subtitle_text_size),
              legend.position = "top",
              legend.title = element_blank(),
              legend.key.size = unit(0.6, "line"), 
              legend.spacing.x = unit(0.1, "cm"),
              legend.text = element_text(size = my_legend_text_size*0.9),
              plot.caption = element_text(hjust = 0.5, color = "#666666", size = my_caption_text_size),
              axis.text = element_text(size = my_axis_text_size),
              axis.title = element_text(size = my_axistitle_text_size),
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank()) +
        scale_x_continuous(breaks=seq(2000, 2050, 5)) +
        labs(x = "Year", y = "Share of total land (%)", caption = str_wrap(paste("Historical data source:", source_id %>%
                                                                                   filter(TAB == "BIODIV_IND_RESULTS") %>%
                                                                                   select(SOURCE)), 100)) +
        guides(fill=guide_legend(nrow=2))
    }
    
    
    ggsave(filename = paste0(country_alpha, "_RESULTS_Fig2_wEV.pdf"), plot = rp2, device = "pdf", path = pathname_outp, scale = 1, width = 14, height = 7, units = "cm", dpi = 320, limitsize = TRUE)
    
  }
  
  if(country_alpha != "ROEU"){
    rp2 <- ggplot(data = Biodiv_data, aes(x = Year, y = Val * 100)) +
      geom_col(data = Biodiv_data %>% filter(Year >= 2000 & ! Src %in% c("target share","historical share")), position="stack", aes(x = Year, y = Val * 100, fill = Var), alpha = 1) +
      geom_line(data = Biodiv_data %>% filter(Year >= 2000 & Src =="target share"), aes(x = Year, y = Val * 100, linetype=Src, color=Src), color = "firebrick", alpha = 0.99, size = 1.2) +
      geom_point(data = Biodiv_data %>% filter(Var %in% c("natural land (historical)")), aes(x = Year, y = Val * 100, shape=Var), size=1.2) + 
      scale_shape_manual(values = 8) +
      scale_linetype_manual(values="solid") +
      scale_fill_manual( values = rp2_colours) +
      geom_hline(yintercept = 0, size = 0.5) +
      ylim(0, 100) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, color = "#666666", size = my_title_text_size),
            plot.subtitle = element_text(hjust = 0.5, color = "#666666", size = my_subtitle_text_size),
            legend.position = "top",
            legend.title = element_blank(),
            legend.key.size = unit(0.6, "line"), 
            legend.spacing.x = unit(0.1, "cm"),
            legend.text = element_text(size = my_legend_text_size*0.9),
            plot.caption = element_text(hjust = 0.5, color = "#666666", size = my_caption_text_size),
            axis.text = element_text(size = my_axis_text_size),
            axis.title = element_text(size = my_axistitle_text_size),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank()) +
      scale_x_continuous(breaks=seq(2000, 2050, 5)) +
      labs(x = "Year", y = "Share of total land (%)", caption = str_wrap(paste("Historical data source:", source_id %>%
                                                                                 filter(TAB == "BIODIV_IND_RESULTS") %>%
                                                                                 select(SOURCE)), 100)) +
      guides(fill=guide_legend(nrow=2))
  }
  
  if(country_alpha == "ROEU"){
    rm(rp2)
    rp2 <- ggplot(data = Biodiv_data, aes(x = Year, y = Val * 100)) +
      geom_col(data = Biodiv_data %>% filter(Year >= 2000 & ! Src %in% c("target share","historical share")), position="stack", aes(x = Year, y = Val * 100, fill = Var), alpha = 1) +
      geom_line(data = Biodiv_data %>% filter(Year >= 2000 & Src =="target share"), aes(x = Year, y = Val * 100, linetype=Src, color=Src), color = "firebrick", alpha = 0.99, size = 1.2) +
      geom_point(data = Biodiv_data %>% filter(Var %in% c("natural land (historical)")), aes(x = Year, y = Val * 100, shape=Var), size=1.2) + 
      geom_point(data = data_glob_ls$BIODIV_GLOBIOM, aes(x = YEAR, y = Val * 100, shape = Var), size = 1.2) + 
      scale_shape_manual(values = c(3, 8)) +
      scale_linetype_manual(values="solid") +
      scale_fill_manual( values = rp2_colours) + 
      geom_hline(yintercept = 0, size = 0.5) +
      ylim(0, 100) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, color = "#666666", size = my_title_text_size),
            plot.subtitle = element_text(hjust = 0.5, color = "#666666", size = my_subtitle_text_size),
            legend.position = "top",
            legend.title = element_blank(),
            legend.key.size = unit(0.6, "line"), 
            legend.spacing.x = unit(0.1, "cm"),
            legend.text = element_text(size = my_legend_text_size*0.9),
            plot.caption = element_text(hjust = 0.5, color = "#666666", size = my_caption_text_size),
            axis.text = element_text(size = my_axis_text_size),
            axis.title = element_text(size = my_axistitle_text_size),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank()) +
      scale_x_continuous(breaks=seq(2000, 2050, 5)) +
      labs(x = "Year", y = "Share of total land (%)", caption = str_wrap(paste("Historical data source:", source_id %>%
                                                                                 filter(TAB == "BIODIV_IND_RESULTS") %>%
                                                                                 select(SOURCE)), 100)) +
      guides(fill=guide_legend(nrow=2))
  }
  
  ggsave(filename = paste0(country_alpha, "_RESULTS_Fig2_woEV.pdf"), plot = rp2, device = "pdf", path = pathname_outp, scale = 1, width = 14, height = 7, units = "cm", dpi = 320, limitsize = TRUE)
  
  if(exists('tmp_name')) {rm(tmp_name)}
  
  #PLOT 3
  
  #GHG RESULTS
  ghg_names <- c("CropsN2O",
                 "CropsCH4",
                 "CropsCO2",
                 "LivestockCH4",
                 "LivestockN2O",
                 "DeforCO2",
                 "OtherLUCCO2",
                 "SequestCO2",
                 "PeatCO2")
  
  if(is_envpe_avail==1) {
    
    tmp_name <- list()
    for(i in seq_along(ghg_names)){
      tmp_name[[i]] <- names(envelope_data)[grep(ghg_names[i], names(envelope_data), ignore.case = FALSE)]
    }
    
    #This sums the components of crop GHGs to produce CO2e
    crop_n2o <- envelope_data %>%
      select(tmp_name[[1]]) %>%
      melt(., id.vars = NULL) %>%
      separate(variable, c(NA, "YEAR", "CLASS")) %>%
      mutate(CropsN2O = value) %>%
      select(YEAR, CropsN2O)
    crop_ch4 <- envelope_data %>%
      select(tmp_name[[2]]) %>%
      melt(., id.vars = NULL) %>%
      mutate(CropsCH4 = value) %>%
      select(CropsCH4)
    crop_co2 <- envelope_data %>%
      select(tmp_name[[3]]) %>%
      melt(., id.vars = NULL) %>%
      mutate(CropsCO2 = value) %>%
      select(CropsCO2)
    
    crop_co2e <- data.frame(YEAR = crop_n2o$YEAR,
                            CROP_CO2 = crop_co2$CropsCO2,
                            CROP_N2O = crop_n2o$CropsN2O,
                            CROP_CH4 = crop_ch4$CropsCH4) %>%
      replace(is.na(.), 0) %>%
      mutate(CROP_CO2e = rowSums(.[,2:4]))
    
    rm(crop_n2o, crop_ch4, crop_co2)
    
    #This sums the components of livestock GHGs to produce CO2e from livestock
    live_ch4 <- envelope_data %>%
      select(tmp_name[[4]]) %>%
      melt(., id.vars = NULL) %>%
      separate(variable, c(NA, "YEAR", "CLASS")) %>%
      mutate(LiveCH4 = value) %>%
      select(YEAR, LiveCH4)
    live_n2o <- envelope_data %>%
      select(tmp_name[[5]]) %>%
      melt(., id.vars = NULL) %>%
      mutate(LiveN2O = value) %>%
      select(LiveN2O)
    live_co2e <- data.frame(YEAR = live_ch4$YEAR, LIVE_CH4 = live_ch4$LiveCH4, LIVE_N2O = live_n2o$LiveN2O) %>%
      replace(is.na(.), 0) %>%
      mutate(LIVE_CO2e = rowSums(.[,2:3]))
    
    #This sums the crop and livestock components to make agricultural CO2e component
    agri_co2e <- data.frame(YEAR = crop_co2e$YEAR, CROP_CO2e = crop_co2e$CROP_CO2e, LIVE_CO2e = live_co2e$LIVE_CO2e) %>%
      mutate(AGRICO2e = rowSums(.[,2:3]))
    
    rm(live_n2o, live_ch4)
    
    #From other land use change, including peat
    defor_co2e <- envelope_data %>%
      select(tmp_name[[6]]) %>%
      melt(., id.vars = NULL) %>%
      separate(variable, c(NA, "YEAR", "CLASS")) %>%
      mutate(DEFORCO2e = value) %>%
      select(YEAR, DEFORCO2e)
    luc_co2e  <- envelope_data %>%
      select(tmp_name[[7]]) %>%
      melt(., id.vars = NULL) %>%
      separate(variable, c(NA, "YEAR", "CLASS")) %>%
      mutate(LUCCO2e = value) %>%
      select(YEAR, LUCCO2e)
    seq_co2e <- envelope_data %>%
      select(tmp_name[[8]]) %>%
      melt(., id.vars = NULL) %>%
      separate(variable, c(NA, "YEAR", "CLASS")) %>%
      mutate(SEQCO2e = value) %>%
      select(YEAR, SEQCO2e)
    peat_co2e <- envelope_data %>%
      select(tmp_name[[9]]) %>%
      melt(., id.vars = NULL) %>%
      separate(variable, c(NA, "YEAR", "CLASS")) %>%
      mutate(PEATCO2e = value) %>%
      select(YEAR, PEATCO2e)
    
    lucf_co2e <- data.frame(YEAR = defor_co2e$YEAR,
                            DEFOR_CO2e = defor_co2e$DEFORCO2e,
                            LUC_CO2e = luc_co2e$LUCCO2e,
                            SEQ_CO2e = seq_co2e$SEQCO2e, 
                            PEAT_CO2e = peat_co2e$PEATCO2e) %>%
      mutate(LUCFCO2e = rowSums(.[,2:5]))
    
    rm(defor_co2e, luc_co2e, seq_co2e, peat_co2e)
    
    #Net Total GHG from agriculture and land use change
    totghg_co2e <- data.frame(YEAR = agri_co2e$YEAR,
                              AGRI_CO2e = agri_co2e$AGRICO2e,
                              LUCF_CO2e = lucf_co2e$LUCFCO2e) %>%
      mutate(LUCFCO2e = rowSums(.[,2:3]))
    
    #Net GHG from agriculture and land use change
    ghg_co2e <- data.frame(YEAR = agri_co2e$YEAR,
                           AGRI_CO2e = agri_co2e$AGRICO2e,
                           LUCF_CO2e = lucf_co2e$LUCFCO2e,
                           AFOLU_CO2e= totghg_co2e$LUCFCO2e)
    
    Ghg_co2e <- ghg_co2e %>%
      group_by(YEAR) %>%
      summarize(MX_AGRI_CO2e = max(AGRI_CO2e),
                MN_AGRI_CO2e = min(AGRI_CO2e),
                MX_LUCF_CO2e = max(LUCF_CO2e),
                MN_LUCF_CO2e = min(LUCF_CO2e),
                MX_AFOLU_CO2e = max(AFOLU_CO2e),
                MN_AFOLU_CO2e = min(AFOLU_CO2e))
    
    
    GHG_CO2e <- rbind(Ghg_co2e %>% 
                        select(MN_AGRI_CO2e) %>%
                        melt(., id.vars = NULL) %>%
                        mutate(YEAR = seq(2015, 2050, 5)),
                      Ghg_co2e %>%
                        select(MX_AGRI_CO2e) %>%
                        melt(., id.vars = NULL) %>%
                        mutate(YEAR = seq(2015, 2050, 5)) %>%
                        arrange(., desc(YEAR)),
                      Ghg_co2e %>%
                        select(MN_LUCF_CO2e) %>%
                        melt(., id.vars = NULL) %>%
                        mutate(YEAR = seq(2015, 2050, 5)),
                      Ghg_co2e %>%
                        select(MX_LUCF_CO2e) %>%
                        melt(., id.vars = NULL) %>%
                        mutate(YEAR = seq(2015, 2050, 5)) %>%
                        arrange(., desc(YEAR)), 
                      Ghg_co2e %>%
                        select(MN_AFOLU_CO2e) %>%
                        melt(., id.vars = NULL) %>%
                        mutate(YEAR = seq(2015, 2050, 5)),
                      Ghg_co2e %>%
                        select(MX_AFOLU_CO2e) %>%
                        melt(., id.vars = NULL) %>%
                        mutate(YEAR = seq(2015, 2050, 5)) %>%
                        arrange(., desc(YEAR)))
    #end of envelope data manipulation
    
  }
  
  ghg_data <- calculat_data %>%
    select(cropsch4, cropsco2, cropsn2o, livestockch4, livestockno2, otherlucc, deforco2, sequestco2, peatco2)
  
  ##
  #If peatco2 was not filled in the SCENATHN_report sheet, it might appear as a character string.
  #This would be a problem, so if it exists, it needs to be replaced with a vector of zeros.
  #Ad hoc test whether peatco2 is filled with character string
  if(is.na(as.numeric(ghg_data$peatco2)[1])) ghg_data$peatco2 <- rep(0, 11)
  ##
  
  Ghg_data <- ghg_data %>%
    replace(is.na(.), 0) %>%
    mutate(cropsco2e = rowSums(.[,1:3]), liveco2e = rowSums(.[,4:5]), agrico2e = rowSums(.[,1:5]), lucfco2e = rowSums(.[,6:8]), peatco2e = .[,9], afoluco2e = rowSums(.[,1:9])) %>%
    select(cropsco2e, liveco2e, agrico2e, lucfco2e, afoluco2e, peatco2e) %>%
    melt(., id.vars = NULL) %>%
    mutate(YEAR = rep(seq(2000, 2050, 5), 6))
  
  if(country_alpha == "ID"){
    level_key <- c(cropsco2e = "crops", liveco2e = "livestock", agrico2e = "crops & livestock", lucfco2e = "LUC", afoluco2e = "AFOLU", peatco2e = "peat")
    Ghg_data$variable <- recode(Ghg_data$variable, !!!level_key)
  } else {
    level_key <- c(cropsco2e = "crops", liveco2e = "livestock", agrico2e = "crops & livestock", lucfco2e = "LUC", afoluco2e = "AFOLU")
    Ghg_data$variable <- recode(Ghg_data$variable, !!!level_key)
  }
  
  
  #Historical data
  ghg_hist <- read_xlsx(path = paste0(pathname_tmpl, template_name), progress = readxl_progress(), skip = 1, sheet = "GHG_RESULTS")
  ghg_hist <- ghg_hist %>% 
    rename(historical = MILLION_TONNES_CO2e_PER_YEAR)
  years_tmp <- ghg_hist$YEAR
  
  Ghg_hist <- ghg_hist %>% 
    data.frame(., variable = "historical") %>% 
    rename(value = "historical")
  
  Ghg_data2 <- full_join(Ghg_data, Ghg_hist, by = c("variable", "value", "YEAR"))
  if(country_alpha == "ID"){
    Ghg_data2$variable <- factor(Ghg_data2$variable, levels=c("crops","livestock","LUC","crops & livestock","AFOLU","historical", "peat"))
  }
  if(country_alpha != "ID"){
    Ghg_data2$variable <- factor(Ghg_data2$variable, levels=c("crops","livestock","LUC","crops & livestock","AFOLU","historical"))
  }
  
  if(country_alpha == "ROEU"){
    Ghg_data2 <- full_join(Ghg_data2, data_glob_ls$GHG_GLOBIOM, by = c("variable", "value", "YEAR"))
    Ghg_data2$variable <- factor(Ghg_data2$variable, levels=c("crops","livestock","LUC","crops & livestock","AFOLU","historical", "GLOBIOM"))
  }
  
  # rp3a_col_key <- colorRampPalette(c("#E1CB27", "#4E0AF6", "#399C2F"))
  # rp3a_colours <- rp3a_col_key(4)
  
  rp3a_col_key <- data.frame(variable = c("crops", 
                                          "livestock", 
                                          "LUC", 
                                          "peat"),
                             COLOURS  = c("#E1CB27", 
                                          "#4E0AF6", 
                                          "#399C2F", 
                                          "#6E2C00"), 
                             stringsAsFactors = FALSE)
  
  rp3a_colours <- rp3a_col_key %>%  
    inner_join(., Ghg_data2, by = "variable") %>% 
    arrange(variable) %>% 
    distinct(COLOURS, .keep_all = TRUE) %>% 
    pull(., COLOURS)
  
  
  rp3a <- ggplot(data = Ghg_data2 %>% 
                   filter(YEAR >= 2000), aes(x = YEAR)) +
    geom_bar(data = Ghg_data2 %>% 
               filter((!variable %in% 
                         c("crops & livestock","historical","AFOLU")) & YEAR > 2000) , aes(weight = value, fill = variable)) +
    geom_hline(yintercept = 0, size = 0.5) +
    scale_fill_manual(values = rp3a_colours) +
    geom_line(data = Ghg_data2 %>% 
                filter((variable=="AFOLU" & YEAR > 2000)), aes(x = YEAR, y = value, linetype = variable), size=0.5, color="black") +
    geom_point(data = Ghg_data2 %>% 
                 filter(variable=="historical" & YEAR > 2000), aes(x = YEAR, y = value, shape=variable), size=1.2) +
    scale_shape_manual(values = 8) +
    scale_linetype_manual(values = "solid") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, color = "#666666", size = my_title_text_size),
          plot.subtitle = element_text(hjust = 0.5, color = "#666666", size = my_subtitle_text_size),
          legend.position = "top",
          legend.title = element_blank(),
          legend.key.size = unit(0.75, "line"), 
          legend.spacing.x = unit(0.1, "cm"),
          legend.text = element_text(size = my_legend_text_size),
          plot.caption = element_text(hjust = 0.5, color = "#666666", size = my_caption_text_size),
          axis.text = element_text(size = my_axis_text_size),
          axis.title = element_text(size = my_axistitle_text_size),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank()) +
    scale_x_continuous(breaks=seq(2005, 2050, 5)) +
    labs(x = "Year", y = expression(Million~ tons~ CO[2]~ e~ per~ year), caption = str_wrap(paste("Historical data source:", source_id %>%
                                                                                                    filter(TAB == "GHG_RESULTS") %>%
                                                                                                    select(SOURCE)), 100))
  
  if(country_alpha == "ROEU"){
    rp3a <- ggplot(data = Ghg_data2 %>% 
                     filter(YEAR >= 2000), aes(x = YEAR)) +
      geom_bar(data = Ghg_data2 %>% 
                 filter((!variable %in% 
                           c("crops & livestock", "GLOBIOM", "historical", "AFOLU")) & YEAR > 2000) , aes(weight = value, fill = variable), colour = "white", size = 0.05) +
      geom_hline(yintercept = 0, size = 0.5) +
      scale_fill_manual(values = rp3a_colours) +
      geom_line(data = Ghg_data2 %>% 
                  filter((variable == "AFOLU" & YEAR > 2000)), aes(x = YEAR, y = value, linetype = variable), size=0.5, color="black") +
      geom_point(data = Ghg_data2 %>% 
                   filter(variable == "historical" & YEAR > 2000), aes(x = YEAR, y = value, shape = variable), size=1.2) + 
      geom_point(data = Ghg_data2 %>% 
                   filter(variable == "GLOBIOM" & YEAR > 2000), aes(x = YEAR, y = value, shape = variable), size=1.2) + 
      geom_point(data = data_glob_ls$GHG_GLOBIOM %>% 
                   filter(YEAR > 2000), aes(x = YEAR, y = value, shape = variable), size = 1.2) + 
      scale_shape_manual(values = c(3, 8)) +
      scale_linetype_manual(values = "solid") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, color = "#666666", size = my_title_text_size),
            plot.subtitle = element_text(hjust = 0.5, color = "#666666", size = my_subtitle_text_size),
            legend.position = "top",
            legend.title = element_blank(),
            legend.key.size = unit(0.75, "line"), 
            legend.spacing.x = unit(0.1, "cm"),
            legend.text = element_text(size = my_legend_text_size),
            plot.caption = element_text(hjust = 0.5, color = "#666666", size = my_caption_text_size),
            axis.text = element_text(size = my_axis_text_size),
            axis.title = element_text(size = my_axistitle_text_size),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank()) +
      scale_x_continuous(breaks=seq(2005, 2050, 5)) +
      labs(x = "Year", y = expression(Million~ tons~ CO[2]~ e~ per~ year), caption = str_wrap(paste("Historical data source:", source_id %>%
                                                                                                      filter(TAB == "GHG_RESULTS") %>%
                                                                                                      select(SOURCE)), 100))
  }
  
  ggsave(filename = paste0(country_alpha, "_RESULTS_Fig3a_woEV.pdf"), plot = rp3a, device = "pdf", path = pathname_outp, scale = 1, width = 14, height = 7, units = "cm", dpi = 320, limitsize = TRUE)
  
  if(is_envpe_avail==1){
    
    rp3a <- ggplot(data = Ghg_data2 %>% filter(YEAR >= 2000), aes(x = YEAR)) +
      geom_polygon(data = GHG_CO2e %>% filter(variable %in% c("MN_AFOLU_CO2e", "MX_AFOLU_CO2e")), aes(x = YEAR, y = value), fill= "#000000", alpha = 0.15) +
      geom_bar(data = Ghg_data2 %>% filter((!variable %in% c("crops & livestock","historical","AFOLU")) & YEAR > 2000) , aes(weight = value, fill = variable)) +
      geom_hline(yintercept = 0, size = 0.5) + 
      scale_fill_manual(values = rp3a_colours) +
      geom_line(data = Ghg_data2 %>% filter((variable=="AFOLU" & YEAR > 2000)), aes(x = YEAR, y = value, linetype = variable), size=0.5, color="black") +
      geom_point(data = Ghg_data2 %>% filter(variable=="historical" & YEAR > 2000), aes(x = YEAR, y = value, shape=variable), size=1.2) +
      scale_shape_manual(values = 8) +
      scale_linetype_manual(values = "solid") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, color = "#666666", size = my_title_text_size),
            plot.subtitle = element_text(hjust = 0.5, color = "#666666", size = my_subtitle_text_size),
            legend.position = "top",
            legend.title = element_blank(),
            legend.key.size = unit(0.75, "line"), 
            legend.spacing.x = unit(0.1, "cm"),
            legend.text = element_text(size = my_legend_text_size),
            plot.caption = element_text(hjust = 0.5, color = "#666666", size = my_caption_text_size),
            axis.text = element_text(size = my_axis_text_size),
            axis.title = element_text(size = my_axistitle_text_size),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank()) +
      scale_x_continuous(breaks=seq(2005, 2050, 5)) +
      labs(x = "Year", y = expression(Million~ tons~ CO[2]~ e~ per~ year), caption = str_wrap(paste("Historical data source:", source_id %>%
                                                                                                      filter(TAB == "GHG_RESULTS") %>%
                                                                                                      select(SOURCE)), 100))
    
    if(country_alpha == "ROEU"){
      rm(rp3a)
      rp3a <- ggplot(data = Ghg_data2 %>% filter(YEAR >= 2000), aes(x = YEAR)) +
        geom_polygon(data = GHG_CO2e %>% filter(variable %in% c("MN_AFOLU_CO2e", "MX_AFOLU_CO2e")), aes(x = YEAR, y = value), fill= "#000000", alpha = 0.15) +
        geom_bar(data = Ghg_data2 %>% filter((!variable %in% c("crops & livestock","GLOBIOM","historical","AFOLU")) & YEAR > 2000) , aes(weight = value, fill = variable)) +
        scale_fill_manual(values = rp3a_colours) +
        geom_hline(yintercept = 0, size = 0.5) +
        geom_line(data = Ghg_data2 %>% filter((variable=="AFOLU" & YEAR > 2000)), aes(x = YEAR, y = value, linetype = variable), size=0.5, color="black") +
        geom_point(data = Ghg_data2 %>% filter(variable=="historical" & YEAR > 2000), aes(x = YEAR, y = value, shape=variable), size=1.2) + 
        geom_point(data = Ghg_data2 %>% filter(variable=="GLOBIOM" & YEAR > 2000), aes(x = YEAR, y = value, shape=variable), size=1.2) + 
        geom_point(data = data_glob_ls$GHG_GLOBIOM %>% 
                     filter(YEAR > 2000), aes(x = YEAR, y = value, shape = variable), size = 1.2) + 
        scale_shape_manual(values = c(3, 8)) +
        scale_linetype_manual(values = "solid") + 
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, color = "#666666", size = my_title_text_size),
              plot.subtitle = element_text(hjust = 0.5, color = "#666666", size = my_subtitle_text_size),
              legend.position = "top",
              legend.title = element_blank(),
              legend.key.size = unit(0.75, "line"), 
              legend.spacing.x = unit(0.1, "cm"),
              legend.text = element_text(size = my_legend_text_size),
              plot.caption = element_text(hjust = 0.5, color = "#666666", size = my_caption_text_size),
              axis.text = element_text(size = my_axis_text_size),
              axis.title = element_text(size = my_axistitle_text_size),
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank()) +
        scale_x_continuous(breaks=seq(2005, 2050, 5)) +
        labs(x = "Year", y = expression(Million~ tons~ CO[2]~ e~ per~ year), caption = str_wrap(paste("Historical data source:", source_id %>%
                                                                                                        filter(TAB == "GHG_RESULTS") %>%
                                                                                                        select(SOURCE)), 100))
    }
    
    ggsave(filename = paste0(country_alpha, "_RESULTS_Fig3a_wEV.pdf"), plot = rp3a, device = "pdf", path = pathname_outp, scale = 1, width = 14, height = 7, units = "cm", dpi = 320, limitsize = TRUE)
    
  }
  
  # - PLOT 4
  
  #FOOD RESULTS
  #Calculator data
  food_data <- calculat_data %>%
    select(kcal_feas, kcal_mder) %>%
    melt(., id.vars = NULL) %>%
    mutate(YEAR = rep(seq(2000, 2050, 5), 2))
  
  level_key <- c(kcal_feas = "computed intake", kcal_mder = "MDER")
  food_data$variable <- recode(food_data$variable, !!!level_key)
  
  #Template data
  name_tmp1 <- names(envelope_data)[grep("kcal_feas", names(envelope_data), ignore.case = FALSE)]
  kcal_env  <- envelope_data %>%
    select(name_tmp1) %>%
    replace(is.na(.), 0) %>%
    melt(., id.vars = NULL) %>%
    separate(variable, c(NA, "YEAR", "CLASS")) %>%
    mutate(KCAL = value) %>%
    select(YEAR, KCAL) %>%
    group_by(YEAR) %>%
    summarize(MXKCAL = max(KCAL), MNKCAL = min(KCAL))
  
  #Envelope data
  if(is_envpe_avail==1) {
    Kcal_env <- rbind(kcal_env %>%
                        select(MNKCAL) %>%
                        melt(., id.vars = NULL) %>%
                        mutate(YEAR = seq(2015, 2050, 5)),
                      kcal_env %>%
                        select(MXKCAL) %>%
                        melt(., id.vars = NULL) %>%
                        mutate(YEAR = seq(2015, 2050, 5)) %>%
                        arrange(., desc(YEAR)))
  }
  
  kcal_tmp <- read_xlsx(path = paste0(pathname_tmpl, template_name), progress = readxl_progress(), skip = 1, sheet = "FOOD_RESULTS")
  
  if(country_alpha == "ROEU"){
    food_granular_data <- full_join(kcal_tmp, data_glob_ls$FOOD_GLOBIOM, by = c("YEAR", "CLASS_AGG", "KCAL", "COLOR"))
  }
  if(country_alpha != "ROEU"){
    food_granular_data <- kcal_tmp
  }
  rm(kcal_tmp)
  
  rp4_colours <- p7_col_key %>% 
    filter(CLASS_AGG != "total") %>% 
    inner_join(., food_granular_data, by = "CLASS_AGG") %>% 
    arrange(CLASS_AGG) %>% 
    distinct(COLOURS) %>% 
    pull(., COLOURS)
  
  level_key <- c(total = "Total consumption (historical)")
  food_granular_data$CLASS_AGG <- recode(food_granular_data$CLASS_AGG, !!!level_key)
  
  
  temporary_filler_data <- calculat_data %>%
    select(kcal_feas) %>%
    melt(., id.vars = NULL) %>%
    mutate(YEAR = seq(2000, 2050, 5))
  
  if(is_envpe_avail==1) {
    if(country_alpha != "ROEU"){
      rp4 <- ggplot(data = temporary_filler_data, aes(x = YEAR)) +
        geom_polygon(data = Kcal_env, aes(x = YEAR, y = value), fill= "#CD5C5C", alpha = 0.15) +
        geom_bar(data = food_granular_data %>% 
                   filter(!CLASS_AGG %in% c("Total consumption (historical)")), aes(weight = KCAL, fill = CLASS_AGG), width = 2) +
        geom_point(data = food_granular_data %>% filter(CLASS_AGG=="Total consumption (historical)"), aes(x = YEAR, y = KCAL_HIST, shape=CLASS_AGG), size = 1.2, alpha = 0.9) +
        geom_line(data = food_data, aes(x = YEAR, y = value, color = variable, linetype = variable, size=variable), alpha = 0.9) +
        geom_hline(yintercept = 0, size = 0.5) +
        # geom_line(data = food_granular_data %>% filter(CLASS_AGG == "total"), aes(x = YEAR, y = KCAL_HIST)) +
        scale_fill_manual(values = rp4_colours) +
        scale_color_manual(values = c("computed intake" = "black", "MDER" = "firebrick")) +
        scale_linetype_manual(values = c("computed intake" = "solid", "MDER" = "dotted")) +
        scale_size_manual(values = c("computed intake" = 0.5, "MDER" = 1)) +
        scale_shape_manual(values = 8) +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, color = "#666666", size = my_title_text_size),
              plot.subtitle = element_text(hjust = 0.5, color = "#666666", size = my_subtitle_text_size),
              legend.position = "right",
              legend.title = element_blank(),
              legend.key.size = unit(0.5, "line"), 
              legend.spacing.x = unit(0.1, "cm"),
              legend.text = element_text(size = my_legend_text_size_2),
              plot.caption = element_text(hjust = 0.5, color = "#666666", size = my_caption_text_size),
              axis.text = element_text(size = my_axis_text_size),
              axis.title = element_text(size = my_axistitle_text_size),
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank()) +
        scale_x_continuous(breaks=seq(2000, 2050, 5)) +
        labs(x = "Year", y = "kcal per capita per day", caption = str_wrap(paste("Historical data source:", source_id %>%
                                                                                   filter(TAB == "FOOD_RESULTS") %>%
                                                                                   select(SOURCE)), 100))
    }
    
    if(country_alpha == "ROEU"){
      rp4 <- ggplot(data = temporary_filler_data, aes(x = YEAR)) +
        geom_polygon(data = Kcal_env, aes(x = YEAR, y = value), fill= "#CD5C5C", alpha = 0.15) +
        geom_bar(data = food_granular_data %>% 
                   filter(!CLASS_AGG %in% 
                            c("Total consumption (historical)", "GLOBIOM")), aes(weight = KCAL, fill = CLASS_AGG), width = 2) +
        geom_point(data = food_granular_data %>% 
                     filter(CLASS_AGG=="Total consumption (historical)"), aes(x = YEAR, y = KCAL_HIST, shape=CLASS_AGG), alpha = 0.9, size = 1.2) + 
        geom_point(data = food_granular_data %>% 
                     filter(CLASS_AGG=="GLOBIOM"), aes(x = YEAR, y = KCAL, shape=CLASS_AGG), size = 1.2, alpha = 0.9) +
        geom_line(data =  food_data, aes(x = YEAR, y = value, color = variable, linetype = variable), alpha = 0.9) +
        geom_hline(yintercept = 0, size = 0.5) +
        # geom_line(data = food_granular_data %>% filter(CLASS_AGG == "total"), aes(x = YEAR, y = KCAL_HIST)) +
        scale_fill_manual(values = rp4_colours) +
        scale_color_manual(values = c("computed intake" = "black", "MDER" = "firebrick")) +
        scale_linetype_manual(values = c("computed intake" = "solid", "MDER" = "dotted")) +
        scale_size_manual(values = c("computed intake" = 0.5, "MDER" = 1)) +
        scale_shape_manual(values = c(3, 8)) +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, color = "#666666", size = my_title_text_size),
              plot.subtitle = element_text(hjust = 0.5, color = "#666666", size = my_subtitle_text_size),
              legend.position = "right",
              legend.title = element_blank(),
              legend.key.size = unit(0.5, "line"), 
              legend.spacing.x = unit(0.1, "cm"),
              legend.text = element_text(size = my_legend_text_size_2),
              plot.caption = element_text(hjust = 0.5, color = "#666666", size = my_caption_text_size),
              axis.text = element_text(size = my_axis_text_size),
              axis.title = element_text(size = my_axistitle_text_size),
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank()) + 
        scale_x_continuous(breaks=seq(2000, 2050, 5)) +
        labs(x = "Year", y = "kcal per capita per day", caption = str_wrap(paste("Historical data source:", source_id %>%
                                                                                   filter(TAB == "FOOD_RESULTS") %>%
                                                                                   select(SOURCE)), 100))
    }
    ggsave(filename = paste0(country_alpha, "_RESULTS_Fig4_wEV.pdf"), plot = rp4, device = "pdf", path = pathname_outp, scale = 1, width = 14, height = 7, units = "cm", dpi = 320, limitsize = TRUE)
  }
  
  if(country_alpha == "ROEU"){
    
    rp4 <- ggplot(data = temporary_filler_data, aes(x = YEAR)) +
      geom_bar(data = food_granular_data %>% 
                 filter(!CLASS_AGG %in% 
                          c("Total consumption (historical)", "GLOBIOM")), aes(weight = KCAL, fill = CLASS_AGG), width = 2) +
      geom_point(data = food_granular_data %>% 
                   filter(CLASS_AGG=="Total consumption (historical)"), aes(x = YEAR, y = KCAL_HIST, shape=CLASS_AGG), alpha = 0.9, size = 1.2) + 
      geom_point(data = food_granular_data %>% 
                   filter(CLASS_AGG=="GLOBIOM"), aes(x = YEAR, y = KCAL, shape=CLASS_AGG), size = 1.2, alpha = 0.9) +
      geom_line(data =  food_data %>% 
                  filter(variable == "MDER"), aes(x = YEAR, y = value, color = variable, linetype = variable), alpha = 0.9) +
      geom_hline(yintercept = 0, size = 0.5) +
      # geom_line(data = food_granular_data %>% filter(CLASS_AGG == "total"), aes(x = YEAR, y = KCAL_HIST)) +
      scale_fill_manual(values = rp4_colours) +
      scale_color_manual(values = c("computed intake" = "black", "MDER" = "firebrick")) +
      scale_linetype_manual(values = c("computed intake" = "solid", "MDER" = "dotted")) +
      scale_size_manual(values = c("computed intake" = 0.5, "MDER" = 1)) +
      scale_shape_manual(values = c(3, 8)) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, color = "#666666", size = my_title_text_size),
            plot.subtitle = element_text(hjust = 0.5, color = "#666666", size = my_subtitle_text_size),
            legend.position = "right",
            legend.title = element_blank(),
            legend.key.size = unit(0.5, "line"), 
            legend.spacing.x = unit(0.1, "cm"),
            legend.text = element_text(size = my_legend_text_size_2),
            plot.caption = element_text(hjust = 0.5, color = "#666666", size = my_caption_text_size),
            axis.text = element_text(size = my_axis_text_size),
            axis.title = element_text(size = my_axistitle_text_size),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank()) + 
      scale_x_continuous(breaks=seq(2000, 2050, 5)) +
      labs(x = "Year", y = "kcal per capita per day", caption = str_wrap(paste("Historical data source:", source_id %>% 
                                                                                 filter(TAB == "FOOD_RESULTS") %>%
                                                                                 select(SOURCE)), 100))
    
    
  }
  
  if(country_alpha != "ROEU"){
    rp4 <- ggplot(data = temporary_filler_data, aes(x = YEAR)) +
      geom_bar(data = food_granular_data %>% filter(CLASS_AGG != "Total consumption (historical)"), aes(weight = KCAL, fill = CLASS_AGG), width = 2) +
      geom_point(data = food_granular_data %>% filter(CLASS_AGG=="Total consumption (historical)"), aes(x = YEAR, y = KCAL_HIST, shape=CLASS_AGG), size = 1.2, alpha = 0.9) +
      geom_line(data =  food_data %>% 
                  filter(variable == "MDER"), aes(x = YEAR, y = value, color = variable, linetype = variable, size=variable), alpha = 0.9) +
      geom_hline(yintercept = 0, size = 0.5) +
      # geom_line(data = food_granular_data %>% filter(CLASS_AGG == "total"), aes(x = YEAR, y = KCAL_HIST)) +
      scale_fill_manual(values = rp4_colours) +
      scale_color_manual(values = c("computed intake" = "black", "MDER" = "firebrick")) +
      scale_linetype_manual(values = c("computed intake" = "solid", "MDER" = "dotted")) +
      scale_size_manual(values = c("computed intake" = 0.5, "MDER" = 1)) +
      scale_shape_manual(values = 8) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, color = "#666666", size = my_title_text_size),
            plot.subtitle = element_text(hjust = 0.5, color = "#666666", size = my_subtitle_text_size),
            legend.position = "right",
            legend.title = element_blank(),
            legend.key.size = unit(0.5, "line"), 
            legend.spacing.x = unit(0.1, "cm"),
            legend.text = element_text(size = my_legend_text_size_2),
            plot.caption = element_text(hjust = 0.5, color = "#666666", size = my_caption_text_size),
            axis.text = element_text(size = my_axis_text_size),
            axis.title = element_text(size = my_axistitle_text_size),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank()) +
      scale_x_continuous(breaks=seq(2000, 2050, 5)) +
      labs(x = "Year", y = "kcal per capita per day", caption = str_wrap(paste("Historical data source:", source_id %>%
                                                                                 filter(TAB == "FOOD_RESULTS") %>%
                                                                                 select(SOURCE)), 100))
  }
  ggsave(filename = paste0(country_alpha, "_RESULTS_Fig4_woEV.pdf"), plot = rp4, device = "pdf", path = pathname_outp, scale = 1, width = 14, height = 7, units = "cm", dpi = 320, limitsize = TRUE)
  
  unlink(c(paste0(country_alpha, "_RESULTS_Fig1.pdf"), 
           paste0(country_alpha, "_RESULTS_Fig1_wEV.pdf"), 
           paste0(country_alpha, "_RESULTS_Fig1_woEV.pdf"), 
           paste0(country_alpha, "_RESULTS_Fig2.pdf"), 
           paste0(country_alpha, "_RESULTS_Fig2_wEV.pdf"), 
           paste0(country_alpha, "_RESULTS_Fig2_woEV.pdf"), 
           paste0(country_alpha, "_RESULTS_Fig3a.pdf"), 
           paste0(country_alpha, "_RESULTS_Fig3a_wEV.pdf"), 
           paste0(country_alpha, "_RESULTS_Fig3a_woEV.pdf"), 
           paste0(country_alpha, "_RESULTS_Fig3b.pdf"), 
           paste0(country_alpha, "_RESULTS_Fig3b_wEV.pdf"), 
           paste0(country_alpha, "_RESULTS_Fig3b_woEV.pdf"), 
           paste0(country_alpha, "_RESULTS_Fig4.pdf"), 
           paste0(country_alpha, "_RESULTS_Fig4_wEV.pdf"), 
           paste0(country_alpha, "_RESULTS_Fig4_woEV.pdf")))
  
}
