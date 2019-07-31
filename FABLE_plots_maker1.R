#FABLE_plots_maker1.R
##
#Authors: Marcus J Thomson (IIASA), Hugo Valin (IIASA), David LeClere (IIASA)
#Intenational Institute for Applied Systems Analysis (IIASA), Laxenburg, Austria
##
#Dependencies: Master.R, FABLE_plots_data.R
#Purpose: Produces the first set of "historic" plots in the country chapters. Use Master.R to run.

#define variables to parametrize plot attributes
barspacer <- 0.1
my_caption_text_size     <- 7
my_legendtitle_text_size <- 8
my_legend_text_size      <- 7
my_legend_text_size_2    <- 6
my_title_text_size       <- 10
my_title_text_size_2     <- 8
my_subtitle_text_size    <- 7
my_axistitle_text_size   <- 7
my_axis_text_size        <- 6
my_anno_text_size        <- 6

# -- number of characters for str_wrap (outputing legend over several lines)
n_chars_wrap  <- 25
caption_width <- 40


for(j in seq(1, dim(r_reg_country_df)[1], 1)){
  
  # - Define data sources by country
  country_alpha <- as.character(r_reg_country_df$reg_country[j])
  template_name <- as.character(r_reg_country_df$file[j])
  source_id     <- read_xlsx(path = paste0(pathname_tmpl, template_name), progress = readxl_progress(), skip = 1, sheet = "Sources")
  print(paste(j,country_alpha))
  
  # - Create path to output if not existing
  pathname_outp <- paste0(pathname_imag, country_alpha)
  ifelse(!dir.exists(pathname_outp), dir.create(pathname_outp), FALSE)
  
  # - Assign Excel tab data to elements of a list
  tab_names <- c("LAND1", "LAND2", "TRADE1", "TRADE2", "GHG1", "GHG2", "FOOD1", "TRADE_ADJ", "LAND_TRADE_ADJ")
  data_ls <- list()
  for(k in seq_along(tab_names)){
    data_ls[[k]] <- read_xlsx(path = paste0(pathname_tmpl, template_name), progress = readxl_progress(), skip = 1, sheet = tab_names[k])
  }
  
  #The following little script (between here and dev.off) makes a rectangular "pie-chart", called _sqFig in the output. It contains the same information as p1.
  # svg(filename = paste0(pathname_outp, "/", country_alpha, "_LAND_FOOD_SYS_sqFig1.svg"), width = 10, height = 10)
  # treemap(data_ls[[1]] %>%
  #         filter(CLASS_AGG != "total"),
  #       index = c("CLASS_AGG"),
  #       vSize = "SHARE",
  #       vColor = "COLOR",
  #       title = "",
  #       force.print.labels = TRUE,
  #       border.col = "white",
  #       position.legend = "none",
  #       aspRatio = 1,
  #       fontface.labels = 1,
  #       fontsize.labels = 8)
  # dev.off()
  
  # - PLOT 1
  
  # Harmonize colours for plots
  
  # # disactivate template colors for now
  # p1_col_key <- data.frame(CLASS = data_ls[[1]]$CLASS_AGG, COLOR = data_ls[[1]]$COLOR, stringsAsFactors = FALSE)
  # p1_colours <- p1_col_key$COLOR[order(p1_col_key$CLASS)]
  
  # p1_col_key <- colorRampPalette(c("goldenrod", "darkgreen", "orange", "purple", "blue"))
  # p1_colours <- p1_col_key(dim(data_ls[[1]])[1]-1)
  
  p1_col_key <- data.frame(CLASS_AGG = c("abandoned agri land", 
                                         "cropland", 
                                         "forest", 
                                         "forest (afforested)", 
                                         "forest (plantation)", 
                                         "forest (protected)", 
                                         "grassland", 
                                         "grassland (other)", 
                                         "not relevant", 
                                         "other land", 
                                         "other land (protected)", 
                                         "pasture", 
                                         "pasture (native)", 
                                         "pasture (modified)", 
                                         "reserves", 
                                         "urban", 
                                         "water", 
                                         "wetlands", 
                                         "total"),
                           COLOURS  = c("#DA70D6", 
                                        "#FFD700", 
                                        "#228B22", 
                                        "#26DF26", 
                                        "#00FF7F", 
                                        "#006400", 
                                        "#32CD32", 
                                        "#90EE90", 
                                        "#C0C0C0", 
                                        "#5B2C6F", 
                                        "#C414E0", 
                                        "#94F894", 
                                        "#56FA56", 
                                        "#90EE90", 
                                        "#006400", 
                                        "#6120DC", 
                                        "#0980CD", 
                                        "#A8C6DA", 
                                        "#FFFFFF"), 
                           stringsAsFactors = FALSE)
  
  p1_colours <- p1_col_key %>% 
    filter(CLASS_AGG != "total") %>% 
    inner_join(., data_ls[[1]], by = "CLASS_AGG") %>% 
    arrange(CLASS_AGG) %>% 
    distinct(COLOURS, .keep_all = TRUE) %>% 
    pull(., COLOURS)
  
  share_prot <- 100*sum(c(data_ls[[1]]$SHARE[which(data_ls[[1]]$CLASS_AGG=="other land (protected)")],
                          data_ls[[1]]$SHARE[which(data_ls[[1]]$CLASS_AGG=="forest (protected)")]),na.rm=T) /
    data_ls[[1]]$SHARE[which(data_ls[[1]]$CLASS_AGG=="total")]
  text_share <- as.character(round(share_prot,digits = 0));
  text_PAs <- paste("Protected area: ",text_share,"% of total land",sep="")
  if(is.na(text_share)) {
    text_share <- "-"
    text_PAs <- paste("Protected area: ",text_share,sep="")
  }
  
  timepoint <- data_ls[[1]]$YEAR[1]
  wrapped_caption <- paste("Source:", source_id %>%
                             filter(TAB == "LAND1") %>%
                             select(SOURCE))
  
  p1 <- ggplot(data = data_ls[[1]] %>%
                 filter(CLASS_AGG != "total"), mapping = aes(x = "", y = SHARE, fill = str_wrap(CLASS_AGG, width = n_chars_wrap))) +
    geom_bar(stat = "identity", width=1, colour="white", size = 0.05) +
    coord_polar(theta = "y", start = 0) +
    labs(x = NULL, y = NULL, caption = str_wrap(wrapped_caption, width = caption_width)) +
    scale_fill_manual(values = p1_colours) +
    theme_classic() +
    theme(axis.line = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          plot.title = element_blank(),
          legend.title = element_blank(),
          plot.subtitle = element_text(size = my_subtitle_text_size),
          legend.text = element_text(size = my_legend_text_size),
          legend.key.size = unit(0.75, "line"), 
          legend.spacing.x = unit(0.1, "cm"), 
          plot.caption = element_text(hjust = 0.5, color = "#666666", size = my_caption_text_size))
  # another option using cowplot while also using a two column frame
  p1b <- get_legend(p1)
  p1a <- ggdraw(p1 + theme(legend.position = 'none'))
  if(share_prot > 0) { p1a <- p1a  + draw_text(text_PAs, angle = 0, size = my_anno_text_size, x = 0.5, y=0.27) } # if there is subtitle use y=0.23, if not use y=0.27
  p1 <- plot_grid(p1a, p1b, labels = c('', ''), ncol = 2, rel_widths  = c(0.6,0.4))
  
 
  
  #The following little script (between here and dev.off) makes a rectangular "pie-chart", called _sqFig in the output. It contains the same information as p2.
  # png(filename = paste0(pathname_outp, "/", country_alpha, "_LAND_FOOD_SYS_sqFig2.pdf"), width = 10, height = 10, units = "cm", res = 280)
  # # grid.newpage()
  # # grid.rect()
  # # pushViewport(viewport(layout = grid.layout(2,1)))
  # #
  # # vp <- viewport(layout.pos.col = 1, layout.pos.row = ind)
  #
  # treemap(data_ls[[2]],
  #       index = c("CROP"),
  #       vSize = "SHARE",
  #       vColor = "COLOR",
  #       title = "",
  #       force.print.labels = TRUE,
  #       border.col = "white",
  #       position.legend = "none",
  #       aspRatio = 1,
  #       fontface.labels = 1,
  #       fontsize.labels = 8)
  #
  # dev.off()
  
  #PLOT 2
  
  p2_col_key <- colorRampPalette(c("firebrick", "darkgreen", "orange", "purple", "cadetblue", "blue"))
  p2_colours <- p2_col_key(dim(data_ls[[2]])[1])
  
  timepoint <- data_ls[[2]]$YEAR[1]
  caption_tmp <- paste("Source:", source_id %>%
                         filter(TAB == "LAND2") %>%
                         select(SOURCE))
  wrapped_caption <- str_wrap(caption_tmp, width = caption_width)
  
  p2 <- ggplot(data = data_ls[[2]] %>% 
                 filter(CROP != "total"), mapping = aes(x = "", y = SHARE, fill = str_wrap(CROP, n_chars_wrap))) +
    geom_bar(stat = "identity", width=1, colour="white", size = 0.05) +
    coord_polar(theta = "y", start = 0) +
    labs(x = NULL, y = NULL, caption = wrapped_caption) +
    scale_fill_manual(values = p2_colours) +
    theme_classic() +
    theme(axis.line = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          plot.title = element_blank(),
          legend.title = element_blank(),
          legend.text = element_text(size = my_legend_text_size),
          legend.key.size = unit(0.75, "line"), 
          legend.spacing.x = unit(0.1, "cm"),
          plot.caption = element_text(hjust = 0.5, color = "#666666", size = my_caption_text_size))
  
  # add a frame of two columns
  p2a <- p2 + theme(legend.position = 'none')
  p2b <- get_legend(p2)
  p2 <- plot_grid(p2a, p2b, labels = c('', ''), ncol = 2, rel_widths  = c(0.6,0.4))
  
  
  
  #PLOT 3
  
  data_ls[[3]] <- read_xlsx(path = paste0(pathname_tmpl, template_name), progress = readxl_progress(), skip = 1, sheet = "TRADE1")
  
  p3_col_key <- colorRampPalette(c("firebrick", "darkgreen", "orange", "purple", "cadetblue", "blue"))
  p3_colours <- p3_col_key(dim(data_ls[[3]])[1])
  
  timepoint <- data_ls[[3]]$YEAR[1]
  caption_tmp <- paste("Source:", source_id %>%
                         filter(TAB == "TRADE1") %>%
                         select(SOURCE))
  wrapped_caption <- str_wrap(caption_tmp, width = caption_width)
  
  p3 <- ggplot(data = data_ls[[3]], mapping = aes(x = "", y = BILLIONS_USD, fill = str_wrap(EXPORTS, n_chars_wrap))) +
    geom_bar(stat="identity", width=1, colour = "white", size=0.05) +
    theme_minimal() +
    scale_fill_manual(values = p3_colours) +
    theme(plot.title = element_text(hjust = 0.5, color = "#666666", size = my_title_text_size),
          plot.subtitle = element_text(hjust = 0.5, color = "#666666", size = my_subtitle_text_size),
          legend.position = "right",
          legend.title = element_blank(),
          legend.key.size = unit(0.75, "line"), 
          legend.spacing.x = unit(0.1, "cm"),
          legend.text = element_text(size = my_legend_text_size),
          plot.caption = element_text(hjust = 0.5, color = "#666666", size = my_caption_text_size),
          axis.text = element_text(size = my_axis_text_size),
          axis.title = element_text(size = my_axistitle_text_size),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank()) +
    labs(x = NULL, y = "Billion USD", caption = wrapped_caption)
  
  # add a frame of two columns
  p3a <- p3 + theme(legend.position = 'none')
  p3b <- get_legend(p3)
  p3 <- plot_grid(p3a, p3b, labels = c('', ''), ncol = 2, rel_widths  = c(0.6,0.4))
  
  
  # - PLOT 4
  
  #IMPORTS
  data_ls[[4]] <- read_xlsx(path = paste0(pathname_tmpl, template_name), progress = readxl_progress(), skip = 1, sheet = "TRADE2")
  
  p4_col_key <- colorRampPalette(c("firebrick", "darkgreen", "orange", "purple", "cadetblue", "blue"))
  p4_colours <- p4_col_key(dim(data_ls[[4]])[1])
  
  caption_tmp <- paste("Source:", source_id %>%
                         filter(TAB == "TRADE2") %>%
                         select(SOURCE))
  wrapped_caption <- str_wrap(caption_tmp, width = caption_width)
  
  p4 <- ggplot(data = data_ls[[4]], mapping = aes(x = "", y = BILLIONS_USD, fill = str_wrap(IMPORTS, n_chars_wrap))) + 
    geom_bar(stat="identity", width=1, colour="white", size = 0.05) +
    theme_minimal() +
    scale_fill_manual(values = p4_colours) +
    theme(plot.title = element_text(hjust = 0.5, color = "#666666", size = my_title_text_size),
          plot.subtitle = element_text(hjust = 0.5, color = "#666666", size = my_subtitle_text_size),
          legend.position = "right",
          legend.title = element_blank(),
          legend.key.size = unit(0.75, "line"), 
          legend.spacing.x = unit(0.1, "cm"),
          legend.text = element_text(size = my_legend_text_size),
          plot.caption = element_text(hjust = 0.5, color = "#666666", size = my_caption_text_size),
          axis.text = element_text(size = my_axis_text_size),
          axis.title = element_text(size = my_axistitle_text_size),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank()) +
    labs(x = "", y = "Billion USD", caption = wrapped_caption)
  
  # add a frame of two columns
  p4a <- p4 + theme(legend.position = 'none')
  p4b <- get_legend(p4)
  p4 <- plot_grid(p4a, p4b, labels = c('', ''), ncol = 2, rel_widths  = c(0.6,0.4))
  
  
  #PLOT 5
  
  #GHG EMISSIONS
  p5_col_key <- data.frame(SECTOR_AGG = c("AFOLU (source)", 
                                          "AFOLU (sink)", 
                                          "agriculture", 
                                          "IPPU", 
                                          "waste", 
                                          "LULUCF", 
                                          "energy", 
                                          "total"),
                           COLOURS  = c("#CCF7AB", 
                                        "#8BFB34", 
                                        "#F8C471", 
                                        "#B29EE1", 
                                        "#633974", 
                                        "#33C863", 
                                        "#C0392B", 
                                        "#FFFFFF"), 
                           stringsAsFactors = FALSE)
  
  p5_colours <- p5_col_key %>% 
    filter(SECTOR_AGG != "total") %>% 
    inner_join(., data_ls[[5]], by = "SECTOR_AGG") %>% 
    arrange(SECTOR_AGG) %>% 
    distinct(COLOURS, .keep_all = TRUE) %>% 
    pull(., COLOURS)
  
  caption_tmp <- paste("Source:", source_id %>%
                         filter(TAB == "GHG1") %>%
                         select(SOURCE))
  wrapped_caption <- str_wrap(caption_tmp, width = caption_width)
  
  p5 <- ggplot(data = data_ls[[5]] %>%
                 filter(SECTOR_AGG != "total"), mapping = aes(x = "", y = MILLION_TONNES_CO2E_PER_YEAR, fill = str_wrap(SECTOR_AGG, n_chars_wrap))) +
    geom_bar(stat = "identity", width=1, colour="white", size = 0.05) + 
    geom_hline(yintercept = 0, colour = "black", size = 0.5) + 
    scale_fill_manual(values = p5_colours) + 
    theme_minimal() +
    theme(axis.line = element_blank(),
          plot.subtitle = element_text(hjust = 0.5, color = "#666666", size = my_subtitle_text_size),
          legend.position = "right",
          legend.title = element_blank(),
          legend.key.size = unit(0.75, "line"), 
          legend.spacing.x = unit(0.1, "cm"),
          legend.text = element_text(size = my_legend_text_size),
          plot.caption = element_text(hjust = 0.5, color = "#666666", size = my_caption_text_size),
          axis.text = element_text(size = my_axis_text_size),
          axis.title = element_text(size = my_axistitle_text_size),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank()) + 
    labs(x = "", y = expression(Million~ tons~ CO[2]~ e~ per~ year), caption = wrapped_caption)
  
  # add a frame of two columns
  p5a <- p5 + theme(legend.position = 'none')
  p5b <- get_legend(p5)
  p5 <- plot_grid(p5a, p5b, labels = c('', ''), ncol = 2, rel_widths  = c(0.6,0.4))
  

  
  #PLOT 6
  p6_col_key <- data.frame(CLASS_AGG = c("crops", 
                                         "forest (sink)", 
                                         "forest (source)",
                                         "land (sink)", 
                                         "land (source)",  
                                         "livestock", 
                                         "peatland fire", 
                                         "peatland decay", 
                                         "residue burning", 
                                         "total", 
                                         "urban"),
                           COLOURS  = c("#FFD700", 
                                        "#006400", 
                                        "#228B22", 
                                        "#16A085", 
                                        "#FAD7A0", 
                                        "#F1948A", 
                                        "#6E1200", 
                                        "#624D16", 
                                        "#154360", 
                                        "#FFFFFF", 
                                        "#6120DC"), 
                           stringsAsFactors = FALSE)
  
  p6_colours <- p6_col_key %>% 
    filter(CLASS_AGG != "total") %>% 
    inner_join(., data_ls[[6]], by = "CLASS_AGG") %>% 
    arrange(CLASS_AGG) %>% 
    distinct(COLOURS, .keep_all = TRUE) %>% 
    pull(., COLOURS)
  
  caption_tmp <- paste("Source:", source_id %>%
                         filter(TAB == "GHG2") %>%
                         select(SOURCE))
  wrapped_caption <- str_wrap(caption_tmp, width = caption_width)
  
  p6 <- ggplot(data = data_ls[[6]] %>% filter(CLASS_AGG != "total"), mapping = aes(x = "", y = MILLION_TONNES_CO2e_PER_YEAR, fill = str_wrap(CLASS_AGG, n_chars_wrap))) +
    geom_bar(stat = "identity", width=1, colour="white", size = 0.05) +
    geom_hline(yintercept = 0, size = 0.25) +
    scale_fill_manual(values = p6_colours) +
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
          axis.title = element_text(size = my_axistitle_text_size),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank()) +
    labs(x = "", y = expression(Million~ tons~ CO[2]~ e), caption = wrapped_caption)
  # add a frame of two columns
  p6a <- p6 + theme(legend.position = 'none')
  p6b <- get_legend(p6)
  p6 <- plot_grid(p6a, p6b, labels = c('', ''), ncol = 2, rel_widths  = c(0.6,0.4))

  
  
  #PLOT 7
  #FOOD
  p7_col_key <- data.frame(CLASS_AGG = c("animal products", 
                                         "cereals", 
                                         "eggs", 
                                         "fish", 
                                         "fruits & veg", 
                                         "meat", 
                                         "milk", 
                                         "monogastric meat", 
                                         "oil & fat", 
                                         "other", 
                                         "pulses", 
                                         "red meat", 
                                         "roots & tubers", 
                                         "sugar", 
                                         "total"),
                           COLOURS  = c("#FADBD8", 
                                        "#FFD700", 
                                        "#A9C3C0", 
                                        "#1E90FF", 
                                        "#239B56", 
                                        "#F08080", 
                                        "#D3D3D3", 
                                        "#F08080", 
                                        "#FF8C00", 
                                        "#CD853F", 
                                        "#8B4513", 
                                        "#CD5C5C", 
                                        "#BA55D3", 
                                        "#00008B", 
                                        "#CBCBC8"), 
                           stringsAsFactors = FALSE)
  
  p7_colours <- p7_col_key %>% 
    filter(CLASS_AGG != "total") %>% 
    inner_join(., data_ls[[7]], by = "CLASS_AGG") %>% 
    distinct(., CLASS_AGG, .keep_all = TRUE) %>% 
    arrange(CLASS_AGG) %>% 
    pull(., COLOURS)
  
  caption_tmp <- paste("Source:", source_id %>%
                         filter(TAB == "FOOD1") %>%
                         select(SOURCE))
  wrapped_caption <- str_wrap(caption_tmp, width = caption_width)
  
  p7 <- ggplot(data = data_ls[[7]] %>% filter(CLASS_AGG != "total"), mapping = aes(x = "", y = KCAL, fill = str_wrap(CLASS_AGG, n_chars_wrap))) +
    geom_bar(stat="identity", width=1) +
    geom_hline(yintercept = 0, size = 0.5) +
    theme_minimal() +
    scale_fill_manual(values = p7_colours) +
    theme(plot.title = element_text(hjust = 0.5, color = "#666666", size = my_title_text_size_2),
          plot.subtitle = element_text(hjust = 0.5, color = "#666666", size = my_subtitle_text_size),
          legend.position = "right",
          legend.title = element_blank(),
          legend.key.size = unit(0.75, "line"), 
          legend.spacing.x = unit(0.1, "cm"),
          legend.text = element_text(size = my_legend_text_size),
          plot.caption = element_text(hjust = 0.5, color = "#666666", size = my_caption_text_size),
          axis.text = element_text(size = my_axis_text_size),
          axis.title = element_text(size = my_axistitle_text_size),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank()) +
    labs(x = "", y = "kcal per capita per day", title = paste("Daily average:", round(as.numeric(data_ls[[7]] %>% filter(CLASS_AGG == "total") %>% select(KCAL))), "kcal"), caption = wrapped_caption)
  
  # add a frame of two columns
  p7a <- p7 + theme(legend.position = 'none')
  p7b <- get_legend(p7)
  p7 <- plot_grid(p7a, p7b, labels = c('', ''), ncol = 2, rel_widths  = c(0.6,0.4))
  
  
  #PLOT 8
  
  #TRADE ADJUSTMENTS
  #These plots show the impacts of trade adjustments on exports and land use
  #Impacts on exports
  p8_col_key <- data.frame(CLASS = data_ls[[8]] %>%
                             filter(TRADE_ADJUSTMENT == "NO") %>%
                             select(PRODUCT),
                           COLOR = data_ls[[8]] %>%
                             filter(TRADE_ADJUSTMENT == "NO") %>%
                             select(COLOR),
                           stringsAsFactors = FALSE)
  p8_colours <- p8_col_key$COLOR[order(p8_col_key$PRODUCT)]
  
  p8 <- ggplot(data = data_ls[[8]], aes(x = YEAR)) +
    geom_line(data = data_ls[[8]], aes(x = YEAR, y = THOUSAND_TONS/1000, color = PRODUCT, linetype = TRADE_ADJUSTMENT), size = 0.7, alpha = 0.9) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, color = "#666666", size = my_title_text_size_2),
          plot.subtitle = element_text(hjust = 0.5, color = "#666666", size = my_subtitle_text_size),
          legend.position = "bottom",
          legend.title = element_text(size = my_legendtitle_text_size),
          legend.key.size = unit(1.5, "line"), 
          legend.spacing.x = unit(0.1, "cm"),
          legend.text = element_text(size = my_legend_text_size_2),
          legend.box = "horizontal",
          plot.caption = element_text(hjust = 0.5, color = "#666666", size = my_caption_text_size),
          axis.text = element_text(size = my_axis_text_size),
          axis.title = element_text(size = my_axistitle_text_size),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank()) +
    labs(x = "Year", y = "Million tons", colour = "product", linetype = "adjusted trade") +
    scale_colour_discrete(guide = guide_legend(title.position = "top", title.hjust = 0.5)) +
    scale_linetype_manual(values = c("NO" = "dashed", "YES" = "solid"),
                          guide = guide_legend(title.position = "top", title.hjust = 0.5))

  
  # PLOT 9
  #Impacts on land use
  p9_col_key <- data.frame(CLASS = data_ls[[9]] %>%
                             filter(TRADE_ADJUSTMENT == "NO") %>%
                             select(LAND_COVER_CLASS),
                           COLOR = data_ls[[9]] %>%
                             filter(TRADE_ADJUSTMENT == "NO") %>%
                             select(COLOR),
                           stringsAsFactors = FALSE)
  p9_colours <- p9_col_key$COLOR[order(p9_col_key$LAND_COVER_CLASS)]
  
  p9 <- ggplot(data = data_ls[[9]], aes(x = YEAR)) +
    geom_line(data = data_ls[[9]] %>% filter(TRADE_ADJUSTMENT == "NO"), aes(x = YEAR, y = THOUSAND_HECTARES/1000, color = LAND_COVER_CLASS, linetype = TRADE_ADJUSTMENT), size = 0.7, alpha = 0.9) +
    geom_line(data = data_ls[[9]] %>% filter(TRADE_ADJUSTMENT == "YES"), aes(x = YEAR, y = THOUSAND_HECTARES/1000, color = LAND_COVER_CLASS, linetype = TRADE_ADJUSTMENT), size = 0.7, alpha = 0.9) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, color = "#666666", size = my_title_text_size_2),
          plot.subtitle = element_text(hjust = 0.5, color = "#666666", size = my_subtitle_text_size),
          legend.position = "bottom",
          legend.title = element_text(size = my_legendtitle_text_size),
          legend.key.size = unit(1.5, "line"), 
          legend.spacing.x = unit(0.1, "cm"),
          legend.text = element_text(size = my_legend_text_size_2),
          legend.box = "horizontal",
          plot.caption = element_text(hjust = 0.5, color = "#666666", size = my_caption_text_size),
          axis.text = element_text(size = my_axis_text_size),
          axis.title = element_text(size = my_axistitle_text_size),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank()) +
    labs(x = "Year", y = "Million hectares", colour = "land cover type", linetype = "adjusted trade") +
    scale_colour_discrete(guide = guide_legend(title.position = "top", title.hjust = 0.5)) +
    scale_linetype_manual(values = c("NO" = "dashed", "YES" = "solid"),
                          guide = guide_legend(title.position = "top", title.hjust = 0.5))
  
  #Save plots
  
  ggsave(filename = paste0(country_alpha, "_LAND_FOOD_SYS_Fig1.pdf"), plot = p1, device = "pdf", path = pathname_outp, scale = 1, width = 9, height = 9, units = "cm", dpi = 320, limitsize = TRUE)
  ggsave(filename = paste0(country_alpha, "_LAND_FOOD_SYS_Fig2.pdf"), plot = p2, device = "pdf", path = pathname_outp, scale = 1, width = 9, height = 9, units = "cm", dpi = 320, limitsize = TRUE)
  ggsave(filename = paste0(country_alpha, "_LAND_FOOD_SYS_Fig3.pdf"), plot = p3, device = "pdf", path = pathname_outp, scale = 1, width = 9, height = 9, units = "cm", dpi = 320, limitsize = TRUE)
  ggsave(filename = paste0(country_alpha, "_LAND_FOOD_SYS_Fig4.pdf"), plot = p4, device = "pdf", path = pathname_outp, scale = 1, width = 9, height = 9, units = "cm", dpi = 320, limitsize = TRUE)
  ggsave(filename = paste0(country_alpha, "_LAND_FOOD_SYS_Fig5.pdf"), plot = p5, device = "pdf", path = pathname_outp, scale = 1, width = 9, height = 9, units = "cm", dpi = 320, limitsize = TRUE)
  ggsave(filename = paste0(country_alpha, "_LAND_FOOD_SYS_Fig6.pdf"), plot = p6, device = "pdf", path = pathname_outp, scale = 1, width = 9, height = 9, units = "cm", dpi = 320, limitsize = TRUE)
  ggsave(filename = paste0(country_alpha, "_LAND_FOOD_SYS_Fig7.pdf"), plot = p7, device = "pdf", path = pathname_outp, scale = 1, width = 9, height = 9, units = "cm", dpi = 320, limitsize = TRUE)
  ggsave(filename = paste0(country_alpha, "_TRADE_ADJ_Fig1.pdf"), plot = p8, device = "pdf", path = pathname_outp, scale = 1, width = 14, height = 10, units = "cm", dpi = 320, limitsize = TRUE)
  ggsave(filename = paste0(country_alpha, "_TRADE_ADJ_Fig2.pdf"), plot = p9, device = "pdf", path = pathname_outp, scale = 1, width = 14, height = 10, units = "cm", dpi = 320, limitsize = TRUE)
  
  unlink(c(paste0(country_alpha, "_LAND_FOOD_SYS_Fig1.pdf"), 
           paste0(country_alpha, "_LAND_FOOD_SYS_Fig2.pdf"), 
           paste0(country_alpha, "_LAND_FOOD_SYS_Fig3.pdf"), 
           paste0(country_alpha, "_LAND_FOOD_SYS_Fig4.pdf"), 
           paste0(country_alpha, "_LAND_FOOD_SYS_Fig5.pdf"), 
           paste0(country_alpha, "_LAND_FOOD_SYS_Fig6.pdf"), 
           paste0(country_alpha, "_LAND_FOOD_SYS_Fig7.pdf"), 
           paste0(country_alpha, "_TRADE_ADJ_Fig1.pdf"), 
           paste0(country_alpha, "_TRADE_ADJ_Fig2.pdf")))
  rm(p1, p2, p3, p4, p5a, p5b, p6, p7, p8, p9)
  
}
