##set unique key per result data frame (here use simply algorithm value of dataframes)
if(!is.null(df_rsrf_nf)){
  df_rsrf_nf$key <- df_rsrf_nf$algorithm
}

if(!is.null(df_rsrf_nf_norep)){
  df_rsrf_nf_norep$key <- df_rsrf_nf_norep$algorithm
}

if(!is.null(df_rf_noreplace)){
  df_rf_noreplace$key <- df_rf_noreplace$algorithm
}

if(!is.null(df_rf_replace)){
  df_rf_replace$key <- df_rf_replace$algorithm
}

if(!is.null(df_intf_replace)){
  df_intf_replace$key <-df_intf_replace$algorithm
}

if(!is.null(df_intf_noreplace)){
  df_intf_noreplace$key <- df_intf_noreplace$algorithm
}



if(!is.null(df_et_replace)){
  df_et_replace$key <- df_et_replace$algorithm
}


#Min result for ET (noreplace) trees
if(!is.null(df_et_sf1)){
  df_et_sf1$key <- df_et_sf1$algorithm
}

#### PREPARE PLOTTING ####
colourcode <- c("rsrf_nf" = "lightblue",
                "rsrf_nf_norep" = "cornflowerblue",
                "rf"="darkred",
                "rf_norep"="indianred",
                "intf" = "lightyellow",
                "intf_norep" = "lightyellow2",
                "et_replace" = "lightgray",
                "et_sf1" = "darkgray")

#Dictionary for abbreviations used in the paper
labels_legend <- c("rsrf_nf" = "RSRF",
                             "rsrf_nf_norep" = "RSRF (no replace)",
                             "rf" = "RF",
                             "rf_norep" = "RF (no replace)",
                             "intf" = "INTF",
                             "intf_norep" = "INTF (no replace)",
                             "et_replace" = "ET",
                             "et_sf1" = "ET (no replace)")


plot_dir <- "plots"
dir.create(plot_dir)

### MAIN PLOT ###
plot_given_subset <- function( subset =  c("rsrf_nf","intf","et_replace"),
                               plot_title = "Plot",
                               name_in_file = NULL,
                               plot_without_mean = FALSE,
                               plot_width = 5,
                               plot_height = 5.5,
                               textsize = NULL,
                               ignore_title = FALSE,
                               legend_label_dictionary = waiver(),
                               outlier_size = 0.7){
  
  xlabel <- xlab("")
  ylabel <- ylab("Test error (nested CV)")
  
  if(ignore_title){
    title <- NULL
  }else{
    title <- ggtitle(plot_title)
  }
  if(is.null(name_in_file)){
    name_in_file <- paste(subset, collapse= "AND")
  }
  
  
  if("rf_replace" %in% subset & !(is.null(df_rf_replace))){
    bplot_rf_replace <- geom_boxplot(df_rf_replace, mapping = aes(x=factor(key)),outlier.color="black",outlier.size = outlier_size)
    summary_mean_rf_replace <-  stat_summary(fun = mean,
                                       data = df_rf_replace,
                                       mapping = aes(x=factor(key)),
                                       geom = "point",
                                       show.legend = FALSE,
                                       shape = 18,
                                       size = 3,
                                       color = "black",
                                       position = position_dodge(0.75) ) 
  } else {
    bplot_rf_replace <- NULL
    summary_mean_rf_replace <- NULL
  }
  
  
  
  if("rf_noreplace" %in% subset & !(is.null(df_rf_noreplace))){
    bplot_rf_noreplace <- geom_boxplot(df_rf_noreplace, mapping = aes(x=factor(key)),outlier.color="black",outlier.size = outlier_size)
    summary_mean_rf_noreplace <-  stat_summary(fun = mean,
                                             data = df_rf_noreplace,
                                             mapping = aes(x=factor(key)),
                                             geom = "point",
                                             show.legend = FALSE,
                                             shape = 18,
                                             size = 3,
                                             color = "black",
                                             position = position_dodge(0.75) ) 
  } else {
    bplot_rf_noreplace <- NULL
    summary_mean_rf_noreplace <- NULL
  }
  
  
  
  if("rsrf_nf" %in% subset & !(is.null(df_rsrf_nf))){
    bplot_rsrf_nf <- geom_boxplot(df_rsrf_nf, mapping = aes(x=factor(key)),outlier.color="black",outlier.size = outlier_size)
    summary_mean_rsrf_nf <-  stat_summary(fun = mean,
                                             data = df_rsrf_nf,
                                             mapping = aes(x=factor(key), y= error),
                                             geom = "point",
                                             show.legend = FALSE,
                                             shape = 18,
                                             size = 3,
                                             color = "black",
                                             position = position_dodge(0.75) ) 
  } else {
    bplot_rsrf_nf <- NULL
    summary_mean_rsrf_nf <- NULL
  }
  
  
  
  if("rsrf_nf_norep" %in% subset & !(is.null(df_rsrf_nf_norep))){
    bplot_rsrf_nf_norep <- geom_boxplot(df_rsrf_nf_norep, mapping = aes(x=factor(key)),outlier.color="black",outlier.size = outlier_size)
    summary_mean_rsrf_nf_norep <-  stat_summary(fun = mean,
                                          data = df_rsrf_nf_norep,
                                          mapping = aes(x=factor(key), y= error),
                                          geom = "point",
                                          show.legend = FALSE,
                                          shape = 18,
                                          size = 3,
                                          color = "black",
                                          position = position_dodge(0.75) ) 
  } else {
    bplot_rsrf_nf_norep <- NULL
    summary_mean_rsrf_nf_norep <- NULL
  }
  
  
  if("intf_noreplace" %in% subset & !(is.null(df_intf_noreplace))){
    bplot_intf_noreplace <- geom_boxplot(df_intf_noreplace, mapping = aes(x=factor(key)),outlier.color="black",outlier.size = outlier_size)
    summary_mean_intf_noreplace <-  stat_summary(fun = mean,
                                       data = df_intf_noreplace,
                                       mapping = aes(x=factor(key)),
                                       geom = "point",
                                       show.legend = FALSE,
                                       shape = 18,
                                       size = 3,
                                       color = "black",
                                       position = position_dodge(0.75) ) 
    
  } else {
    bplot_intf_noreplace <- NULL
    summary_mean_intf_noreplace <- NULL
  }
  
  if("intf_replace" %in% subset & !(is.null(df_intf_replace))){
    bplot_intf_replace <- geom_boxplot(df_intf_replace, mapping = aes(x=factor(key)), outlier.color="black",outlier.size = outlier_size)
    summary_mean_intf_replace <-  stat_summary(fun = mean,
                                               data = df_intf_replace,
                                               mapping = aes(x=factor(key)),
                                               geom = "point",
                                               show.legend = FALSE,
                                               shape = 18,
                                               size = 3,
                                               color = "black",
                                               position = position_dodge(0.75) ) 
  } else {
    bplot_intf_replace <- NULL
    summary_mean_intf_replace <- NULL
  }
  
  
  if("et_replace" %in% subset & !(is.null(df_et_replace))){
    bplot_et_replace <- geom_boxplot(df_et_replace, mapping = aes(x=factor(key) ), outlier.color="black",outlier.size = outlier_size )
    summary_mean_et_replace <-  stat_summary(fun = mean,
                                             data = df_et_replace,
                                             mapping = aes(x=factor(key)),
                                             geom = "point",
                                             show.legend = FALSE,
                                             shape = 18,
                                             size = 3,
                                             color = "black",
                                             position = position_dodge(0.75) ) 
  
  } else {
    bplot_et_replace <- NULL
    summary_mean_et_replace <- NULL
  }
  
  if("et_sf1" %in% subset & !(is.null(df_et_sf1))){
    bplot_et_sf1 <- geom_boxplot(df_et_sf1, mapping = aes(x=factor(key) ),outlier.color="black",outlier.size = outlier_size )
    summary_mean_et_sf1 <-  stat_summary(fun = mean,
                                               data = df_et_sf1,
                                               mapping = aes(x=factor(key)),
                                               geom = "point",
                                               show.legend = FALSE,
                                               shape = 18,
                                               size = 3,
                                               color = "black",
                                               position = position_dodge(0.75) )
  } else {
    bplot_et_sf1 <- NULL
    summary_mean_et_sf1 <- NULL
  }
  
  plot_min_subset <- ggplot(  mapping=aes(y=error,fill=algorithm)) +
    bplot_rsrf_nf +
    bplot_rsrf_nf_norep +
    bplot_rf_replace +
    bplot_rf_noreplace +
    bplot_intf_replace +
    bplot_intf_noreplace + 
    bplot_et_replace +
    bplot_et_sf1 + 
    scale_fill_manual(values=colourcode, labels= legend_label_dictionary) +
    title +
    xlabel +
    ylabel + 
    scale_x_discrete(labels=NULL) +
    theme( text= element_text(size= textsize))
  
  #PLOT
  plot_min_subset_mean <- plot_min_subset + 
    summary_mean_rsrf_nf + 
    summary_mean_rsrf_nf_norep + 
    summary_mean_rf_replace + 
    summary_mean_rf_noreplace + 
    summary_mean_intf_noreplace + 
    summary_mean_intf_replace +
    summary_mean_et_replace +
    summary_mean_et_sf1
  ggsave( paste0(plot_dir,"/plot_",name_in_file, ".png"), plot_min_subset_mean, width = plot_width, height = plot_height) 
  
  ### END: MAIN PLOT ###
  print(paste0("MAIN PLOTS FINISHED. Plotted for: ", paste(subset,collapse=", ")) )
  
  cat( "---------------------------------  \n\n",
       "MAIN PLOTS FINISHED. SAVED PLOTS.\n",
       "Plot directory: ", plot_dir,"\n",
       "Plot name:      ", name_in_file,"\n\n",
       "Which plots:    ", paste(subset,collapse=", "),"\n\n",
       "---------------------------------  \n\n\n")
  return(plot_min_subset_mean)
}  
