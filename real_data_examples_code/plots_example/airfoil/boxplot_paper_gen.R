#This file is used to generate the boxplots as in the paper.
#To do so for the airfoil datasets, run the file "boxplots_example_airfoil.R"

#Min result for RF
df_rf$key <- paste0("mtry\n",df_rf$mtry)
min_mean_error <- df_rf[which.min(df_rf$cv_error_mean),]$cv_error_mean
df_rf_min <- subset(df_rf, cv_error_mean == min_mean_error)

#Min result for INTF (no replace)
df_intf$key <- paste0("intf_npairs: ",df_intf$npairs)
min_mean_error <- df_intf[which.min(df_intf$cv_error_mean),]$cv_error_mean
df_intf_min <- subset(df_intf, cv_error_mean == min_mean_error)

#Min result for INTF (replace)
df_intf_replace$key <- paste0("intf_rep_500_npairs: ",df_intf_replace$npairs)
min_mean_error <- df_intf_replace[which.min(df_intf_replace$cv_error_mean),]$cv_error_mean
df_intf_replace_min <- subset(df_intf_replace, cv_error_mean == min_mean_error)

#Min result for ET (replace)
df_et_replace$key <- paste0("rep_nrs",df_et_replace$num.random.splits,"_mtry", df_et_replace$mtry)
min_mean_error <- df_et_replace[which.min(df_et_replace$cv_error_mean),]$cv_error_mean
df_et_replace_min <- subset(df_et_replace, cv_error_mean == min_mean_error)

#Min result for ET (noreplace and sample.fraction = 1)
df_et_norep_sf1$key <- paste0("norep_sf1_nrs",df_et_norep_sf1$num.random.splits,"_mtry", df_et_norep_sf1$mtry)
min_mean_error <- df_et_norep_sf1[which.min(df_et_norep_sf1$cv_error_mean),]$cv_error_mean
df_et_norep_sf1_min <- subset(df_et_norep_sf1, cv_error_mean == min_mean_error)

#Min results for RSRF
#Set index for each RSRF result
for( index in 1:length(ldf_rsrf)){
  ldf_rsrf[[index]]$key <- paste0("index", index)
}
df_rsrf <- do.call( rbind, ldf_rsrf)

#Min results for RSRF versions
#helper function
extract_min <- function(df_temp){
  min_mean_error <- df_temp[which.min(df_temp$cv_error_mean),]$cv_error_mean
  return( subset(df_temp, cv_error_mean == min_mean_error) )
}

# Set keys for each simulation run
# create new dataframe containing the min results
ldf_rsrf_min <- list()
k <- 1
for(width_value in unique(df_rsrf$width)){
  df_rsrf_wselect <- subset(df_rsrf, width==width_value)
  for( alg in unique(df_rsrf_wselect$algorithm)){
    df_temp <- extract_min(subset( df_rsrf_wselect, algorithm == alg ) )
    df_temp$key <- k
    ldf_rsrf_min[[k]] <- df_temp
    k <- k+1
  }
}
print( paste("Collected",k-1," min-results for RSRF."))
df_min_rsrf <- do.call( rbind, ldf_rsrf_min)
df_min_rsrf <- df_min_rsrf[order(df_min_rsrf$width), ]

### MAKE LABEL DICTIONARIES ###
#INTF
label_dict_intf <- paste0( "npairs\n", df_intf$npairs) #set label names for x axis ticks in plot
names(label_dict_intf) <- df_intf$key #use key as identifier for label name
label_dict_intf <- label_dict_intf[!duplicated(names(label_dict_intf))] #make unique
#INTF REPLACE 500
label_dict_intf_replace <- paste0( "npairs\n", df_intf_replace$npairs) #set label names for x axis ticks in plot
names(label_dict_intf_replace) <- df_intf_replace$key #use key as identifier for label name
label_dict_intf_replace <- label_dict_intf_replace[!duplicated(names(label_dict_intf_replace))] #make unique
#ET
label_dict_et_replace <- paste0( "rand: ", df_et_replace$num.random.splits, "\n", "mtry: ",df_et_replace$mtry) #set label names for x axis ticks in plot
names(label_dict_et_replace) <- df_et_replace$key #use key as identifier for label name
label_dict_et_replace <- label_dict_et_replace[!duplicated(names(label_dict_et_replace))] #make unique
#ET
label_dict_et_norep_sf1 <- paste0( "rand: ", df_et_norep_sf1$num.random.splits, "\n", "mtry: ",df_et_norep_sf1$mtry) #set label names for x axis ticks in plot
names(label_dict_et_norep_sf1) <- df_et_norep_sf1$key #use key as identifier for label name
label_dict_et_norep_sf1 <- label_dict_et_norep_sf1[!duplicated(names(label_dict_et_norep_sf1))] #make unique

labels_dict <- c(label_dict_intf,
                 label_dict_intf_replace,
                 label_dict_et_replace,
                 label_dict_et_norep_sf1)

#### PREPARE PLOTTING ####
colourcode <- c("rsrf_af"="lightgreen",
                "rsrf_nf" = "lightblue",
                "rf"="darkred",
                "intf_replace" = "lightyellow",
                "intf" = "lightyellow2",
                "et_replace" = "lightgray",
                "et_norep_sf1" = "darkgray")
#Dictionary for abbreviations used in the paper
labels_paper_dictionary <- c("rsrf_af" = "RSRF (af)",
                             "rsrf_nf" = "RSRF",
                             "rf" = "RF",
                             "intf_replace" = "INTF",
                             "intf" = "INTF (no replace)",
                             "et_replace" = "ET",
                             "et_norep_sf1" = "ET (no replace)")


df_min_rsrf$width_plt <- paste0("width: ", df_min_rsrf$width)

#Labels and titles
title_min <-   ggtitle(plot_title) 
xlabel <- xlab("")
ylabel <- ylab("Test error (CV)")
plot_dir <- "plots"
dir.create(plot_dir)

#for additionally plotting the mean of the cv-results in the boxplot
#using stat_summary from ggplot2
summary_mean_rsrf <- stat_summary(fun = mean, color = "black", position = position_dodge(0.75),
                                  geom = "point", shape = 18, size =3,
                                  show.legend = FALSE)
summary_mean_rf <-  stat_summary(fun = mean, data = df_rf_min, mapping = aes(x=factor(key)), geom = "point", show.legend = FALSE, shape = 18, size = 3, color = "black", position = position_dodge(0.75) ) 

### MAIN PLOT ###
plot_given_subset <- function( subset =  c("rsrf_af","rsrf_nf","intf","et_replace"),
                               width_subset = c("15","30"),
                               name_in_file = NULL,
                               angle_x_text = 45,
                               manual_width_rsrf = NULL,
                               plot_width = 5,
                               plot_height = 5.5,
                               textsize = NULL,
                               additional_directory = NULL,
                               ignore_title = FALSE,
                               ignore_legend = FALSE,
                               legend_label_dictionary = waiver()){
  if(ignore_title){
    title_min <- NULL
  }
  if(is.null(name_in_file)){
    name_in_file <- paste(subset, collapse= "AND")
  }
  if("intf" %in% subset){
    bplot_intf <- geom_boxplot(df_intf_min, mapping = aes(x=factor(key)),outlier.color="black",outlier.size = 1)
    summary_mean_intf <-  stat_summary(fun = mean,
                                     data = df_intf_min,
                                     mapping = aes(x=factor(key)),
                                     geom = "point",
                                     show.legend = FALSE,
                                     shape = 18,
                                     size = 3,
                                     color = "black",
                                     position = position_dodge(0.75) ) 
    
  } else {
    bplot_intf <- NULL
    summary_mean_intf <- NULL
  }
  
  if("intf_replace" %in% subset){
    bplot_intf_replace <- geom_boxplot(df_intf_replace_min, mapping = aes(x=factor(key)), outlier.color="black",outlier.size = 1)
    summary_mean_intf_replace <-  stat_summary(fun = mean,
                                       data = df_intf_replace_min,
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
  
  if("et_replace" %in% subset){
    bplot_et_replace <- geom_boxplot(df_et_replace_min, mapping = aes(x=factor(key) ), outlier.color="black",outlier.size = 1 )
    summary_mean_et_replace <-  stat_summary(fun = mean,
                                          data = df_et_replace_min,
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
  
  if("et_norep_sf1" %in% subset){
    bplot_et_norep_sf1 <- geom_boxplot(df_et_norep_sf1_min, mapping = aes(x=factor(key) ),outlier.color="black",outlier.size = 1 )
    summary_mean_et_norep_sf1 <-  stat_summary(fun = mean,
                                               data = df_et_norep_sf1_min,
                                               mapping = aes(x=factor(key)),
                                               geom = "point",
                                               show.legend = FALSE,
                                               shape = 18,
                                               size = 3,
                                               color = "black",
                                               position = position_dodge(0.75) ) 
    
  } else {
    bplot_et_norep_sf1 <- NULL
    summary_mean_et_norep_sf1 <- NULL
  }
  
  ##PLOT
  plot_min_subset <- ggplot(data = df_min_rsrf[(df_min_rsrf$algorithm %in% subset) & (df_min_rsrf$width %in% width_subset), ],
                            mapping = aes(x= factor(width),
                            y= cv_error,
                            fill=algorithm,
                            group=key) ) + 
    geom_boxplot(width=manual_width_rsrf, outlier.color="black", outlier.size = 1) +
    geom_boxplot(df_rf_min, mapping = aes(x=factor(key)), outlier.color="black", outlier.size = 1 ) +
    bplot_intf_replace +
    bplot_intf + 
    bplot_et_replace +
    bplot_et_norep_sf1 + 
    scale_fill_manual(values=colourcode, labels= legend_label_dictionary) +
    title_min +
    xlabel +
    ylabel + 
    scale_x_discrete(labels=labels_dict) +
    theme(axis.text.x = element_text(angle=angle_x_text,
          face=c(rep("bold",length(width_subset)),rep("plain",10) ) ),
          text = element_text(size = textsize) )
  

  #PLOT
  plot_min_subset_mean <- plot_min_subset + 
    summary_mean_rsrf + 
    summary_mean_rf + 
    summary_mean_intf + 
    summary_mean_intf_replace +
    summary_mean_et_replace +
    summary_mean_et_norep_sf1

  if(ignore_legend){
    plot_min_subset_mean <- plot_min_subset_mean +  theme(legend.position="none")
  }
  #save plots into plot directory
  ggsave( paste0(plot_dir,"/plot_",name_in_file, ".png"), plot_min_subset_mean, width = plot_width, height = plot_height) 
  
  ### END: MAIN PLOT ###
  cat( "---------------------------------  \n\n",
       "MAIN PLOTS GENERATED. SAVED PLOTS.\n",
       "Plot directory: ", plot_dir,"\n",
       "Plot name:      ", name_in_file,"\n\n",
       "Which plots:    ", paste(subset,collapse=", "),"\n\n",
       "---------------------------------  \n\n\n")
  return(plot_min_subset_mean)
}  
  
### GENERATE PLOTS SHOWN IN PAPER ###
labels_legend <- labels_paper_dictionary #use dictionary set above
textsize <- 15
ignore_title <- TRUE

if(shortcut_for_filename == "chd" || shortcut_for_filename == "chd_hd"){
  #for california housing
  width_subset_paper1 <- c(45)
  width_subset_paper2 <- c(30,45)
} else{
  #for all others
  width_subset_paper1 <- c(30)    #only width 30 plotted in main text
  width_subset_paper2 <- c(15,30) #both width values plotted in appendix
}

subset_small <- c("rsrf_nf", "intf_replace", "et_replace") #main text
subset_medium <- c("rsrf_nf","rsrf_af", "intf", "intf_replace", "et_replace", "et_norep_sf1") #appendix

## PLOT MAIN TEXT
plot_given_subset(subset = subset_small,
                  name_in_file = paste0(shortcut_for_filename,"_paper1"),
                  angle = 0,
                  plot_width = 5.7,
                  plot_height = 5.5,
                  width_subset = width_subset_paper1,
                  manual_width_rsrf = NULL,
                  textsize = textsize,
                  ignore_title = TRUE,
                  ignore_legend = TRUE,
                  additional_directory = NULL,
                  legend_label_dictionary = labels_legend)

## PLOT APPENDIX
plot_given_subset(subset = subset_medium,
                  name_in_file = paste0(shortcut_for_filename,"_paper2"),
                  angle = 45,
                  plot_width = 7.5,
                  plot_height = 5.5,
                  manual_width_rsrf = NULL,
                  width_subset = width_subset_paper2,
                  textsize = textsize,
                  ignore_title = TRUE,
                  ignore_legend = TRUE,
                  additional_directory = NULL,
                  legend_label_dictionary = labels_legend)

### Additionally produce plots with title/legend
plot_given_subset(subset = subset_small,
                  name_in_file = paste0(shortcut_for_filename,"_with_title_legend_1"),
                  angle = 0,
                  plot_width = 5.7,
                  plot_height = 5.5,
                  width_subset = width_subset_paper1,
                  manual_width_rsrf = NULL,
                  textsize = textsize,
                  ignore_title = FALSE,
                  ignore_legend = FALSE,
                  additional_directory = NULL,
                  legend_label_dictionary = labels_legend)

## PLOT APPENDIX
plot_given_subset(subset = subset_medium,
                  name_in_file = paste0(shortcut_for_filename,"_with_title_legend_2"),
                  angle = 45,
                  plot_width = 7.5,
                  plot_height = 5.5,
                  manual_width_rsrf = NULL,
                  width_subset = width_subset_paper2,
                  textsize = textsize,
                  ignore_title = FALSE,
                  ignore_legend = FALSE,
                  additional_directory = NULL,
                  legend_label_dictionary = labels_legend)