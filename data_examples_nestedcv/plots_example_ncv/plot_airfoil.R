### README BEFORE RUNNING ####
###Modify "util_load_files_airfoil.R" such that:
### - after sourcing "util_load_files_airfoil.R" data frame objects are available with the following names:
#df_rsrf_nf
#df_rsrf_nf_norep
#df_intf
#df_intf_norep
#df_rf_replace
#df_rf_noreplace
#df_et_replace
#df_et_sf1
### - and the following content (10 entries each):
# 1) one olumn data_id (e.g. airfoil)
# 2) one column algorithm (e.g. intf)
# 3) one column error containing the error on the outer folds, for each of the two rounds

library(ggplot2)
data_id <- "airfoil"
plot_title <- "Airfoil"
setwd(data_id)

source(paste0("util_load_files_", data_id, ".R") )

### The following lines will then create the boxplots shown in the paper in Figure 6b) and in Figure E.9a)
setwd("..")
source("boxplot.R")
textsize <- 16
subset_main_text <- c("rsrf_nf","intf_replace", "rf_replace", "et_replace") ## MAIN TEXT
subset_appendix <- c("rsrf_nf_norep","intf_noreplace", "rf_noreplace", "et_sf1") ##APPENDIX

plot_given_subset( subset =  subset_main_text,
                   name_in_file = paste0(data_id,"_main"),
                   plot_title = plot_title,
                   plot_width = 5.7,
                   plot_height = 5.5,
                   textsize = textsize,
                   legend_label_dictionary = labels_legend)

plot_given_subset( subset =  subset_appendix,
                   name_in_file = paste0(data_id,"_appendix"),
                   plot_title = plot_title,
                   plot_width = 5.7,
                   plot_height = 5.5,
                   textsize = textsize,
                   legend_label_dictionary = labels_legend)