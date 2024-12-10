#Running this file will generate the plots shown in:
# - Figure 2 (b) from the main text (airfoil)
# - Figure E.9 (b) from the appendix (airfoil)
# Required package "ggplot2"

# ggplot2 version used: ggplot2_3.5.1
# R version used: R version 4.4.0

library(ggplot2)

plot_title <- "Airfoil"
shortcut_for_filename <- "airfoil"

### LOAD RESULT FILES ###
source("util_convert.R")

#Rsrf results
ldf_rsrf <- get_df_seperate( "cv_rsrf" )
#Read all results saved in other folders
df_rf <- get_df("cv_rf", setwd_after = ".." )
df_intf <- get_df("cv_intf_noreplace", setwd_after = ".." )
df_intf_replace <- get_df("cv_intf_replace", setwd_after = ".." )
df_intf_replace$algorithm <- "intf_replace" #rename from intf-replace
df_et_replace <- get_df("cv_et_replace", setwd_after = ".." )
df_et_norep_sf1 <- get_df("cv_et_norep_sf1", setwd_after = ".." )

#source file to create boxplots
source("boxplot_paper_gen.R")



