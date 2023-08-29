### Generates plot only for Random Forests results.
library(ggplot2)
#Make sure to set wd to place where folder "rsrf_plot" is
setwd("rsrf_plot")

#Get files for plot.
#Get RSRF results
list_RDS_filenames <- list.files(pattern="*.RDS")
list_RDS_objects <- lapply(list_RDS_filenames, readRDS)
results_rsrf <- do.call( cbind, list_RDS_objects)

name_plot <- results_rsrf[,1]$reg_function
name_plot <- paste0( name_plot, " with noise ~N(0,1)")


results_rsrf_df <- data.frame( replace =  unlist( results_rsrf[,1:ncol(results_rsrf)]["replace",] ),
                               n =  unlist( results_rsrf[,1:ncol(results_rsrf)]["n",] ),
                               error_rsrf =  unlist( results_rsrf[,1:ncol(results_rsrf)]["error_rsrf",]),
                               error_train_rsrf = unlist( results_rsrf[,1:ncol(results_rsrf)]["error_train_rsrf",]),
                               mtry_mode = as.factor( unlist( results_rsrf[,1:ncol(results_rsrf)]["mtry_mode",] )),
                               mtry_rsrf = as.factor( unlist( results_rsrf[,1:ncol(results_rsrf)]["mtry_rsrf_step",] )) )
results_rsrf_df["algorithm"] <- "RSRF"

### PLOT
break_manual_log <- c(500, 1000, 2500,5000, 10000, 25000, 50000)
labels_manual_log <- c("500", "1000", "2500", "5000", "10000", "25000", "50000")
xlimit <- 50000
# 
# ### Plot logarithmic scale
plt_log <- ggplot(data = results_rsrf_df, aes( x= n, y = error_rsrf) ) +
  scale_x_log10(breaks = break_manual_log,
                minor_breaks = seq(1,xlimit, by=1000),
                label = labels_manual_log, limits = c(500,xlimit)) +
  scale_y_log10(limits = c(0.045,0.6))  +
  scale_colour_grey(start=0.1, end=0.5) +
  scale_shape_manual(values = c(18)) +
  ylab( "mse") +
  geom_point( data = results_rsrf_df, aes(x=n, y= error_rsrf, color = algorithm, shape = algorithm), size = 2.8) +
  ggtitle(name_plot)
plt_log
ggsave("plot_figure2_only_rsrf.png", width = 8, height=6)




