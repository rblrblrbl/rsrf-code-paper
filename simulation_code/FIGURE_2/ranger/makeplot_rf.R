### Generates plot only for Random Forests results.
library(ggplot2)
# library(ggnewscale)
#Make sure to set wd to place where folder "ranger_plot" is
setwd("ranger_plot")
filename_mtry1 <- paste0( "simulations_results_ranger_mtry_1.RDS")
results_mtry1 <- readRDS(filename_mtry1)
filename_mtry2 <- paste0( "simulations_results_ranger_mtry_2.RDS")
results_mtry2 <- readRDS(filename_mtry2)
filename_mtry3 <- paste0( "simulations_results_ranger_mtry_3.RDS")
results_mtry3 <- readRDS(filename_mtry3)
filename_mtry4 <- paste0( "simulations_results_ranger_mtry_4.RDS")
results_mtry4 <- readRDS(filename_mtry4)
filename_mtry5 <- paste0( "simulations_results_ranger_mtry_5.RDS")
results_mtry5 <- readRDS(filename_mtry5)
filename_mtry6 <- paste0( "simulations_results_ranger_mtry_6.RDS")
results_mtry6 <- readRDS(filename_mtry6)


results <- cbind( results_mtry1, 
                  results_mtry2,
                  results_mtry3,
                  results_mtry4,
                  results_mtry5,
                  results_mtry6
)


name_plot <- results_mtry1[,1]$reg_function
name_plot <- paste0( name_plot, " with noise ~N(0,1)")

#rf df
results_df <- data.frame( replace =  unlist( results[,1:ncol(results)]["replace",] ),
                          n =  unlist( results[,1:ncol(results)]["n",] ),
                          error_rf =  unlist( results[,1:ncol(results)]["error_rf",]),
                          mtry = as.factor( unlist( results[,1:ncol(results)]["mtry_rf",] ) ) )

break_manual_log <- c(500, 1000, 2500,5000, 10000, 25000, 50000)
labels_manual_log <- c("500", "1000", "2500", "5000", "10000", "25000", "50000")

### PLOT Ranger

### logscale
xlimit <- 50000
# 
# ### Plot logarithmic scale
plt_log <- ggplot(data = results_df, aes( x= n, y = error_rf) ) +
  scale_x_log10(breaks = break_manual_log,
                minor_breaks = seq(1,xlimit, by=1000),
                label = labels_manual_log, limits = c(500,xlimit)) +
  scale_y_log10(limits = c(0.045,0.6))  +
  scale_color_brewer(palette="Dark2") +
  geom_point(size = 2.5, aes(color=mtry)) +
  # new_scale_colour() +
  # scale_colour_grey(start=0.1, end=0.5) +
  # scale_shape_manual(values = c(5,18)) +
  ylab( "mse") +
  # geom_point( data = results_intf_df, aes(x=n, y= error_intf, color = algorithm, shape = algorithm), size = 1.8) +
  # geom_point( data = results_rsrf_df, aes(x=n, y= error_rsrf, color = algorithm, shape = algorithm), size = 2.8) +
   ggtitle(name_plot)
plt_log
ggsave("plot_figure2_only_ranger.png", width = 8, height=6)




