CV CODE for Interaction Forests. 

- R Session Info, see below.

Example: For model F6 (pure-3)

1) Run file "run_intf_cv_F6.R" (seperately for each cv_sim_index to 1, 2, 3,... 5 (set these manually))
2) Once this is done, files like "result_F6_intf_cv_1_10.RDS" will be in results (note that results_F6_paper contains the files obtained from the simulation for the paper)
3) Then, you can run "results_generate_output.R" which will calculate mse error and standard deviation based on the simulation results both for the results used in paper (the ones in folder "results_F6_paper" and the newly generated one in "results, shown in console output. 

For all other models: See folder "others" and use file "run_intf_cv_MODEL.R" therein to produce *.RDS files -> These will be saved in "INTF_CV/results". Using the file "results_genereate_output_other.R" simply extracts mean error and sd from ALL (!) *.RDS files found in INTF_CV/results. We note that, for dimension d=10, the results from the paper for Interaction Forests (CV) are not reproducible.



--------------------------------------------------------------------



R Session Info

R version 4.1.2 (2021-11-01)
Platform: x86_64-pc-linux-gnu (64-bit)
Running under: Red Hat Enterprise Linux 8.6 (Ootpa)

attached base packages:
[1] parallel  stats     graphics  grDevices utils     datasets  methods
[8] base

other attached packages:
[1] doParallel_1.0.16 iterators_1.0.13  doRNG_1.8.6       rngtools_1.5.2
[5] foreach_1.5.1

loaded via a namespace (and not attached):
[1] compiler_4.1.2   codetools_0.2-18 digest_0.6.31



 Info when loading "diversityForest" (for INTF)
 
 [1] stats     graphics  grDevices utils     datasets  methods   base

other attached packages:
[1] diversityForest_0.3.4

loaded via a namespace (and not attached):
 [1] Rcpp_1.0.9       magrittr_2.0.3   ggpubr_0.5.0     tidyselect_1.2.0
 [5] munsell_0.5.0    colorspace_2.1-0 lattice_0.20-45  R6_2.5.1
 [9] rlang_1.0.6      rstatix_0.7.2    carData_3.0-5    fansi_1.0.3
[13] car_3.1-1        dplyr_1.1.0      grid_4.1.2       broom_1.0.3
[17] gtable_0.3.1     utf8_1.2.2       cli_3.4.1        abind_1.4-5
[21] tibble_3.1.8     lifecycle_1.0.3  ggsignif_0.6.4   Matrix_1.5-3
[25] tidyr_1.3.0      purrr_1.0.1      ggplot2_3.4.0    vctrs_0.5.2
[29] glue_1.6.2       compiler_4.1.2   pillar_1.8.1     backports_1.4.1
[33] generics_0.1.3   scales_1.2.1     pkgconfig_2.0.3