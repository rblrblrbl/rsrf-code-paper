1) For the datasets concrete (+HD), airfoil (+HD), abalone (+HD), california housing, the files listed below have been used for running RSRF with nested CV on the real data examples.
2) For california housing (HD) the additional files provided in folder "additional" have been used.

### ncv_rsrf_nf.R ###
This will run nested CV for rsrf_nf resp. rsrf_nf_norep.
Please check the necessary settings to behanded over as args to R, see also the example below.
Note that for the width, in the paper, we used 30 for all datasets apart from California Housing (HD) where we chose 45.

Example: 
R CMD BATCH --no-save --no-restore --slave '--args airfoil FALSE 30 rsrf_nf 4 TRUE 874 2' ncv_rsrfnf.R

### ncv_rsrf_af.R ###
This will run nested CV for rsrf_af resp. rsrf_af_norep.
Please check the necessary settings to behanded over as args to R, see also the example below.
Note that for the width, in the paper, we used 30 for all datasets apart from California Housing (HD) where we chose 45.

Example:
R CMD BATCH --no-save --no-restore --slave '--args airfoil FALSE 30 rsrf_af 4 TRUE 329 2' nested_cv_rsrfaf.R

### run_on_best_rsrf.R ###
This file can be used, if one wants to run RSRF (for a given outer fold), given the best tuning parameters found on corrseponding inner fold, by supplying the outputfiles of the form "best_param_robot_rsrf_nf_w30_outerfold4_round2_with_folds.RDS".
These folds have to be stored in a directory of the structure "results_inner/robot/rsrf_nf" (lying in the same directory as the run_on_best_rsrf.R file")