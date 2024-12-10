README.

The file "error_rsrf_var.R" was used instead of the file "error_rsrf.R" for "california housing (HD)", "robot (HD)" (only for RSRF (af)) and for "abalone (HD)" (only for W=30 and mtry=58). 
RSRF is not optimize for computational speed, and the code provided here will parallize over the trees.
Running "error_rsrf_var.R" will produce the results for a single fold (a fold ID, e.g. 1,2,3,... needs to be supplied when running the R file). The folds used are supplied in folder "folds".
After running these separately for each fold and saving the result files of the form "cv_data_id_abalone_all_hd_*_fold-no_1.RDS" in a single folder, the helper file called "aggregate_single" can be used to combine the different results into a single .RDS file, similar to the one being output by "error_rsrf.R".

### INFORMATION ON SEEDS USED FOR THE SIMULATIONS ###
The seeds can be supplied with the different simulation files. For the results that were used for the plots in the paper, the following seeds were set for the different data examples:
california_housing_div_hd/robot_hd: 611 (fold 1), ..., 615 (fold 5) for W=15, respectively 711,...,715 for W=30, respectively 811,...,815 for W=45 
abalone_all_hd: 111, ..., 119, 1110
