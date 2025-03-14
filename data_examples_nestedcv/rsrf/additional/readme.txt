Only for California Housing (HD) dataset:

- The file "ncv_rsrf_blockwise.R" has been used for the California Housing (HD) dataset.
- The file "manual_grid/grid_chd.R" needs to be available in the same directory as the .R script (as well as the datasets and folds folders)
- For each setting_id, this will run RSRF for a certain outerfold_id, a certain innerfold_id and a certain mtry value.
- Note that, to reproduce all the results, this file needs to be run for any setting_id from 1 to 100.
	- For this the seed was set to 1028 (note that this will set a RNG given the setting_id, internally)
	- see the example below

R CMD BATCH --no-save --no-restore --slave '--args chd_hd 45 rsrf_nf 1028 5 2 15' ncv_rsrf_blockwise.R
Arguments in order:
  #data_id_from_input <- as.character(args[[1]]) #e.g. robot / robot_small
  #width_from_input <- as.integer( args[[2]]) #e.g. 5
  #alg_from_input <- as.character( args[[3]])  #rsrf_af & rsrf_nf
  #seed_from_input <- as.integer( args[[4]] ) #e.g. 123, or "NA"
  #block_size_from_input <- as.integer( args[[5]])
  #round_id_from_input <- as.integer( args[[6]] ) #e.g. 1, or 2"
  #setting_id <- as.integer( args[[7]])

- After running this, the best parameters need to be collected manually
	- In the folder results_inner, these can be found.

- Then one can use the file "run_on_best_rsrf_blockwise.R" (similar to "run_on_best_rsrf.R") to run the method given a outerfold id
	-> runs Train/test on corresponing outer fold, with best parameters taken from the files in "results_inner")
