### ncv_intf.R ###
This will run nested CV for ncv_intf resp. intf_norep.
Please check the necessary settings to be handed over as args to R, see also the example below. For the seeds used, check the file details_intf.txt.

Example: 
R CMD BATCH --no-save --no-restore --slave '--args airfoil intf 500 2663 TRUE 4 round2' ncv_intf.R

Remark: For california housing, the inner cv has been run separately for npairs 1,10,25,50,...,250 and for npairs 300,400,...,700. The results have been combined manually.
For the second, use the file "ncv_intf_chd_300-700.R" in the folder "additional"

### run_on_best_intf.R ###
- This file can be used, if one wants to run INTF (for a given outer fold), given the best tuning parameters found on corrseponding inner fold, by supplying the outputfiles of the form "best_param_chd_intf_o4_round1". These folds have to be stored in a directory of the structure "results_inner/chd/intf" (lying in the same directory as the run_on_best_intf.R file").
- This is useful, if the file ncv_intf.R is run with the fifth argument of args set to FALSE. Then, only inner CV is performed and the best performing parameters are stored, but the method is not run on the outer fold.
	- In the paper, this has been done for chd/chd_hd. An example of the best parameter output from inner-CV is provided in "results_inner/chd".  
    - Note that, for all other datasets, we have not used the file "run_on_best_intf.R", but used "ncv_intf.R" (with the fifth argument of args set to TRUE)