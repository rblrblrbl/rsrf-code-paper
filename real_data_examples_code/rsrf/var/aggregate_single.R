#Takes different .RDS files generated from "error_rsrf_var.R" (one such file per fold from CV) and aggregates them into a single result file, similar to the ones produced by "error_rsrf.R".
list_RDS_filenames <- list.files(pattern="*.RDS")
list_RDS_objects <- lapply(list_RDS_filenames, readRDS)

#take details from first
first_fold <- list_RDS_objects[[1]]


names_new <- names(first_fold)[!(names(first_fold) %in% c("cv_error","seed","folds","fold_number") )]
results_all_folds <- first_fold[names_new]
cv_errors <- sapply(1:length(list_RDS_objects), function(j) return( list_RDS_objects[[j]]$cv_error$mse) )
seeds_per_fold <- sapply(1:length(list_RDS_objects), function(j) return( list_RDS_objects[[j]]$seed) )

results_all_folds$cv_errors <- cv_errors
results_all_folds$seeds_per_fold <- seeds_per_fold
results_all_folds$mean_error <- mean(results_all_folds$cv_errors)
results_all_folds$sd <- sd(results_all_folds$cv_errors)
names( results_all_folds )

fname <- paste0("rsrf_abalone_hd", "_", paste(sep="_",first_fold$algorithm, paste0("w",first_fold$width), paste0("m", first_fold$mtry)) )
saveRDS(results_all_folds, paste0(fname,".RDS"))
