### converts .RDS files generated trough main_generate_tasks_setups.R (using mlr3) into two lists containing train data and test data.

train_list <- list()
test_list <- list()
#
fileID <- "F6"
fromRDS <- readRDS(paste0(fileID,".RDS"))

for( i in 1: fromRDS$nsims){
  train_list[[i]] <- fromRDS$tasks[[i]]$data()
  test_list[[i]] <- fromRDS$tasks[[i]]$data(rows = fromRDS$tasks[[i]]$row_roles$holdout )
}

saveRDS( train_list, file=paste0(fileID,"_train_list.RDS") )
saveRDS( test_list, file = paste0(fileID,"_test_list.RDS") )
