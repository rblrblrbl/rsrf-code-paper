df_rsrf_nf_r1 <- readRDS(file=paste0(data_id,"_rsrf_nf_round1.RDS"))
df_rsrf_nf_r2 <- readRDS(file=paste0(data_id,"_rsrf_nf_round2.RDS"))
df_rsrf_nf_r1$round_id <- "round: 1" #make sure both loaded files have same columns
df_rsrf_nf <- rbind(df_rsrf_nf_r1,df_rsrf_nf_r2)

df_rsrf_nf_norep_r1 <- readRDS(file=paste0(data_id,"_rsrf_nf_norep_round1.RDS"))
df_rsrf_nf_norep_r2 <- readRDS(file=paste0(data_id,"_rsrf_nf_norep_round2.RDS"))
df_rsrf_nf_norep <- rbind(df_rsrf_nf_norep_r1,df_rsrf_nf_norep_r2)

df_rf_replace1 <- readRDS(file=paste0("nested_cv_",data_id, "_rf_round1_df.RDS"))
df_rf_replace2 <- readRDS(file=paste0("nested_cv_",data_id, "_rf_round2_df.RDS"))
df_rf_replace <- rbind(df_rf_replace1,df_rf_replace2)

df_rf_noreplace1 <- readRDS(file=paste0("nested_cv_",data_id, "_rf_norep_round1_df.RDS"))
df_rf_noreplace2 <- readRDS(file=paste0("nested_cv_",data_id, "_rf_norep_round2_df.RDS"))
df_rf_noreplace <- rbind(df_rf_noreplace1,df_rf_noreplace2)

df_et_replace1 <- readRDS(file=paste0("nested_cv_",data_id,"_et_replace_round1_df.RDS"))
df_et_replace2 <- readRDS(file=paste0("nested_cv_",data_id,"_et_replace_round2_df.RDS"))
df_et_replace <- rbind(df_et_replace1,df_et_replace2)
rownames(df_et_replace) <- NULL

df_et_sf1_1 <- readRDS(file=paste0("nested_cv_",data_id,"_et_sf1_round1_df.RDS"))
df_et_sf1_2 <- readRDS(file=paste0("nested_cv_",data_id,"_et_sf1_round2_df.RDS"))
df_et_sf1 <- rbind(df_et_sf1_1,df_et_sf1_2)
rownames(df_et_sf1) <- NULL

df_intf_replace <- readRDS(file=paste0(data_id,"_intf.RDS"))
df_intf_noreplace <- readRDS(file=paste0(data_id,"_intf_norep.RDS"))

#make sure column with errors is called "error"
names(df_rf_replace)[names(df_rf_replace) == "errors"] <- "error"
names(df_rf_noreplace)[names(df_rf_noreplace) == "errors"] <- "error"