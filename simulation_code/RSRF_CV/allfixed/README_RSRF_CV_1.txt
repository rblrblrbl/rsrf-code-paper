CV CODE for RSRF (fixed variant). 

- The code rsrf_cv_server_af.R was used for simulation for model F (pure-2).
- R Session Info, see below.

1) Run file "run_rsrf_af_F6.R" (seperately for each cv_sim_index to 1, 2, 3,... 10 (set these manually))
2) Once this is done, files like "result_F6_rsrf_cv_1_10.RDS" will be in results (note that results_F6 contains the files obtained from the simulation for the paper)
3) Then, you can run "results_generate_output.R" which will calculate mse error and standard deviation based on the simulation results both for the results used in paper (the ones in folder "results_F6" and the newly generated one in "results, shown in console output. 


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



- Note 1: The script "start_F6_1" is an example jobscript usable for SLURM systems
- Note 2:The version in "older_version" had a slightly different output style and there was no option to set a seed. This was used for Models A,B,C,E.