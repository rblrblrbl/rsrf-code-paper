FIND OPT Code for RSRF. (Abbreviation in paper: (opt))

- This code was used for for finding optimal parameter for model F (pure-2).
- R Session Info, see below.

To reproduce results:

1) Run file "run_rsrf_af_F6_findopt.R" (rsrf-af) or "run_rsrf_nf_F6_findopt.R" (rsrf-nf)
2) Results will be stored in files/results_find_opt. In "results_paper_F6", the results used in paper can be found.

- Note: In files/slurm_submit_example_files are example jobscript usable for SLURM systems


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



