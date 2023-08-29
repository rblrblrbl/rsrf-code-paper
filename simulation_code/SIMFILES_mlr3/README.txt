README

------------
run_simulation
------------

This file will start desired simulation runs by setting up required variables and sourcing "main_simfile.R"

NOTE: This required generation of tasks beforehand, see main_generate_tasks_setup.R below.

0) Make sure that tasks_dir is specified as the directory where tasks are stored.
1) Make sure that results_dir is specified as the directory where results shall be stored.
2) Each file in "tasks_serious" is a list object with entries: "task" a mlr3 task object, "nsims" number of simulated data sets in task, "n" size of each data set and the holdout data set, "d" dimension of the features
3) Source the file, hand over sim ID and isTEST. 

e.g. Rscript.exe  run_simulation.R 158 FALSE

This will run the simulations with IDs "158" (here Random Forests with opt values for Model F6) and saves the results in as "results_158.RDS" in folder specified by results_dir. If the directory does not exist, it will be created. Seed will be set to 158, too.

Note that in the file, seed are set to the handed ID, too. For the simulation results reported in the paper, this choice was only applied to the simulations for "opt" values!


README
-------------
main_generate_tasks_setup.R
-------------

1) ONCE. Before all simulations: Creates tasks and list of simulation setups (i.e. information about algorithms, sample size etc. to be used from "main_simfile.R". 
Specify task directory name (where to save the genereated tasks) in main_generate_tasks_setups by specifying the tdir = "taskfile_name" argument in function execution "create_files_and_tasks". Task filenames will be of the form "A10", "B4" etc. (do not change them). Additionally, a data table "setup_dt.RDS" is generated with the simulation specifications.


README
------------
find_opt:
------------
1) Set up find_out.R with desired setting.
2) Make sure that output folder with name as set in find_out.R exists
3) For parallel: Set future plan in find_out.R via to multicore (or multisession), hand number of
4) Run.

Task results (a txt file with name like "C10_rsrf_nf.txt", a RDS file with name like "C10_rsrf_nf_all_results.RDS" and a progress file) will be created in output_folder. "c10_rsrf_nf.txt" contains best parameter results.