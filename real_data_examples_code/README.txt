README: Here we provide an overview over the code used for the simulations on the real datasets from the paper.

### Abbreviations. For the (HD) versions, add "_hd".###
PAPER -> DATASET NAME USED IN THE CODE
concrete -> concrete
airfoil -> airfoil
abalone -> abalone_all
robot -> robot
california housing -> california_housing_div

### Datasets ###
See "datasets" where the pre-prepared .RDS files are stored. Description of datasets is provided in the paper.

### Generation of noisy covariates ###
See "datasets/files_hd/gen_hd.R". This file takes e.g. "airfoil.RDS" and will generate "airfoil_hd.RDS".
This contains the 50 newly added covariables, as described in the paper. The seeds used here can be found in the code "gen_hd.R"

### ET (Extremely Randomized Trees) ###
Needs package ranger. 
See "et" and the file "error_et.R". In this file, details can be added manually, or one may use a script to supply the
arguments (parameter values) in order to run the simulations. An example is set in the file and example output is given 
in "et/output_cv_local" for the abalone dataset.
One needs to set the variable alg_from_input in the R script with either "et_replace" (using bootstrap samples in the trees) resp.
"et_norep_sf1" (for using original samples in the trees) to run the two versions considered in the paper.

### RF (Random Forests) ###
Needs package ranger.
See "rf" and the file "error_rf.R". In this file, details can be added manually, or one may use a batch script to supply the arguments (parameter values) in order to run the simulations.
An example output is set in the file and example output is given in "rf/output_cv_local" for the abalone dataset.

### RSRF (Random Split Random Forests) ###
Needs package simpleRSRF provided at github. The simulations were run on a computing cluster system because code is only available in plain R and not optimized. 
See "rsrf" and the file "error_rsrf.R". Best use a batch script to supply the arguments (parameter values) in order to run the simulations.
An example output is given in "output/example_output" for the abalone dataset.

### INTF (Interaction Forests) ###
Needs package diversityForest. See "intf" and the file "error_intf.R". The simulations were run on a computing cluster system.
See "intf" and the file "error_intf.R". In the code, it is shown which arguments are needed to be supplied when running the R file.

### INFORMATION ON SEEDS USED FOR THE SIMULATIONS ###
The seeds can be supplied with the different simulation files. For the results that were used for the plots in the paper, the following seeds were set for the different data examples:
concrete / concrete_hd: 3
airfoil / airfoil_hd: 9
abalone_all / abalone_all_hd: 2
robot / robot_hd: 423 
california_housing_div / california_housing_div_hd: 141
