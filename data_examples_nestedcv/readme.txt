README.

Here we provide an overview over the code used for the simulations on the real datasets from the paper.
Note that the folders "datasets" and "folds" need to be in the same directory as the ".R" files (e.g. "ncv_et.R" etc.).

### Abbreviations. For the (HD) versions, add "_hd".###
PAPER -> DATASET ABBREVIATION USED IN THE CODE
concrete -> concrete
airfoil -> airfoil
abalone -> abalone_enc
robot -> robot
california housing -> chd

### Datasets ###
See directory "datasets" where the pre-prepared .RDS files are stored. Description of datasets is provided in the paper.

### Generation of noisy covariates ###
See "datasets/files_hd/gen_hd.R". This file takes e.g. "airfoil.RDS" and will generate "airfoil_hd.RDS".
This contains the 50 newly added covariables, as described in the paper. The seeds used here can be found in the code "gen_hd.R"

### ET (Extremely Randomized Trees) ###
Needs package ranger. 
See folder "et" and the file "ncv_et.R" and "ncv_et_v2.R" and the readme in the folder "et".

Note that we use the following abbreviations for the two variants considered in the paper:
"et_replace" (using bootstrap samples in the trees)
"et_sf1" (using original sample)
This needs to be set for the variable alg_from_input when running the R script 

### RF (Random Forests) ###
Needs package ranger. 
See folder "rf" and the file "ncv_rf.R".

Note that we use the following abbreviations for the two variants considered in the paper:
"rf" (using bootstrap samples in the trees)
"rf_norep" (using subsamples, setting as in ranger package)

### RSRF (Random Split Random Forests) ###
Needs package simpleRSRF provided at github. The simulations were run on a computing cluster system because code is only available in plain R and not optimized. 
See folder "rsrf" and the files/readme therein. Best use a batch script to supply the arguments in order to run the simulations.

### INTF (Interaction Forests) ###
Needs package diversityForest. See "intf" and the file "ncv_intf.R" and "run_on_best_intf.R".
"intf" (using bootstrap samples in the trees)
"intf_norep" (using subsamples, setting as in diversityForest package)

### SEEDS ###
The following seeds were used for the results shown in the paper for RF / ET. For the other methods, see the details provided in the files RSRF resp. INTF.
concrete / concrete_hd: 3
airfoil / airfoil_hd: 9
abalone_enc / abalone_enc_hd : 11
robot / robot_hd: 52
chd / chd_hd: 7

################################################

### PACKAGE VERSIONS USED FOR RF/ET/INTF/RSRF###
Ranger (ranger_0.16.0)
diversityForest (diversityForest_0.3.4)
simpleRSRF (version used in paper can be found in "../package_version_used_in_simulation")

### R-Versions/Systems
For INTF/RSRF the following system and R version was used:
R version 4.1.2 (2021-11-01)
Platform: x86_64-pc-linux-gnu (64-bit)
Running under: Red Hat Enterprise Linux 8.6 (Ootpa)
Package versions: doParallel_1.0.16 iterators_1.0.13  doRNG_1.8.6 rngtools_1.5.2 foreach_1.5.1