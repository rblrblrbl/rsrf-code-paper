Required Packages (with versions used in Paper)

simpleRSRF (version used in paper can be found in "../package_version_used_in_simulation")
diversityForest (diversityForest_0.3.4)
mlr3 (mlr3_0.14.1)
mlr3tuning (mlr3tuning_0.17.2)
mlr3learners (mlr3learners_0.5.6)
mlr3extralearners (use the package provided in directory "install_mlr3_extralearners" - this version includes the required learners for interaction forests / simpleRSRF )


#############################################################
		    MODEL ABBREVIATIONS
#############################################################

In the code and its documentation, the following abbreviations for the models were used:
A: pure-type
B: hierarchical
C: additive
E: pure-2
F: pure-3


#############################################################
 README for running simulations using the OPTIMAL parameters. 
#############################################################

To run the simulations using the optimal parameters, do

1) Go to file "sim_opt_parameters.R" and set which simulations shall be carried out by modifying lines 17-19. For instance, if you wish to run model F with d=6 for all algorithms, then you need to set: 

models_chosen <- c("F")
dimensions_chosen <- c(6)
algorithms_chosen <- c("extratrees_opt", "ranger_opt", "rsrf_nf_opt", "rsrf_af_opt", "intf_opt" )

2) Run "sim_opt_parameters.R" (make sure that the working directory is set to the directory where "sim_opt_parmaeters.R" is placed. Needs files "setup_dt.RDS", "sim_opt_parameters.R" and folder SIMFILES_mlr3. Results will be saved in PDF files.



#############################################################
        README for running simulations using CV 
#############################################################

- see folder RSRF_CV / INTF_CV (here mlr3 Packages are not required) and the READMEs therein.
- for Random forests/ Extratrees: Go to file "sim_cv_ranger.R" and set which simulations shall be carried out by modifying lines 17-19. For instance, if you wish to run model F with d=6 for all algorithms, then you need to set:  

models_chosen <- c("F")
dimensions_chosen <- c(6)
algorithms_chosen <- c("extratrees_cv", "ranger_cv")



#############################################################
          README for finding optimal parameters
#############################################################

- for RSRF (model F) / Interaction Forests: See folders RSRF_FINDOPT / INTF_FINDOPT and the READMEs therein
- for Random Forest, Ranger, RSRF (other models), the code in file "SIMFILES_mlr3/findopt.R" was used.