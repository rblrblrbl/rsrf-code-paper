### ncv_ranger.R ###
In this file, details can be added manually, or one may use a batch script to supply the arguments in order to run the simulations. Check the comments in the R file.

Example: R CMD BATCH --no-save --no-restore --slave '--args airfoil rf 500 9 both' ncv_ranger.R

This runs the whole nested CV procedure on the airfoil datasets for both rounds, using RF, with seed set to 9.

The following seeds were used for the results shown in the paper.
concrete / concrete_hd: 3
airfoil / airfoil_hd: 9
abalone_enc / abalone_enc_hd : 11
robot / robot_hd: 52
chd / chd_hd: 7
