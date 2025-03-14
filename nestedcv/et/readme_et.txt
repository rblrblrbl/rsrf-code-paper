### ncv_et.R ###
This will run nested CV for ET, either for "round1", for "round2" or "both" rounds, provided the round value supplied to the R file.
Example: 
R CMD BATCH --no-save --no-restore --slave '--args airfoil et_sf1 500 9 both' ncv_et.R

### ncv_et_v2.R ###
For a specified outerfold ID, this will run only the inner CV search corresponding to that outer fold, and then the training/testing using parameters found in inner CV search, on the outer fold.
