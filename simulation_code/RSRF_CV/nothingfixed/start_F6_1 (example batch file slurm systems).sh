#!/bin/bash
#SBATCH --ntasks=20
#SBATCH --nodes=1
#SBATCH --time=72:00:00
#SBATCH --job-name=nf_cv_F6_1-10
#SBATCH --mail-type=ALL
#SBATCH --output=rsrf_cv_nf.%j.txt
#SBATCH --mail-user=ricardo.blum@uni-heidelberg.de

#Get file ID from batch submission using following line in shell
#sbatch --partition=single script_name.sh export=RFILE_ID="test"

#Name of .tgz file stored in workspace names as in INPUT_WS
FOLDERNAME=files_nf_seed
INPUT_WS=$(ws_find rsrf_cv)

#generate temporary short time result workspace
ws_allocate rsrf_cv_results_${SLURM_JOB_ID} 4

# workspace for results. Needs to exist!
RESULTDIR=$(ws_find rsrf_cv_results_${SLURM_JOB_ID})/results_${SLURM_JOB_ID}
mkdir $RESULTDIR

# Copy input dataset to local $TMP
tar -C $TMP/ -xvzf $INPUT_WS/${FOLDERNAME}.tgz
# Start application and write results to on-demand file system

cd $TMP/$FOLDERNAME

echo "Working Directory:                    $PWD"
echo "Running on host                       `hostname`"
echo "Job id:                               $SLURM_JOB_ID"
echo "Job name:                             $SLURM_JOB_NAME"
echo "Number of nodes allocated to job:     $SLURM_NNODES"
echo "Number of cores allocated to job:     $SLURM_NTASKS"

module load math/R/4.1.2

# Set variables needed in R script below


# Start program
R CMD BATCH --no-save --no-restore --slave '--args 1 10 F 6 65998 FALSE' rsrf_cv_server_nf_slurm.R


#Copy back results after your application finished
cp rsrf_cv_server_nf_slurm.Rout $RESULTDIR/intf_cv_server_${SLURM_JOB_ID}.Rout
rsync -av $TMP/$FOLDERNAME/results $RESULTDIR/

#Copy back to home directory
rsync -av $RESULTDIR ~/RSRF_CV/nothingfixed_seed/F6/results_F6


