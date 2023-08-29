#!/bin/bash
#SBATCH --ntasks=40
#SBATCH --nodes=1
#SBATCH --time=72:00:00
#SBATCH --job-name=nf_fo_F6
#SBATCH --mail-type=ALL
#SBATCH --output=rsrf_nf_fo_F6.%j.txt
#SBATCH --mail-user=ricardo.blum@uni-heidelberg.de

#Get file ID from batch submission using following line in shell
#sbatch --partition=single script_name.sh export=RFILE_ID="test"

#Name of .tgz file stored in workspace names as in INPUT_WS
FOLDERNAME=files
INPUT_WS=$(ws_find rsrf_findopt)

#generate temporary short time result workspace
ws_allocate rsrf_findopt_${SLURM_JOB_ID} 5

# workspace for results. Needs to exist!
RESULTDIR=$(ws_find rsrf_findopt_${SLURM_JOB_ID})/results_${SLURM_JOB_ID}
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


# Start program. Hand over to R:  type, dimension, algorithm, test(TRUE/FALSE), seed
R CMD BATCH --no-save --no-restore --slave '--args F 6 rsrf_nf FALSE 198' findopt_rsrf_doparallel_server_slurm.R


#Copy back results after your application finished
cp findopt_rsrf_doparallel_server_slurm.Rout $RESULTDIR/rsrf_findopt_${SLURM_JOB_ID}.Rout
rsync -av $TMP/$FOLDERNAME/results_find_opt $RESULTDIR/

#Copy back to home directory
rsync -av $RESULTDIR ~/RSRF_findopt/F6/results_nf
