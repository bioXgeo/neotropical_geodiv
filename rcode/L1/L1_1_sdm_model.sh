#!/bin/bash

#SBATCH --time=03:00:00
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=8
#SBATCH --mem=32gb
#SBATCH --constraint="[intel16|intel18|amd20|amd22]"
#SBATCH --array=1-3
#SBATCH --output joboutput/%x-%a-output-%j.txt
#SBATCH --error joboutput/%x-%a-error-%j.txt

# L1_1_sdm_model.sh 
# shellscript to launch a slurm job to run SDM model for different species. 

#
# usage: sbatch --array=1-3 --export=SDM_SPECIES=Tremarto_ornatus,SDM_OUTPUT_PATH=`pwd`/test_output  sdm_run_all_radii.sh

# notes: 
#  this script runs all radii and multiple replicates (run number) for a single species
#  if looks to environment variables SDM_SPECIES and SDM_OUTPUT_PATH, which must be propaged to the job
#  --export is used to set those environment variables when you start the job.  
#  could also set those variables in your shell, then use --export=ALL
#  you could write a bash for loop to read the occurrence data folders to  submit jobs for each species folder
#  this is an array job but for safety, by default only runs 1 job; use the sparam --array to set more runs

# the .Renviron in this folder needs to set the following variables used by the script
# export SDM_BASE_PATH="/mnt/scratch/billspat/neotropical_frugivores/andes_geodiv"
# export SDM_OCCS_PATH="occurrence_records"
# export SDM_ENVS_PATH="environmental_variables"
# export SDM_OCCS_PATH_TEMPLATE='occurrence_records/%s_thinned_full'
# export SDM_OCCS_FILE_TEMPLATE='%s_thinned_wallace.csv'

# export SDM_OUTPUT_PATH


# time: 7 radii * 5 runs; 5 minutes per run; => time =~ 3 hrs
# memory: 4gb per process X cpus-per-task

# use env variables, but use defaults if they are not set

# require the env variable SDM_SPECIES to be set
if [ -z "$SDM_SPECIES" ]; then 
  echo "the var SDM_SPECIES is not set, exiting" 
  exit 1
fi 

RUN_NUMBER="${SLURM_ARRAY_TASK_ID:-1}"
OUTPUT_PATH="${SDM_OUTPUT_PATH:-$HOME/sdm_model_runs}"

RADII=( 1 3 9 15 21 27 33 )

module purge
module load GCC/11.2.0  OpenMPI/4.1.1 GDAL R Automake UDUNITS
export RVER="4.2.2-GCC-11.2.0"
export R_LIBS_USER=$HOME/R/$RVER

for RADIUS in "${RADII[@]}";do
  Rscript --no-save  --no-init-file --no-restore  \
  L1_1_sdm_model.R $SDM_SPECIES $RADIUS $RUN_NUMBER $OUTPUT_PATH \
  $SLURM_JOB_CPUS_PER_NODE
done  
