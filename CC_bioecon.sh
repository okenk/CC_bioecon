#!/bin/bash

#SBATCH -D /home/kloken/CC_bioecon
#SBATCH -o /home/kloken/CC_bioecon/slurm_log/output%a.txt
#SBATCH -e /home/kloken/CC_bioecon/slurm_log/error%a.txt
#SBATCH -J bioecon_sim
#SBATCH -t 01:00:00
#SBATCH --array=0-2

scenarios=( 'synchrony' 'access' 'interaction')

scenario=${scenarios[$SLURM_ARRAY_TASK_ID]}

Rscript ~/CC_bioecon/Code/for_cluster/${scenario}_sims.R 1000
