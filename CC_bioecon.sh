#!/bin/bash

scenarios = c("synchrony", "access", "interaction")

for scenario in ${scenarios[@]}; do
  job_file = "home/kloken/CC_bioecon/slurm_log/${scenario}.sh"
  
  echo "#!/bin/bash
#SBATCH -D /home/kloken/CC_bioecon
#SBATCH -o /home/kloken/CC_bioecon/slurm_log/${scenario}_output.txt
#SBATCH -e /home/kloken/CC_bioecon/slurm_log/${scenario}_error.txt
#SBATCH -J ${scenario}_sim
#SBATCH -t 01:00:00

Rscript ~/CC_bioecon/Code/for_cluster/${scenario}.R 10" > job_file
  sbatch -p high --ntasks=8 job_file
  
done

