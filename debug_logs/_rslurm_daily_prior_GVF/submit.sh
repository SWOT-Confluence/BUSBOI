#!/bin/bash
#
#SBATCH --array=0-1
#SBATCH --cpus-per-task=100
#SBATCH --job-name=daily_prior_GVF
#SBATCH --output=slurm_%a.out
#SBATCH --mem=256000
#SBATCH --time=50:00:00
#SBATCH --partition=ceewater_cjgleason-cpu
#SBATCH --error=slurm-%A_%a.err
/work/pi_cjgleason_umass_edu/.conda/envs/lightweight/lib/R/bin/Rscript --vanilla slurm_run.R
