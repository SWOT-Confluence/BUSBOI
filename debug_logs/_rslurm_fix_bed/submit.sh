#!/bin/bash
#
#SBATCH --array=0-0
#SBATCH --cpus-per-task=2
#SBATCH --job-name=fix_bed
#SBATCH --output=slurm_%a.out
#SBATCH --mem=12000
#SBATCH --time=50:00:00
#SBATCH --partition=ceewater_cjgleason-cpu
#SBATCH --error=slurm-%A_%a.err
/work/pi_cjgleason_umass_edu/.conda/envs/lightweight/lib/R/bin/Rscript --vanilla slurm_run.R
