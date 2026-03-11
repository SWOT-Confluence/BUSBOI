.libPaths(c(.libPaths(), '/nas/cee-water/cjgleason/r-lib/'))
library(base, quietly = TRUE)
library(methods, quietly = TRUE)
library(datasets, quietly = TRUE)
library(utils, quietly = TRUE)
library(grDevices, quietly = TRUE)
library(graphics, quietly = TRUE)
library(stats, quietly = TRUE)
library(dplyr, quietly = TRUE)
library(parallel, quietly = TRUE)
library(ggplot2, quietly = TRUE)
library(tidyr, quietly = TRUE)
library(zoo, quietly = TRUE)
library(hydroGOF, quietly = TRUE)
library(RNetCDF, quietly = TRUE)
library(optimx, quietly = TRUE)
library(deSolve, quietly = TRUE)
library(purrr, quietly = TRUE)
library(geosphere, quietly = TRUE)
library(rslurm, quietly = TRUE)
library(whisker, quietly = TRUE)


.rslurm_func <- readRDS('f.RDS')
.rslurm_x <- readRDS('x.RDS')
.rslurm_more_args <- readRDS('more_args.RDS')
.rslurm_id <- as.numeric(Sys.getenv('SLURM_ARRAY_TASK_ID'))
.rslurm_istart <- .rslurm_id * 1640 + 1
.rslurm_iend <- min((.rslurm_id + 1) * 1640, length(.rslurm_x))
.rslurm_result <- do.call(parallel::mclapply, c(list(
    X = .rslurm_x[.rslurm_istart:.rslurm_iend],
    FUN = .rslurm_func),
    .rslurm_more_args,
    mc.cores = 30,
    mc.preschedule = FALSE
    ))

saveRDS(.rslurm_result, file = paste0('results_', .rslurm_id, '.RDS'))
