suppressMessages({
  library(dplyr)
  library(parallel)
  library(ggplot2)
  library(tidyr)
  library(hydroGOF)
})

gauge_df=readRDS('/nas/cee-water/cjgleason/colin/analyze confluence runs/SVS_df.rds')
gauged_reaches=unique(gauge_df$reach_id)
swot_base='/nas/cee-water/cjgleason/ellie/SWOT/confluence/confluence_relPermML/relPermML_mnt/input/swot/'
sos_base='/nas/cee-water/cjgleason/ellie/SWOT/confluence/confluence_relPermML/relPermML_mnt/input/sos/'
reach_ids=gauged_reaches[gauged_reaches %in% substr(list.files(swot_base),1,11)]

output_path='/nas/cee-water/cjgleason/colin/BUSBOI/debug tests/multiobj/'
run_ids=substr(list.files(output_path),1,11)
unrun= reach_ids[!reach_ids %in% run_ids]
length(unrun)

source('/nas/cee-water/cjgleason/colin/BUSBOI/BUSBOI/main_function.R')
# suppressWarnings({
for (i in 6:12){
  

  print(unrun[i])
  
  test= main_function(unrun[i],
                      output_path=output_path,
                      swot_base=swot_base,
                      sos_base=sos_base,
                      Q_prior='daily', #'daily' or 'monthly'
                      tulip='OFF', #'ON' or 'OFF'
                      GVF_on=0, # 0 or 1
                      fix_bed=0) # 0 = 5 pt, 1 = 1pt, 2 = fixed
  
}

# })

# a= Sys.time()
# clust=makeCluster(4)
# test=parLapply(clust,unrun,main_function,
#                    output_path=output_path,
#                    swot_base=swot_base,
#                    sos_base=sos_base,
#                    Q_prior='daily', #or 'monthly'
#                    tulip='OFF', #or 'OFF'
#                    GVF_on=0,
#                    fix_bed=0) # 0 = 5 pt, 1 = 1pt, 2 = fixed
# stopCluster(clust)
# print(Sys.time()-a)