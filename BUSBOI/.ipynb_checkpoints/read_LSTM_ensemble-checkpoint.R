read_LSTM_ensemble=function(reach_id_in){

    # filename=paste0('/nas/cee-water/cjgleason/colin/SWOT_global_Q_paper/ML_trad_SES_dfs/',reach_id_in,'_model_trad_SES.rds')
   filename=paste0('/nas/cee-water/cjgleason/colin/SWOT_global_Q_paper/daily_ensembles_FSSO/',reach_id_in,'daily_ensemble.rds')


    if(!file.exists(filename)){
    return('no ensemble')}else{

            global_ensemble=readRDS(filename)%>%
                select(ML_ensemble,date,reach_id)%>%
                filter(!is.na(ML_ensemble))
    }

   #sometimes tehre is no ML ensemble despite there being a file
   if(nrow(global_ensemble)==0){
       return('no non NA ML values')
       }

   return(global_ensemble)
   }