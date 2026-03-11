read_LSTM_ensemble=function(reach_id_in){

    #by reading from the SES ensemble, we've sampled for SWOT times

    filename=paste0('/nas/cee-water/cjgleason/colin/SWOT_global_Q_paper/SES_dfs_individual_reach_version_D/',reach_id_in,'_model_trad_SES.rds')
    # filename=paste0('/nas/cee-water/cjgleason/colin/SWOT_global_Q_paper/ML_trad_SES_dfs/',reach_id_in,'_model_trad_SES.rds')



    if(!file.exists(filename)){
    return('no ensemble')}else{

     
            global_ensemble=readRDS(filename)%>%
            filter(model=='ML_ensemble')%>%
            filter(!is.na(flow))
      
    if(nrow(global_ensemble)==0){
       return('no non NA ML values')
       }else{

 

        global_ensemble=global_ensemble%>%
        transmute(date=date,reach_id=reach_id,ML_ensemble=flow)

       
      
    }
        }#end file exists

   #sometimes tehre is no ML ensemble despite there being a file


   return(global_ensemble)
   }