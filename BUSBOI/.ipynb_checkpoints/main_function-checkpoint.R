main_function=function(this_reach_id,swot_base,sos_base,output_path,fix_bed,GVF_on,Q_prior,tulip){
 
    # Source the output writer
    # source(file.path(dirname(sys.frame(1)$ofile), 'output.R'))

    BUSBOI_DIR='/nas/cee-water/cjgleason/colin/BUSBOI/BUSBOI/'

    # Source all BUSBOI functions
source(paste0(BUSBOI_DIR, 'input.R'))
source(paste0(BUSBOI_DIR, 'jeff_tulip.R'))
source(paste0(BUSBOI_DIR, 'calcHgivenparams_fixedbed.R'))
source(paste0(BUSBOI_DIR, 'calcHgivenparams_bedonly.R'))
source(paste0(BUSBOI_DIR, 'GVF.R'))
source(paste0(BUSBOI_DIR, 'calculate_cum_dist.R'))
source(paste0(BUSBOI_DIR, 'calc_Sf_newH.R'))
source(paste0(BUSBOI_DIR, 'calculate_spline.R'))
source(paste0(BUSBOI_DIR, 'fit_hydraulics.R'))
source(paste0(BUSBOI_DIR, 'get_Q_prior.R'))
source(paste0(BUSBOI_DIR, 'rejection_sample.R'))
source(paste0(BUSBOI_DIR, 'Jeff_solver_bedthenQ.R'))
source(paste0(BUSBOI_DIR, 'read_LSTM_ensemble.R'))
source(paste0(BUSBOI_DIR, 'run_BUSBOI.R'))
source(paste0(BUSBOI_DIR, 'Slope_empirical.R'))
source(paste0(BUSBOI_DIR, 'main_function.R'))


source('/nas/cee-water/cjgleason/colin/BUSBOI/BUSBOI/config.R')
    
  # Construct file paths from base directories
    swot_file <- paste0(swot_base, this_reach_id, '_SWOT.nc')
    
    # Determine continent code from reach_id
    continent_code <- substr(this_reach_id, 1, 1)
    
    # SOS files - now read from input directory
    sos_files=paste0(sos_base,
                     c('eu_sword_v17b_SOS_priors.nc',
                     'na_sword_v17b_SOS_priors.nc',
                     'sa_sword_v17b_SOS_priors.nc',
                     'oc_sword_v17b_SOS_priors.nc',
                     'af_sword_v17b_SOS_priors.nc',
                     'as_sword_v17b_SOS_priors.nc'))

    # SWORD files - now read from input directory
    swordpaths <- paste0(sos_base,
                        c('eu_sword_v17b.nc',
                          'na_sword_v17b.nc',
                          'sa_sword_v17b.nc',
                          'oc_sword_v17b.nc',
                          'af_sword_v17b.nc',
                          'as_sword_v17b.nc'))

    if(continent_code=='7'){sos_file=sos_files[2]}
    if(continent_code=='8'){sos_file=sos_files[2]}
    if(continent_code=='1'){sos_file=sos_files[5]}
    if(continent_code=='2'){sos_file=sos_files[1]}
    if(continent_code=='9'){sos_file=sos_files[2]}
    if(continent_code=='6'){sos_file=sos_files[3]}
    if(continent_code=='5'){sos_file=sos_files[4]}
    if(continent_code=='4'){sos_file=sos_files[6]}
    if(continent_code=='3'){sos_file=sos_files[6]}


    #fit hydraulics
    #do rejection sampling
    #get Q priors
    #either return all data if good, or return a 
    #null object if not good
    busboi_data_object =get_input(swot_file=swot_file, 
                              sos_file=sos_file, 
                              reach_id_in=this_reach_id)

    # # Prepare metadata for output
    # out_data <- list(
    #     reach_id = this_reach_id,
    #     node_ids = busboi_data_object$node_ids,  # Get from input if available
    #     nt = seq(1, length(busboi_data_object$swot_data$obs_times)),
    #     invalid_times = c()  # Track invalid times if needed
    # )
    
    #if we have something to run on 
    if(busboi_data_object$valid==TRUE){

        ## CODE to test/prove the value of the daily prior for the paper. Turn OFF
        ## for confluence production runs.
        
        #switch to daily Q priors if needed
            if(Q_prior =='daily'){
        
          
                #get daily Q
               ML_Qt= read_LSTM_ensemble(this_reach_id)
            
                #sometimes there is none
                if(typeof(ML_Qt)!='character'){

                    ML_Qt=ML_Qt%>%
                        distinct()

                  
                swot_dates=data.frame(date=as.Date(busboi_data_object$swot_data$obs_times))
        
                #left joining gives us a vector of exactly the right size
                ML_prior=left_join(swot_dates,ML_Qt,by='date')
        
                ML_Qhat= ML_prior$ML_ensemble
                #replace NA
                ML_Qhat[is.na(ML_Qhat)]=mean(ML_Qhat,na.rm=TRUE)
                minQ=0.6*min(ML_Qhat)
                maxQ=1.4*max(ML_Qhat)
                Qsd=sd(ML_Qhat)
        
                    busboi_data_object$Qpriors$Q_hat=ML_Qhat
                    busboi_data_object$Qpriors$Q_sd=rep(Qsd,times=length(ML_Qhat))
                    busboi_data_object$Qpriors$lowerbound_Q=minQ
                    busboi_data_object$Qpriors$upperbound_Q=maxQ


                  }#end if there is no q prior
        
                }


        #sovle for Q
        outputs=run_BUSBOI(this_reach_id,
                   priors=busboi_data_object$priors,
                   data=busboi_data_object$swot_data,
                   fix_bed=fix_bed,
                   GVF_on=GVF_on,
                    tulip=tulip,
                   Q_priors=busboi_data_object$Qpriors)


        # print(outputs)
        finalQ=outputs$posterior_Q
        #if 90% or more are at one boudn or the other, reject
        
        upper=busboi_data_object$Qpriors$upperbound_Q
        lower=busboi_data_object$Qpriors$lowerbound_Q
        nt=busboi_data_object$priors$nt

       #if   number of || within 10% of upper || greater than || 90% of data
        if( sum(finalQ > (0.9*upper)) > 0.9*nt ){
                    BUSBOI_df=data.frame(busboi_Q=NA,
                             date=NA,
                             reach_id=this_reach_id,
                             prior_Q=NA,
                             r=NA,
                             bed=NA)
        #toggle this saveRDS on for local work, otherwise use the 'output' function
        saveRDS(BUSBOI_df,paste0(output_path,this_reach_id,'BUSBOIQ.rds'))
            return(NULL)
        
        }
        if( sum(finalQ > (0.9*upper)) > 0.9*nt ){
        BUSBOI_df=data.frame(busboi_Q=NA,
                             date=NA,
                             reach_id=this_reach_id,
                             prior_Q=NA,
                             r=NA,
                             bed=NA)
        #toggle this saveRDS on for local work, otherwise use the 'output' function
        saveRDS(BUSBOI_df,paste0(output_path,this_reach_id,'BUSBOIQ.rds'))
            return(NULL)
            }



        # #format the output
        BUSBOI_df=data.frame(BUSBOI_Q=outputs$posterior_Q,
                      date=as.Date(busboi_data_object$swot_data$obs_times),
                      reach_id=this_reach_id,
                      r=outputs$posterior_r,
                      bed=paste(outputs$posterior_bed,collapse=','),
                      prior_Q=busboi_data_object$Qpriors$Q_hat)

        #toggle this saveRDS on for local work, otherwise use the 'output' function
        saveRDS(BUSBOI_df,paste0(output_path,this_reach_id,'BUSBOIQ.rds'))



    } else { #no data to run


        BUSBOI_df=data.frame(busboi_Q=NA,
                             date=NA,
                             reach_id=this_reach_id,
                             prior_Q=NA,
                             r=NA,
                             bed=NA)
        #toggle this saveRDS on for local work, otherwise use the 'output' function
        saveRDS(BUSBOI_df,paste0(output_path,this_reach_id,'BUSBOIQ.rds'))

  

    } #end if statment checking for good input
}#end main