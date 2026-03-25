main_function=function(this_reach_id,swot_base,sos_base,output_path,fix_bed,GVF_on,Q_prior,tulip){
 
    # Source the output writer
    source(file.path(dirname(sys.frame(1)$ofile), 'output.R'))
    
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
    swordpaths <- paste0(sword_base,
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

    # Prepare metadata for output
    out_data <- list(
        reach_id = this_reach_id,
        node_ids = busboi_data_object$node_ids,  # Get from input if available
        nt = seq(1, length(busboi_data_object$swot_data$obs_times)),
        invalid_times = c()  # Track invalid times if needed
    )
    
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

          # Prepare posteriors for NetCDF output
        posteriors <- list(
            r = outputs$posterior_r,
            r_sd = sd(outputs$posterior_r, na.rm = TRUE),  # Calculate if not provided
            bed = outputs$posterior_bed,
            prior_Q = busboi_data_object$Qpriors$Q_hat
        )

        # #format the output
        # BUSBOI_df=data.frame(BUSBOI_Q=outputs$posterior_Q,
        #               date=as.Date(busboi_data_object$swot_data$obs_times),
        #               reach_id=this_reach_id,
        #               r=outputs$posterior_r,
        #               bed=paste(outputs$posterior_bed,collapse=','),
        #               prior_Q=busboi_data_object$Qpriors$Q_hat)

        #toggle this saveRDS on for local work, otherwise use the 'output' function
        # saveRDS(BUSBOI_df,paste0(output_path,this_reach_id,'BUSBOIQ.rds'))

            # Calculate discharge uncertainty if available
        discharge_sd <- if(!is.null(outputs$posterior_Qsd)) {
            outputs$posterior_Qsd
        } else {
            sd(outputs$posterior_Q, na.rm = TRUE)
        }
        
 # Write NetCDF output
        write_output(
            data = out_data,
            posteriors = posteriors,
            discharge = outputs$posterior_Q,
            discharge_sd = discharge_sd,
            out_dir = output_path,
            is_valid = TRUE,
            obs_times = as.character(as.Date(busboi_data_object$swot_data$obs_times))
        )


    } else { #no data to run

     # No valid data to run - write invalid output
        posteriors <- list(
            r = NA,
            r_sd = NA,
            bed = NA,
            prior_Q = NA
        )

        write_output(
            data = out_data,
            posteriors = posteriors,
            discharge = NA,
            discharge_sd = NA,
            out_dir = output_path,
            is_valid = FALSE,
            obs_times = NA
        )
        # BUSBOI_df=data.frame(busboi_Q=NA,
        #                      date=NA,
        #                      reach_id=this_reach_id,
        #                      prior_Q=NA,
        #                      r=NA,
        #                      bed=NA)
        # #toggle this saveRDS on for local work, otherwise use the 'output' function
        # saveRDS(BUSBOI_df,paste0(output_path,this_reach_id,'BUSBOIQ.rds'))

     # return(BUSBOI_df)

    } #end if statment checking for good input
}#end main