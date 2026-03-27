main_function=function(this_reach_id,swot_base,sos_base,sword_base,output_path,fix_bed,GVF_on,Q_prior,tulip){
 
    
    # Construct file paths from base directories
    swot_file <- paste0(swot_base, this_reach_id, '_SWOT.nc')
    
    # Determine continent code from reach_id
    continent_code <- substr(this_reach_id, 1, 1)
    
    # SOS files - read from input directory
    sos_files <- list.files(sos_base, full.names=TRUE)
    
    # SWORD files - read from input directory
    swordpaths <- list.files(sword_base, full.names=TRUE)
    
    # Continent code to two-letter prefix mapping
    continent_index <- c(
        '1' = 'af',
        '2' = 'eu',
        '3' = 'sa',
        '4' = 'sa',
        '5' = 'oc',
        '6' = 'sa',
        '7' = 'na',
        '8' = 'na',
        '9' = 'na'
    )
    
    prefix <- continent_index[continent_code]
    sos_file <- sos_files[grepl(paste0("^", prefix), basename(sos_files))]
    sword_file <- swordpaths[grepl(paste0("^", prefix), basename(swordpaths))]
    
    #fit hydraulics
    #do rejection sampling
    #get Q priors
    #either return all data if good, or return a 
    #null object if not good
    busboi_data_object =get_input(swot_file=swot_file, 
                              sos_file=sos_file, 
                              reach_id_in=this_reach_id)
    
    #if we have something to run on 
    if(busboi_data_object$valid==TRUE){

    # Prepare metadata for output
    out_data <- list(
        reach_id = this_reach_id,
        node_ids = busboi_data_object$node_ids,  # Get from input if available
        nt = seq(1, length(busboi_data_object$swot_data$obs_times)),
        invalid_times = c()  # Track invalid times if needed
    )
    

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
            r = outputs$r,
            bed = outputs$bed,
            prior_Q = busboi_data_object$Qpriors$Q_hat,
            Q=outputs$Q,
            chainage=outputs$chainage
        )


  
 # Write NetCDF output
        write_output(
            reach_id=this_reach_id,
            posteriors = posteriors,
            out_dir = output_path,
            is_valid = TRUE,
            obs_times = as.character(as.Date(busboi_data_object$swot_data$obs_times))
        )


    } else { #no data to run

     # No valid data to run - write invalid output
        posteriors <- list(
            r = NA,
            bed = NA,
            prior_Q = NA,
            Q=NA,
            chainage=NA
        )


        write_output(
            reach_id=this_reach_id,
            posteriors = posteriors,
            out_dir = output_path,
            is_valid = TRUE,
            obs_times = NA
        )


    } #end if statment checking for good input
}#end main