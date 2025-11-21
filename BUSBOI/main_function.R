main_function=function(this_reach_id,swot_base,sos_base,output_path,fix_bed,GVF_on,Q_prior,tulip){
 
    suppressMessages({
    library(dplyr)
    library(tidyr)
    library(RNetCDF)
    library(optimx)
    library(ggplot2)
    library(deSolve)
    library(hydroGOF)
     })


    source('/nas/cee-water/cjgleason/colin/BUSBOI/BUSBOI/input.R')
    source('/nas/cee-water/cjgleason/colin/BUSBOI/BUSBOI/jeff_tulip.R')
    source('/nas/cee-water/cjgleason/colin/BUSBOI/BUSBOI/calcHgivenparams.R')
    source('/nas/cee-water/cjgleason/colin/BUSBOI/BUSBOI/GVF.R')
    source('/nas/cee-water/cjgleason/colin/BUSBOI/BUSBOI/calculate_cum_dist.R')
    source('/nas/cee-water/cjgleason/colin/BUSBOI/BUSBOI/calc_Sf_newH.R')
    source('/nas/cee-water/cjgleason/colin/BUSBOI/BUSBOI/calculate_spline.R')
    source('/nas/cee-water/cjgleason/colin/BUSBOI/BUSBOI/fit_hydraulics.R')
    source('/nas/cee-water/cjgleason/colin/BUSBOI/BUSBOI/get_Q_prior.R')
    source('/nas/cee-water/cjgleason/colin/BUSBOI/BUSBOI/rejection_sample.R')
    source('/nas/cee-water/cjgleason/colin/BUSBOI/BUSBOI/Jeff_solver.R')
    source('/nas/cee-water/cjgleason/colin/BUSBOI/BUSBOI/read_LSTM_ensemble.R')
    source('/nas/cee-water/cjgleason/colin/BUSBOI/BUSBOI/run_BUSBOI.R')
    source('/nas/cee-water/cjgleason/colin/BUSBOI/BUSBOI/Slope_empirical.R')

    
    swot_file=paste0(swot_base,this_reach_id,'_SWOT.nc')
    continent_code=substr(this_reach_id,1,1)
    sos_files=paste0(sos_base,
                     c('eu_sword_v16_SOS_priors.nc',
                     'na_sword_v16_SOS_priors.nc',
                     'sa_sword_v16_SOS_priors.nc',
                     'oc_sword_v16_SOS_priors.nc',
                     'af_sword_v16_SOS_priors.nc',
                     'as_sword_v16_SOS_priors.nc'))

    swordpaths=c( '/nas/cee-water/cjgleason/data/SWORD/SWORDv16/netcdf/eu_sword_v16.nc',
                  '/nas/cee-water/cjgleason/data/SWORD/SWORDv16/netcdf/na_sword_v16.nc',
                  '/nas/cee-water/cjgleason/data/SWORD/SWORDv16/netcdf/sa_sword_v16.nc',
                  '/nas/cee-water/cjgleason/data/SWORD/SWORDv16/netcdf/oc_sword_v16.nc',
                  '/nas/cee-water/cjgleason/data/SWORD/SWORDv16/netcdf/af_sword_v16.nc',
                  '/nas/cee-water/cjgleason/data/SWORD/SWORDv16/netcdf/as_sword_v16.nc')

    if(continent_code=='7'){sos_file=sos_files[2]}
    if(continent_code=='8'){sos_file=sos_files[2]}
    if(continent_code=='1'){sos_file=sos_files[5]}
    if(continent_code=='2'){sos_file=sos_files[1]}
    if(continent_code=='9'){sos_file=sos_files[2]}
    if(continent_code=='6'){sos_file=sos_files[3]}
    if(continent_code=='5'){sos_file=sos_files[4]}


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

        ## CODE to test/prove the value of the daily prior for the paper. Turn OFF
        ## for confluence production runs.
        
        #switch to daily Q priors if needed
            if(Q_prior =='daily'){
        
          
                #get daily Q
               ML_Qt= read_LSTM_ensemble(this_reach_id)
                #sometimes there is none
                if(typeof(ML_Qt)!='character'){
        
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

        #format the output
        BUSBOI_df=data.frame(BUSBOI_Q=outputs$posterior_Q,
                      date=as.Date(busboi_data_object$swot_data$obs_times),
                      reach_id=this_reach_id,
                      r=outputs$posterior_r,
                      bed=paste(outputs$posterior_bed,sep=','),
                      prior_Q=busboi_data_object$Qpriors$Q_hat)

        #toggle this saveRDS on for local work, otherwise use the 'output' function
        saveRDS(BUSBOI_df,paste0(output_path,this_reach_id,'BUSBOIQ.rds'))
        bonk
        return(BUSBOI_df)

    } else { #no data to run

    
        BUSBOI_df=data.frame(busboi_Q=NA,
                             date=NA,
                             reach_id=this_reach_id,
                             prior_Q=NA)
        #toggle this saveRDS on for local work, otherwise use the 'output' function
        saveRDS(BUSBOI_df,paste0(output_path,this_reach_id,'BUSBOIQ.rds'))

     return(BUSBOI_df)

    } #end if statment checking for good input
}#end main


