
get_input=function(swot_file,sos_file,reach_id_in){
 

    #fit hydraulics
    #this returns Hobs, Sobs, Wobs, nx, nt, xs ids, and the obs times
    fitted_hydraulics= fit_hydraulics(swot_file,sos_file,reach_id_in)
    #if there is no fit, then the result will be a character string
    #catch that and return nothing
    if(typeof(fitted_hydraulics)=='character'){
            valid=FALSE
            return(list('reach_id'=reach_id_in,'swot_data'=NA, 'priors'= NA,'Qpriors'=NA,'valid'=valid))
    }


    #rejection sample the priors on those fitted hydraulics
    #given
        #Hobs, Wobs, and Sobs
    #this returns priors on
        #r, Db, Wb, Zo 
    #it returns them in a list, and it also contains an index
    #for which there were valid priors:
        #good_nodes
    prior_object=do_rejection_sampling(fitted_hydraulics)

    if(prior_object$validpriors ==TRUE){
     #we might have jettisoned some nodes
     fitted_hydraulics$Hobs=fitted_hydraulics$Hobs[prior_object$good_nodes,]
     fitted_hydraulics$new_x=fitted_hydraulics$new_x[prior_object$good_nodes]
     fitted_hydraulics$Wobs=fitted_hydraulics$Wobs[prior_object$good_nodes,]
        
    priors=prior_object$nodepriors
    #format priors. This is a vestig of using STAN for the MCMC, but it will take
    #too much effort to change it and it doesn't slow anything down
    prior_list=list(

        lowerbound_r= min(priors$r),
        upperbound_r= max(priors$r),
        r_hat=priors$r,
        r_sd= priors$rsd,
        
        Wb_hat= priors$wb,
        Db_hat= priors$db,

        lowerbound_Zo= min(priors$Zo),
        upperbound_Zo= max(priors$Zo),
        Zo_hat= priors$Zo,
        Zo_sd= priors$Zosd,
    
        Herr_sd=0.15, #15cm
        Serr_sd=1.7e-5, #unbiased 1.7cm/km
            
        nx=nrow(fitted_hydraulics$Hobs),
        nt=ncol(fitted_hydraulics$Hobs),
    
        new_x=fitted_hydraulics$new_x
    )

   valid =TRUE
        #this ELSE statement is what happens if the rejection sampling
        #couldn't find anything it likes
        } else{
            valid=FALSE
            return(list('reach_id'=reach_id_in,'swot_data'=NA, 'priors'= NA,'Qpriors'=NA,'valid'=valid))
              }


    #need to check to make sure that H-Zo is always positive
    Hobs=fitted_hydraulics$Hobs
    Zo_max=prior_list$upperbound_Zo
    Hobsdif=min(Hobs,na.rm=TRUE)-Zo_max

    if(Hobsdif<0){
        #drop the zo do that it is deep enough
        #must also lower the lowerboudn and mean
        prior_list$upperbound_Zo=prior_list$upperbound_Zo + Hobsdif -0.01
        prior_list$lowerbound_Zo=prior_list$lowerbound_Zo + Hobsdif -0.01
        prior_list$Zo_hat=prior_list$Zo_hat + Hobsdif -0.01
 }
    #get Q priors
    #returns a list in log space
        # logQ_hat
        # lowerbound_logQ
        # upperbound_logQ
        # logQ_sd
    Q_priors=get_Q_prior(sos_file,reach_id_in,fitted_hydraulics$obs_times)
    #vestiage, needs to be nt long


    #finally, return the hydrualics, priors, and q priors
    return(list('reach_id'=reach_id_in,'swot_data'=fitted_hydraulics, 'priors'= prior_list,'Qpriors'=Q_priors,'valid'=valid))

    }#end input

    
