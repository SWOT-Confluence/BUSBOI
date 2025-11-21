jeff_solver=function(this_reach_id,priors,data,Q_priors,fix_bed,GVF_on,tulip){

  
    #set the hyperparams from the inputs
    hyperparams=list(
                    r=mean(priors$r_hat),
                    db=mean(priors$Db_hat), #unlog
                    wb=mean(priors$Wb_hat), #unlog
                    nx=priors$nx,
                    nt=priors$nt,
                    Sf=data$Sobs[1,], #since it repeats, only need row 1
                    So=data$Sobs[1,], #since it repeats, only need row 1
                    ###### n is fixed in this method! 
                    n=0.03,
                    #######-----------------------
                    new_x=priors$new_x, #station chainage
                    Hobs=as.matrix(data$Hobs) #Height
               )

   
    #make a Q vector and initial Zo vector for solving
    Q_init=Q_priors$Q_hat
 
    #Zo is the elevation of a single point, since we want to 
    #reduce the number of things we're solving for
    Zo_init=mean(priors$Zo_hat)

    nt=hyperparams$nt
    # make a 'downstream' height vector that drops the Zo 
    # exactly as the Sf, which was caluclated from the SWOT data
    Zo_ds=Zo_init + (mean(hyperparams$Sf)*hyperparams$new_x)

    #reduce the bed to 5 points then reconstruct it back to 50 on new_x
    #using the downstream slope vector
    new_x=hyperparams$new_x
    #take 5 evenly spaced samples. Since new_x is constant, these are always
    #the same points
    sample_index= seq(1,length(new_x),length.out=5)
    #draw the stations and bed elevation from those points
    sample_x=new_x[sample_index]
    sample_Zo=Zo_ds[sample_index]
    hyperparams$sample_x=sample_x

    #this is a uniform bed that starts at the Zo hat min
    #NOT USED if fix_bed = 0 or 1
    hyperparams$Zo_ds=Zo_ds

    #need to repeat the bounds to match the sizes of the parameters
    lowerQ=rep(Q_priors$lowerbound_Q,times=nt) 
    upperQ=rep(Q_priors$upperbound_Q,times=nt)
    
    upperZo=priors$lowerbound_Zo
    lowerZo=priors$upperbound_Zo

    lower_r=priors$lowerbound_r
    upper_r=priors$upperbound_r
    
    #optimizer is old school and doesn't let me use named inputs, which I frigging hate
    #therefore, the parameters are
        #positoin 1- r
        #position 2:2+nt - Q
        #final positoins - zo (either 1, 5 or 0 based on the hyperparam)
   

    #how we handle the bed has a major influence on skill
    #we have three options
        #0 - 5 points- reconsstruct a bed from 5 sampled bpoints
        #1-  single DS point
        #2 - fixed bed from Sf
    
    if(fix_bed ==1){ #single bed
        pars_bed=sample_Zo[1] #take the lowest one
        lower_bed=lowerZo[1]
        upper_bed=upperZo[1]
    }else{ #full bed in 0 or 2, 2 will ignore
        pars_bed=sample_Zo #this initial value is  5 elevations from the sampled linear bed from above
        lower_bed=rep(lowerZo,times=length(sample_Zo))
        upper_bed=rep(upperZo,times=length(sample_Zo))
        }

    #bunch of exceptions
        #r can go neg
        r_min=lower_r-mean(priors$r_sd)
        if(r_min<0){r_min=0.01}
    
        #Zo could be a problem
        if(any(lower_bed> pars_bed)){
               lower_bed[lower_bed>pars_bed]=0.8*min(pars_bed)
        }
        if(any(upper_bed< pars_bed)){
            upper_bed[upper_bed<pars_bed]=1.2*max(pars_bed)
        }

   #bounded optimization
    #in the same parameter order
     lower=c(r_min,
             lowerQ,
            lower_bed)
     upper=c(upper_r+mean(priors$r_sd), #since r is usually <1, we can nudge its upper bound upward for more phyiscal solutions
            upperQ,
            upper_bed)

    #sometimes the bed elevation is below sea level, which does wierd things
    if(any(lower_bed<0)){
        #the min statement before works in reverse if <0, so swap limits
        bed_shift=50 #new min height of 50m
        old_lower=lower_bed
        lower_bed=upper_bed+bed_shift
        upper_bed=old_lower+bed_shift
        pars_bed=pars_bed+bed_shift
        hyperparams$Hobs=hyperparams$Hobs+bed_shift
    }

    if (fix_bed!=2){ #in the 5 points and the single point case)
        pars=c(hyperparams$r, Q_init,pars_bed)}
    else { #fix_bed = 2
        #in this case, we don't need a bed parameter
        pars=c(hyperparams$r,Q_init)}
    #load optimizer, and run


    #more hyperparams
        plot_switch=0
        obj_error='absolute'
        replacement_error=15 #units of type of obj. error
        smooth_sf=1 # set to 1, for sure. takes the mean Sf
        Qpenalty=0 # penalize adverse bed slopes, units of obj. error
        Sfpenalty=0 # penalize adverse Sf, units of obj. error
        GVF_on=GVF_on # set to 1 to force a full-froude GVF
        H_DS_init='fixed' # set to'fixed' to always use the most ds swot obs

    

# # debugging toggle
#    jeff_calcHgivenparams(variables=pars,
#                      hyperparams=hyperparams,
#                     plot_switch=1,
#                     this_reach_id=this_reach_id,
#                     obj_error=obj_error,
#                     replacement_error=replacement_error,
#                     smooth_sf=smooth_sf,
#                     Qpenalty=Qpenalty,
#                     Sfpenalty=Sfpenalty,
#                     GVF_on=GVF_on,
#                     H_DS_init=H_DS_init,
#                     fix_bed=fix_bed,
#                         tulip=tulip)

#     bonk

 
   
    #solve
    optparams=optim(par=pars, fn=jeff_calcHgivenparams,
                      method='L-BFGS-B',
                      lower=lower, 
                      upper=upper,
                    #solver params from top level function
                     hyperparams=hyperparams,
                    plot_switch=plot_switch,
                    this_reach_id=this_reach_id,
                    obj_error=obj_error,
                    replacement_error=replacement_error,
                    smooth_sf=smooth_sf,
                    Qpenalty=Qpenalty,
                    Sfpenalty=Sfpenalty,
                    GVF_on=GVF_on,
                    H_DS_init=H_DS_init,
                    fix_bed=fix_bed,
                   tulip=tulip)

#  print(optparams)

# ### debugging toggle
#    jeff_calcHgivenparams(variables=optparams$par,
#                      hyperparams=hyperparams,
#                     plot_switch=1,
#                     this_reach_id=this_reach_id,
#                     obj_error=obj_error,
#                     replacement_error=replacement_error,
#                     smooth_sf=smooth_sf,
#                     Qpenalty=Qpenalty,
#                     Sfpenalty=Sfpenalty,
#                     GVF_on=GVF_on,
#                     H_DS_init=H_DS_init,
#                     fix_bed=fix_bed,
#     tulip=tulip)

#  bonk

    #the par variable is of the form (r, Q, Zo). Dimensions vary with hyperparameters
      return(optparams$par)
    
    }


