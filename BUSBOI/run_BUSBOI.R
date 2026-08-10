
 run_BUSBOI = function(reach_id,priors, data,Q_priors, fix_bed,GVF_on,tulip) {


    #solve for Q
    optimal_hydrograph=jeff_solver_bedthenQ(this_reach_id=reach_id,
                                   priors=priors,
                                   data=data,
                                   Q_priors=Q_priors,
                                   fix_bed=fix_bed,
                                   GVF_on=GVF_on,
                                   tulip=tulip)


   
    nt=priors$nt
    #structure is 
    #first position: r
    # 2nd posittion through nt: Q
    #final positions: Zo
    hydrograph_posterior=optimal_hydrograph[2:(nt+1)]
        hydrograph_posterior_sd=NA
        posterior_r=optimal_hydrograph[1]
        posterior_bed=optimal_hydrograph[(nt+2):(length(optimal_hydrograph))]
        
   

    #send the output back to main
    output=list('posterior_Q'= hydrograph_posterior,
            'posterior_Q_sd'= hydrograph_posterior_sd,
               'posterior_r'=posterior_r,
                'posterior_bed'=posterior_bed)


    return(output)

  # }
}

