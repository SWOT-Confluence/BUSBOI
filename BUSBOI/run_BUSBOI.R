
run_BUSBOI = function(reach_id,priors, data,Q_priors, fix_bed,GVF_on) {

    #solve for Q
    optimal_hydrograph=jeff_solver(this_reach_id,priors,data,Q_priors,fix_bed,GVF_on)
    
    #structure is 
    #first position: r
    # 2nd posittion through nt: Q
    #final positions: Zo
    hydrograph_posterior=optimal_hydrograph[2:(nt+1)]
        hydrograph_posterior_sd=NA
        posteriors=c(optimal_hydrograph[1],optimal_hydrograph[length(optimal_hydrograph)])

    #send the output back to main
    output=list('posterior_Q'= hydrograph_posterior,
            'posterior_Q_sd'= hydrograph_posterior_sd,
            'posteriors'=posteriors)

    return(output)

  }


