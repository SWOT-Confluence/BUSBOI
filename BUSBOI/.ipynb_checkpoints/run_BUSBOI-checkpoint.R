
run_BUSBOI = function(reach_id,priors, data,Q_priors, fix_bed,GVF_on,tulip) {



    #solve for Q
    optimal_hydrograph=jeff_solver_bedthenQ(this_reach_id=reach_id,
                                   priors=priors,
                                   data=data,
                                   Q_priors=Q_priors,
                                   fix_bed=fix_bed,
                                   GVF_on=GVF_on,
                                   tulip=tulip)




    return(optimal_hydrograph)

 

  }


