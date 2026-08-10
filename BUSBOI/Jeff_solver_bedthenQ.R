jeff_solver_bedthenQ=function(this_reach_id,priors,data,Q_priors,fix_bed,GVF_on,tulip){

 
  
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
    Zo_init=max(priors$Zo_hat)
   

    nt=hyperparams$nt
    # make a 'downstream' height vector that drops the Zo 
    # exactly as the Sf, which was caluclated from the SWOT data
    Zo_ds=Zo_init + (mean(hyperparams$Sf)*hyperparams$new_x)

    # print('Zo_ds')
    # print(Zo_ds)

    #reduce the bed to 5 points then reconstruct it back to 50 on new_x
    #using the downstream slope vector
    new_x=hyperparams$new_x
    
    #take 10 evenly spaced samples. Since new_x is constant, these are always
    #the same points
    sample_index= seq(1,length(new_x),length.out=10)
    
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

    #intentional error
    lowerZo=priors$lowerbound_Zo
    upperZo=priors$upperbound_Zo

    lower_r=priors$lowerbound_r
    upper_r=priors$upperbound_r
    
    #optimizer is old school and doesn't let me use named inputs, which I frigging hate
    #therefore, the parameters are
        #positoin 1- r
        #final positoins - zo (10)
   

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
        #the Zo rejection sampling is interesting, as it is highly summarized. When we calculate a 
        #Zo_ds, we blow through the sampled limits, and the max and min we read in are the max/min of the MEAN Zo
        #from the sampling
        lower_bed=rep(lowerZo,times=length(sample_Zo))
        upper_bed=rep(upperZo,times=length(sample_Zo))
        }

    #bunch of exceptions
        #r can go neg
        r_min=lower_r-mean(priors$r_sd)
        if(r_min<0){r_min=0.01}
    
        #there could be issues with the lower bed as well
        lower_bed[lower_bed>pars_bed]=0.9*min(pars_bed)
       # lower_bed= rep(0.8*min(pars_bed),times=length(sample_Zo))
        upper_bed[upper_bed<pars_bed]=1.1*max(pars_bed)
    # upper_bed= rep(1.2*max(pars_bed),times=length(sample_Zo))


      #Zo could be a problem
        minH= min(hyperparams$Hobs,na.rm=TRUE)
        if ((minH-upper_bed[1])<0){
            #subtract 0.1 meter
            upper_bed=upper_bed -0.1
        }


    # print('4')
    # print(lower_bed)
    # print(upper_bed)
    # print('pars')
    # print(pars_bed)


   #bounded optimization
    #in the same parameter order
     lower=c(r_min,
             # lowerQ,
            lower_bed)
     upper=c(upper_r+mean(priors$r_sd), #since r is usually <1, we can nudge its upper bound upward for more phyiscal solutions
            # upperQ,
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
        pars=c(hyperparams$r, pars_bed)}
    else { #fix_bed = 2
        #in this case, we don't need a bed parameter
        pars=c(hyperparams$r)}
    #load optimizer, and run

    MLbias=mean(log(Q_init),na.rm=TRUE)
    #more hyperparams
        hyperparams$logML_mean=MLbias
        hyperparams$Q_init=Q_init
        plot_switch=0
        obj_error='absolute'
        replacement_error=15 #units of type of obj. error
        smooth_sf=1 # set to 1, for sure. takes the mean Sf
        Qpenalty=0 # penalize adverse bed slopes, units of obj. error
        Sfpenalty=0 # penalize adverse Sf, units of obj. error
        GVF_on=GVF_on # set to 1 to force a full-froude GVF
        H_DS_init='free' # set to'fixed' to always use the most ds swot obs




#solve for an R and bed    
    optparams=optim(par=pars, fn=jeff_calcHgivenparams_bedonly,
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

    # print(optsolution)
    # print(optparams)
    # browser()

#  ##debugging toggle

    # debugging toggle to visualize initial bed.-----------------------------
initial_plots=jeff_calcHgivenparams_bedonly(variables=pars,
                  hyperparams=hyperparams,
                 plot_switch=1,
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


saveRDS(initial_plots,paste0('/nas/cee-water/cjgleason/colin/BUSBOI/BUSBOI/channel_examples/',
                                        this_reach_id,'initial_channel.rds'))

#------------------------------------------------------------------------------------
    #OK!!! following meeting with Kostas on 2/27, let's do a two step solution. So we just optimized a bed and Q
    #that minimizes errors in water levels. However, if you look at the surfces, it is clear that the Q could be refined
    #to better move the surfaces. so, we need to optimize again, this time AT EACH TIME INDPENDENTLY, given the bed
    #we just solved in the global setup.
    

    #the bed is the final set of bed points
    global_bed=optparams$par[2:(length(optparams$par))]

    #r is the first parameter
    global_r=optparams$par[1]

    #Q has been fixed so far
    global_Q=Q_init

    #we need to optimize for one time, so we need a new function that fixes the bed. the pars of this are just Q
    Qmin= Q_priors$lowerbound_Q
    Qmax= Q_priors$upperbound_Q
    
    optimize_one_Q=function( Q_initial,index, Qmin,Qmax, global_r,global_bed,hyperparams,plot_switch,this_reach_id,obj_error,
                            replacement_error,smooth_sf,Qpenalty, Sfpenalty,GVF_on,H_DS_init,fix_bed,tulip){
        
    
     opt_single_Q=optim(par=Q_initial, fn=jeff_calcHgivenparams_fixedbed,
                        index=index,
                      method='L-BFGS-B',
                      lower=Qmin, 
                      upper=Qmax,
                    #solver params from top level function
                     global_r=global_r,
                     global_bed=global_bed,
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

        return(opt_single_Q$par)
        } #end single function

    #loop over the Q values
    

    allQ=rep(NA,times=length(global_Q))

    # print(length(global_Q))
    # print(ncol(hyperparams$Hobs))
    # bonk

for (i in 1:length(global_Q)){
  
  
  allQ[i]=optimize_one_Q(Q_initial=global_Q[i],
                    index=i,      
                     Qmin=Qmin,
                     Qmax=Qmax,
                     global_r=global_r,
                     global_bed=global_bed,
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
  
  
}
   


    
    final_pars=c(global_r,allQ,global_bed)


    #plot final channel
    Hobs=hyperparams$Hobs
    db=hyperparams$db
    wb=hyperparams$wb
    n=hyperparams$n
    r=global_r
    sample_Zo=global_bed
    Q_est=allQ

    nx=nrow(Hobs)
    nt=ncol(Hobs)
  #re interp the bed  
 Zo_ds=approx(sample_x,sample_Zo ,xout=new_x ,method = "linear")$y
    
 Sf=apply(Hobs,2,Slope_empirical,chainage=new_x)
      #this comes out as one element shorter than the original one
       #handle zeroes
    Sf[Sf==0]=1e-6

    #smooth that Sf
    if(smooth_sf==1){
        Sf_smooth=mean(Sf,na.rm=TRUE)
        }
    else{
        Sf_smooth=Sf
    }

    #excepton handle
    Sf_smooth[Sf_smooth=='NaN']=NA

    #set an adverse slope penalty
    #usually 0, but capability is here
    if(any(Sf_smooth<0,na.rm=TRUE)){
         Sf_smooth[Sf_smooth<0]=1e-6
     }
    
     #we need an H estimate that is a function of Q
        Zo_mat=matrix(rep(Zo_ds,times=nt),nrow=nx,ncol=nt)
        Q_mat=matrix(rep(Q_est,times=nx),nrow=nx,ncol=nt,byrow=TRUE)
    
        exponent=1/(1.66+(1/r))
        a=Q_mat
        b=(Sf_smooth)^-0.5
        c=db^(1/r)
        d=n
        e=wb^-1
        f=(r/(r+1))^(-5/3) 
       
        H_est=((a*b*c*d*e*f)^exponent)+Zo_mat
        H_est[is.infinite(H_est)]=NA
        H_est[H_est=='NaN']=NA

            plotlist=list()
            sequence=floor(seq(from=1,to=nt,length.out=15))
            count=0
            for (index in sequence){
                count=count+1
    
                         plotter=data.frame(SWOT=Hobs[,index],Estimated=H_est[,index],
                                   Zo_ds=Zo_ds,new_x=new_x)%>%
                    gather(source,height,-new_x)
                        
 
               
                plotlist[[count]]=plotter
           }#end loop
        


        saveRDS(plotlist,paste0('/nas/cee-water/cjgleason/colin/BUSBOI/BUSBOI/channel_examples/',
                                        this_reach_id,'final_channel.rds'))


    #the par variable is of the form (r, Q, Zo). Dimensions vary with hyperparameters
    # return(optsolution@solution) 
    return(final_pars)
    
    }