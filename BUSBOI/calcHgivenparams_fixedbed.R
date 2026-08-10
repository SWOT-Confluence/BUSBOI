#function to run the optimization on Q only

jeff_calcHgivenparams_fixedbed= function(variables,
                                         index,
                                         global_r,
                                         global_bed,
                           hyperparams,
                           plot_switch,
                           this_reach_id,
                          obj_error,
                          replacement_error,
                          smooth_sf,
                          Qpenalty,
                          Sfpenalty,
                          GVF_on,
                          H_DS_init,
                          fix_bed,
                               tulip){

    
    

    #height as fit to the obs
    Hobs=hyperparams$Hobs #nx by nt

   
    
    #only for plotting purposes
    OGheight=hyperparams$OGheight
    OGchainage=hyperparams$OGchainage
   
    #fixed parameters from teh rejection sampling
    db=hyperparams$db
    wb=hyperparams$wb
    n=hyperparams$n

    #the equally spaced 50 point station vector that defines where the surface exists
    new_x=hyperparams$new_x
    
    #control parameters
    nx=nrow(Hobs)
    nt=ncol(Hobs)

    #from the solver parameters:
    Q_est=variables

    #from the global soluution r
    r=global_r
    
    #from global solution 5 points for bed
    sample_Zo=global_bed
    #from hyperparams as before
    sample_x=hyperparams$sample_x
    
   

    Zo_ds=approx(sample_x,sample_Zo ,xout=new_x ,method = "linear")$y
     
    #define Sf for each time from the observations
    #FFD- this is an instantaneoulsy changing slope in space
    #smoothing it to average Sf is a hyperparameter, we will handle later
    Sf=Slope_empirical(Hobs[,index],chainage=new_x)
    #take a mean SF for the whole timestep
    Sf_smooth=mean(Sf,na.rm=TRUE)
    
   
   
  

    #set an adverse slope penalty
    #usually 0, but capability is here
    if(any(Sf_smooth<0,na.rm=TRUE)){
         Sf_smooth[Sf_smooth<0]=1e-6
         Sfpenalty=Sfpenalty
     }
    
    #define So from the bed
    #since this is piecewise lienar, the slope is well behaved
    #and we don't want a single uniform value
    So=Slope_empirical(Zo_ds,chainage=new_x)
     #handle zeroes
    So[So==0]=1e-6
    if(any(So<0,na.rm=TRUE)){
         So[So<0]=1e-6
         Qpenalty=Qpenalty
     }

    #we need an H estimate that is a function of Q, otherwise we can fit too well if we use Hobs as DS boundary
    #simply solve the flow law given Q

    #vector, not matrix, for space only problem
    
 
        Zo_mat=Zo_ds
        Q_mat=rep(Q_est,times=nx)
    
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

  

    #caculalte the error of the objective function
        H_tulip=jeff_tulip(H_est=H_est,
                           #need only this time
                           Hobs=Hobs[,index],
                           nx=nx,
                           errortype=obj_error,
                           replacement_error=replacement_error,
                           tulip=tulip)



    #joint error- Q and SF penalties are set to 0, making this unecessary
    objective= Sfpenalty + Qpenalty + H_tulip #+ Sf_tulip +#= Sfpenalty 



return(objective)
}#end function
