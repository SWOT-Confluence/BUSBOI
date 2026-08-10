jeff_calcHgivenparams_bedonly= function(variables,
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

    #prior Q to calibrate bed
    Q_init=hyperparams$Q_init
    
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
        #r
            r=variables[1]
        #Q 
            Q_est=Q_init
        #bed
            # at 5 'kink points' 
            sample_Zo=variables[2:length(variables)] 
            # corresonding chainage
            sample_x=hyperparams$sample_x

    #define Sf for each time from the observations
    #FFD- this is an instantaneoulsy changing slope in space
    #smoothing it to average Sf is a hyperparameter, we will handle later
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
         Sfpenalty=Sfpenalty
     }
    
    #linearly interpolate between the 5 bed points to define the bed
    #which then grows to match new x and is therefore 50 points
    if (fix_bed ==0){ # 5 point bed solution
        #this approximates at the 5 points by making a piecewise linear fit
        Zo_ds=approx(sample_x,sample_Zo ,xout=new_x ,method = "linear")$y
    } else if (fix_bed ==1) {#linear bed from first point
        #sample Zo should ahve a dimension of 1, but leaving here anyway
       Zo_ds= sample_Zo[1] + mean(Sf_smooth,na.rm=TRUE)*new_x
    } else {#fix the bed
        #in this case, read the linear original slope directly and hold
        Zo_ds=hyperparams$Zo_ds
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

          #this just makes plots so we can do science diagnosis---------
         if(plot_switch==1){

         
            plotlist=list()
            sequence=floor(seq(from=1,to=nt,length.out=15))
            count=0
            for (index in sequence){
                count=count+1
    
                         plotter=data.frame(SWOT=Hobs[,index],Estimated=H_est[,index],
                                   Zo_ds=Zo_ds,new_x=new_x)%>%
                    gather(source,height,-new_x)
                      
               
                plotlist[[count]]=plotter
           
          }
            
           #returns this plot list
        return(plotlist)

             }

    #regardless of whether we have a free fit or a GVF fit, there are not always
    #hobs for all x. Since GVF needs to fit from downstream to upstream, we need
    #the starting node with data to be aware of 1) what the first point with data is
    #and 2) what the domain for the fit is.

    #since the error can be caluculated in the presence of NAs, the net effect is that
    #gvf will solve for the starting node and then continue to the end, regardless
    #of whether or not there is an observation there for error calculation

    #we have returned to the problem of the station vector that is not constant.

    #to solve this, we could make a concatentated pairwise column matrix where
    #each timestep (column) of Hobs has a 1:1 relationship between HObs and x. BUT~~~~
    #~~~~~~ that won't work, because we're trying to solve for a 5-point bed that covers the
    #entire chainage length.  so, we need a solution for when we want GVF that finds the first
    #value of x with H and then only estimates after that

    #then, there is a second GVF problem in that we want to be able to let the DS
    #coundary condition freesolve sometimes.

    H_DS_init='free'
    if(GVF_on==1){
        #given this bed (Zo_ds), chainage (new_x), and Q (Q_est), caclulate a
        #height profile from the most downstream node 
            paramsODE=list()
            paramsODE$r=r
            paramsODE$n=n
            paramsODE$wb=wb
            paramsODE$db=db
            paramsODE$r=r
            paramsODE$So=So
            #loop over all the columns to solve at each time t with the same bed and that times
            #qestimate
            solved_ode=list()
        
            for(i in 1:nt){
                stations = new_x #chainage for the other heights  
                paramsODE$timeindex=i
                paramsODE$all_stations=new_x
                paramsODE$Q_est=Q_est
                paramsODE$Zo_ds=Zo_ds

               if(H_DS_init=='fixed'){
                     H_init=Hobs[1,i]

                   #sometimes there are no data on the downstream most
                   #node...
                     if (is.na(H_init)){
                        first_H_index=first(which(!is.na(Hobs[,i])))
                         H_init=Hobs[first_H_index,i]
                         stations=stations[first_H_index:nrow(Hobs)]
                         }
                }else{ #free fit, use the Q-estimated height
                     H_init=H_est[1,i]
                }

                solved_ode[[i]]= ode(y=H_init, times=stations, func=GVF,
                                     parms=paramsODE,method='ode45')  

                # where the first column is labelled 'time' but is really station
                # the second coumn is water surface elevation
         

            }#end time loop
       # -----------------------------------    

        #since these are different numbers of stations in the 'fixed' case, we need a for loop
        #which i hate,but is best here

         H_temp=data.frame(station=solved_ode[[1]][,1],height=solved_ode[[1]][,2])
        
        for(i in 2:length(solved_ode)){
            tempdf=data.frame(station=solved_ode[[i]][,1],height=solved_ode[[i]][,2])
            H_temp=full_join(H_temp,tempdf,by='station')
            }
        #the names of this dataframe are not meaningful, nor is the first column (station) which is just there
        #to ensure the correct alignments
        H_est=unname(as.matrix(H_temp[,2:ncol(H_temp)]))
       } #end GVF ON------------------------------------------

    #caculalte the error of the objective function


     
        H_tulip=jeff_tulip(H_est=H_est,
                           Hobs=Hobs,
                           nx=nx,
                           errortype=obj_error,
                           replacement_error=replacement_error,
                           tulip=tulip)

    #penalize bias 
    mean_logQ_est=mean(log(Q_est),na.rm=TRUE)

    #from the original
    prior_mean_log_Q=hyperparams$logML_mean

    Q_tulip= jeff_tulip(H_est=mean_logQ_est,
                           Hobs=prior_mean_log_Q,
                           nx=nx,
                           errortype=obj_error,
                           replacement_error=replacement_error,
                           tulip=tulip)

    

    #joint error- not for bed only
    objective= H_tulip #+Q_tulip  #+ Sf_tulip 




 

return(objective)
}#end function