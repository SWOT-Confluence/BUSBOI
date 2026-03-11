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
            paramsODE$So=So
            #loop over all the columns to solve at each time t with the same bed and that times
            #qestimate
            solved_ode=list()
        

                stations = new_x #chainage for the other heights  
                paramsODE$timeindex=i
                paramsODE$all_stations=new_x
                paramsODE$Q_est=Q_est
                paramsODE$Zo_ds=Zo_ds

            #free fit, use the Q-estimated height
                    H_init=H_est[1,i]
                

                solved_ode= ode(y=H_init, times=stations, func=GVF,
                                     parms=paramsODE,method='ode45')  

             # where the first column is labelled 'time' but is really station
            # the second coumn is water surface elevation
 
        H_est=unname(as.matrix(H_temp[,2:ncol(H_temp)]))
       } #end GVF ON------------------------------------------

    #caculalte the error of the objective function

   
  
     
        H_tulip=jeff_tulip(H_est=H_est,
                           #need only this time
                           Hobs=Hobs[,index],
                           nx=nx,
                           errortype=obj_error,
                           replacement_error=replacement_error,
                           tulip=tulip)



    #joint error
    objective= Sfpenalty + Qpenalty + H_tulip #+ Sf_tulip +#= Sfpenalty 


  
    #if plot switch is 2, we return the bias
    if (plot_switch ==2){

       return(mean(Hobs,na.rm=TRUE)-mean(H_est,na.rm=TRUE))
           
        }

    #this just makes plots so we can do science diagnosis
        if(plot_switch==1){

         
            
 
                plotter=data.frame(SWOT=Hobs[,index],Estimated=H_est,
                                   Zo_ds=Zo_ds,new_x=new_x)%>%
                    gather(source,height,-new_x)
                        
               p1= ggplot(plotter)+
                   geom_point(aes(x=new_x,y=height,col=source))+
                    scale_color_manual(values=c('magenta','blue','red'))+
              
                     annotate('text',x=min(new_x,na.rm=TRUE),y=(0.98*max(Hobs[,index],na.rm=TRUE)),
                             label=paste("obj. error=",round(objective,digits=2),"m"),hjust=0)+
    
                    annotate('text',x=min(new_x,na.rm=TRUE),y=(1.01*max(Hobs[,index],na.rm=TRUE)),
                             label=paste("Q est.=",round(Q_est[index],digits=2),"m3/s"),hjust=0)
    
    
               
    
  
           
              print(p1)
            
            bonk #will kill the code!
    
            } #end plotswitch

return(objective)
}#end function
