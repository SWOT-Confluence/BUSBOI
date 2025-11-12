GVF=function(station,State,Pars){
    

    #time is really station vector (x), we're solving dh/dx not dh/dt
    #state is the height at any given station (H(x))

    with(as.list(c(State,Pars)),{
        
        #get the index of the clsoest station
        #somtimes there are two of equal distance!
        spaceindex=first(which.min(abs(all_stations  - station)) )
        
        #get the So from this bed estimate
        So= So[spaceindex]

        #get the Q from this time
        Q_est=Q_est[timeindex]
        
        H_est=State
        
        #solve the dingman eq for Sf!!
        a=(H_est-Zo_ds[spaceindex])^((5/3)+(1/r))
        b=Q_est^-1
        c=db^(-1/r)
        d=n^-1
        e=wb
        f=(r/(r+1))^(5/3)
        
        Sf_est=((a*b*c*d*e*f)^-2)

        #get the froude number
        fr= Q_est/( wb*( db^(1/r) )*( (r/(r+1))^(3/2)  )*( (H_est-Zo_ds[spaceindex])^((3/2)+(1/r))  ) * sqrt(9.81) )

        #if any instablitiy arises, switch to uniform flow
        if (is.infinite(Sf_est)){
                 Sf_est=0
                dhdx=list('dhdx'=So)
            } else if (is.na(Sf_est)){ 
                 Sf_est=0
                dhdx=list('dhdx'=So)
            } else if (Sf_est=='NaN'){ 
                Sf_est=0
                dhdx=list('dhdx'=So)
            } else if (abs(Sf_est)>0.001) {
                Sf_est=0
                dhdx=list('dhdx'=So)
            } else {
        #full Froude
        slopediff=Sf_est-So
        dhdx=list('dhdx'=(Sf_est-So)/(1-fr^2))
        #unless the numerator is negative
            if(slopediff < -1e-4){
                dhdx=list('dhdx'=So)
            }
             
        }#end of exception handles

        #final exception handle
        if (is.null(dhdx)){
            dhdx=list('dhdx'=So)
        }
        
      return(dhdx)
    })
}



