jeff_tulip=function(H_est,Hobs,nx,errortype,
                   replacement_error,tulip){
    
    #squared error divided by variance of Hobs fit
    #we use this error as we've made our own heights
    # H_var=standard_error*sqrt(nx)


    #basic error
    if(errortype=='relative'){
        basic_error=100*((H_est-Hobs)/Hobs)
        True_var=sd(Hobs/mean(Hobs,na.rm=TRUE),na.rm=TRUE)^2
            #Jeff's logic, more or less
    #use the variance of the observations to set the threholds
    }
    
    if(errortype=='normalized'){
        basic_error=100*((H_est-Hobs)/mean(Hobs,na.rm=TRUE))
        True_var=sd(Hobs/mean(Hobs,na.rm=TRUE),na.rm=TRUE)^2
    }
    
      if(errortype=='absolute'){
        basic_error=(H_est-Hobs)
        True_var=sd(Hobs,na.rm=TRUE)^2

          # this should be downstream error
    }


    E2=basic_error^2
    rel_E2 =  E2/True_var 
    
    #when the non relative squared error is greater than the variance,
    #we want to penalize that
    replace_index = which(E2 > True_var)

    #replace the rel_E2 with its log in this case
    #if the E2 is greater than the variance, then e2/sig2 is greater
    #than 1, so this should be all postive
    rel_E2[replace_index]= log(rel_E2[replace_index])
    
    #now we have an error, take the RMSE
    objective= sqrt(mean(rel_E2,na.rm=TRUE))
    #this is in units of E/sigma

    if(tulip=='ON'){
        objective_m= objective*sqrt(True_var)}
    else{
         objective_m= sqrt(mean(E2,na.rm=TRUE))
        }

    #sometimes the objective is inf or NA
    

    return(objective_m)
    
    }