
get_Q_prior=function(sos_file,reach_id_in,times){

    
    #read the SoS
    sos_in=open.nc(sos_file)
    sos=read.nc(sos_in,recursive=TRUE)
    close.nc(sos_in)

  # sos comes in by continent. Need to filter to this reach
    sos_reach_index=which(sos$reaches$reach_id == reach_id_in)

    #comes in as a vector 1:12
    Q_hat_df= data.frame(Q_hat=sos$model$monthly_q[,sos_reach_index], month=as.numeric(1:12))

    #change the date to a datetime object 
    #time is nx by nt, we want an nt vector only
    obs_date= as.Date(times)

    #make it a month
    obs_month=data.frame(month=as.numeric(format(obs_date,'%m')))
      
    #now join the Qhat_df by month
    Q_hat_month=left_join(obs_month,Q_hat_df,by='month')
    
    #some of these will be blank, so we need to drop the NAs in order to make the data work out
    Q_hat_month$Q_hat[is.na(Q_hat_month$Q_hat)]=mean(Q_hat_month$Q_hat,na.rm=TRUE)

    #get min and max
    minQ=0.6*sos$model$min_q[sos_reach_index]
    maxQ=1.4*sos$model$max_q[sos_reach_index]

    #there is an IDF curve, we can read Qsd from that
    sigma_index=which(sos$model$probability==66)
    Qsd=sos$model$flow_duration_q[,sos_reach_index][sigma_index]
    
    #exception handle
        if(is.na(minQ)){minQ=0.1*min(Q_hat_month)}
        if(is.na(maxQ)){maxQ=50*max(Q_hat_month)}
        if(is.na(Qsd)){Qsd=sd(Q_hat_month$Q_hat_month)}
        #if all the same, still NA
        if(is.na(Qsd)){Qsd=min(Q)}


    #retun these 
    return(list(Q_hat=Q_hat_month$Q_hat,
                lowerbound_Q=minQ,
                upperbound_Q=maxQ,
                Q_sd=Qsd
                 ))

    }