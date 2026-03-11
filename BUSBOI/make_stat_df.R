make_stat_df=function(FLPE,gauge_df,min_pairs){


joined_df=left_join(FLPE,gauge_df,by=c('reach_id','date'))%>%
    distinct()%>%
    filter(!is.na(gauge_Q))%>%
    filter(gauge_Q!=0)%>%
    filter(!is.na(Q))

if("prior_Q" %in% colnames(joined_df)){
    prior_df=joined_df%>%
        mutate(Q=prior_Q)%>%
        select(-prior_Q)

    joined_df=joined_df%>%
        select(-prior_Q)
  
}

stat_df=joined_df%>%
    group_by(reach_id)%>%
    #calculate the number of pairs
    mutate(npairs=n())%>%
    #stats need at least 20 pairs
    filter(npairs>=min_pairs)%>%
    #calculate nbias
    mutate(nbias=abs(mean(gauge_Q-Q)/mean(gauge_Q)))%>%
    #calculate r
    mutate(r=cor(gauge_Q,Q,method='pearson'))%>%
    #calculate NSE
    mutate(NSE=NSE(Q,gauge_Q))%>%
    mutate(KGE=KGE(Q,gauge_Q))%>%
    # calculate 1 sigma, instructions from Mike
        # •	Calculate the error e=Qhat-Qgauge 
        # •	Take the absolute value of the error |e|
        # •	Take the 67th percentile of |e|
        # •	Divide by the average of the gauge discharge.
    mutate(sigE= quantile(abs(gauge_Q-Q),probs=0.68)/mean(gauge_Q))%>%
    #luisa
    mutate(normalized_MAE= mean(abs(gauge_Q-Q))/mean(gauge_Q))%>%
    mutate(normalized_median_error= median(gauge_Q-Q)/mean(gauge_Q))%>%
    mutate(RMSE= sqrt(mean((gauge_Q-Q)^2)))%>%
    mutate(rRMSE= sqrt(mean(((gauge_Q-Q)/gauge_Q)^2)))%>%
    mutate(ID2='algo')%>%
    
    select(reach_id,nbias,r,NSE,KGE,ID,ID2)%>%
    #get rid of repeats, since calculation is made for all times and number is repeated
    distinct()

if(exists('prior_df')){
prior_stat_df=prior_df%>%
    group_by(reach_id)%>%
    #calculate the number of pairs
    mutate(npairs=n())%>%
    #stats need at least 20 pairs
    filter(npairs>=min_pairs)%>%
    #calculate nbias
    mutate(nbias=abs(mean(gauge_Q-Q)/mean(gauge_Q)))%>%
    #calculate r
    mutate(r=cor(gauge_Q,Q,method='pearson'))%>%
    #calculate NSE
    mutate(NSE=NSE(Q,gauge_Q))%>%
    mutate(KGE=KGE(Q,gauge_Q))%>%
    # calculate 1 sigma, instructions from Mike
        # •	Calculate the error e=Qhat-Qgauge 
        # •	Take the absolute value of the error |e|
        # •	Take the 67th percentile of |e|
        # •	Divide by the average of the gauge discharge.
    mutate(sigE= quantile(abs(gauge_Q-Q),probs=0.68)/mean(gauge_Q))%>%
    #luisa
    mutate(normalized_MAE= mean(abs(gauge_Q-Q))/mean(gauge_Q))%>%
    mutate(normalized_median_error= median(gauge_Q-Q)/mean(gauge_Q))%>%
    mutate(RMSE= sqrt(mean((gauge_Q-Q)^2)))%>%
    mutate(rRMSE= sqrt(mean(((gauge_Q-Q)/gauge_Q)^2)))%>%

    mutate(ID2='prior')%>%
    
    select(reach_id,nbias,r,NSE,KGE,ID,ID2)%>%
    #get rid of repeats, since calculation is made for all times and number is repeated
    distinct()

    stat_df=rbind(stat_df,prior_stat_df)
}

      

    return(stat_df)
}