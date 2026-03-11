read_busboi=function(path,gauge_df,n_pairs,gauge_id){

 

source('/nas/cee-water/cjgleason/colin/analyze confluence runs/make_stat_df.R')

    
ID=paste(strsplit(path,'/')[[1]][length(strsplit(path,'/')[[1]])],gauge_id)



hydrograph=bind_rows(lapply(list.files(path,full.names=TRUE),readRDS))%>%
    filter(!is.na(BUSBOI_Q))


if(any(grepl('neobam',names(hydrograph))==TRUE)){
    hydrograph=hydrograph%>%
        mutate(BUSBOI_Q=neobam_Q)%>%
        select(-neobam_Q)
    }
    
hydrograph=hydrograph%>%
    mutate(Q=BUSBOI_Q)%>%
    select(-BUSBOI_Q)%>%
    mutate(ID=ID)%>%
    filter(!is.na(Q))%>%
    mutate(date=as.Date(date))

    #only one per reach
params= select(hydrograph,reach_id,r,bed)%>%
            unique()

    #just the Q variables
hydrograph=hydrograph%>%
    select(Q,ID,reach_id,date,prior_Q)

stats=make_stat_df(hydrograph,gauge_df,n_pairs)%>%
    ungroup()%>%
    mutate(n=length(unique(reach_id)))

    return(list('hydrograph'=hydrograph,
                'stats'=stats))

    }