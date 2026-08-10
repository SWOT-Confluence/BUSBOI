
fit_hydraulics=function(swot_file,sos_file,reach_id_in){

  # Libraries
    library(RNetCDF,quietly=TRUE,warn.conflicts = FALSE)
    library(purrr)

  # getSWOT
    swot_in=open.nc(swot_file)
    swot_data=read.nc(swot_in,recursive=TRUE)
    close.nc(swot_in)
    
  # Get SOS
    sos_in=open.nc(sos_file)
    sos=read.nc(sos_in,recursive=TRUE)
    close.nc(sos_in)

  # sos comes in by continent. Need to filter to this reach
    sos_reach_index=which(sos$reaches$reach_id == reach_id_in)
    sos_node_index=which(sos$nodes$reach_id == reach_id_in)

  # now get SWOT data
    obs_times=swot_data$reach$time_str

  # get nt, it is 0-based index, so we take the length for R, which is 1-based
    nt = length(swot_data$nt)
  # get nx
    nx = length(swot_data$nx)
  
    
  # node data are nt by nx
    node_wse=data.frame(matrix(swot_data$node$wse,nrow=nt,ncol=nx))
    node_width=data.frame(matrix(swot_data$node$width,nrow=nt,ncol=nx))

  # give them proper names
    node_ids=as.character(swot_data$node$node_id)
    names(node_wse)= node_ids
    names(node_width)= node_ids

  #reshape for later filtering
    node_wse_df=mutate(node_wse,time=obs_times)%>%
            gather(xs_id,node_wse,-time)
    node_width_df=mutate(node_width,time=obs_times)%>%
            gather(xs_id,node_width,-time)
   
  # to Obs matrices in expected format
    Hobs=select(node_wse_df,time,xs_id,node_wse)%>%
        #combos of NA time and NA width/height make the key value pair break
        pivot_wider(names_from=time,values_from=node_wse,values_fn=first)

  # save id column
    xs_ids=Hobs$xs_id

  # to obs matrix, for width
    Wobs=select(node_width_df,time,xs_id,node_width)%>%
         pivot_wider(names_from=time,values_from=node_width,values_fn=first)

  # get the chainage here, use the node ids in this order

   
    
    station_df=calculate_cum_dist(sos,xs_ids)
if(typeof(station_df)=='character'){return('no good')}
    chainage=station_df$stationvec

  # need to reorder the Hobs and Wobs matrices to match the chainage
    Hobs_station=left_join(Hobs,station_df,by='xs_id')%>%
        arrange(stationvec)%>%
        #drop the id and station columns
        select(-xs_id,stationvec)
    
    Wobs_station=left_join(Wobs,station_df,by='xs_id')%>%
        arrange(stationvec)

    Hobs=Hobs_station
    
  # num times to invert, mimimum
    num_times_to_invert =5

  # num nodesto invert, minimum
    num_nodes_to_invert =10

  # filter times without enough data
    num_nodes_with_data=apply(Hobs,2,function(x){sum(!is.na(x))})
    bad_times=which(num_nodes_with_data < num_nodes_to_invert)

    #R negative indexing removes
    Hobs=Hobs[,-bad_times]  

   if(ncol(Hobs)<num_times_to_invert){return('no good')}
   if(nrow(Hobs)<num_nodes_to_invert){return('no good')}


    #make an n-node spline
    newHX=apply(Hobs,2,calc_newH,num_nodes_to_invert=num_nodes_to_invert,
                     chainage=chainage,reach_id=reach_id_in,obs_date=names(Hobs))

    #sometimes none pass the spline-ing
    suppressWarnings({
   checkval=do.call(rbind,lapply(newHX,function(x){x$new_Hobs}))
        })
    if(all(checkval==-9999)){return('no good')}

    suppressWarnings({
        doit=function(x){
        if(length(x$Sobs)>0){
        data.frame(station=x$new_x,Hobs=x$new_Hobs)
            }
        }

    #this just aligns all of teh data on the same times    
    all_dfs=lapply(newHX,doit)
    index_to_keep=!unlist(lapply(all_dfs,is.null))
    all_dfs=all_dfs[index_to_keep]
    all_names=names(all_dfs)

    #joining them aligns them by station (node id)
    joined_df <- all_dfs %>%
      reduce(full_join, by = "station")
    # the name of the data frames are the times of the observations
    names(joined_df)=c('station',all_names)
    
     })

    #now, make variables
    Hobs=joined_df%>%
        select(-station)
    new_x=joined_df$station

    Sobs_list=do.call(rbind,lapply(newHX,function(x){x$Sobs}))
    Sobs_df=data.frame(date=rownames(Sobs_list),Sobs=Sobs_list)%>%
        transmute(date=date,Sobs=chainage)
    rownames(Sobs_df)=NULL

    #this is the data we have
    dates_we_have=names(Hobs)

    #reject if the matrix is too small
    if(ncol(Hobs)<num_times_to_invert){return('no good')}
    if(nrow(Hobs)<num_nodes_to_invert){return('no good')}

    #now, count the observations per done
    num_times_per_node=apply(Hobs,1,function(x){sum(!is.na(x))})
    ranked=sort(num_times_per_node)
    # if more than 50 nodes have enough time data
    #keep the 50 with the most obs
    if(length(ranked)>50){
        best_50th=ranked[length(ranked)-49]
        keep_index=which(num_times_per_node>=best_50th)
    }else{
        keep_index=1:nrow(Hobs)
    }

    #filter down
    Hobs=Hobs[keep_index,]
    new_x=new_x[keep_index]

    #we have a final set of times, which we use to name the obs
    times_we_have=names(Hobs)
    xs_we_have=xs_ids[keep_index]

    #filter width to match
    Wobs_keep_x=which(Wobs$xs_id %in% xs_we_have)
    Wobs=Wobs%>%select(-xs_id)
    
    Wobs_keep_y=which(names(Wobs) %in% times_we_have)

    #align, this shouldn't do anything, but will ensure 
    #we never bump a size error
    Wobs=Wobs[Wobs_keep_x,Wobs_keep_y]
    Hobs=Hobs[,which(names(Hobs) %in% names(Wobs))]

    #ok- often we have height but not width. So, we need to fill
    #the widths since we onlyuse it for rejection sampling
    all_na_widths=which(apply(Wobs,1,function(x){sum(is.na(x))})==ncol(Wobs))

    if(length(all_na_widths)==ncol(Wobs)){return('no good')}
    
    if(length(all_na_widths)>0){
        #replace
        for (i in all_na_widths){
            #the 't' makes this a row vector instead of a column vector
            #which R cares about deeply
       Wobs[i,]=t(abs(rnorm(ncol(Wobs), 
                                      mean=mean(as.matrix(Wobs),na.rm=TRUE),
                                      sd=sd(as.matrix(Wobs),na.rm=TRUE))))
                  }
        }


 
    #now, need to redefine the nx and nt for the problem
    nx=nrow(Hobs)
    nt=ncol(Hobs)

    #finally, go back to Sobs, which is uniform and derived from teh spline
    # to make it have the right dimensions. Since it is a repeating matrix
    # we can wait until now
    Sobs_df=Sobs_df%>%
        filter(date %in% names(Wobs))

    Sobs=matrix(rep(Sobs_df$Sobs,times=nx),nrow=nx,ncol=nt,byrow=TRUE)

    #this will just garble later joins
    rownames(Hobs)=NULL

    

    #these are our inputs
    return(list('Wobs'=Wobs,
                'Sobs'=Sobs,
                'Hobs'=Hobs,
                'obs_times'=names(Hobs),
                'xs_ids'=xs_ids,
                'new_x'=new_x,
                'nx'=nx,
                'nt'=nt))
    }

    
