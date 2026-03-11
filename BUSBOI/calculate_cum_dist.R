calculate_cum_dist=function(sos,node_ids){
    library(geosphere)
  

    node_index=which(sos$nodes$node_id %in% as.numeric(node_ids) )

    #calculate a station vector
        x=sos$nodes$x[node_index]
        y=sos$nodes$y[node_index]
        sword_id=sos$nodes$node_id[node_index]


  
#some node issues on 17b
    if(length(x)==0){

        return('no nodes')
    }
   

distance=rep(0,times=(length(x)-1))
    
    for (i in 1:(length(x)-1)){
distance[i]=distm(rbind(c(x[i],y[i]),c(x[i+1],y[i+1])),fun = distHaversine)[1,2]
        
        }
    #first station is 0
    distance=c(0,distance)
    

     node_cum_dist =cumsum(distance)

    output=data.frame(xs_id=as.character(sword_id),
                      stationvec=node_cum_dist)


       return( output)
    
    }