
rejection_sample_single_node=function(seed,H,W){

    H=unlist(H)
    W=unlist(W)

 #all base R functions
# for(i in 1:1000){
    #sample an r, bankfull depth
    r= abs(rnorm(1, mean = 1, sd = 1))
    db=abs(rnorm(1, mean = 1, sd = 1))
    #rescale to truncated normal
        x1=0 # not exactly
        x2=3 # not exactly
        y1=2
        y2=20
        m=(y2-y1)/(x2-x1)
        b=y1-m*x1
    db = db*m +b
    
    #wb won't go negative, but just in case
    wb=abs(rnorm(1, mean=max(W*1.2,na.rm=TRUE),sd=sd(W,na.rm=TRUE)))

    #by definition:
    #max depth is strictly less than bankfull.   

    #max depth given these parameteres is defined as:
    max_depth=db*(W/wb)^r


    #given the observations of width, are all the guesses at parameters valid?
    if(all(max_depth<db,na.rm=TRUE)){
       #now, we can calibrate zo to match ovserved heights
        Zo= H-max_depth

        #since this is a single cross section, then in theory the variation of Zo should be quite small.
        #this is different than saying the max depth is constant, since that needs to trade with H
        #test some meaningful value.
        #if we use SWOT uncertainties of W of about 30% and height of 11cm, we can say the level to which
        #this is resolved
        #the width error doens't necessarily dominate
        width_error=((mean(W,na.rm=TRUE)*0.3)^r)*(db/(wb^r))
        height_error=0.11

        if(is.na(sd(Zo,na.rm=TRUE))){
            return(data.frame(r=NA,
                          db=NA,
                          wb=NA,
                          Zo_min=NA,
                          Zo_max=NA,
                          Zo=NA))}

        #what we want is for the variability in bottom elevation
        #to be less than our combined error terms.
        #that is, below the limit we can actually resolve.
        if(sd(Zo,na.rm=TRUE) < width_error+height_error){

           
            return(data.frame(r=r,
                          db=db,
                          wb=wb,
                          Zo_min=min(Zo,na.rm=TRUE),
                          Zo_max=max(Zo,na.rm=TRUE),
                          Zo=mean(Zo,na.rm=TRUE)))
            # count=count+1
            # save[[count]]=list('r'=r,'db'=db,'wb'=wb,'Zo'=mean(Zo,na.rm=TRUE))
        }else{return(data.frame(r=NA,
                          db=NA,
                          wb=NA,
                          Zo_min=NA,
                          Zo_max=NA,
                          Zo=NA))}
        
    }else{return(data.frame(r=NA,
                          db=NA,
                          wb=NA,
                          Zo_min=NA,
                          Zo_max=NA,
                          Zo=NA))}

}

do_rejection_sampling=function(cleaned_data){

    
    H=cleaned_data$Hobs
    W=cleaned_data$Wobs


   # if(is.na(mean(W,na.rm=TRUE))){
   #           return(list('validpriors'=FALSE,
   #          'nodepriors'=NA,'good_nodes'=NA))}

    # library(parallel)
    minifunc=function(row_index,H,W){
        library(dplyr)

        output=do.call(rbind,lapply(1:500,rejection_sample_single_node,
                                    H=H[row_index,],W=W[row_index,]))%>%
        mutate(node_id=row_index)%>%
            filter(!is.na(r))

    }


    all_nodes=do.call(rbind,lapply(1:nrow(H),
                                      minifunc,
                                      H=H,
                                      W=W)) 


   #since we filter for NA in the minifunction, we are left with a 
    #reduced lsit of IDs
    good_nodes=unique(all_nodes$node_id)
   
    #check how many have good physical systems
    physical_pass_rate=length(good_nodes)/cleaned_data$nx

    # #we might have jettisoned some nodes
    # cleaned_data$Hobs=cleaned_data$Hobs[good_nodes,]
    # cleaned_data$Wobs=cleaned_data$Hobs[good_nodes,]
    # cleaned_data$Sobs=cleaned_data$Hobs[good_nodes,]

         if(nrow(cleaned_data$Hobs)<10){
             return(list('validpriors'=FALSE,
            'nodepriors'=NA,'good_nodes'=good_nodes))}
   
    #what we have now is pretty dope! we have a node-by-node channel system that guarantees a few things:

    #1. bankfull depth is always greater than the instantaneous max depth
    #2. the bed elevation is always within error of the height and width obeservations
    #3. r, db, wb, and zo all exist in a physically plausible hydraulic system per node

    #generate priors per node
    nodepriors=all_nodes%>%
        group_by(node_id)%>%
        summarize(rsd=sd(r,na.rm=TRUE),
                  dbsd=sd(db,na.rm=TRUE),
                  wbsd=sd(wb,na.rm=TRUE),
                  Zosd=sd(Zo,na.rm=TRUE),

                  r=mean(r,na.rm=TRUE),
                  db=mean(db,na.rm=TRUE),
                  wb=mean(wb,na.rm=TRUE),
                  Zo=mean(Zo,na.rm=TRUE),

                  Zo_min=min(Zo_min,na.rm=TRUE),
                  Zo_max=max(Zo_max,na.rm=TRUE),
                 
                  n=n())

    #the sampling tells you how many nodes had a lot of good solutions
    #small numbers mean we'll be chasing shadows later.
    
     percent_samples_less_than_20= sum(nodepriors$n>20)/nrow(nodepriors)
        # if(percent_samples_less_than_20<percent_solutions_pass_rate){
        #      return(list('validpriors'=FALSE,
        #     'nodepriors'=NA))}
    
    #this throws some NA values sometimes. catch
    nodepriors$r[is.na(nodepriors$r)]=mean(nodepriors$r,na.rm=TRUE)
    nodepriors$db[is.na(nodepriors$db)]=mean(nodepriors$db,na.rm=TRUE)
    nodepriors$wb[is.na(nodepriors$wb)]=mean(nodepriors$wb,na.rm=TRUE)
    nodepriors$Zo[is.na(nodepriors$Zo)]=mean(nodepriors$Zo,na.rm=TRUE)
    nodepriors$rsd[is.na(nodepriors$rsd)]=mean(nodepriors$rsd,na.rm=TRUE)
    nodepriors$dbsd[is.na(nodepriors$dbsd)]=mean(nodepriors$dbsd,na.rm=TRUE)
    nodepriors$wbsd[is.na(nodepriors$wbsd)]=mean(nodepriors$wbsd,na.rm=TRUE)
    nodepriors$Zosd[is.na(nodepriors$Zosd)]=mean(nodepriors$Zosd,na.rm=TRUE)


        return(list('validpriors'=TRUE,
            'nodepriors'=nodepriors, 'good_nodes'=good_nodes))
   

        
}
    
    
