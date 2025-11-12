    
Slope_empirical=function(series,chainage){

    #forward finite difference approximation of slope
        FFD=c(series,0)-c(0,series)
        FFD=FFD[2:(length(FFD)-1)]
        
        FFD_chainage=c(chainage,0)-c(0,chainage)
        FFD_chainage=FFD_chainage[2:(length(FFD_chainage)-1)]
        
        So=FFD/FFD_chainage
    
        #this comes out as one element shorter than the original one
        pad1=So[1]
        So=c(pad1,So)
        
    }