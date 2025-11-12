calc_newH=function(height_in,chainage,num_nodes_to_invert){

    #mini function
    calc_newSf=function(height_in,chainage){
   #reading in the output from the last function
    heightfit=lm(height_in~chainage)
    new_Sf=heightfit$coefficients[2]
    standard_error=summary(heightfit)$coefficients[1,2]

    return(list('Sf'=new_Sf,
               'standard_error'=standard_error))
    }

    library(zoo)
    
    x=chainage
    y=height_in

    #for plotting/debugging
    ogx=x
    ogy=y

    #get y values that have data
    good_data=which(!is.na(y))


    #run the spline fit, error handle
    spline_outputs<- tryCatch(
      {
        # Code that might produce a warning
        spline_outputs=calculate_spline(x,y)
      },
      warning = function(w) {
        # Code to execute when a warning occurs
      spline_outputs=list('new_Hobs'='dim_mismatch',
                 'new_x'='dim_mismatch')
      }
    )
    #error handle
   if(typeof(spline_outputs$new_Hobs[1])=='character'){
         return(list('new_Hobs'=rep(-9999,num_nodes_to_invert),
                 'new_x'=rep(-9999,num_nodes_to_invert)))
        }


    #new x and y outputs from the spline
    new_x=spline_outputs$new_x
    new_H=spline_outputs$new_H

    # sometimes x and new x (and therefore new H) are different lengths
    x_index = which(new_x %in% x)
    new_x=new_x[x_index]
    new_H=new_H[x_index]
    y=y[x_index]
    x=x[x_index]


    #force a sort, now that we have aligned the new and old chainage vectors
     sorted_x=sort(x,index.return=TRUE)
     x=x[sorted_x$ix]
     y=y[sorted_x$ix]

    #calculate the error to the spline
    errors=new_H-y

    #what happens if we take out >50cm errors and redo the spline?
    big_errors=which(errors>0.25)
    x2=x
    y2=y

    y2[big_errors]=NA

    #there might be a bad fit
    if(sum(!is.na(y2) )< num_nodes_to_invert) {
       return(list('new_Hobs'=rep(-9999,num_nodes_to_invert),
                 'new_x'=rep(-9999,num_nodes_to_invert)))
        }


    #2nd spline, error handled
    spline_outputs2<- tryCatch(
      {
        # Code that might produce a warning
        spline_outputs2=calculate_spline(x2,y2)
      },
      warning = function(w) {
        # Code to execute when a warning occurs
       
      spline_outputs2=list('new_Hobs'='dim_mismatch',
                 'new_x'='dim_mismatch')
      }
    )

   if(typeof(spline_outputs2$new_Hobs[1])=='character'){
         return(list('new_Hobs'=rep(-9999,num_nodes_to_invert),
                 'new_x'=rep(-9999,num_nodes_to_invert)))
        }

    #as before
    new_x2=spline_outputs2$new_x
    new_H2=spline_outputs2$new_H

    # sometimes x and new x (and therefore new H) are different lengths
    x_index2 = which(new_x2 %in% x)
    new_x2=new_x2[x_index2]
    new_H2=new_H2[x_index2]
    y2=y2[x_index2]
    
    final_error=new_H2-y2

    ###toggle this on to check the plots
    # par(bg='white')
    # plot(ogx,rev(ogy))
    # points(new_x,new_H,col='red')
    # points(new_x2,new_H2,col='blue')
    # bonk


   #get the overall error of the 2nd spline
   second_rmse=sqrt(mean(final_error^2,na.rm=TRUE))

    #there might be a bad fit
   if(is.na(second_rmse)){
       return(list('new_Hobs'=rep(-9999,num_nodes_to_invert),
                 'new_x'=rep(-9999,num_nodes_to_invert)))
        }
    
    if(second_rmse>1){
       return(list('new_Hobs'=rep(-9999,num_nodes_to_invert),
                 'new_x'=rep(-9999,num_nodes_to_invert)))
        }

    #finally, when height is flat with a ton of scatter, the spline makes a parabola
    #the RMSE of that parabola is quite low, so we don't catch it above

    #check the monotonicity
    FFD= sign(c(0,new_H2)- c(new_H2,0))
    percent_increase= sum(FFD==-1,na.rm=TRUE)/sum(!is.na(FFD))

        if(is.na(percent_increase)){
       return(list('new_Hobs'=rep(-9999,num_nodes_to_invert),
                 'new_x'=rep(-9999,num_nodes_to_invert)))
        }

        #there might be a bad fit
    if(percent_increase<0.85){
       return(list('new_Hobs'=rep(-9999,num_nodes_to_invert),
                 'new_x'=rep(-9999,num_nodes_to_invert)))
        }

    ##important##---------
    #this little line of code caculates the new Sf of the final surface
    Sobs=calc_newSf(new_H2,new_x2)$Sf
    ###-------------------

    return(list('new_Hobs'=new_H2,
                 'new_x'=new_x2,
               'Sobs'=Sobs))
    }


