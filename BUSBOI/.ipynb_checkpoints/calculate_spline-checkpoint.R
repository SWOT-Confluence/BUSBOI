calculate_spline=function(chainage,height_in){

        library(zoo)

#the heights change with each time, and are on different x values,
    #with NA's padding values where there are no data for a given station
    x=chainage
    y=height_in

    #sort this mother right off the bat
    #for reasons that are opaque, the chainage is backwards sometimes. This is a known
    #SWORD error in many cases. We can treat it empirically

    #fit a linear model, and make sure the slope is POSITIVE
    testslope= lm(y~x)$coefficients[2]

    #does it go 'uphill' from station 0 to station end?
    #it could be flat, and in that case neg might not be tue
    if (testslope < 0  & abs(testslope) > 1e-6){
     
        #sort x, get the positions
    sorted_x=sort(x,index.return=TRUE)

     #this doesn't actually change anything, but gets
     #x and y into sorted vectors that follow teh same exact
     #ordering
     x2=x[sorted_x$ix]
     y2=y[sorted_x$ix]

     #now, switch the order of y to be opposite with a base R function
     y3=rev(y2)

     #if we redo the linear model, we should get the same slope but positive
     testslope2=lm(y3~x2)$coefficients[2]

        # check1=round(testslope,digits=3)
        # check2=round(testslope2,digits=3)
        # print(check1)
        # print(check2)
        # print(check1 == -check2)

        x=x2
        y=y3

        }
    #there could be an issue here where e.g. one date was negative water surface but the others
    #were not, and we correctl only one. In theory, this flip should be applied to
    #all days.

    #the smooth.spline function creates the water surfaces we want, but it 
    #gives us a problem- it can't accept NA values. We can first interpolate
    #the NAs, but if they exist at the end or the start of teh reach, which is 
    #SUPER COMMON, then we'll end up with a flat line that hte splint will turn into
    #a parabola when it is done.

    #what we really need is to just fit the data within the range of y values
    #that we have and then add NAs later.
    
    na_y=which(is.na(y))
    not_na_y=which(!is.na(y))

   
    if (length(na_y)>0){
        x2=x[not_na_y]
        y2=y[not_na_y]

    #now we have a limited set of x and y data that form a surface to fit
    #that are continous in having data at every element 
    
    #if we fit this surface, it should be smooth.
    spline=smooth.spline(x=x2, y = y2,spar=0.95)
    spline_x=spline$x
    spline_H=spline$y

    #insert into x, at positions of not na_y, the value of the spline
    new_x = x
    new_x[not_na_y]=spline_x
    
    new_H=y
    new_H[not_na_y]=spline_H
 
    } else {
        
        x2=x
        y2=y
    #here we have complete data and fit the spline.
    spline=smooth.spline(x=x2, y = y2,spar=0.95)
    new_H=spline$y
    new_x=spline$x

    }

 

    #force a sort.
     sorted_x=sort(new_x,index.return=TRUE)

     #this doesn't actually change anything, but gets
     #x and y into sorted vectors that follow teh same exact
     #ordering
     new_x=new_x[sorted_x$ix]
     new_H=new_H[sorted_x$ix]

   

  return(list('new_Hobs'=new_H,
                 'new_x'=new_x))
}

