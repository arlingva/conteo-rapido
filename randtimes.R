

rand.date=function(start.time,end.time, size){   
  size=size
  times=seq(from=as.POSIXct(start.time), 
            to=as.POSIXct(end.time), by="min")  
  pick.times=runif(size,1,length(times))  
  date=times[pick.times]  
}

Times=rand.date("2021-06-06 18:00:00","2021-06-07 08:00:00", 380)


data.frame(Times)
