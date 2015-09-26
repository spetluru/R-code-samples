pollutantmean <- function(directory, pollutant, id = 1:332) {
        ## set working directory, here we append to the name
        directory<-(paste(getwd(),"/",directory,sep=""))
        chrid<-as.character(id)
        good<-vector()
        j<-1
        for(i in id)
        {
            if(i<10)
              chrid[j]<-paste(directory,"/00",i,".csv",sep="")
            else if(i<100)
		chrid[j]<-paste(directory,"/0",i,".csv",sep="")
            else
		chrid[j]<-paste(directory,"/",i,".csv",sep="")
	    j<-j+1
        }
        j<-j-1
        if(pollutant=="sulfate")
        {
           for(i in 1:j)
          {
                x<- read.csv(chrid[i])
                good<-c(good,x$sulfate[complete.cases(x$sulfate)])
           }
        }
        else if(pollutant=="nitrate")
        {
           for(i in 1:j)
          {
                x<- read.csv(chrid[i])
                good<-c(good,x$nitrate[complete.cases(x$nitrate)])
           }
        }
        mean(good)
}
      
