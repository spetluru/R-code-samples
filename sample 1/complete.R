complete <- function(directory,id = 1:332) {
        ## set working directory, here we append to the name
        directory<-(paste(getwd(),"/",directory,sep=""))
        chrid<-as.character(id)
        j<-1
        dat <- data.frame(id= integer(0), nobs= integer(0))
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
	for(i in 1:j)
          {
                x<- read.csv(chrid[i])
		x<-x[complete.cases(x),]
		dat<-rbind(dat,data.frame(id = mean(x$ID), nobs = dim(x)[1]))
           }
           dat

}
