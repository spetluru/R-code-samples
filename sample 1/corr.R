corr <- function(directory,threshold = 0) {
	dir<-directory
         x<-complete(directory,1:332)
	 x<- x[x$nobs>threshold,]
         v<- vector()
	chrid<-as.character(x$id)
         if( dim(x)[1]==0)
         {
		vector(mode="numeric", length=0)
	}
      
      else
      { 
        j<-1
        for(i in x$id)
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
                y<- read.csv(chrid[i])
		y<-y[complete.cases(y),]
		v<-c(v,cor(y$sulfate,y$nitrate))
	   }
	v
	}
   
		
}
