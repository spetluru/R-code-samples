best <- function(state, outcome) {
## Read outcome data
dat<- read.csv("outcome-of-care-measures.csv", colClasses = "character")

## Check that state and outcome are valids!
	##if(outcome!="heart attack"|outcome!="heart failure"|outcome!="pneumonia") stop("invalid outcome")
	if(state%in%dat$State==FALSE) stop("invalid state")
  if(outcome=="heart attack")
  sub<-dat[dat$State==state,c(2,7,11)]
  else if(outcome=="heart failure")
    sub<-dat[dat$State==state,c(2,7,17)]
  else if(outcome=="pneumonia")
    sub<-dat[dat$State==state,c(2,7,23)]
  else stop("invalid outcome")
  sub[,3]<-as.numeric(sub[,3])
  bad<-is.na(sub[,3])
  fin<-sub[!bad,]
  bad<-min(fin[,3])
  x<-vector("character",)
  for(i in seq_len(nrow(fin)))
  {
    if(fin[i,3]==bad)
      x<-c(x,fin[i,1])
  }
  
 v<-sort(x)
 v
      
}
