


rankhospital <- function(state, outcome, num = "best") {


  dat<- read.csv("outcome-of-care-measures.csv", colClasses = "character")
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
   v<-data.frame(Hospital = character(),state=character(),mean=numeric(),rank=numeric())
  repeat
  { 
     x<-data.frame(Hospital = character(),state=character(),mean=numeric())
    bad<-min(fin[,3])
    if(bad==1000)
      break;
    for(i in seq_len(nrow(fin)))
    {
      if(fin[i,3]==bad)
      {
        x<-rbind(x,fin[i,])
         fin[i,3]<-1000
        
      }
    }
    v<-rbind(v,x)
  }
  v<-v[order(v[,3],v[,1]),]
  if(num=="best") num<-1
  else if(num=="worst") num<-nrow(v)
 v[num,1]
  
  #ft<-fin[order(fin[,3]),c(1,3)]
  
}
