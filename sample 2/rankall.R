rankall <- function(outcome, num = "best") {
## Read outcome data
dat<- read.csv("outcome-of-care-measures.csv", colClasses = "character")
## Check that state and outcome are valid
if(outcome=="heart attack")
  sub<-dat[,c(2,7,11)]
  else if(outcome=="heart failure")
    sub<-dat[,c(2,7,17)]
  else if(outcome=="pneumonia")
    sub<-dat[,c(2,7,23)]
  else stop("invalid outcome")
sub[,3]<-as.numeric(sub[,3])
k<-data.frame(hospital = character(),state=character(),mean=numeric(),rank=numeric())
## For each state, find the hospital of the given rank
for(j in unique(sub$State))
{
	ss<-sub[sub$State==j,]
	bad<-is.na(ss[,3])
  fin<-ss[!bad,]
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
  if(num>nrow(v))
  {
     num<-nrow(v)+1
     v[num,1]<-NA
     v[num,2]<-v[j,2]
     v[num,3]<-NA
  }  
 k<-rbind(k,v[num,])
## Return a data frame with the hospital names and the
## (abbreviated) state name
}
s<-k[,c(1,2)]
s
}
