complete <- function(dir, id = 1:332) {
  
  r<-1
  m<-matrix(0,length(id),2)
  for(i in id){
    idc<-sprintf("%03.0f",i)
    filename<-paste(dir,'/',idc,".csv",sep='')
    x<-read.csv(filename)
    count<-0
    for(j in 1:nrow(x)){ 
      if(!is.na(x[j,2]) && !is.na(x[j,3])){
        count<-count+1
      }
    }
    m[r,1]<-i
    m[r,2]<-count
    r<-r+1
    
  }
m<-data.frame(m)  
colnames(m)<-c("id","nobs")
m
}