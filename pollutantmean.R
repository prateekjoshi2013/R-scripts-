pollutantmean<- function(dir,pol,id=1:332){
  l1<-NA
  for(i in id){
    idc<-sprintf("%03.0f",i)
    filename<-paste(dir,'/',idc,".csv",sep='')
    x<-read.csv(filename)
    l2<-x[,pol]
    l1<-c(l1,l2)
  }
  
  m<-mean(l1,na.rm=TRUE)
  noquote(sprintf("%1.3f",m))
}