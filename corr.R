corr<-function(dir,thres=0){
  
  li<-complete(dir,1:332)
  print(li)
  li1<-li[,1]
  li2<-li[,2]
  v<-vector()
  for(i in 1:nrow(li)){
    if(li[i,2]>thres){
      
      id<-sprintf("%03d",li[i,1])
      
      
      
      filename<-paste(dir,'/',id,".csv",sep='')
      
      x<-read.csv(filename)
      x1<-x[,2]
      x2<-x[,3]
      
      v<-c(v,cor(x1,x2,use ="complete.obs"))
      
    }
    
  }
  v
}