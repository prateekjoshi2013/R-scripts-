rankall <- function(outcome, num = "best") {
## Read outcome data
y<-read.csv("outcome-of-care-measures.csv")
y1<-as.matrix(y[,c(2,7,11,17,23)])
st<-"State"
stV<-unique(y1[,"State"])

# Check that state and outcome are valid
#check state
#if (!(is.element(,stV))) {
#stop("invalid state ")
#}

v<-c("heart attack","pneumonia","heart failure")

#check outcome
if(!(is.element(tolower(outcome),v))){
stop("invalid outcome")
}

ind<-which(tolower(outcome)==v)
if(ind==1){
outcomefield=3
}
else if(ind==2){
outcomefield=5
}
else{
outcomefield=4
}



y1[y1=="Not Available"]<-NA

resv<-matrix(NA,1,2)
for( i in 1:(length(stV)-1)){

y2<-y1[y1[,2]==stV[i],c(1,2,outcomefield)]


y3<-y2[complete.cases(y2),]
#print(y3)
v2<-as.numeric(y3[,3])
v1<-y3[,1]
#print(y3)

ord<-order(v2,v1)
sortedMatrix<-y3[ord,]
if(identical(num,"best")){
resv<-rbind(resv,c(sortedMatrix[1,2],sortedMatrix[1,1]))
}
else if(identical(num,"worst")){
resv<-rbind(resv,c(sortedMatrix[nrow(sortedMatrix),2],sortedMatrix[nrow(sortedMatrix),1]))
}
else if(num>nrow(sortedMatrix)){
resv <-rbind(resv,c(stV[i],NA))
}
else{
resv<-rbind(resv,c(sortedMatrix[num,2],sortedMatrix[num,1]))
}





## For each state, find the hospital of the given rank



## Return a data frame with the hospital names and the



## (abbreviated) state name


}
resv<-resv[!is.na(resv[,1]),]
resv1<-resv[order(resv[,1]),]
#resv1<-resv1[!is.na(resv1[,2]),]
resv1<-data.frame(resv1)
row.names(resv1) <- resv1[,1]

resv1<-resv1[,c(2,1)]
colnames(resv1)<-c("hospital","state")
resv1
}
