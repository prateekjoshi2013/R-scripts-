rankhospital <- function(state, outcome, num = "best") {
## Read outcome data
y<-read.csv("outcome-of-care-measures.csv")
y1<-as.matrix(y[,c(2,7,11,17,23)])

## Check that state and outcome are valid
outcome<-tolower(outcome)

#check state
if (!(is.element(state,y1[,"State"]))) {
stop("invalid state ")
}

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


## Return hospital name in that state with the given rank 30-day death rate

y2<-y1[y1[,2]==state,c(outcomefield,1)]
y2<-y2[complete.cases(y2),]
v2<-as.numeric(y2[,1])
v1<-y2[,2]
ord<-order(v2,v1)
sortedMatrix<-y2[ord,]
if(identical(num,"best")){
return (sortedMatrix[1,2])
}
else if(identical(num,"worst")){
return (sortedMatrix[nrow(y2),2])

}
else{
if(num>nrow(y2)){
return (NA)
}
else{
return (sortedMatrix[num,2])
}
}
}