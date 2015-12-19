best <- function(state, outcome) {

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


## Return hospital name in that state with lowest 30-day death rate

y2<-y1[y1[,2]==state,c(outcomefield,1)]
y2<-y2[complete.cases(y2),]
v2<-as.numeric(y2[,1])
Min<-min(v2)
MinV<-y2[which(v2==Min),2]
V<-sort(MinV)
V[1]


}
