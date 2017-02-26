rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    data<-read.csv("outcome-of-care-measures.csv", na = "Not Available",stringsAsFactors = FALSE)
    
    ## Check outcome are valid
    if(outcome == "heart attack")  {b<-names(data)[11]}
    else if(outcome == "heart failure") {b<-names(data)[17]}
    else if(outcome == "pneumonia") {b<-names(data)[23]}
    else {print("invalid outcome") }
    
    ## Check state are valid
    if(state%in%unique(data$State) == TRUE) {c<-state}
    else {print("invalid state")  }
    
    ## Return hospital name in that state with the given rank
    a<-subset(data,State == c,select = c(names(data)[2], b))
    a0<-complete.cases(a)
    a1<-a[a0,]
    a2<-a1[order(a1[,2],a1[,1]),]
    
    ## 30-day death rate
    if (num == "best") {
        a3<-a2[1,1]
    } else if (num == "worst") {
        a3<-a1[order(-a1[,2],a1[,1]),][1,1]
    } else {
        a3<-a2[num,1]
    }
    print(a3)
}