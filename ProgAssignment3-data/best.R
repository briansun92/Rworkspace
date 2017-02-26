best <- function(state, outcome) {
    ##read data
    data<-read.csv("outcome-of-care-measures.csv", colClasses = "character")
    ## Check outcome are valid
    if(outcome == "heart attack")  {b<-names(data)[11]}
    else if(outcome == "heart failure") {b<-names(data)[17]}
    else if(outcome == "pneumonia") {b<-names(data)[23]}
    else {print("invalid outcome") }
    ## Check state are valid
    if(state%in%unique(data$State) == TRUE) {c<-state}
    else {print("invalid state")  }
    ## Return hospital name in that state with lowest 30-day death
    a<-subset(data,State == c,select = c(names(data)[2], b))
    a1<-subset(a,a[2]!="Not Available")
    a11<-a1[[2]]
    a12<-as.numeric(a11)
    a2<-min(a12)
    a22<-as.character(a2)
    a3<-subset(a1,a1[2]==a2,select= Hospital.Name,drop = TRUE)
    ## rate
    print(a3)
}