rankall <- function(outcome, num = "best") {
    ## Read outcome data
    data<-read.csv("outcome-of-care-measures.csv", na = "Not Available",stringsAsFactors = FALSE)
    
    ## Check that state and outcome are valid
    if(outcome == "heart attack")  {b<-names(data)[11]}
    else if(outcome == "heart failure") {b<-names(data)[17]}
    else if(outcome == "pneumonia") {b<-names(data)[23]}
    else {print("invalid outcome") }
    
    ## For each state, find the hospital of the given rank
    a<-subset(data,select = c(names(data)[2], names(data)[7], b))
    a0<-complete.cases(a)
    a1<-a[a0,]
    x0<-unique(a1[,2])
    x1<-sort(x0)
    z1<-data.frame (hospital=NaN,state=NaN)
    z<-z1[-1,]
    for(i in 1:54) {
        x2<-x1[i]
        y1<-subset(a1,State == x2)
        a2<-y1[order(y1[,3],y1[,1]),]
        if (num == "best") {
            a3<-a2[1,c(1,2)]
        } else if (num == "worst") {
            a3<-y1[order(-y1[,3],y1[,1]),][1,c(1,2)]
        } else {
            a3<-a2[num,c(1,2)]
            if(complete.cases(a3)){
                a3
            } else {
                a3<-data.frame (Hospital.Name=NA,State = x1[i],stringsAsFactors=FALSE)
            }
        }
        z<-rbind(z,a3)
        
    }
    names(z)<-c("hospital","state")
    return(z)
    
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
}