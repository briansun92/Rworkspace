complete<- function(directory, id = 1:332) {
        file<- dir(directory)
        setwd(directory)
        data0<-data.frame (NaN,NaN)
        data1<-data0[-1,]
        for(i in id) {
                tempcsv<-read.csv(file[i])
                good<-complete.cases(tempcsv)
                tempsub<-tempcsv[good, ]
                b<-nrow(tempsub)
                a <- data.frame (i,b)
                data1<-rbind(data1,a)
        }
        names(data1)=c("id","nobs")
        result<-unique(data1)
        setwd("~/Desktop/Coursera/R workspace")
        print(result)
}