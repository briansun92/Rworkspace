
pollutantmean<-function(directory,pollutant,id = 1:332){
        file<- dir(directory)
        setwd(directory)
        data1<-data.frame (NaN)
        names(data1)= pollutant
        for(i in id){
                tempcsv<-read.csv(file[i])
                tempsub<-subset(tempcsv,select = pollutant)
                data1<-rbind(data1,tempsub)
        }
        result<-colMeans(data1,na.rm = TRUE)
        setwd("~/Desktop/Coursera/R workspace")
        print(result)
}

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


corr <- function(directory, threshold = 0) {
        df <- complete(directory)
        fi<- dir(directory)
        ids<-subset(df,nobs>threshold,select = id,drop = TRUE)
        data0<-vector()
        data1<-as.numeric(data0)
        for (i in ids) {
                tempcsv<-read.csv(paste(directory, "/", fi[i],sep = ""))
                good<-complete.cases(tempcsv)
                tempsub<-tempcsv[good, ]
                data11<-subset(tempsub,select = sulfate)
                data12<-subset(tempsub,select = nitrate)
                data1 = c(data1, cor(data11, data12))
        }
        
        print(data1)
}