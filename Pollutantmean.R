
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