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