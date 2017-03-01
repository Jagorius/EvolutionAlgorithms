NToResults <- function() {
  path <- "C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/CEC2017/CMADE-BBcomp5=Lconst=4N"
  
  for(n in c(10,30,50,100)){
    csvResults <-  matrix(0, nrow = 51, ncol = 1)
    if(file.exists(paste(path,"/N/N1-D",n,sep="")) ){
      for(i in 1:30){
        resultVector <- read.table(paste(path,"/N/N",i,"-D",n,sep=""), sep=",",header = TRUE)
        resultVector <- t(resultVector)
        print( paste(paste("CEC2017 N=",n," D=",i,sep=""),median(resultVector), min(resultVector), max(resultVector), mean(resultVector),sd(resultVector), "NoData", sep=",") , quote=FALSE)
    
      }


    }
  }
  
}
  