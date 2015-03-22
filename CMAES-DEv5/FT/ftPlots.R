ftPlots <- function(){
  
  for (n in 0:999) {
    ftTable <- read.table(paste("C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/CMAES-DEv5/FT/",n,sep=""), fill = TRUE)
    ftTable$V1 <-NULL
    ftVector <- numeric()
    for (i in 1:nrow(ftTable)) {
      ftVector <- c(ftVector,ftTable[i,])
    }
    
    png(filename=paste("C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/CMAES-DEv5/FT/",n,".png",sep=""))
    plot(c(1:length(ftVector)),ftVector, col="blue", ann=FALSE)
    title(xlab="Population number", col.lab=rgb(0,0.5,0))
    title(ylab="Ft  value", col.lab=rgb(0,0.5,0))
    title(main=paste("Ft volatility graph\nProblem=",n,sep=""), col.main="red", font.main=4)
    dev.off()
  }
}