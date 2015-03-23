ftPlots <- function(){
  
  for (n in 0:499) {
    ftTable <- read.table(paste("C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/CMAES-DEv5/FT/0-499/",n,sep=""), fill = TRUE)
    ftTable$V1 <-NULL
    ftVector <- numeric()
    for (i in 1:nrow(ftTable)) {
      ftVector <- c(ftVector,ftTable[i,])
    }
    
    png(filename=paste("C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/CMAES-DEv5/FT/0-499/",n,".png",sep=""))
    plot(c(1:length(ftVector)),ftVector, col="blue", ann=FALSE, log="y", type="l")
    title(xlab="Population number", col.lab=rgb(0,0.5,0))
    title(ylab="Ft  value", col.lab=rgb(0,0.5,0))
    title(main=paste("Ft volatility graph\nProblem=",n,sep=""), col.main="red", font.main=4)
    dev.off()
  }
  
  for (n in 500:999) {
    ftTable <- read.table(paste("C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/CMAES-DEv5/FT/500-999/",n,sep=""), fill = TRUE)
    ftTable$V1 <-NULL
    ftVector <- numeric()
    for (i in 1:nrow(ftTable)) {
      ftVector <- c(ftVector,ftTable[i,])
    }
    
    png(filename=paste("C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/CMAES-DEv5/FT/500-999/",n,".png",sep=""))
    plot(c(1:length(ftVector)),ftVector, col="blue", ann=FALSE, log="y", type="l")
    title(xlab="Population number", col.lab=rgb(0,0.5,0))
    title(ylab="Ft  value", col.lab=rgb(0,0.5,0))
    title(main=paste("Ft volatility graph\nProblem=",n,sep=""), col.main="red", font.main=4)
    dev.off()
  }
}

ftPlots2 <- function(){
  for (n in 0:141) {
    ftTable <- read.table(paste("C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/CMAES-DEv5/FT2/FT/",n,sep=""), fill = TRUE)
    ftTable$V1 <-NULL
    ftVector <- numeric()
    for (i in 1:nrow(ftTable)) {
      ftVector <- c(ftVector,ftTable[i,])
    }
    
    png(filename=paste("C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/CMAES-DEv5/FT2/FT/",n,".png",sep=""))
    plot(c(1:length(ftVector)),ftVector, col="blue", ann=FALSE, log="y", type="l")
    title(xlab="Population number", col.lab=rgb(0,0.5,0))
    title(ylab="Ft  value * norm(pc)", col.lab=rgb(0,0.5,0))
    title(main=paste("Ft volatility graph\nProblem=",n,sep=""), col.main="red", font.main=4)
    dev.off()
  }
}