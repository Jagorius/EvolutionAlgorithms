benchmarkBoxPlot <- function() {
  
  standarization <- function(x){(x-min(x))/(max(x)-min(x))}
  
  library(reshape2)
  library(lattice)
  library(ggplot2)
  
  v1resultsFolder <- "C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/Basic/CEC2013"
  v2resultsFolder <- "C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/Basicv2/CEC2013_cpc=0.375"
   
  # for dimmension=10
  resultMatrix <- matrix(0, nrow=0, ncol = 3)
  colnames(resultMatrix) <- c("Version","Function","error")
  d <- 10
  
  # for every problem in CEC2013
  for(n in 1:28){
    allVresults <- matrix(0, nrow=0, ncol = 3)
    colnames(allVresults) <- c("Version","Function","error")
    for(v in c(1,2)){
      vresults <- read.table(file = paste(ifelse(v==1,v1resultsFolder,v2resultsFolder),"/N",n,"-D",d,sep=""),sep=",")
      names(vresults) <- 'error'
      vresults[["Version"]] <- ifelse(v==1,"CMADE v1","CMADE v2(c_pc=0.375)")
      vresults[["Function"]] <- paste("F",n,sep="")
      allVresults <- rbind(allVresults, vresults)
      
    }
    allVresults[["error"]] <- standarization(allVresults[["error"]])
    resultMatrix <- rbind(resultMatrix, allVresults)
    
  }
  
  ggplot(data = resultMatrix, aes(x=Function, y=error)) + geom_boxplot(aes(fill=Version))
  
 # p <- ggplot(data = resultMatrix, aes(x=Function, y=error)) + 
  #  geom_boxplot(aes(fill=Version))
  #p + facet_wrap( ~ Function, scales="free")
  
}

CmadeDistribuition <- function() {
  set.seed(42)
  population <- replicate(100, runif(2,-100,100))
  plot(population[1,],population[2,], col="red")

  iter      <- iter + 1L
  histHead  <- (histHead %% histSize) + 1
  
}