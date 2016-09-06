AbsSigmaPlotCEC2013 <- function(){
  source('C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/CEC2013/CMADE-New6-FT/CMADEv12.R')
  setwd("C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/CEC2013/CMADE-New6-FT/Plots/")
  
  N <- 10
  for(i in 1:28){
    print(paste("PROBLEM:",i))
    res <- CMADE(rep(0,N),fn=function(x){ 
                                  cec2013(i,x)
                                },
                control=list("diag"=TRUE)
    )
    
    best <- abs(-1400 - apply(res$diagnostic$value,1,min))
    best[best<1e-08] <- 1e-08
    
    mean <- abs(-1400 - res$diagnostic$mean)
    mean[mean<1e-08] <- 1e-08
    
    res$diagnostic$Ft[1] <- max(res$diagnostic$Ft)
    
    functionEvalVec <- (1:length(res$diagnostic$Ft))*(dim(res$diagnostic$pop)[2]+1)
    
    png(paste(i,".png"), width = 1024, height = 768)
    # Plot Ft
    plot(functionEvalVec,res$diagnostic$Ft, log="y", ylim=c(min(best,res$diagnostic$Ft),max(res$diagnostic$Ft,best)), xlab="function evaluations", ylab="green:Ft, blue:best, black:mean",cex=0)
    lines(functionEvalVec,res$diagnostic$Ft, lwd=2, col="green")
  
    # Plot f(best)
    lines(functionEvalVec,best, lwd=3, col="blue")
    
    # Plot f(mean)
    lines(functionEvalVec,mean, lwd=3, col="black")
    
    title(paste("CEC2013 Problem ",i))
    
    dev.off()
  }
}