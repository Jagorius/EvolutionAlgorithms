AbsSigmaPlotCEC2013 <- function(){
  library(cec2013)
  source('C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/CEC2013/CMADE-New6-FT/CMADEv12.R')
  setwd("C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/CEC2013/CMADE-New6-FT/tol/")
  
  scores <- c(seq(from = -1400, to = -100, by=100),seq(from = 100, to = 1400, by=100))
  N <- 10
  for(i in c(2)){
    print(paste("PROBLEM:",i))
    res <- CMADE(rep(0,N),fn=function(x){ 
                                  cec2013(i,x)
                                },
                control=list("diag"=TRUE)
    )
    
    best <- abs(scores[i] - apply(res$diagnostic$value,1,min))
    best[best<1e-08] <- 1e-08
    
    mean <- abs(scores[i]- res$diagnostic$mean)
    mean[mean<1e-08] <- 1e-08
    
    #res$diagnostic$Ft[1] <- max(res$diagnostic$Ft)
    
    functionEvalVec <- (1:length(res$diagnostic$Ft))*(dim(res$diagnostic$pop)[2]+1)
    
    png(paste(i,".png"), width = 1024, height = 768)
    # Plot Ft
    plot(functionEvalVec,res$diagnostic$Ft, log="y", ylim=c(min(best,abs(res$diagnostic$Ft)),max(res$diagnostic$Ft,best)), xlab="function evaluations", ylab=expression(bold(red):total/direct~~bold(green):Ft~~bold(blue):best~~bold(black):mean),cex=0)
    xlab=expression()
    lines(functionEvalVec,res$diagnostic$Ft, lwd=2, col="green")
  
    # Plot f(best)
    lines(functionEvalVec,best, lwd=3, col="blue")
    
    # Plot f(mean)
    lines(functionEvalVec,mean, lwd=3, col="black")
    
    # Plot path ratio
    lines(functionEvalVec,res$diagnostic$pathRatio, lwd=3, col="red")
    
    title(paste("CEC2013 Problem ",i))
    
    dev.off()
  }
}