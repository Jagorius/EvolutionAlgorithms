FtTotalToDirectPlot <- function(){
  source('C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/CEC2013/CMADE-New6-FT/CMADEv12.R')
  
  N <- 100
  res <- CMADE(rep(0,N),fn=function(x){ 
                                rnorm(1)
                                      },
                        control=list("diag"=TRUE)
              )
  
  
  functionEvalVec <- (1:length(res$diagnostic$Ft))*(dim(res$diagnostic$pop)[2]+1)
  
  # Plot Ft
  plot(functionEvalVec,res$diagnostic$Ft, log="y", ylim=c(0.00001,100), xlab="function evaluations", ylab=expression(bold(red):total/direct~~bold(green):Ft),cex=0)
  xlab=expression()
  lines(functionEvalVec,res$diagnostic$Ft, lwd=2, col="green")
  
  # Plot path ratio
  lines(functionEvalVec,res$diagnostic$pathRatio, lwd=3, col="red")
  
  title(paste("Fitness Function = N(0,1)\nN=",N,"\nMean Total\ direct=",mean(res$diagnostic$pathRatio)))
  
}