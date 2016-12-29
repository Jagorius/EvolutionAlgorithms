FtTotalToDirectPlot <- function(){
  source('C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/CEC2013/CMADE-New6-FT/CMADEv12.R')
  
  N <- 10
  res <- CMADE(rep(0,N),fn=function(x){ 
                                rnorm(1)
                                      },
                        control=list("diag"=TRUE)
              )
  
  
  functionEvalVec <- (1:length(res$diagnostic$Ft))*(dim(res$diagnostic$pop)[2]+1)
  
  print(min(res$diagnostic$pathRatio))
  print(min(res$diagnostic$Ft))
  print(max(res$diagnostic$pathRatio))
  print(max(res$diagnostic$Ft))
  
  # Plot Ft
  plot(functionEvalVec,res$diagnostic$Ft, log="y", ylim=c(0.0001,100), xlab="function evaluations", ylab=expression(bold(red):total/direct~~bold(green):Ft),cex=0)
  xlab=expression()
  lines(functionEvalVec,res$diagnostic$Ft, lwd=2, col="green")
  
  # Plot path ratio
  lines(functionEvalVec,res$diagnostic$pathRatio, lwd=3, col="red")
  
  title("Fitness Function = N(0,1)")
  
}