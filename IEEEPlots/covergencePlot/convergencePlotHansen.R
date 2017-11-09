convergencePlotHansen <- function(){
  source('C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/IEEEPlots/CMADEv2017.R')
  source('C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/IEEEPlots/CMAES.R')
  source('C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/IEEEPlots/cmaesNoS.R')
  
  N <- 50
  fnHansen = function(x){ 
    res <- 0
    for(i in 1:length(x))
      res <- res + 10^(6*(i-1)/(length(x)-1))*x[i]^2
    return(res)
  }
  
  Iters <- 50
  resDESBestVal <- c()
  resCMAESNosBestVal <- c()
  resCMAESBestVal <- c()
  
  for (i in 1:Iters){
    print(i)
    #================================
    resDES <- CMADE(rep(0,N),
                    fn=fnHansen, 
                lower=-10^100, upper=10^100,
    control=list("budget"=1000*N,"diag.pop"=TRUE,"diag.Ft"=TRUE,diag.mean=TRUE,"diag.bestVal"=TRUE,"diag.worstVal"=TRUE)
    )
    
    resDESBestVal <- cbind(resDESBestVal,resDES$diagnostic$bestVal)
    #================================
    resCMAESNos <- cma_esNos(rep(0,N),
                          fn=fnHansen, 
                lower=-10^100, upper=10^100,
                control=list("diag.bestVal"=TRUE,"budget"=1000*N)                
    )
    resCMAESNosBestVal <- cbind(resCMAESNosBestVal,resCMAESNos$diagnostic$bestVal)
    #================================
    resCMAES <- cma_es(rep(0,N),
                             fn=fnHansen, 
                             lower=-10^100, upper=10^100,
                             control=list("diag.bestVal"=TRUE,"budget"=1000*N)                
    )
    resCMAESBestVal <- cbind(resCMAESBestVal,resCMAES$diagnostic$bestVal)
    #================================
    
  }
  functionEvalVecDES <- (1:length(rowMeans(resDESBestVal)))*(ncol(resDES$diagnostic$pop[,,1])+1)
  functionEvalVecCMAES <<- (1:length(rowMeans(resCMAESNosBestVal)))*(4+floor(3*log(N)))
  
  setEPS()
  postscript( paste("N",N,".eps",sep=""), width = 8, height = 8)
  
  plot(functionEvalVecDES,rowMeans(resDESBestVal),log="y",ylim=c(min(min(rowMeans(resDESBestVal)),min(rowMeans(resDESBestVal))),max(rowMeans(resDESBestVal))), xlab="function evaluations", ylab="green:CMAES, blue:DESv2, black:CMAES w/o Sigma",cex=0)
  
  # Plot DES
  lines(functionEvalVecDES,rowMeans(resDESBestVal), lwd=3, col="blue")
  
  # Plot CMAESNos
  lines(functionEvalVecCMAES,rowMeans(resCMAESNosBestVal), lwd=3, col="black")
  
  # Plot CMAES
  lines(functionEvalVecCMAES,rowMeans(resCMAESBestVal), lwd=3, col="green")
  
  dev.off()
  
  
}