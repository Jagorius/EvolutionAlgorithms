eigenplotDES<- function(){
  source('C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/IEEEPlots/CMADEv2017.R')
  N <- 10
  
  resDES <- CMADE(rep(0,N),fn=function(x){ 
    res <- 0
    for(i in 1:length(x))
      res <- res + 10^(6*(i-1)/(length(x)-1))*x[i]^2
    return(res)
  }, 
  lower=-10^100, upper=10^100,
  control=list("budget"=6000,"diag.pop"=TRUE,"diag.eigen"=TRUE)
  )
  
  # Square root of each value in vector
  eigen <- sqrt(abs(resDES$diagnostic$eigen))
  
  # Divide each column by corresponding fitness function weight
  #for(i in 1:ncol(eigen))
  # eigen[,i] <- eigen[,i] / 10^(6*( (i-1)/(ncol(eigen)-1) ))
  
  functionEvalVec <- (1:nrow(eigen))*(ncol(resDES$diagnostic$pop[,,1])+1)
  
  setEPS()
  postscript( paste("DES-N",N,".eps",sep=""), width = 8, height = 8)
  
  # Plot eigen values changes for each dimmension
  options(scipen=10)
  plot(functionEvalVec,eigen[,N],log="y",ylim=c(0.000625,4), xlab="function evaluations", ylab="Eigenvalues",cex=0)

  
  lines(functionEvalVec,eigen[,N], lwd=2)
  
  colours <- c("red","darkgreen","orchid","blue","tomato4","yellow4","snow3","plum","seashell4","black","black","black","black")
  for(c in (ncol(eigen)-1):1){
    points(functionEvalVec,eigen[,c],col=colours[c],cex=0)
    lines(functionEvalVec,eigen[,c],col=colours[c], lwd=2)
  }
  
  dev.off()
  
}

eigenplotCMAES<- function(){
  source('C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/IEEEPlots/CMAES.R')
  N <- 10
  
  resCMAES <- cma_es(rep(0,N),
                     fn=function(x){ 
                       res <- 0
                       for(i in 1:length(x))
                         res <- res + 10^(6*(i-1)/(length(x)-1))*x[i]^2
                       return(res)
                     }, 
                     lower=-10^100, upper=10^100,
                     control=list("diag.eigen"=TRUE,"budget"=6000)                
  )
  
  # Square root of each value in vector
  eigen <- sqrt(abs(resCMAES$diagnostic$eigen))
  
  # Divide each column by corresponding fitness function weight
  #for(i in 1:ncol(eigen))
  # eigen[,i] <- eigen[,i] / 10^(6*( (i-1)/(ncol(eigen)-1) ))
  
  functionEvalVec <- (1:nrow(eigen))*(4+floor(3*log(N)))
  
  setEPS()
  postscript( paste("CMAES-N",N,".eps",sep=""), width = 8, height = 8)
  
  # Plot eigen values changes for each dimmension
  options(scipen=0) 
  
  plot(functionEvalVec,eigen[,N],log="y",ylim=c(min(eigen),max(eigen)), xlab="function evaluations", ylab="Eigenvalues",cex=0)
  lines(functionEvalVec,eigen[,N], lwd=2)
  
  colours <- c("red","darkgreen","orchid","blue","tomato4","yellow4","snow3","plum","seashell4","black","black","black","black")
  for(c in (ncol(eigen)-1):1){
    points(functionEvalVec,eigen[,c],col=colours[c],cex=0)
    lines(functionEvalVec,eigen[,c],col=colours[c], lwd=2)
  }
  
  dev.off()
  
}

eigenplotCMAESNos<- function(){
  source('C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/IEEEPlots/cmaesNoS.R')
  N <- 10
  
  resCMAESNos <- cma_esNos(rep(0,N),
                     fn=function(x){ 
                       res <- 0
                       for(i in 1:length(x))
                         res <- res + 10^(6*(i-1)/(length(x)-1))*x[i]^2
                       return(res)
                     }, 
                     lower=-10^100, upper=10^100,
                     control=list("diag.eigen"=TRUE,"budget"=6000)                
  )
  

  # Square root of each value in vector
  eigen <- sqrt(abs(resCMAESNos$diagnostic$eigen))

  eigenCMAES <<- eigen
  # Divide each column by corresponding fitness function weight
  #for(i in 1:ncol(eigen))
  # eigen[,i] <- eigen[,i] / 10^(6*( (i-1)/(ncol(eigen)-1) ))
  
  functionEvalVec <- (1:nrow(eigen))*(4+floor(3*log(N)))
  
  setEPS()
  postscript( paste("CMAESNos-N",N,".eps",sep=""), width = 8, height = 8)
  
  # Plot eigen values changes for each dimmension
  plot(functionEvalVec,eigen[,N],log="y",ylim=c(min(eigen),max(eigen)), xlab="function evaluations", ylab="Eigenvalues",cex=0)
  lines(functionEvalVec,eigen[,N], lwd=2)
  
  colours <- c("red","darkgreen","orchid","blue","tomato4","yellow4","snow3","plum","seashell4","black","black","black","black")
  for(c in (ncol(eigen)-1):1){
    points(functionEvalVec,eigen[,c],col=colours[c],cex=0)
    lines(functionEvalVec,eigen[,c],col=colours[c], lwd=2)
  }
  
  dev.off()
  
}