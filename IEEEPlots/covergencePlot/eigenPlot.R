eigenplotDES<- function(){
  source('C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/DESv2/DES - tol_v2/DESv2017.R')
  #source('C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/DESv2/DES2- finalVersion geomSelection/DESv2017.R')

  N <- 10
  Iters <- 50

  eigenLog <- list()
  for (i in 1:Iters){
    print(i)
    resDES <- DES(rep(0,N),fn=function(x){
      res <- 0
      for(i in 1:length(x))
        res <- res + 10^(6*(i-1)/(length(x)-1))*x[i]^2
      return(res)
    },
    lower=-10^100, upper=10^100,
    control=list("budget"=6000,"diag.pop"=TRUE,"diag.eigen"=TRUE)
    )
    eigenLog[[length(eigenLog)+1]] <- abs(resDES$diagnostic$eigen)
  }
  # Mean of each value in vector
  eigen <- Reduce("+", eigenLog) / length(eigenLog)

  # Divide each column by corresponding fitness function weight
  #for(i in 1:ncol(eigen))
  # eigen[,i] <- eigen[,i] / 10^(6*( (i-1)/(ncol(eigen)-1) ))

  functionEvalVec <- (1:nrow(eigen))*(ncol(resDES$diagnostic$pop[,,1])+1)

  setEPS()
  postscript( paste("DES-N",N,".eps",sep=""), width = 8, height = 8)

  # Plot eigen values changes for each dimmension
  options(scipen=3)
  plot(functionEvalVec,eigen[,N],log="y",ylim=c(1e-7,1e1), xlab="function evaluations", ylab="Eigenvalues",cex=0,cex.axis=1.5,cex.lab=1.5)


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
  Iters <- 50
  L = 4*N

  eigenLog <- list()
  for (i in 1:Iters){
    print(i)
    resCMAES <- cma_es(rep(0,N),
                       fn=function(x){
                         res <- 0
                         for(i in 1:length(x))
                           res <- res + 10^(6*(i-1)/(length(x)-1))*x[i]^2
                         return(res)
                       },
                       lower=-10^100, upper=10^100,
                       control=list("diag.eigen"=TRUE,"budget"=6000,"lambda"=L)
    )
    eigenLog[[length(eigenLog)+1]] <- abs(resCMAES$diagnostic$eigen)
  }
  # Mean of each value in vector
  eigen <- Reduce("+", eigenLog) / length(eigenLog)

  # Divide each column by corresponding fitness function weight
  #for(i in 1:ncol(eigen))
  # eigen[,i] <- eigen[,i] / 10^(6*( (i-1)/(ncol(eigen)-1) ))

  functionEvalVec <- (1:nrow(eigen))*(L)

  setEPS()
  postscript( paste("CMAES-N",N,".eps",sep=""), width = 8, height = 8)

  # Plot eigen values changes for each dimmension
  options(scipen=3)
                                        #ylim=c(min(eigen),max(eigen))
  plot(functionEvalVec,eigen[,N],log="y",ylim=c(min(eigen),max(eigen)), xlab="function evaluations", ylab="Eigenvalues",cex=0,cex.axis=1.5,cex.lab=1.5)
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
  Iters <- 50
  L = 4*N

  eigenLog <- list()
  for (i in 1:Iters){
    print(i)
    resCMAESNos <- cma_esNos(rep(0,N),
                       fn=function(x){
                         res <- 0
                         for(i in 1:length(x))
                          res <- res + 10^(6*(i-1)/(length(x)-1))*x[i]^2
                         return(res)
                         #return(Cigar(x))
                       },
                       lower=-10^100, upper=10^100,
                       control=list("diag.eigen"=TRUE,"budget"=6000,"lambda"=L )
    )
    eigenLog[[length(eigenLog)+1]] <- abs(resCMAESNos$diagnostic$eigen)
  }

  # Mean of each value in vector
  eigen <- Reduce("+", eigenLog) / length(eigenLog)

  # Divide each column by corresponding fitness function weight
  #for(i in 1:ncol(eigen))
  # eigen[,i] <- eigen[,i] / 10^(6*( (i-1)/(ncol(eigen)-1) ))

  functionEvalVec <- (1:nrow(eigen))*(L)

  setEPS()
  postscript( paste("CMAESNos-N",N,".eps",sep=""), width = 8, height = 8)

  options(scipen=3)
  # Plot eigen values changes for each dimmension
                                        #ylim=c(min(eigen),max(eigen))
  plot(functionEvalVec,eigen[,N],log="y",ylim=c(1e-7,1e1), xlab="function evaluations", ylab="Eigenvalues",cex=0)
  lines(functionEvalVec,eigen[,N], lwd=2)

  colours <- c("red","darkgreen","orchid","blue","tomato4","yellow4","snow3","plum","seashell4","black","black","black","black")
  for(c in (ncol(eigen)-1):1){
    points(functionEvalVec,eigen[,c],col=colours[c],cex=0)
    lines(functionEvalVec,eigen[,c],col=colours[c], lwd=2)
  }

  dev.off()

}

eigenplotCMADE<- function(){
  source('C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/DESv2/CMA-DE/DESv2017.R')
  
  N <- 10
  Iters <- 50
  
  eigenLog <- list()
  for (i in 1:Iters){
    print(i)
    resDES <- DES(rep(0,N),fn=function(x){
      res <- 0
      for(i in 1:length(x))
        res <- res + 10^(6*(i-1)/(length(x)-1))*x[i]^2
      return(res)
      #return(Cigar(x))
    },
    lower=-10^100, upper=10^100,
    control=list("budget"=6000,"diag.pop"=TRUE,"diag.eigen"=TRUE)
    )
    eigenLog[[length(eigenLog)+1]] <- abs(resDES$diagnostic$eigen)
  }
  # Mean of each value in vector
  eigen <- Reduce("+", eigenLog) / length(eigenLog)
  
  # Divide each column by corresponding fitness function weight
  #for(i in 1:ncol(eigen))
  # eigen[,i] <- eigen[,i] / 10^(6*( (i-1)/(ncol(eigen)-1) ))
  
  functionEvalVec <- (1:nrow(eigen))*(ncol(resDES$diagnostic$pop[,,1])+1)
  
  setEPS()
  postscript( paste("CMADE-N",N,".eps",sep=""), width = 8, height = 8)
  
  # Plot eigen values changes for each dimmension
  options(scipen=3)
  plot(functionEvalVec,eigen[,N],log="y",ylim=c(1e-7,1e1), xlab="function evaluations", ylab="Eigenvalues",cex=0,cex.axis=1.5,cex.lab=1.5)
  
  
  lines(functionEvalVec,eigen[,N], lwd=2)
  
  colours <- c("red","darkgreen","orchid","blue","tomato4","yellow4","snow3","plum","seashell4","black","black","black","black")
  for(c in (ncol(eigen)-1):1){
    points(functionEvalVec,eigen[,c],col=colours[c],cex=0)
    lines(functionEvalVec,eigen[,c],col=colours[c], lwd=2)
  }
  
  dev.off()
  
}

Cigar <- function(x){
  res <- 0
  for(i in 2:length(x))
    res <- res + x[i]^2
  res <- res*10^6 + x[1]^2
  return(res)
}