EigenPlot <- function() {
  source('C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/Constriants/DES/Substitution penalty/CMADEv2017.R')
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
  # Plot eigen values changes for each dimmension
  plot(functionEvalVec,eigen[,N],log="y",ylim=c(min(eigen),max(eigen)), xlab="function evaluations", ylab="Eigenvalues",cex=0)
  lines(functionEvalVec,eigen[,N], lwd=2)

  colours <- c("red","darkgreen","orchid","blue","tomato4","yellow4","snow3","plum","seashell4","black","black","black","black")
  for(c in (ncol(eigen)-1):1){
    points(functionEvalVec,eigen[,c],col=colours[c],cex=0)
    lines(functionEvalVec,eigen[,c],col=colours[c], lwd=2)
  }

}

AbsSigmaPlotQuadric <- function(){
  source('C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/Constriants/DES/Substitution penalty/CMADEv2017.R')
  N <- 10

  resDES <- CMADE(rep(0,N),fn=function(x){
    res <- 0
    for(i in 1:length(x))
      res <- res + 10^(6*(i-1)/(length(x)-1))*x[i]^2
    return(res)
  },
  lower=-10^100, upper=10^100,
  control=list("budget"=6000,"diag.pop"=TRUE,"diag.Ft"=TRUE,diag.mean=TRUE,"diag.bestVal"=TRUE,"diag.worstVal"=TRUE)
  )

  functionEvalVec <- (1:nrow(resDES$diagnostic$Ft))*(ncol(resDES$diagnostic$pop[,,1])+1)
  # Plot
  plot(functionEvalVec,resDES$diagnostic$Ft,log="y",ylim=c(min(min(resDES$diagnostic$Ft),min(resDES$diagnostic$bestVal)),max(resDES$diagnostic$worstVal)), xlab="function evaluations", ylab="blue:best, black:mean",cex=0)

  # Plot f(best)
  lines(functionEvalVec,resDES$diagnostic$bestVal, lwd=3, col="blue")

  # Plot f(mean)
  lines(functionEvalVec,resDES$diagnostic$mean, lwd=3, col="black")



}



Sphere <- function(x){
  return(sum(x^2))
}

Cigar <- function(x){
  res <- 0
  for(i in 2:length(x))
    res <- res + x[i]^2
  res <- res*10^6 + x[1]^2
  return(res)
}

Discus <- function(x){
  res <- 10^6 * x[1]^2
  for(i in 2:length(x))
    res <- res + x[i]^2
  return(res)
}

Ellipsoid <- function(x){
  res <- 0
  for(i in 1:length(x))
      res <- res + 10^(6*(i-1)/(length(x)-1))*x[i]^2
  return(res)
}

DifferentPowers<-function(x){
  res <- 0
  for(i in 1:(length(x)))
    res <- res + (x[i]^2)^(1+5*(i-1)/(length(x)-1))
  return(res)
}

SharpRidge<-function(x){
  res <- 0
  for(i in 2:(length(x)))
    res <- res + (x[i])^2
  return(x[1] + res^(1/2)*100)
}

ParabolicRidge<-function(x){
  res <- 0
  for(i in 2:(length(x)))
    res <- res + (x[i])^2
  return(x[1] + res^(2/2)*100)
}

RosenBrock = function(x){
  res <- 0
  for(i in 1:(length(x)-1))
    res <- res + 100*(x[i+1]-x[i]^2)^2 + (x[i]-1)^2
  return(res)
}

AbsSigmaPlot <- function(N,func,isYaxt="s"){
  Iters <- 51
  bud   <- 1500*N
    
  source('C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/DESv2/DES - tol_v2/DESv2017.R')
  lambda_DES <- 4+floor(3*sqrt(N))
  #lambda_DES <- 4*N

  ##### DES
  DESLog <- list()
  for (i in 1:Iters){
    print(paste("DES",i,sep=" "))
    resDES <- DES(rep(0,N),fn=func,
                  lower=-10^100, upper=10^100,
                  control=list("lambda"=lambda_DES, "budget"=bud,"diag.pop"=TRUE,"diag.Ft"=TRUE,diag.mean=TRUE,"diag.bestVal"=TRUE,"diag.worstVal"=TRUE)
    )
    DESLog[[length(DESLog)+1]] <- resDES$diagnostic$bestVal
  }

  resDES <- Reduce("+", DESLog) / length(DESLog)
  #setEPS()
  #postscript( paste("Ellipsoid",",N=",N,".eps",sep=""), width = 8, height = 8)

  functionEvalVec <- (1:nrow(resDES))*(lambda_DES)
  # Plot
  if(any((resDES)<=10^-10))
    x_limit = c(0,which((resDES)<=10^-10)[1]*lambda_DES)
  else
    x_limit = c(0,bud-100*N)

  #resDES[resDES<=0] <- 10^-64
  plot(functionEvalVec,functionEvalVec, log="y",ylim=c(10^-10,10^6),xlim=x_limit,cex=0, yaxt=isYaxt,cex.axis=1.5)
  # Plot f(best)
  lines(functionEvalVec,abs(resDES), lwd=3, col="blue")
  # Plot f(mean)
  #lines(functionEvalVec,resDES$diagnostic$mean, lwd=3, col="black")

  #source('C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/DESv2/DES2- finalVersion geomSelection/DESv2017.R')
  source('C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/DESv2/CMA-DE/DESv2017.R')
  ##### DES-CMAES
  DESCMAESLog <- list()
  for (i in 1:Iters){
    print(paste("CMADE",i,sep=" "))
    resDESCMAES <- DES(rep(0,N),fn=func,
                       lower=-10^100, upper=10^100,
                       control=list( "lambda"=lambda_DES, "budget"=bud,"diag.pop"=TRUE,"diag.Ft"=TRUE,diag.mean=TRUE,"diag.bestVal"=TRUE,"diag.worstVal"=TRUE)
    )
    DESCMAESLog[[length(DESCMAESLog)+1]] <- resDESCMAES$diagnostic$bestVal

  }
  resDESCMAES <- Reduce("+", DESCMAESLog) / length(DESCMAESLog)
  #resDESCMAES[resDESCMAES<=0] <- 10^-64

  functionEvalVec2 <- (1:nrow(resDESCMAES))*(lambda_DES)
  lines(functionEvalVec2,abs(resDESCMAES), lwd=3, col="orange")

  ##### CMA-ES
  #source('C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/IEEEPlots/cmaesNoS.R')
  #resCMAES <- cma_esNos(rep(0,N),
  source('C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/IEEEPlots/CMAES.R')
  CMAESLog <- list()
  for (i in 1:Iters){
    print(paste("CMAES",i,sep=" "))
    resCMAES <- cma_es(rep(0,N),
                       fn=func,
                       lower=-10^100, upper=10^100,
                       control=list("lambda"=lambda_DES,"diag.bestVal"=TRUE,"budget"=bud)
    )
    CMAESLog[[length(CMAESLog)+1]] <- resCMAES$diagnostic$bestVal

  }
  min_l <- Inf
  for(l in 1:length(CMAESLog))
    min_l = min(min_l,length(CMAESLog[[l]]))
  for(l in 1:length(CMAESLog))
    CMAESLog[[l]] <- CMAESLog[[l]][1:min_l]
  resCMAES <- Reduce("+", CMAESLog) / length(CMAESLog)

  #resCMAES[resCMAES<=0] <- 10^-64

  functionEvalVec3 <- (1:length(resCMAES))*(lambda_DES)
  lines(functionEvalVec3,abs(resCMAES), lwd=3, col="red")
  
  
  ##### CMA-ES No Sigma
  if(FALSE){
    source('C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/IEEEPlots/cmaesNoS.R')
    CMAESnoSLog <- list()
    for (i in 1:Iters){
      print(paste("CMAESNos",i,sep=" "))
      resCMAESnoS <- cma_esNos(rep(0,N),
                         fn=func,
                         lower=-10^100, upper=10^100,
                         control=list("lambda"=lambda_DES,"diag.bestVal"=TRUE,"budget"=bud)
      )
      CMAESnoSLog[[length(CMAESnoSLog)+1]] <- resCMAESnoS$diagnostic$bestVal
      
    }
    min_l <- Inf
    for(l in 1:length(CMAESnoSLog))
      min_l = min(min_l,length(CMAESnoSLog[[l]]))
    for(l in 1:length(CMAESnoSLog))
      CMAESnoSLog[[l]] <- CMAESnoSLog[[l]][1:min_l]
    resCMAESnoS <- Reduce("+", CMAESnoSLog) / length(CMAESnoSLog)
    
    #resCMAESnoS[resCMAESnoS<=0] <- 10^-64
    functionEvalVec4 <- (1:length(resCMAESnoS))*(lambda_DES)
    lines(functionEvalVec4,abs(resCMAESnoS), lwd=3, col="grey")
  }
  
  ##### DE/rand/1/bin
  if(FALSE){
    library(DEoptim)
    DELog <- list()
    for (i in 1:Iters){
      print(paste("DE",i,sep=" "))
      resDE <- DEoptim(    fn=func,lower=rep(-10^100,N), upper=rep(10^100,N),
                  DEoptim.control(strategy = 1, NP=lambda_DES, 
                                  itermax=ceiling(bud/lambda_DES), trace=FALSE,
                                  initialpop=t(replicate(lambda_DES, runif(N,0,3)))
                                  )
        )
      DELog[[length(DELog)+1]] <- resDE$member$bestvalit
      
    }
    min_l <- Inf
    for(l in 1:length(DELog))
      min_l = min(min_l,length(DELog[[l]]))
    for(l in 1:length(DELog))
      DELog[[l]] <- DELog[[l]][1:min_l]
    DEoptim <- Reduce("+", DELog) / length(DELog)
    
    functionEvalVec5 <- (1:length(DEoptim))*(lambda_DES)
    lines(functionEvalVec5,abs(DEoptim), lwd=3, col="black")
  }
  #dev.off()


}

combinedPlotExperimentum <- function(){
  setEPS()
  postscript( "combinedPlotExperimentum.eps", width = 20, height = 18)

  layout(mat = matrix(1:16,nrow = 4,ncol = 4,byrow = TRUE),heights = c(1,1,1,1,0.2))
  par(oma=c(6, 8, 0, 0), mar=4*c(.5,.1,.1,.1), cex=1, las=1)
  AbsSigmaPlot(3,func=Sphere)
  AbsSigmaPlot(3,func=Cigar,"n")
  AbsSigmaPlot(3,func=Discus,"n")
  AbsSigmaPlot(3,func=Ellipsoid,"n")
  AbsSigmaPlot(30,func=Sphere)
  AbsSigmaPlot(30,func=Cigar,"n")
  AbsSigmaPlot(30,func=Discus,"n")
  AbsSigmaPlot(30,func=Ellipsoid,"n")
  AbsSigmaPlot(3,func=DifferentPowers)
  AbsSigmaPlot(3,func=SharpRidge,"n")
  AbsSigmaPlot(3,func=ParabolicRidge,"n")
  AbsSigmaPlot(3,func=RosenBrock,"n")
  AbsSigmaPlot(30,func=DifferentPowers)
  AbsSigmaPlot(30,func=SharpRidge,"n")
  AbsSigmaPlot(30,func=ParabolicRidge,"n")
  AbsSigmaPlot(30,func=RosenBrock,"n")
  mtext("function evaluations", 1, 0, outer=TRUE,padj=3,cex=2.5)
  mtext("fitness value", 2, 6, outer=TRUE, las=0,cex=2.5)

  dev.off()
}
