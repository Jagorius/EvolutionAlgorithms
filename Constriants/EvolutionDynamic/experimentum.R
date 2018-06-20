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
AbsSigmaPlotEllpsoidd <- function(N){
  func <- function(x){
    res <- 0
    for(i in 1:length(x))
      res <- res + 10^(6*(i-1)/(length(x)-1))*x[i]^2
    return(res)
  }
  source('C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/DESv2/DES - tol_v2/DESv2017.R')
  lambda_DES <- 4+floor(3*log(N))
  #lambda_DES <- 4*N

  ##### DES
  resDES <- DES(rep(0,N),fn=func,
                lower=-10^100, upper=10^100,
                control=list("lambda"=lambda_DES, "budget"=1000*N,"diag.pop"=TRUE,"diag.Ft"=TRUE,diag.mean=TRUE,"diag.bestVal"=TRUE,"diag.worstVal"=TRUE)
  )

  #setEPS()
  #postscript( paste("Ellipsoid",",N=",N,".eps",sep=""), width = 8, height = 8)

  functionEvalVec <- (1:nrow(resDES$diagnostic$bestVal))*(lambda_DES)
  # Plot
  plot(functionEvalVec,resDES$diagnostic$Ft,log="y",ylim=c(10^-10,10^5), xlim=c(0,6000),xlab="function evaluations",ylab="blue:DES, red:CMAES, orange:CMADE",cex=0)
  # Plot f(best)
  lines(functionEvalVec,resDES$diagnostic$bestVal, lwd=3, col="blue")
  # Plot f(mean)
  #lines(functionEvalVec,resDES$diagnostic$mean, lwd=3, col="black")

  source('C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/DESv2/DES2- finalVersion geomSelection/DESv2017.R')
  ##### DES-CMAES
  resDESCMAES <- DES(rep(0,N),fn=func,
                     lower=-10^100, upper=10^100,
                     control=list( "lambda"=lambda_DES, "budget"=1000*N,"diag.pop"=TRUE,"diag.Ft"=TRUE,diag.mean=TRUE,"diag.bestVal"=TRUE,"diag.worstVal"=TRUE)
  )

  functionEvalVec2 <- (1:nrow(resDESCMAES$diagnostic$bestVal))*(lambda_DES)
  lines(functionEvalVec2,resDESCMAES$diagnostic$bestVal, lwd=3, col="orange")

  ##### CMA-ES
  #source('C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/IEEEPlots/cmaesNoS.R')
  #resCMAES <- cma_esNos(rep(0,N),
  source('C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/IEEEPlots/CMAES.R')
  resCMAES <- cma_es(rep(0,N),
                     fn=func,
                     lower=-10^100, upper=10^100,
                     control=list("lambda"=lambda_DES,"diag.bestVal"=TRUE,"budget"=1000*N)
  )
  functionEvalVec3 <- (1:length(rowMeans(resCMAES$diagnostic$bestVal)))*(lambda_DES)
  lines(functionEvalVec3,abs(resCMAES$diagnostic$bestVal), lwd=3, col="red")
  #dev.off()


}

AbsSigmaPlotSphere <- function(N){
  func <- function(x){
    return(sum(x^2))
  }
  source('C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/DESv2/DES - tol_v2/DESv2017.R')
  lambda_DES <- 4+floor(3*log(N))
  #lambda_DES <- 4*N

  ##### DES
  resDES <- DES(rep(0,N),fn=func,
  lower=-10^100, upper=10^100,
  control=list("lambda"=lambda_DES, "budget"=1000*N,"diag.pop"=TRUE,"diag.Ft"=TRUE,diag.mean=TRUE,"diag.bestVal"=TRUE,"diag.worstVal"=TRUE)
  )

  setEPS()
  postscript( paste("Sphere",",N=",N,".eps",sep=""), width = 8, height = 8)

  functionEvalVec <- (1:nrow(resDES$diagnostic$bestVal))*(lambda_DES)
  # Plot
  plot(functionEvalVec,resDES$diagnostic$Ft,log="y",ylim=c(10^-15,10^5), xlim=c(0,which(resDES$diagnostic$bestVal<=10^-15)[1]*lambda_DES),xlab="function evaluations",ylab="blue:DES, red:CMAES, orange:CMADE",cex=0)
  # Plot f(best)
  lines(functionEvalVec,resDES$diagnostic$bestVal, lwd=3, col="blue")
  # Plot f(mean)
  #lines(functionEvalVec,resDES$diagnostic$mean, lwd=3, col="black")

  source('C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/DESv2/DES2- finalVersion geomSelection/DESv2017.R')
  ##### DES-CMAES
  resDESCMAES <- DES(rep(0,N),fn=func,
  lower=-10^100, upper=10^100,
  control=list( "lambda"=lambda_DES, "budget"=1000*N,"diag.pop"=TRUE,"diag.Ft"=TRUE,diag.mean=TRUE,"diag.bestVal"=TRUE,"diag.worstVal"=TRUE)
  )

  functionEvalVec2 <- (1:nrow(resDESCMAES$diagnostic$bestVal))*(lambda_DES)
  lines(functionEvalVec2,resDESCMAES$diagnostic$bestVal, lwd=3, col="orange")

  ##### CMA-ES
  #source('C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/IEEEPlots/cmaesNoS.R')
  #resCMAES <- cma_esNos(rep(0,N),
  source('C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/IEEEPlots/CMAES.R')
  resCMAES <- cma_es(rep(0,N),
                     fn=func,
                     lower=-10^100, upper=10^100,
                     control=list("lambda"=lambda_DES,"diag.bestVal"=TRUE,"budget"=1000*N)
  )
  functionEvalVec3 <- (1:length(rowMeans(resCMAES$diagnostic$bestVal)))*(lambda_DES)
  lines(functionEvalVec3,abs(resCMAES$diagnostic$bestVal), lwd=3, col="red")
  dev.off()


}

AbsSigmaPlotEllipsoid <- function(){
  func <- function(x){
    res <- 0
    for(i in 1:length(x))
      for(j in 1:i)
        res <- res + x[j]^2
      return(res)
  }
  source('C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/Constriants/DES/Substitution penalty/CMADEv2017.R')
  N <- 10

  ##### DES
  resDES <- CMADE(rep(0,N),fn=func,
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

  ##### CMA-ES
  source('C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/IEEEPlots/CMAES.R')
  resCMAES <- cma_es(rep(0,N),
                     fn=func,
                     lower=-10^100, upper=10^100,
                     control=list("diag.bestVal"=TRUE,"budget"=1000*N)
  )
  functionEvalVec3 <- (1:length(rowMeans(resCMAES$diagnostic$bestVal)))*(4+floor(3*log(N)))
  lines(functionEvalVec3,abs(resCMAES$diagnostic$bestVal), lwd=3, col="orange")
}

AbsSigmaPlotDiscus <- function(N){
  func <- function(x){
    res <- 10^6 * x[1]^2
    for(i in 2:length(x))
      res <- res + x[i]^2
    return(res)
  }
  source('C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/DESv2/DES - tol_v2/DESv2017.R')
  lambda_DES <- 4+floor(3*log(N))
  #lambda_DES <- 4*N

  ##### DES
  resDES <- DES(rep(0,N),fn=func,
                lower=-10^100, upper=10^100,
                control=list("lambda"=lambda_DES, "budget"=1000*N,"diag.pop"=TRUE,"diag.Ft"=TRUE,diag.mean=TRUE,"diag.bestVal"=TRUE,"diag.worstVal"=TRUE)
  )

  setEPS()
  postscript( paste("Discus",",N=",N,".eps",sep=""), width = 8, height = 8)

  functionEvalVec <- (1:nrow(resDES$diagnostic$bestVal))*(lambda_DES)
  # Plot
  plot(functionEvalVec,resDES$diagnostic$Ft,log="y",ylim=c(10^-15,10^5), xlim=c(0,which(resDES$diagnostic$bestVal<=10^-15)[1]*lambda_DES),xlab="function evaluations",ylab="blue:DES, red:CMAES, orange:CMADE",cex=0)
  # Plot f(best)
  lines(functionEvalVec,resDES$diagnostic$bestVal, lwd=3, col="blue")
  # Plot f(mean)
  #lines(functionEvalVec,resDES$diagnostic$mean, lwd=3, col="black")

  source('C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/DESv2/DES2- finalVersion geomSelection/DESv2017.R')
  ##### DES-CMAES
  resDESCMAES <- DES(rep(0,N),fn=func,
                     lower=-10^100, upper=10^100,
                     control=list( "lambda"=lambda_DES, "budget"=1000*N,"diag.pop"=TRUE,"diag.Ft"=TRUE,diag.mean=TRUE,"diag.bestVal"=TRUE,"diag.worstVal"=TRUE)
  )

  functionEvalVec2 <- (1:nrow(resDESCMAES$diagnostic$bestVal))*(lambda_DES)
  lines(functionEvalVec2,resDESCMAES$diagnostic$bestVal, lwd=3, col="orange")

  ##### CMA-ES
  #source('C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/IEEEPlots/cmaesNoS.R')
  #resCMAES <- cma_esNos(rep(0,N),
  source('C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/IEEEPlots/CMAES.R')
  resCMAES <- cma_es(rep(0,N),
                     fn=func,
                     lower=-10^100, upper=10^100,
                     control=list("lambda"=lambda_DES,"diag.bestVal"=TRUE,"budget"=1000*N)
  )
  functionEvalVec3 <- (1:length(rowMeans(resCMAES$diagnostic$bestVal)))*(lambda_DES)
  lines(functionEvalVec3,abs(resCMAES$diagnostic$bestVal), lwd=3, col="red")
  dev.off()

}

AbsSigmaPlotCigar <- function(N){
  func <- function(x){
    res <- 0
    for(i in 2:length(x))
      res <- res + x[i]^2
    res <- res*10^6 + x[1]^2
    return(res)
  }
  source('C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/DESv2/DES - tol_v2/DESv2017.R')
  lambda_DES <- 4+floor(3*log(N))
  #lambda_DES <- 4*N

  ##### DES
  resDES <- DES(rep(0,N),fn=func,
                lower=-10^100, upper=10^100,
                control=list("lambda"=lambda_DES, "budget"=1000*N,"diag.pop"=TRUE,"diag.Ft"=TRUE,diag.mean=TRUE,"diag.bestVal"=TRUE,"diag.worstVal"=TRUE)
  )

  setEPS()
  postscript( paste("Cigar",",N=",N,".eps",sep=""), width = 8, height = 8)

  functionEvalVec <- (1:nrow(resDES$diagnostic$bestVal))*(lambda_DES)
  # Plot
  plot(functionEvalVec,resDES$diagnostic$Ft,log="y",ylim=c(10^-15,10^10), xlim=c(0,which(resDES$diagnostic$bestVal<=10^-15)[1]*lambda_DES),xlab="function evaluations",ylab="blue:DES, red:CMAES, orange:CMADE",cex=0)
  # Plot f(best)
  lines(functionEvalVec,resDES$diagnostic$bestVal, lwd=3, col="blue")
  # Plot f(mean)
  #lines(functionEvalVec,resDES$diagnostic$mean, lwd=3, col="black")

  source('C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/DESv2/DES2- finalVersion geomSelection/DESv2017.R')
  ##### DES-CMAES
  resDESCMAES <- DES(rep(0,N),fn=func,
                     lower=-10^100, upper=10^100,
                     control=list( "lambda"=lambda_DES, "budget"=1000*N,"diag.pop"=TRUE,"diag.Ft"=TRUE,diag.mean=TRUE,"diag.bestVal"=TRUE,"diag.worstVal"=TRUE)
  )

  functionEvalVec2 <- (1:nrow(resDESCMAES$diagnostic$bestVal))*(lambda_DES)
  lines(functionEvalVec2,resDESCMAES$diagnostic$bestVal, lwd=3, col="orange")

  ##### CMA-ES
  #source('C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/IEEEPlots/cmaesNoS.R')
  #resCMAES <- cma_esNos(rep(0,N),
  source('C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/IEEEPlots/CMAES.R')
  resCMAES <- cma_es(rep(0,N),
                     fn=func,
                     lower=-10^100, upper=10^100,
                     control=list("lambda"=lambda_DES,"diag.bestVal"=TRUE,"budget"=1000*N)
  )
  functionEvalVec3 <- (1:length(rowMeans(resCMAES$diagnostic$bestVal)))*(lambda_DES)
  lines(functionEvalVec3,abs(resCMAES$diagnostic$bestVal), lwd=3, col="red")
  dev.off()

}

AbsSigmaPlotRosenbrock <- function(N){
  func = function(x){
    res <- 0
    for(i in 1:(length(x)-1))
      res <- res + 100*(x[i+1]-x[i]^2)^2 + (x[i]-1)^2
    return(res)
  }
  source('C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/DESv2/DES - tol_v2/DESv2017.R')
  lambda_DES <- 4+floor(3*log(N))
  #lambda_DES <- 4*N

  ##### DES
  resDES <- DES(rep(0,N),fn=func,
                lower=-10^100, upper=10^100,
                control=list("lambda"=lambda_DES, "budget"=5000*N,"diag.pop"=TRUE,"diag.Ft"=TRUE,diag.mean=TRUE,"diag.bestVal"=TRUE,"diag.worstVal"=TRUE)
  )

  #setEPS()
  #postscript( paste("Rosenbrock",",N=",N,".eps",sep=""), width = 8, height = 8)

  functionEvalVec <- (1:nrow(resDES$diagnostic$bestVal))*(lambda_DES)
  # Plot
  plot(functionEvalVec,resDES$diagnostic$Ft,log="y",ylim=c(10^-10,10^10),xlab="function evaluations",ylab="blue:DES, red:CMAES, orange:CMADE",cex=0)
  # Plot f(best)
  lines(functionEvalVec,resDES$diagnostic$bestVal, lwd=3, col="blue")
  # Plot f(mean)
  #lines(functionEvalVec,resDES$diagnostic$mean, lwd=3, col="black")

  source('C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/DESv2/DES2- finalVersion geomSelection/DESv2017.R')
  ##### DES-CMAES
  resDESCMAES <- DES(rep(0,N),fn=func,
                     lower=-10^100, upper=10^100,
                     control=list( "lambda"=lambda_DES, "budget"=5000*N,"diag.pop"=TRUE,"diag.Ft"=TRUE,diag.mean=TRUE,"diag.bestVal"=TRUE,"diag.worstVal"=TRUE)
  )

  functionEvalVec2 <- (1:nrow(resDESCMAES$diagnostic$bestVal))*(lambda_DES)
  lines(functionEvalVec2,resDESCMAES$diagnostic$bestVal, lwd=3, col="orange")

  ##### CMA-ES
  #source('C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/IEEEPlots/cmaesNoS.R')
  #resCMAES <- cma_esNos(rep(0,N),
  source('C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/IEEEPlots/CMAES.R')
  resCMAES <- cma_es(rep(0,N),
                     fn=func,
                     lower=-10^100, upper=10^100,
                     control=list("lambda"=lambda_DES,"diag.bestVal"=TRUE,"budget"=5000*N)
  )
  functionEvalVec3 <- (1:length(rowMeans(resCMAES$diagnostic$bestVal)))*(lambda_DES)
  lines(functionEvalVec3,abs(resCMAES$diagnostic$bestVal), lwd=3, col="red")
  #dev.off()
}
# lambda_ = 1  - Sharp ridge
# lambda_ = 2  - Parabolic ridge
AbsSigmaPlotParabolicRidge <- function(N,lambda_){

  func<-function(x){
    res <- 0
    for(i in 2:(length(x)))
      res <- res + (x[i])^2
    return(x[1] + res^(lambda_/2)*100)
  }

  source('C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/DESv2/DES - tol_v2/DESv2017.R')
  lambda_DES <- 4+floor(3*log(N))
  #lambda_DES <- 4*N

  ##### DES
  resDES <- DES(rep(0,N),fn=func,
                lower=-10^100, upper=10^100,
                control=list("lambda"=lambda_DES, "budget"=5000*N,"diag.pop"=TRUE,"diag.Ft"=TRUE,diag.mean=TRUE,"diag.bestVal"=TRUE,"diag.worstVal"=TRUE)
  )

 # setEPS()
  #if(lambda_ == 1 )
  #  postscript( paste("SharpRidge",",N=",N,".eps",sep=""), width = 8, height = 8)
  #else
  #  postscript( paste("ParabolicRidge",",N=",N,".eps",sep=""), width = 8, height = 8)

  resDES$diagnostic$bestVal[resDES$diagnostic$bestVal<=0] <-10^-64
  functionEvalVec <- (1:nrow(resDES$diagnostic$bestVal))*(lambda_DES)
  # Plot
  #xlim=c(0,which(resDES$diagnostic$bestVal<=10^-10)[1]*lambda_DES)
  # ylim=c(10^-10,10^5)
  plot(functionEvalVec,resDES$diagnostic$Ft,log="y",xlab="function evaluations",ylim=c(10^-10,10^10),ylab="blue:DES, red:CMAES, orange:CMADE",cex=0)
  # Plot f(best)
  lines(functionEvalVec,resDES$diagnostic$bestVal, lwd=3, col="blue")
  # Plot f(mean)
  #lines(functionEvalVec,resDES$diagnostic$mean, lwd=3, col="black")

  source('C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/DESv2/DES2- finalVersion geomSelection/DESv2017.R')
  ##### DES-CMAES
  resDESCMAES <- DES(rep(0,N),fn=func,
                     lower=-10^100, upper=10^100,
                     control=list( "lambda"=lambda_DES, "budget"=5000*N,"diag.pop"=TRUE,"diag.Ft"=TRUE,diag.mean=TRUE,"diag.bestVal"=TRUE,"diag.worstVal"=TRUE)
  )

  resDESCMAES$diagnostic$bestVal[resDESCMAES$diagnostic$bestVal<=0] <-10^-64

  functionEvalVec2 <- (1:nrow(resDESCMAES$diagnostic$bestVal))*(lambda_DES)
  lines(functionEvalVec2,resDESCMAES$diagnostic$bestVal, lwd=3, col="orange")

  ##### CMA-ES
  #source('C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/IEEEPlots/cmaesNoS.R')
  #resCMAES <- cma_esNos(rep(0,N),
  source('C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/IEEEPlots/CMAES.R')
  resCMAES <- cma_es(rep(0,N),
                     fn=func,
                     lower=-10^100, upper=10^100,
                     control=list("lambda"=lambda_DES,"diag.bestVal"=TRUE,"budget"=5000*N)
  )
  resCMAES$diagnostic$bestVal[resCMAES$diagnostic$bestVal<=0] <-10^-64

  functionEvalVec3 <- (1:length(rowMeans(resCMAES$diagnostic$bestVal)))*(lambda_DES)
  lines(functionEvalVec3,abs(resCMAES$diagnostic$bestVal), lwd=3, col="red")
  print(resCMAES$diagnostic$bestVal)

  #dev.off()
}


AbsSigmaPlotDifferentPowers <- function(N){

  func<-function(x){
    res <- 0
    for(i in 1:(length(x)))
      res <- res + (x[i]^2)^(1+5*(i-1)/(length(x)-1))
    return(res)
  }

  source('C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/DESv2/DES - tol_v2/DESv2017.R')
  lambda_DES <- 4+floor(3*log(N))
  #lambda_DES <- 4*N

  ##### DES
  resDES <- DES(rep(0,N),fn=func,
                lower=-10^100, upper=10^100,
                control=list("lambda"=lambda_DES, "budget"=5000*N,"diag.pop"=TRUE,"diag.Ft"=TRUE,diag.mean=TRUE,"diag.bestVal"=TRUE,"diag.worstVal"=TRUE)
  )

  setEPS()
  postscript( paste("DifferentPowers",",N=",N,".eps",sep=""), width = 8, height = 8)

  functionEvalVec <- (1:nrow(resDES$diagnostic$bestVal))*(lambda_DES)
  # Plot
  plot(functionEvalVec,resDES$diagnostic$Ft,log="y",ylim=c(10^-10,10^10),xlim=c(0,which(resDES$diagnostic$bestVal<=10^-10)[1]*lambda_DES),xlab="function evaluations",ylab="blue:DES, red:CMAES, orange:CMADE",cex=0)
  # Plot f(best)
  lines(functionEvalVec,resDES$diagnostic$bestVal, lwd=3, col="blue")
  # Plot f(mean)
  #lines(functionEvalVec,resDES$diagnostic$mean, lwd=3, col="black")

  source('C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/DESv2/DES2- finalVersion geomSelection/DESv2017.R')
  ##### DES-CMAES
  resDESCMAES <- DES(rep(0,N),fn=func,
                     lower=-10^100, upper=10^100,
                     control=list( "lambda"=lambda_DES, "budget"=5000*N,"diag.pop"=TRUE,"diag.Ft"=TRUE,diag.mean=TRUE,"diag.bestVal"=TRUE,"diag.worstVal"=TRUE)
  )

  functionEvalVec2 <- (1:nrow(resDESCMAES$diagnostic$bestVal))*(lambda_DES)
  lines(functionEvalVec2,resDESCMAES$diagnostic$bestVal, lwd=3, col="orange")

  ##### CMA-ES
  #source('C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/IEEEPlots/cmaesNoS.R')
  #resCMAES <- cma_esNos(rep(0,N),
  source('C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/IEEEPlots/CMAES.R')
  resCMAES <- cma_es(rep(0,N),
                     fn=func,
                     lower=-10^100, upper=10^100,
                     control=list("lambda"=lambda_DES,"diag.bestVal"=TRUE,"budget"=5000*N)
  )
  functionEvalVec3 <- (1:length(rowMeans(resCMAES$diagnostic$bestVal)))*(lambda_DES)
  lines(functionEvalVec3,abs(resCMAES$diagnostic$bestVal), lwd=3, col="red")
  dev.off()


}
