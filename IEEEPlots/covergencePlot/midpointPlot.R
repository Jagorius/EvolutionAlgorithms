Cigar <- function(x){
  res <- 0
  for(i in 2:length(x))
    res <- res + x[i]^2
  res <- res*10^6 + x[1]^2
  return(res)
}


midpointPlot <- function(){
 
  N <- 10
  L = 4+floor(3*sqrt(N))
 # L = 4*N
  bud = 6000
  Iters <- 50
  resDESMidVal <- c()
  resCMAESNosMidVal <- c()
  resCMADEMidVal <- c()
  resCMADEhistConstMidVal <- c()
  
  for (i in 1:Iters){
    print(i)
    #================================
    source('C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/DESv2/DES - tol_v2/DESv2017.R')
    resDES <- DES(rep(0,N),
                    fn=Cigar, 
                    lower=-10^100, upper=10^100,
                    control=list("budget"=bud,"lambda"=L,"diag.mean"=TRUE)
    )
    
    resDESMidVal <- cbind(resDESMidVal,resDES$diagnostic$mean)
    #================================
    source('C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/IEEEPlots/cmaesNoS.R')
    resCMAESNos <- cma_esNos(rep(0,N),
                             fn=Cigar, 
                             lower=-10^100, upper=10^100,
                             control=list("diag.mean"=TRUE,"budget"=bud,"lambda"=L)                
    )
    resCMAESNosMidVal <- cbind(resCMAESNosMidVal,resCMAESNos$diagnostic$mean)
    #================================
    source('C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/DESv2/DES2- finalVersion geomSelection/DESv2017.R')
    resCMADE_histConst <- DES(rep(0,N),
                  fn=Cigar, 
                  lower=-10^100, upper=10^100,
                  control=list("budget"=bud,"lambda"=L,"diag.mean"=TRUE,"histSize"=ceiling(6+ceiling(3*sqrt(N))))
    )
    resCMADEhistConstMidVal <- cbind(resCMADEhistConstMidVal,resCMADE_histConst$diagnostic$mean)
    #================================
    source('C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/DESv2/DES2- finalVersion geomSelection/DESv2017.R')
    resCMADE <- DES(rep(0,N),
                    fn=Cigar, 
                    lower=-10^100, upper=10^100,
                    control=list("budget"=bud,"lambda"=L,"diag.mean"=TRUE,"histSize"=floor(bud/(L+1)))
    )
    resCMADEMidVal <- cbind(resCMADEMidVal,resCMADE$diagnostic$mean)
  }
  functionEvalVecDES <- (1:length(rowMeans(resDESMidVal)))*(L+1)
  functionEvalVecCMAESNos <- (1:length(rowMeans(resCMAESNosMidVal)))*(L)

  setEPS()
  postscript( paste("MidPoint_N=",N,".eps",sep=""), width = 8, height = 8)
  
  plot(functionEvalVecDES,rowMeans(resDESMidVal),log="y", xlab="function evaluations", ylab="fitness funtion value" ,main="green:CMADE, red:CMADE-HistConst, blue:DESv2, black:CMAES w/o Sigma",cex=0)
  
  # Plot DES
  lines(functionEvalVecDES,rowMeans(resDESMidVal), lwd=3, col="blue")
  
  # Plot CMAESNos
  lines(functionEvalVecCMAESNos,rowMeans(resCMAESNosMidVal), lwd=3, col="black")
  
  # Plot CMADE Normal
  lines(functionEvalVecDES,rowMeans(resCMADEMidVal), lwd=3, col="green")
  
  # Plot CMADE Hist Const
  lines(functionEvalVecDES,rowMeans(resCMADEhistConstMidVal), lwd=3, col="red")
  
  
  dev.off()
  
}

eigenplotDESCords<- function(){
  source('C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/DESv2/DES - tol_v2/DESv2017.R')
  #source('C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/DESv2/DES2- finalVersion geomSelection/DESv2017.R')
  
  N <- 10
  Iters <- 50
  
  eigenLog <- list()
  meanCordsLog <- list()
  for (i in 1:Iters){
    print(i)
    resDES <- DES(rep(0,N),fn=function(x){
      return(Cigar(x))
    },
    lower=-10^100, upper=10^100,
    control=list("budget"=6000,"diag.pop"=TRUE,"diag.eigen"=TRUE, "diag.meanCords"=TRUE)
    )
    eigenLog[[length(eigenLog)+1]] <- abs(resDES$diagnostic$eigen)
    meanCordsLog[[length(meanCordsLog)+1]] <- resDES$diagnostic$meanCord
  }
  # Mean of each value in vector
  eigen <- Reduce("+", eigenLog) / length(eigenLog)
  meansCords <- Reduce("+", meanCordsLog) / length(meanCordsLog)
  # Divide each column by corresponding fitness function weight
  #for(i in 1:ncol(eigen))
  # eigen[,i] <- eigen[,i] / 10^(6*( (i-1)/(ncol(eigen)-1) ))
  
  functionEvalVec <- (1:nrow(eigen))*(ncol(resDES$diagnostic$pop[,,1])+1)
  
  setEPS()
  postscript( paste("DES-N",N,".eps",sep=""), width = 24, height = 24)
  
  # Plot eigen values changes for each dimmension
  options(scipen=3)
  par(mfrow=c(4,3))
  plot(functionEvalVec,eigen[,N],log="y",ylim=c(1e-7,1e1), xlab="function evaluations", ylab="Eigenvalues",cex=0,cex.axis=1.5,cex.lab=1.5)
  
  
  lines(functionEvalVec,eigen[,N], lwd=2)
  
  colours <- c("red","darkgreen","orchid","blue","tomato4","yellow4","snow3","plum","seashell4","black","black","black","black")
  for(c in (ncol(eigen)-1):1){
    points(functionEvalVec,eigen[,c],col=colours[c],cex=0)
    lines(functionEvalVec,eigen[,c],col=colours[c], lwd=2)
  }
  plot(functionEvalVec,abs(meansCords[,1]),log="y",main="Mean point [,1] coordinations",cex=0)
  lines(functionEvalVec,abs(meansCords[,1]),col="black",lwd=2)
  plot(functionEvalVec,abs(meansCords[,2]),log="y",main="Mean point [,2] coordinations",cex=0)
  lines(functionEvalVec,abs(meansCords[,2]),col="black",lwd=2)
  plot(functionEvalVec,abs(meansCords[,3]),log="y",main="Mean point [,3] coordinations",cex=0)
  lines(functionEvalVec,abs(meansCords[,3]),col="black",lwd=2)
  plot(functionEvalVec,abs(meansCords[,4]),log="y",main="Mean point [,4] coordinations",cex=0)
  lines(functionEvalVec,abs(meansCords[,4]),col="black",lwd=2)
  plot(functionEvalVec,abs(meansCords[,5]),log="y",main="Mean point [,5] coordinations",cex=0)
  lines(functionEvalVec,abs(meansCords[,5]),col="black",lwd=2)
  plot(functionEvalVec,abs(meansCords[,6]),log="y",main="Mean point [,6] coordinations",cex=0)
  lines(functionEvalVec,abs(meansCords[,6]),col="black",lwd=2)
  plot(functionEvalVec,abs(meansCords[,7]),log="y",main="Mean point [,7] coordinations",cex=0)
  lines(functionEvalVec,abs(meansCords[,7]),col="black",lwd=2)
  plot(functionEvalVec,abs(meansCords[,8]),log="y",main="Mean point [,8] coordinations",cex=0)
  lines(functionEvalVec,abs(meansCords[,8]),col="black",lwd=2)
  plot(functionEvalVec,abs(meansCords[,9]),log="y",main="Mean point [,9] coordinations",cex=0)
  lines(functionEvalVec,abs(meansCords[,9]),col="black",lwd=2)
  plot(functionEvalVec,abs(meansCords[,10]),log="y",main="Mean point [,10] coordinations",cex=0)
  lines(functionEvalVec,abs(meansCords[,10]),col="black",lwd=2)
  dev.off()
  
}

eigenplotCMAESNos<- function(){
  source('C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/IEEEPlots/cmaesNoS.R')
  N <- 10
  Iters <- 50
  L = 4*N
  
  eigenLog <- list()
  meanCordsLog <- list()
  
  for (i in 1:Iters){
    print(i)
    resCMAESNos <- cma_esNos(rep(0,N),
                             fn=function(x){
                               return(Cigar(x))
                             },
                             lower=-10^100, upper=10^100,
                             control=list("diag.eigen"=TRUE,"budget"=3000,"lambda"=L,"diag.meanCords"=TRUE)
    )
    eigenLog[[length(eigenLog)+1]] <- abs(resCMAESNos$diagnostic$eigen)
    meanCordsLog[[length(meanCordsLog)+1]] <- resCMAESNos$diagnostic$meanCord
    
  }
  
  # Mean of each value in vector
  eigen <- Reduce("+", eigenLog) / length(eigenLog)
  meansCords <- Reduce("+", meanCordsLog) / length(meanCordsLog)
  
  # Divide each column by corresponding fitness function weight
  #for(i in 1:ncol(eigen))
  # eigen[,i] <- eigen[,i] / 10^(6*( (i-1)/(ncol(eigen)-1) ))
  
  functionEvalVec <- (1:nrow(eigen))*(L)
  
  setEPS()
  postscript( paste("CMAESNos-N",N,".eps",sep=""), width = 24, height = 24)
  
  options(scipen=3)
  par(mfrow=c(4,3))
  
  # Plot eigen values changes for each dimmension
  #ylim=c(min(eigen),max(eigen))
  plot(functionEvalVec,eigen[,N],log="y",ylim=c(1e-7,1e1), xlab="function evaluations", ylab="Eigenvalues",cex=0)
  lines(functionEvalVec,eigen[,N], lwd=2)
  
  colours <- c("red","darkgreen","orchid","blue","tomato4","yellow4","snow3","plum","seashell4","black","black","black","black")
  for(c in (ncol(eigen)-1):1){
    points(functionEvalVec,eigen[,c],col=colours[c],cex=0)
    lines(functionEvalVec,eigen[,c],col=colours[c], lwd=2)
  }
  plot(functionEvalVec,abs(meansCords[,1]),log="y",main="Mean point [,1] coordinations",cex=0)
  lines(functionEvalVec,abs(meansCords[,1]),col="black",lwd=2)
  plot(functionEvalVec,abs(meansCords[,2]),log="y",main="Mean point [,2] coordinations",cex=0)
  lines(functionEvalVec,abs(meansCords[,2]),col="black",lwd=2)
  plot(functionEvalVec,abs(meansCords[,3]),log="y",main="Mean point [,3] coordinations",cex=0)
  lines(functionEvalVec,abs(meansCords[,3]),col="black",lwd=2)
  plot(functionEvalVec,abs(meansCords[,4]),log="y",main="Mean point [,4] coordinations",cex=0)
  lines(functionEvalVec,abs(meansCords[,4]),col="black",lwd=2)
  plot(functionEvalVec,abs(meansCords[,5]),log="y",main="Mean point [,5] coordinations",cex=0)
  lines(functionEvalVec,abs(meansCords[,5]),col="black",lwd=2)
  plot(functionEvalVec,abs(meansCords[,6]),log="y",main="Mean point [,6] coordinations",cex=0)
  lines(functionEvalVec,abs(meansCords[,6]),col="black",lwd=2)
  plot(functionEvalVec,abs(meansCords[,7]),log="y",main="Mean point [,7] coordinations",cex=0)
  lines(functionEvalVec,abs(meansCords[,7]),col="black",lwd=2)
  plot(functionEvalVec,abs(meansCords[,8]),log="y",main="Mean point [,8] coordinations",cex=0)
  lines(functionEvalVec,abs(meansCords[,8]),col="black",lwd=2)
  plot(functionEvalVec,abs(meansCords[,9]),log="y",main="Mean point [,9] coordinations",cex=0)
  lines(functionEvalVec,abs(meansCords[,9]),col="black",lwd=2)
  plot(functionEvalVec,abs(meansCords[,10]),log="y",main="Mean point [,10] coordinations",cex=0)
  lines(functionEvalVec,abs(meansCords[,10]),col="black",lwd=2)
  dev.off()
  
}

eigenplotCMADECords<- function(){
  source('C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/DESv2/DES2- finalVersion geomSelection/DESv2017.R')
  
  N <- 10
  Iters <- 50
  
  meanCordsLog <- list()
  eigenLog <- list()
  for (i in 1:Iters){
    print(i)
    resDES <- DES(rep(0,N),fn=function(x){
      return(Cigar(x))
    },
    lower=-10^100, upper=10^100,
    control=list("budget"=6000,"diag.pop"=TRUE,"diag.eigen"=TRUE,"diag.meanCords"=TRUE)
    )
    eigenLog[[length(eigenLog)+1]] <- abs(resDES$diagnostic$eigen)
    meanCordsLog[[length(meanCordsLog)+1]] <- resDES$diagnostic$meanCord
    
  }
  # Mean of each value in vector
  eigen <- Reduce("+", eigenLog) / length(eigenLog)
  meansCords <- Reduce("+", meanCordsLog) / length(meanCordsLog)
  
  # Divide each column by corresponding fitness function weight
  #for(i in 1:ncol(eigen))
  # eigen[,i] <- eigen[,i] / 10^(6*( (i-1)/(ncol(eigen)-1) ))
  
  functionEvalVec <- (1:nrow(eigen))*(ncol(resDES$diagnostic$pop[,,1])+1)
  
  setEPS()
  postscript( paste("CMADE-N",N,".eps",sep=""), width = 24, height = 24)
  
  # Plot eigen values changes for each dimmension
  options(scipen=3)
  par(mfrow=c(4,3))
  plot(functionEvalVec,eigen[,N],log="y",ylim=c(1e-7,1e1), xlab="function evaluations", ylab="Eigenvalues",cex=0,cex.axis=1.5,cex.lab=1.5)
  
  
  lines(functionEvalVec,eigen[,N], lwd=2)
  
  colours <- c("red","darkgreen","orchid","blue","tomato4","yellow4","snow3","plum","seashell4","black","black","black","black")
  for(c in (ncol(eigen)-1):1){
    points(functionEvalVec,eigen[,c],col=colours[c],cex=0)
    lines(functionEvalVec,eigen[,c],col=colours[c], lwd=2)
  }
  plot(functionEvalVec,abs(meansCords[,1]),log="y",main="Mean point [,1] coordinations",cex=0)
  lines(functionEvalVec,abs(meansCords[,1]),col="black",lwd=2)
  plot(functionEvalVec,abs(meansCords[,2]),log="y",main="Mean point [,2] coordinations",cex=0)
  lines(functionEvalVec,abs(meansCords[,2]),col="black",lwd=2)
  plot(functionEvalVec,abs(meansCords[,3]),log="y",main="Mean point [,3] coordinations",cex=0)
  lines(functionEvalVec,abs(meansCords[,3]),col="black",lwd=2)
  plot(functionEvalVec,abs(meansCords[,4]),log="y",main="Mean point [,4] coordinations",cex=0)
  lines(functionEvalVec,abs(meansCords[,4]),col="black",lwd=2)
  plot(functionEvalVec,abs(meansCords[,5]),log="y",main="Mean point [,5] coordinations",cex=0)
  lines(functionEvalVec,abs(meansCords[,5]),col="black",lwd=2)
  plot(functionEvalVec,abs(meansCords[,6]),log="y",main="Mean point [,6] coordinations",cex=0)
  lines(functionEvalVec,abs(meansCords[,6]),col="black",lwd=2)
  plot(functionEvalVec,abs(meansCords[,7]),log="y",main="Mean point [,7] coordinations",cex=0)
  lines(functionEvalVec,abs(meansCords[,7]),col="black",lwd=2)
  plot(functionEvalVec,abs(meansCords[,8]),log="y",main="Mean point [,8] coordinations",cex=0)
  lines(functionEvalVec,abs(meansCords[,8]),col="black",lwd=2)
  plot(functionEvalVec,abs(meansCords[,9]),log="y",main="Mean point [,9] coordinations",cex=0)
  lines(functionEvalVec,abs(meansCords[,9]),col="black",lwd=2)
  plot(functionEvalVec,abs(meansCords[,10]),log="y",main="Mean point [,10] coordinations",cex=0)
  lines(functionEvalVec,abs(meansCords[,10]),col="black",lwd=2)
  
  dev.off()
  
}

Cigar <- function(x){
  res <- 0
  for(i in 2:length(x))
    res <- res + x[i]^2
  res <- res*10^6 + x[1]^2
  return(res)
}
