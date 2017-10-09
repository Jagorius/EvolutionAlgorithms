EigenPlot <- function() {
  source('C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/DESv2/Experimentum Crucis/CMADEv2017.R')
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

DevCoordinatesPlot <- function(){
  source('C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/DESv2/Experimentum Crucis/CMADEv2017.R')
  N <- 9 
  
  resDES <- CMADE(rep(0,N),fn=function(x){ 
    res <- 0
    for(i in 1:length(x))
      res <- res + 10^(6*(i-1)/(length(x)-1))*x[i]^2
    return(res)
  }, 
  lower=-10^100, upper=10^100,
  control=list("budget"=6000,"diag.pop"=TRUE,"diag.Ft"=TRUE)
  )
  
  # Prepare points to draw plot
  sdCordPopulation <- matrix(0, nrow=0, ncol=N)
  for(p in 1:(dim(resDES$diagnostic$pop)[3])){
    sdCordVector <- c()
    for(i in 1:N){
      # for each coordinates of given population number 
      # store standard deviation, divided by corresponding Ft
      # value in result vector
      sdCordVector <- c(sdCordVector, sd(resDES$diagnostic$pop[i,,p])/resDES$diagnostic$Ft[p])
    }
    sdCordPopulation <- rbind(sdCordPopulation,sdCordVector)
  }
  
  functionEvalVec <- (1:nrow(sdCordPopulation))*(ncol(resDES$diagnostic$pop[,,1])+1)
  # Plot Standard Deviations divided by Ft for each dimmension
  plot(functionEvalVec,sdCordPopulation[,N],log="y",ylim=c(min(sdCordPopulation),max(sdCordPopulation)), xlab="function evaluations", ylab="Standard Deviations divided by Ft",cex=0)
  lines(functionEvalVec,sdCordPopulation[,N], lwd=2)
  
  colours <- c("red","darkgreen","orchid","blue","tomato4","yellow4","snow3","plum","seashell4")
  for(c in (ncol(sdCordPopulation)-1):1){
    points(functionEvalVec,sdCordPopulation[,c],col=colours[c],cex=0)
    lines(functionEvalVec,sdCordPopulation[,c],col=colours[c], lwd=2)
  }
}

ObjectPlot <- function(){
  source('C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/DESv2/Experimentum Crucis/CMADEv2017.R')
  N <- 10 
  
  resDES <- CMADE(rep(0,N),fn=function(x){ 
    res <- 0
    for(i in 1:length(x))
      res <- res + 10^(6*(i-1)/(length(x)-1))*x[i]^2
    return(res)
  }, 
  lower=-10^100, upper=10^100,
  control=list("budget"=1000, "lambda"=10, "diag.pop"=TRUE)
  )
  
  # Prepare points to draw plot
  PopulationChanges <- matrix(0, nrow=0, ncol=(dim(resDES$diagnostic$pop)[3]))
  # For each individual
  for(n in 1:(dim(resDES$diagnostic$pop)[2])){
    individualChangesVector <- c()
    # For each generation
    for(i in 1:(dim(resDES$diagnostic$pop)[3]) ){
      # Store mean 
      individualChangesVector <- c(individualChangesVector, mean(resDES$diagnostic$pop[,n,i]) )
    }
    PopulationChanges <<- rbind(PopulationChanges,individualChangesVector)
  }
  
  functionEvalVec <- (1:ncol(PopulationChanges))*(ncol((dim(resDES$diagnostic$pop)[2]))+1)
  plot(functionEvalVec,PopulationChanges[nrow(PopulationChanges),],ylim=c(min(PopulationChanges),max(PopulationChanges)), xlab="function evaluations", ylab="Object Variables (mean, lambda=10)",cex=0)
  lines(functionEvalVec,PopulationChanges[nrow(PopulationChanges),], lwd=2)
  
  colours <- c("red","darkgreen","orchid","blue","tomato4","yellow4","snow3","plum","seashell4")
  for(c in (nrow(PopulationChanges)-1):1){
    points(functionEvalVec,PopulationChanges[c,],col=colours[c],cex=0)
    lines(functionEvalVec,PopulationChanges[c,],col=colours[c], lwd=2)
  }
}

AbsSigmaPlot <- function(){
  source('C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/Basicv2/CMADE.R')
  N <- 10
  
  CMADE(rep(0,N),fn=function(x){ 
    res <- 0
    for(i in 1:length(x))
      res <- res + 10^(6*(i-1)/(length(x)-1))*x[i]^2
    return(res)
  }, 
  lower=-10^100, upper=10^100,
  control=list("budget"=6000)
  )
  
  functionEvalVec <- (1:nrow(all_FT))*(ncol(all_populations)+1)
  # Plot Ft
  plot(functionEvalVec,all_FT,log="y",ylim=c(min(min(all_FT),min(all_FITNES)),max(all_FITNES)), xlab="function evaluations", ylab="green:Ft, blue:best, black:mean",cex=0)
  lines(functionEvalVec,all_FT, lwd=2, col="green")
  
  # Plot f(best)
  lines(functionEvalVec,apply(all_FITNES,1,min), lwd=3, col="blue")
  
  # Plot f(mean)
  lines(functionEvalVec,ALL_FITMEAN, lwd=3, col="black")
  
  
  
}


