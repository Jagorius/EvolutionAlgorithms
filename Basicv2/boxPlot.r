benchmarkBoxPlot <- function() {
  
  standarization <- function(x){(x-min(x))/(max(x)-min(x))}
  
  library(reshape2)
  library(lattice)
  library(ggplot2)
  
  v1resultsFolder <- "C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/Basic/CEC2013"
  v2resultsFolder <- "C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/Basicv2/CEC2013_lamark_bBack1_cpc=0.25"
   
  # for dimmension=10
  resultMatrix <- matrix(0, nrow=0, ncol = 3)
  colnames(resultMatrix) <- c("Version","Function","error")
  d <- 10
  
  # for every problem in CEC2013
  for(n in 1:28){
    allVresults <- matrix(0, nrow=0, ncol = 3)
    colnames(allVresults) <- c("Version","Function","error")
    for(v in c(1,2)){
      vresults <- read.table(file = paste(ifelse(v==1,v1resultsFolder,v2resultsFolder),"/N",n,"-D",d,sep=""),sep=",")
      names(vresults) <- 'error'
      vresults[["Version"]] <- ifelse(v==1,"CMADE v1","CMADE v2(c_pc=0.5, Lamarckism)")
      vresults[["Function"]] <- paste("F",n,sep="")
      allVresults <- rbind(allVresults, vresults)
      
    }
    allVresults[["error"]] <- standarization(allVresults[["error"]])
    resultMatrix <- rbind(resultMatrix, allVresults)
    
  }
  
  ggplot(data = resultMatrix, aes(x=Function, y=error)) + geom_boxplot(aes(fill=Version))
  
 # p <- ggplot(data = resultMatrix, aes(x=Function, y=error)) + 
  #  geom_boxplot(aes(fill=Version))
  #p + facet_wrap( ~ Function, scales="free")
  
}

CmadeDistribuition <- function() {
  library(devtools)
  library(animation)
  library(cec2013)
  
  source('C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/CMAES-DEvN1/CMADE-vN1.R')
  source('C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/Basicv2/CMADE.R')
  
  setwd("C:/Users/JS/Documents/frames")

  for(p in 1:28){
    set.seed(42)
    #CMADEN1(rep(0,2),fn=function(x){cec2013my(p,x)}, control=list("lambda"=500,"budget"=14500))
    CMADE(rep(0,2),fn=function(x){cec2013(p,x)}, control=list("lambda"=500,"budget"=14500, "Lamarckism"=TRUE))
    frames = floor(nrow(all_populations)/2)
    
    for(i in 1:frames){
      
      if (i < 10) {name = paste('000',i,'plot.png',sep='')}
      if (i < 100 && i >= 10) {name = paste('00',i,'plot.png', sep='')}
      if (i >= 100) {name = paste('0', i,'plot.png', sep='')}
      
      png(name)
      plot(all_populations[2*i-1,],all_populations[2*i,],   xlab="x", ylab="y", xlim=c(-100, 100), ylim=c(-100, 100),
           main = paste("CMADE OLD\nCE2013 P=",p,"\nPopulation number ", i,sep=""), col="red", pch=19)
      #plot(all_populations[2*i-1,],all_populations[2*i,],   xlab="x", ylab="y", xlim=c(-100, 100), ylim=c(-100, 100),
       #    main = paste("CMADE NEW c_pc=0.5, Lamarckism, Ft(REP_num)\nCE2013 P=",p,"\nPopulation number ", i,sep=""), col="red", pch=19)
      text(80, 90, paste("FT=",round(all_FT[i], digits = 6)), col='blue')
      text(80, 80, paste("REP_NUM=",all_REP[i]), col='blue')
      text(80, 70, paste("pc=(",round(all_PC[i,1], digits = 3),",",round(all_PC[i,2], digits = 3),")",sep=''), col='blue')
      
      points(all_NEWMEAN[i,1], all_NEWMEAN[i,2],pch = 21,col="green")
      arrows(all_NEWMEAN[i,1],all_NEWMEAN[i,2],all_NEWMEAN[i,1]+all_PC[i,1],all_NEWMEAN[i,2]+all_PC[i,2],length=0.15,angle=40,lwd=2, col="dodgerblue4")
      dev.off()
        
    } 
    system(paste('"C:\\Program Files\\ImageMagick-6.9.3-Q16\\convert.exe" -delay 80 *.png ',p,".gif",sep=""))
    
    # remove frames
    file.remove(list.files(pattern=".png"))
    
    print(paste("DONE",p))
  }
  
  setwd("C:/Users/JS/Documents/R")
}

EigenPlot <- function() {
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
  
  # Square root of each value in vector
  eigen <- sqrt(abs(all_EIGEN))

  # Divide each column by corresponding fitness function weight
  #for(i in 1:ncol(eigen))
   # eigen[,i] <- eigen[,i] / 10^(6*( (i-1)/(ncol(eigen)-1) ))
  
  functionEvalVec <- (1:nrow(eigen))*(ncol(all_populations)+1)
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
    source('C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/Basicv2/CMADE.R')
    N <- 9 
    
    CMADE(rep(0,N),fn=function(x){ 
      res <- 0
      for(i in 1:length(x))
        res <- res + 10^(6*(i-1)/(length(x)-1))*x[i]^2
      return(res)
    }, 
    lower=-10^100, upper=10^100,
    control=list("budget"=6000)
    )
    
    # Prepare points to draw plot
    sdCordPopulation <- matrix(0, nrow=0, ncol=N)
    for(p in 1:(nrow(all_populations)/N)){
      sdCordVector <- c()
      for(i in 1:N){
        # for each coordinates of given population number 
        # store standard deviation, divided by corresponding Ft
        # value in result vector
        sdCordVector <- c(sdCordVector, sd(all_populations[N*(p-1)+i,])/all_FT[p])
      }
      sdCordPopulation <- rbind(sdCordPopulation,sdCordVector)
    }
    
    functionEvalVec <- (1:nrow(sdCordPopulation))*(ncol(all_populations)+1)
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
  source('C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/Basicv2/CMADE.R')
  N <- 10 
  
  CMADE(rep(0,N),fn=function(x){ 
                                  res <- 0
                                  for(i in 1:length(x))
                                    res <- res + 10^(6*(i-1)/(length(x)-1))*x[i]^2
                                  return(res)
                                }, 
                                lower=-10^100, upper=10^100,
                                control=list("budget"=1000, "lambda"=10)
  )
  
  # Prepare points to draw plot
  PopulationChanges <- matrix(0, nrow=0, ncol=(nrow(all_populations)/N))
  # For each individual
  for(n in 1:ncol(all_populations)){
    individualChangesVector <- c()
    # For each generation
    for(i in 1:(nrow(all_populations)/N) ){
      # Store mean 
      individualChangesVector <- c(individualChangesVector, mean(all_populations[(N*(i-1)+1):(N*i),n]) )
    }
    PopulationChanges <- rbind(PopulationChanges,individualChangesVector)
  }
  
  functionEvalVec <- (1:ncol(PopulationChanges))*(ncol(all_populations)+1)
  # Plot Standard Deviations divided by Ft for each dimmension
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
