benchmarkBoxPlot <- function() {
  
  standarization <- function(x){(x-min(x))/(max(x)-min(x))}
  
  library(reshape2)
  library(lattice)
  library(ggplot2)
  
  v1resultsFolder <- "C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/Basic/CEC2013"
  v2resultsFolder <- "C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/Basicv2/CEC2013_cpc=0.375"
   
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
      vresults[["Version"]] <- ifelse(v==1,"CMADE v1","CMADE v2(c_pc=0.375)")
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
    #CMADEN1(rep(0,2),fn=function(x){cec2013(p,x)}, control=list("lambda"=500,"budget"=14500))
    CMADE(rep(0,2),fn=function(x){cec2013(p,x)}, control=list("lambda"=500,"budget"=14500, "Lamarckism"=FALSE))
    frames = floor(nrow(all_populations)/2)
    
    for(i in 1:frames){
      
      if (i < 10) {name = paste('000',i,'plot.png',sep='')}
      if (i < 100 && i >= 10) {name = paste('00',i,'plot.png', sep='')}
      if (i >= 100) {name = paste('0', i,'plot.png', sep='')}
      
      png(name)
     # plot(all_populations[2*i-1,],all_populations[2*i,],   xlab="x", ylab="y", xlim=c(-100, 100), ylim=c(-100, 100),
      #     main = paste("CMADE OLD\nCE2013 P=",p,"\nPopulation number ", i,sep=""), col="red", pch=19)
      plot(all_populations[2*i-1,],all_populations[2*i,],   xlab="x", ylab="y", xlim=c(-100, 100), ylim=c(-100, 100),
           main = paste("CMADE NEW c_pc=0.5\nCE2013 P=",p,"\nPopulation number ", i,sep=""), col="red", pch=19)
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