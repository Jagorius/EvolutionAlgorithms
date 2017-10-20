CEC2013AnimationGIF <- function() {
  library(devtools)
  library(animation)
  library(cec2013)
  
  source('C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/DESv2/Population Explosion/CMADEv2017.R')
  
  setwd("C:/Users/JS/Documents/frames")
  
  for(p in 7:7){
    set.seed(42)
    aa <<- CMADE(rep(0,2),fn=function(x){cec2013(p,x)}, control=list("Lamarckism"=FALSE, "diag.pop"=TRUE))
    frames = dim(aa$diagnostic$pop)[3]
    
    for(i in 1:frames){
      
      if (i < 10) {name = paste('000',i,'plot.png',sep='')}
      if (i < 100 && i >= 10) {name = paste('00',i,'plot.png', sep='')}
      if (i >= 100) {name = paste('0', i,'plot.png', sep='')}
      
      png(name)
      plot(aa$diagnostic$pop[1,,i],aa$diagnostic$pop[2,,i],   xlab="x", ylab="y", xlim=c(-1000, 1000), ylim=c(-1000, 1000),
           main = paste("DES \nCE2017 P=",p,"\nPopulation number ", i,sep=""), col="red", pch=19)
      #text(80, 90, paste("FT=",round(all_FT[i], digits = 6)), col='blue')
      #text(80, 80, paste("REP_NUM=",all_REP[i]), col='blue')
      #text(80, 70, paste("pc=(",round(all_PC[i,1], digits = 3),",",round(all_PC[i,2], digits = 3),")",sep=''), col='blue')
      
      #points(all_NEWMEAN[i,1], all_NEWMEAN[i,2],pch = 21,col="green")
      #arrows(all_NEWMEAN[i,1],all_NEWMEAN[i,2],all_NEWMEAN[i,1]+all_PC[i,1],all_NEWMEAN[i,2]+all_PC[i,2],length=0.15,angle=40,lwd=2, col="dodgerblue4")
      dev.off()
      
    } 
    system(paste('"C:\\Program Files\\ImageMagick-6.9.3-Q8\\convert.exe" -delay 80 *.png ',p,".gif",sep=""))
    
    # remove frames
    file.remove(list.files(pattern=".png"))
    
    print(paste("DONE",p))
  }
  
  setwd("C:/Users/JS/Documents/R")
}