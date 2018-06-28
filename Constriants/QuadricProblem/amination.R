QuadricAnimationGIF <- function() {
  library(devtools)
  library(animation)

  DATA_VERS = c( "Lamarckian projection", "Lamarckian reflection", "Lamarckian wrapping", "Reinitialization", 
                                "Resampling", "Darwinian projection", "Darwinian reflection", 
                                "Darwinian wrapping", "Substitution penalty", "Scaled mutant", 
                                "Quadratic penalty", "Midpoint target", "Scaled to base", "Rand base", "Conservatism", "Midpoint base" ) 

  for(ver in DATA_VERS){   
    if(DATA_VER == "Darwinian projection" || DATA_VER == "Darwinian reflection" || 
       DATA_VER == "Darwinian wrapping" || DATA_VER == "Quadratic penalty" || 
       DATA_VER == "Substitution penalty")
      isDarw = TRUE
    else
      isDarw= FALSE
    
    b <- 1
    print(ver)
    source(paste("C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/Constriants/DES/",ver,"/CMADEv2017.R",sep=""))
    
    setwd("C:/Users/JS/Documents/frames")
    
    lambda<- 80
    POPULATION_DES_FIRST <<- replicate(lambda, runif(2,-0.8,0.8))
    
      aa <<- CMADE(rep(0,2),fn=function(x){
        sum(x-b)^2
      },  lower=-1,
      upper=1, control=list("lambda"=lambda,"budget"=8000, "diag.pop"=TRUE,"diag.bestVal"=TRUE))
      
      frames = dim(aa$diagnostic$pop)[3]
      
      for(i in 1:frames){
        
        if (i < 10) {name = paste('000',i,'plot.png',sep='')}
        if (i < 100 && i >= 10) {name = paste('00',i,'plot.png', sep='')}
        if (i >= 100) {name = paste('0', i,'plot.png', sep='')}
        
        png(name)
        plot(aa$diagnostic$pop[1,,i],aa$diagnostic$pop[2,,i],   xlab="x", ylab="y", xlim=c(-1, 1), ylim=c(-1, 1),
             main = paste("DES ",ver ,"\nsum(x-1)^2", "\nPopulation number ", i,sep=""), col="red", pch=19)
        text(-0.7, -1, paste("BestVal=",round(aa$diagnostic$bestVal[i], digits = 8)), col='blue')
        #text(80, 80, paste("REP_NUM=",all_REP[i]), col='blue')
        #text(80, 70, paste("pc=(",round(all_PC[i,1], digits = 3),",",round(all_PC[i,2], digits = 3),")",sep=''), col='blue')
        
        #points(all_NEWMEAN[i,1], all_NEWMEAN[i,2],pch = 21,col="green")
        #arrows(all_NEWMEAN[i,1],all_NEWMEAN[i,2],all_NEWMEAN[i,1]+all_PC[i,1],all_NEWMEAN[i,2]+all_PC[i,2],length=0.15,angle=40,lwd=2, col="dodgerblue4")
        dev.off()
        
      } 
      system(paste('"C:\\Program Files\\ImageMagick-6.9.3-Q8\\convert.exe" -delay 80 *.png ',gsub(" ", "_", ver),".gif",sep=""))
      
      # remove frames
      file.remove(list.files(pattern=".png"))
      
      print(paste("DONE",p))
    
  }
  setwd("C:/Users/JS/Documents/R")
}