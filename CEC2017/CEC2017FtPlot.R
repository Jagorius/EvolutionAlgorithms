FtPlot <- function(){

  library(foreach)
  library(doParallel)
  
  # Calculate the number of cores
  no_cores <- detectCores() - 1
  
  # Initiate cluster
  registerDoParallel(no_cores)
  
  # For each of problem dimmension
  for(d in c(30,50)){
    # Make parallel computing for each of 30 problems
    results = foreach(n = 1:30, 
                      .combine = c,
                      .export = c("scores","d") )  %dopar%  {
                        
                        source('CMADEv2017.R')
                        library(cec2017)

                        res <- tryCatch(
                        {
                            CMADE(
                              rep(0,d),
                              fn=function(x){
                                  cec2017(n,x)
                              },
                              lower=-100,
                              upper=100,
                              control=list("Lamarckism"=FALSE, "diag"=TRUE)
                            )
                        },
                          error=function(cond) {
                            print(paste("Problem:", d," ",cond))
                          }
                        )
                        
                        functionEvalVec <- (1:length(res$diagnostic$Ft))*(dim(res$diagnostic$pop)[2]+1)
                        
			png(paste("FTPlot/N",d,"-P",n,".png",sep=""), width = 1024, height = 768)
                        
                        # Plot Ft
                        plot(functionEvalVec,res$diagnostic$Ft, log="y", ylim=c(0.00001,1000), xlab="function evaluations", ylab=expression(bold(red):total/direct~~bold(green):Ft),cex=0)
                        xlab=expression()
                        lines(functionEvalVec,res$diagnostic$Ft, lwd=2, col="green")
                        
                        title(paste("CMADEv2017-v4\n DIM=",d," ","Problem=",n))
                        
                        dev.off()
                        

                      }

  }
  stopImplicitCluster()
  
}

FtPlot()