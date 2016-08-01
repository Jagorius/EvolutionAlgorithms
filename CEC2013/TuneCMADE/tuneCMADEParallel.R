tuneCMADE <- function() {
  start.time  <- Sys.time()
  
  source('CMADE.R')
  result <- CMADE(
      rep(0,2),
      fn=function(x){
                      library(foreach)
                      library(doParallel)
                      library(cec2013)
                      # Calculate the number of cores
                      no_cores <- detectCores() - 1
                      # Initiate cluster
                      registerDoParallel(no_cores)
                      
                      # Minimal fitness values for each problem
                      scores <- c(seq(from = -1400, to = -100, by=100),seq(from = 100, to = 1400, by=100))
                      
                      FITNESS_VAL <- 0
                      
                      # For defined problem numbers
                      for(n in c(3,15,23)){
                        results = foreach(i = 1:no_cores, 
                                          .combine = c,
                                          .export = c("scores","n","x") )  %dopar%  {
                                            res <- CMADE(
                                                rep(0,10), 
                                                fn=function(x){
                                                  cec2013(n,x)
                                                              },
                                                control=list("cpc"=x[1], "c_Ft"=x[2])
                                                )
                                            return(abs(res$value-scores[n]))
                                          }
                        FITNESS_VAL <- FITNESS_VAL + sum(results)
                      }
                      stopImplicitCluster()
                      return(FITNESS_VAL)
                      
                    },
      lower=0, 
      upper=1,
      control=list("Lamarckism"=FALSE, "budget"=360)
  )  
  
  print(result)
  
  time.taken  <- Sys.time() - start.time
  print(paste("Calculation time[hours]: ",as.numeric(time.taken, units = "hours")))
  
  
}

## Rscript convention
tuneCMADE()
