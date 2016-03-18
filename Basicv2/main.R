benchmarkCMADE <- function(fnumber) {
	source("CMADE.R")
  
  library(cec2013)
  library(cec2005benchmark)
  
  start.time  <- Sys.time()

  # CEC2013 Benchmarking
  # 28 minimalization problems
  for(n in 1:28){
    # each problem is considered in three dimensions
    for(d in c(10)){
      # 51 runs per problem
      resultVector <- c()
      for(i in 1:51){
        result <- CMADE(
                      rep(0,d),
                      fn=function(x){
                              cec2013(n,x)
                                    }
                      )  
        
        resultVector <- c(resultVector, result$value)
      }
      write.table(resultVector, file = paste("./CEC2013/N",n,"-D",d,sep=""), sep = ",")
      print(paste("CEC2013 N=",n," D=",d, " --> Best: ", min(resultVector)," Worst: ",max(resultVector)," Mean: ", mean(resultVector)," Sd: ",sd(resultVector),sep=""))
      
      
    }
  }
    
  time.taken  <- Sys.time() - start.time
  print(paste("Calculation time[hours]: ",as.numeric(time.taken, units = "hours")))
  
  
}

## Rscript style execution
benchmarkCMADE()
