cec2013my <- function(n,x){
  if(any(x>100) || any(x<(-100)))
    print(paste("ERROR ON N=",n))
  return (cec2013(n,x))
}

benchmarkCMADE <- function(fnumber) {
  source('C:/Users/JS/Desktop/CMADE-v12.R')

  library(cec2013)
  library(cec2005benchmark)
  
  start.time  <- Sys.time()

  # CEC2013 Benchmarking
  # 28 minimalization problems
  scores <- c(seq(from = -1400, to = -100, by=100),seq(from = 100, to = 1400, by=100))
  for(n in 1:28){
    # each problem is considered in three dimensions
    for(d in c(10)){
      # 51 runs per problem
      resultVector <- c()
      for(i in 1:51){
        result <- CMADE(
                      rep(0,d),
                      fn=function(x){
                          cec2013my(n,x)
                                    }
                      )  
        
        resultVector <- c(resultVector, abs(result$value-scores[n]))
        print(paste("PROBLEM=",n," ,DIM=",d," ,ITER=",i))
      }
      #write.table(resultVector, file = paste("C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/Basicv2/CEC2013/N",n,"-D",d,sep=""), sep = ",")
      print(paste("CEC2013 N=",n," D=",d, " --> Best: ", min(resultVector)," Worst: ",max(resultVector)," Mean: ", mean(resultVector)," Sd: ",sd(resultVector),sep=""))
      #cat(paste("CEC2013 N=",n," D=",d, " --> Best: ", min(resultVector)," Worst: ",max(resultVector)," Mean: ", mean(resultVector)," Sd: ",sd(resultVector),sep="","\n"),file="C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/Basicv2/CEC2013/output.txt",append=TRUE)
    }
  }
  time.taken  <- Sys.time() - start.time
  print(paste("Calculation time[hours]: ",as.numeric(time.taken, units = "hours")))
  
  
}

## Rscript style execution
benchmarkCMADE()
