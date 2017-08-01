benchmarkParallelCMADE <- function() {
  suppressMessages(library(foreach))
  suppressMessages(library(doParallel))
  
  start.time  <- Sys.time()
  
  # Minimal fitness values for each problem
  scores <- c(seq(from = -1400, to = -100, by=100),seq(from = 100, to = 1400, by=100))
  
  # Calculate the number of cores
  no_cores <- detectCores() - 1
  
  # Initiate cluster
  registerDoParallel(no_cores)
  
  cat("Problem(N=Dim D=Problem),Median, Best, Worst, Mean, Sd, Resets\n")

  # For each of problem dimmension
  for(d in c(2,10,30,50,100)){
    # Make parallel computing for each of 30 problems
    results = foreach(n = 1:28, 
                      .combine = c,
                      .export = c("scores","d") )  %dopar%  {
                        
                        source('CMADEv2017.R')
                        library(cec2013)
                        resultVector <- c()
			resets <- c()
      informMatrix <- matrix(0,nrow=14,ncol=51)
			# 51 runs per problem
      for(i in 1:51){
			  result <- tryCatch(
				{
					CMADE(
						rep(0,d),
						fn=function(x){
							cec2013(n,x)
						},
						lower=-100,
						upper=100,
						control=list("Lamarckism"=FALSE,"diag.bestVal"=TRUE)
					)
				},
				error=function(cond) {
					print(paste("Dim:",d," Problem:",n," ",cond))
				}

			  )   
                          
        resultVector <- c(resultVector, abs(result$value-scores[n]))
			  resets <- c(resets,result$resets)
			  
			  # Record function error value after specified bellow * MaxFES for each run
			  recordedTimes <- c(0.01, 0.02, 0.03, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)
			  for(bb in 1:14)
			    informMatrix[bb,i] <- abs(result$diagnostic$bestVal[recordedTimes[bb]*ceiling(nrow(result$diagnostic$bestVal)),] - scores[n])
			  
      }
      write.table(resultVector, file = paste("N/N",n,"-D",d,sep=""), sep = ",")
      write.table(informMatrix, file = paste("M/DES_",n,"_",d,".txt",sep=""), sep = ",", col.names = F, row.names = F)
                        
      return( paste(paste("CEC2013 N=",n," D=",d,sep=""),median(resultVector), min(resultVector), max(resultVector), mean(resultVector),sd(resultVector), mean(resets), sep=",") )
    }
    # print results on the output
    print(results, quote=FALSE)
    
  }
  stopImplicitCluster()
  time.taken  <- Sys.time() - start.time
  noquote(paste("Calculation time[hours]: ",as.numeric(time.taken, units = "hours"),",,,,,,"))
}

CEC2017tableCreate <- function(x){
  
  path <- getwd()
  
  for(n in c(2,10,30,50,100)){
    csvResults <-  matrix(0, nrow = 51, ncol = 1)
    if(file.exists(paste(path,"/N/N1-D",n,sep="")) ){
      for(i in 1:28){
        resColumn <- read.table(paste(path,"/N/N",i,"-D",n,sep=""), sep=",",header = TRUE)
        colnames(resColumn) <- paste("P",i,sep = "")
        csvResults <- cbind(csvResults,resColumn)
      }
      csvResults <- csvResults[,-1]
          
      write.csv(csvResults, file = paste(path,paste("/resTable-",n,".csv",sep=""),sep=""), row.names = TRUE)
    }
  }
  
  
}

## Rscript convention
benchmarkParallelCMADE()
CEC2017tableCreate()
