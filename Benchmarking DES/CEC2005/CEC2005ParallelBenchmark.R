benchmarkParallelCMADE <- function() {
  suppressMessages(library(foreach))
  suppressMessages(library(doParallel))
  
  start.time  <- Sys.time()
  
  # Minimal fitness values for each problem
  scores <- c(-450,-450,-450,-450,-310, 390, -180, -140, -330, -330, 90, -460, -130, -300, 120, 120, 120, 10, 10, 10, 360, 360, 360, 260, 260)
  
  # Constraints
  constraintsLow = c(-100,-100,-100,-100,-100,-100,0,-32,-5,-5,-0.5,-3.1415926535,-3,-100,-5,-5,-5,-5,-5,-5,-5,-5,-5,-5,2)
  constraintsUpp = c(100,100,100,100,100,100,600,32,5,5,0.5,3.1415926535,1,100,5,5,5,5,5,5,5,5,5,5,5)

  # Calculate the number of cores
  no_cores <- detectCores() - 1
  
  # Initiate cluster
  registerDoParallel(no_cores)
  
  cat("Problem(N=Dim D=Problem),Median, Best, Worst, Mean, Sd, Resets\n")

  # For each of problem dimmension
  for(d in c(10,30,50)){
    # Make parallel computing for each of 30 problems
    results = foreach(n = 1:25, 
                      .combine = c,
                      .export = c("scores","d","constraintsLow","constraintsUpp") )  %dopar%  {
                        
                        source('CMADEv2017.R')
                        library(cec2005benchmark)
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
							cec2005benchmark(n,x)
						},
						lower=constraintsLow[n],
						upper=constraintsUpp[n],
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
                        
      return( paste(paste("CEC2005 N=",n," D=",d,sep=""),median(resultVector), min(resultVector), max(resultVector), mean(resultVector),sd(resultVector), mean(resets), sep=",") )
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
  
  for(n in c(10,30,50)){
    csvResults <-  matrix(0, nrow = 51, ncol = 1)
    if(file.exists(paste(path,"/N/N1-D",n,sep="")) ){
      for(i in 1:25){
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
