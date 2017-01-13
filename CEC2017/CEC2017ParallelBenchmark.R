benchmarkParallelCMADE <- function() {
  library(foreach)
  library(doParallel)
  
  start.time  <- Sys.time()
  
  # Minimal fitness values for each problem
  scores <- seq(from = 100, to = 3000, by=100)
  
  # Calculate the number of cores
  no_cores <- detectCores() - 1
  
  # Initiate cluster
  registerDoParallel(no_cores)
  
  cat("Problem(N=Dim,D=Problem),Median, Best, Worst, Mean, Sd\n")

  # For each of problem dimmension
  for(d in c(50,100)){
    # Make parallel computing for each of 30 problems
    results = foreach(n = 1:30, 
                      .combine = c,
                      .export = c("scores","d") )  %dopar%  {
                        
                        source('CMADEv12.R')
                        library(cec2017)
                        resultVector <- c()

			# 51 runs per problem
                        for(i in 1:51){
			  result <- tryCatch(
				{
					CMADE(
						rep(0,d),
						fn=function(x){
							cec2017(n,x)
						},
						lower=-100,
						upper=100,
						control=list("Lamarckism"=FALSE)
					)
				},
				error=function(cond) {
					print(paste("Problem:", d," ",cond))
				}

			  )   
                          
                          resultVector <- c(resultVector, abs(result$value-scores[n]))
                        }
                        write.table(resultVector, file = paste("N/N",n,"-D",d,sep=""), sep = ",")
                        
                        return( paste(paste("CEC2017 N=",n," D=",d,sep=""),median(resultVector), min(resultVector), max(resultVector), mean(resultVector),sd(resultVector),sep=",") )
                      }
    # print results on the output
    noquote(results)
    
  }
  stopImplicitCluster()
  time.taken  <- Sys.time() - start.time
  noquote(paste("Calculation time[hours]: ",as.numeric(time.taken, units = "hours")))
}

CEC2017tableCreate <- function(x){
  
  path <- getwd()
  
  for(n in c(10,30,50,100)){
    csvResults <-  matrix(0, nrow = 51, ncol = 1)
    if(file.exists(paste(path,"/N/N1-D",n,sep="")) ){
      for(i in 1:30){
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
