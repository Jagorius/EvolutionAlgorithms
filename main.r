main <- function(fnumber) {
  
  library("cec2013")
  
  source('C:/Users/JS/Desktop/Doktorat/CMAES-roznicowy/CMAES-DE.R')
  source('CMAES.R')
  
  outputFile <- "result1"
 
  cat("               TIME-MIN     BEST","\n")
  cat("               TIME-MIN     BEST","\n", file=outputFile,append=TRUE)
  for (i in 1:15){
    cat("Function ",i,"\n")
    cat("Function ",i,"\n", file=outputFile,append=TRUE)
    
    start.time  <- Sys.time()
    result <- CMADE(rep(0,10),fn=function(x){cec2013(i,x)})  
    time.taken  <- Sys.time() - start.time
    cat("  CMADE:      ",as.numeric(time.taken, units = "mins"),"     ",result$value,"\n")
    cat("  CMADE:      ",as.numeric(time.taken, units = "mins"),"     ",result$value,"\n", file=outputFile,append=TRUE)
        
    start.time  <- Sys.time()   
    result <- cma_es(rep(0,10),fn=function(x){cec2013(i,x)})   
    time.taken  <- Sys.time() - start.time
    cat("  CMAES:      ",as.numeric(time.taken, units = "mins"),"     ",result$value,"\n")
    cat("  CMADE:      ",as.numeric(time.taken, units = "mins"),"     ",result$value,"\n", file=outputFile,append=TRUE)
    
  }
  
}