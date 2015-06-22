mainCEC <- function(dim) {
  
  library("cec2013")
  
  source("CMADE-vN1.R")
  cat("Dimensions =", dim,"\n") 
  cat("                 TIME-MIN,       BEST,        RESTARTS","\n")
  for (i in 1:28){ 
    
    start.time  <- Sys.time()
    startPoint <- rep(0,dim)
    result <- CMADEN1(startPoint,fn=function(x){cec2013(i,x)},control=list("budget"=length(startPoint)*100))  
    time.taken  <- Sys.time() - start.time
    cat("Function ",i,":\t",as.numeric(time.taken, units = "mins")," , ",result$value," , ",result$countreset, "\n")
    
  }
  cat("\n\n")
  
}
mainCEC(10)
mainCEC(30)
mainCEC(50)
