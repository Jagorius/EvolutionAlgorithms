mainCEC <- function(dim,lambda) {
  
  library("cec2013")
  
  source("C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/CMAES-DEvN1/CMADE-vN1.R")
  cat("                 TIME-MIN,       BEST,      ERR,       RESTARTS","\n")
  RES = rep (-1,28)
  for (i in 1:1){ 
    
    start.time  <- Sys.time()
    result <- CMADEN1(rep(0,dim),fn=function(x){cec2013(i,x)},control=list("budget"=dim*100, "lambda" = lambda))  
    time.taken  <- Sys.time() - start.time
    err = abs(result$value - (-1400 + 100*(i-1)) ) 
    if(i>15) err = err + 100
    RES[i] <- err
    cat("DIM=",dim," Function ",i,":\t",as.numeric(time.taken, units = "mins")," , ", result$value, " , " ,err," , ",result$countreset, "\n")
    
  }
  # Return average error 
  return(mean(RES))
}

mat10 <- matrix(-1, 100, 2)
N <- 10
for(l in 1:100){
  err <- mainCEC(N,l+N-1)
  mat10[l,] <- c(l+N-1,err)
}
write.csv(mat10, file = "C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/CMAES-DEvN1/res10.csv")

mat30 <- matrix(-1, 100, 2)
N <- 30
for(l in 1:100){
  err <- mainCEC(N,l+N-1)
  mat10[l,] <- c(l+N-1,err)
}
write.csv(mat30, file = "C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/CMAES-DEvN1/res30.csv")

mat50 <- matrix(-1, 100, 2)
N <- 50
for(l in 1:100){
  err <- mainCEC(N,l+N-1)
  mat10[l,] <- c(l+N-1,err)
}
write.csv(mat50, file = "C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/CMAES-DEvN1/res50.csv")