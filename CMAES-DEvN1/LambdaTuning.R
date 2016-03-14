mainCEC <- function(dim,lambda) {
  
  library("cec2013")
  
  source("CMADE-vN1.R") 
  cat("                 TIME-MIN,       BEST,      ERR,       RESTARTS","\n")
  RES = rep (-1,4)
  CECfuncs <- c(2,9,21,22)
  for (i in 1:4){ 
    
    start.time  <- Sys.time()
    result <- CMADEN1(rep(0,dim),fn=function(x){cec2013(CECfuncs[i],x)},control=list("budget"=dim*100, "lambda" = lambda))  
    time.taken  <- Sys.time() - start.time
    err = abs(result$value - (-1400 + 100*(CECfuncs[i]-1)) ) 
    if(CECfuncs[i]>15) err = err + 100
    RES[i] <- err
    cat("DIM=",dim," Function ",CECfuncs[i],":\t",as.numeric(time.taken, units = "mins")," , ", result$value, " , " ,err," , ",result$countreset, "\n")
    
  }
  # Return average error 
  return(mean(RES))
}

options( scipen = 20 )

mat10 <- matrix(-1, 100, 2)
N <- 10
for(l in 4:103){
 
  errtmp <- 0
  for(i in 1:10){
    errtmp <- errtmp + mainCEC(N,l)
  }
  err = errtmp/10
  mat10[l,] <- c(l,err)
}
write.csv(mat10, file = "/home/djagodzi/BBcomp/CMADE-vN1/res10-2.csv")

mat30 <- matrix(-1, 100, 2)
N <- 30
for(l in 4:103){
  errtmp <- 0
  for(i in 1:10){
    errtmp <- errtmp + mainCEC(N,l)
  }
  err = errtmp/10
  mat30[l,] <- c(l,err)
}
write.csv(mat30, file = "/home/djagodzi/BBcomp/CMADE-vN1/res30-2.csv")

mat50 <- matrix(-1, 150, 2)
N <- 50
for(l in 4:153){
  errtmp <- 0
  for(i in 1:10){
    errtmp <- errtmp + mainCEC(N,l)
  }
  err = errtmp/10
  mat50[l,] <- c(l,err)
}
write.csv(mat50, file = "/home/djagodzi/BBcomp/CMADE-vN1/res50-2.csv")
