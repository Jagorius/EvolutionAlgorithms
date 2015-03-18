results <- function(){
  
  cmade5  <- read.table("C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/CMAES-DEv5/Cresult5.txt")  
  cmade4  <- read.table("C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/CMAES-DEv4/Cresult49.txt")  
  
  cmade5vs4         <- array(0,1000)
  cmade5vs4         <- cmade4[,4] - cmade5[,4]
  cmade5vs4[1001]   <- sum(cmade5vs4)
  cmade5vs4[1002]   <- median(cmade5vs4)

  
  resmatrix <- matrix(0, nrow=1002, ncol=1)
  #resmatrix[,1] <- cmade3[,4]
  #resmatrix[,2] <- cmade4[,4]
  #resmatrix[,3] <- cmade4vs3
  resmatrix[,1] <- cmade5vs4
  
  
  colnames(resmatrix) <- c("CMADEv4-CMADEv5")
  rownames(resmatrix) <- c(0:999,"SUM", "MEDIAN")
  resmatrix <- as.table(resmatrix)
  
  sink("C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/CMAES-DEv5/Cv4vsCv5.txt")
  print(resmatrix, print.gap=8)
  cat("\n\nLEGENDA:\n\tCMADE5:\t c_Ft=1/sqrt(N)")
  sink()
  #capture.output(print(resmatrix, print.gap=8), file="C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/CMAES-DEv4/Cv3vsCv4vsCv45.txt")
  
  #fileConn<-file("C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/CMAES-DEv4/Cv3vsCv4vsCv45.txt")
  #writeLines(c("Hello","World"), fileConn)
  #close(fileConn)
}