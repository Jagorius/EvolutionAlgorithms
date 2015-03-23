results <- function(){
  
  cmade56 <- read.table("C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/CMAES-DEv5/Cresult56.txt")  
  cmade55 <- read.table("C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/CMAES-DEv5/Cresult55.txt")  
  cmade54 <- read.table("C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/CMAES-DEv5/Cresult54.txt")  
  cmade53 <- read.table("C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/CMAES-DEv5/Cresult53.txt")  
  cmade52 <- read.table("C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/CMAES-DEv5/Cresult52.txt")  
  cmade51 <- read.table("C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/CMAES-DEv5/Cresult51.txt")  
  cmade5  <- read.table("C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/CMAES-DEv5/Cresult5.txt")  
  cmade4  <- read.table("C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/CMAES-DEv4/Cresult49.txt")  

  cmade51vs4         <- array(0,1000)
  cmade51vs4         <- cmade4[,4] - cmade51[,4]
  cmade51vs4[1001]   <- sum(cmade51vs4)
  cmade51vs4[1002]   <- median(cmade51vs4)
  
  cmade52vs4         <- array(0,1000)
  cmade52vs4         <- cmade4[,4] - cmade52[,4]
  cmade52vs4[1001]   <- sum(cmade52vs4)
  cmade52vs4[1002]   <- median(cmade52vs4)
  
  cmade53vs4         <- array(0,1000)
  cmade53vs4         <- cmade4[,4] - cmade53[,4]
  cmade53vs4[1001]   <- sum(cmade53vs4)
  cmade53vs4[1002]   <- median(cmade53vs4)
  
  cmade54vs4         <- array(0,1000)
  cmade54vs4         <- cmade4[,4] - cmade54[,4]
  cmade54vs4[1001]   <- sum(cmade54vs4)
  cmade54vs4[1002]   <- median(cmade54vs4)
  
  cmade55vs4         <- array(0,1000)
  cmade55vs4         <- cmade4[,4] - cmade55[,4]
  cmade55vs4[1001]   <- sum(cmade55vs4)
  cmade55vs4[1002]   <- median(cmade55vs4)
  
  cmade56vs4         <- array(0,1000)
  cmade56vs4         <- cmade4[,4] - cmade56[,4]
  cmade56vs4[1001]   <- sum(cmade56vs4)
  cmade56vs4[1002]   <- median(cmade56vs4)
  
  resmatrix <- matrix(0, nrow=1002, ncol=6)
  #resmatrix[,1] <- cmade3[,4]
  #resmatrix[,2] <- cmade4[,4]
  #resmatrix[,3] <- cmade4vs3
  resmatrix[,1] <- cmade51vs4
  resmatrix[,2] <- cmade52vs4
  resmatrix[,3] <- cmade53vs4
  resmatrix[,4] <- cmade54vs4
  resmatrix[,5] <- cmade55vs4
  resmatrix[,6] <- cmade56vs4
    
  rankMatrix <- resmatrix[1:1000,]
  resRank <- rep(0,6)
  for (n in 1:1000) {
    resRank <- resRank + rank(rankMatrix[n,])
  }
  resRank <- 6-(resRank/1000)
  print(resRank)
  
  colnames(resmatrix) <- c("CMADEv4-CMADEv51", "CMADEv4-CMADEv52", "CMADEv4-CMADEv53", "CMADEv4-CMADEv54", "CMADEv4-CMADEv55", "CMADEv4-CMADEv56")
  rownames(resmatrix) <- c(0:999,"SUM", "MEDIAN")
  resmatrix <- as.table(resmatrix)
  
  sink("C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/CMAES-DEv5/Cv4vsCv5.txt")
  print(resmatrix, print.gap=8)
  cat("\n\nLEGENDA:\n\tCMADE5 :\t c_Ft=1/sqrt(N)
      \tCMADE51:\t c_Ft=2/sqrt(N)
      \tCMADE52:\t pathRatio<- stale, nowe c_Ft (wektor chi)
      \tCMADE53:\t pathLength=10
      \tCMADE54:\t pathLength=if(N<10) 10 else 5
      \tCMADE55:\t Monitorowanie Ft
      \tCMADE56:\t Ft*pc")
  sink()
  #capture.output(print(resmatrix, print.gap=8), file="C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/CMAES-DEv4/Cv3vsCv4vsCv45.txt")
  
  capture.output(print(resmatrix, print.gap=8), file="C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/CMAES-DEv4/Cv3vsCv4vsCv45.txt")
  
  #fileConn<-file("C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/CMAES-DEv4/Cv3vsCv4vsCv45.txt")
  #writeLines(c("Hello","World"), fileConn)
  #close(fileConn)
}

results2 <- function(){
  cmade54 <- read.table("C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/CMAES-DEv5/Cresult54.txt")  
  cmade52 <- read.table("C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/CMAES-DEv5/Cresult52.txt")  
  
  c54         <- array(0,50)
  c54         <- cmade54[1:50,4]
  
  c52         <- array(0,50)
  c52         <- cmade52[1:50,4]
  
  cmade54vs52        <- array(0,50)
  cmade54vs52        <- c52 - c54
  cmade54vs52[51]   <- sum(cmade54vs52)
  cmade54vs52[52]   <- median(cmade54vs52)
  
  print(cmade54vs52)
}