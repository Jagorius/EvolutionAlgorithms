results <- function(){
  
  cmade46<- read.table("C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/CMAES-DEv4/Cresult46.txt")  
  cmade45<- read.table("C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/CMAES-DEv4/Cresult45.txt")  
  cmade4 <- read.table("C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/CMAES-DEv4/Cresult.txt")  
  cmade3 <- read.table("C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/CMAES-DEv3/Cresult.txt")
  
  cmade4vs3         <- array(0,1000)
  cmade4vs3         <- cmade3[,4] - cmade4[,4]
  cmade4vs3[1001]   <- sum(cmade4vs3)
  cmade4vs3[1002]   <- median(cmade4vs3)
  
  cmade45vs3        <- array(0,1000)
  cmade45vs3        <- cmade3[,4] - cmade45[,4]
  cmade45vs3[1001]  <- sum(cmade45vs3)
  cmade45vs3[1002]  <- median(cmade45vs3)
  
  cmade46vs3        <- array(0,1000)
  cmade46vs3        <- cmade3[,4] - cmade46[,4]
  cmade46vs3[1001]  <- sum(cmade46vs3)
  cmade46vs3[1002]  <- median(cmade46vs3)
  
  resmatrix <- matrix(0, nrow=1002, ncol=3)
  #resmatrix[,1] <- cmade3[,4]
  #resmatrix[,2] <- cmade4[,4]
  #resmatrix[,3] <- cmade4vs3
  resmatrix[,1] <- cmade4vs3
  resmatrix[,2] <- cmade45vs3
  resmatrix[,3] <- cmade46vs3
  
  colnames(resmatrix) <- c("CMADEv3-CMADEv4  ","CMADEv3-CMADEv4,5   ","CMADEv3-CMADEv4,6")
  rownames(resmatrix) <- c(0:999,"SUM", "MEDIAN")
  resmatrix <- as.table(resmatrix)
  
  sink("C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/CMAES-DEv4/Cv3vsCv4vsCv45vsCv46.txt")
  print(resmatrix, print.gap=8)
  cat("\n\nLEGENDA:\n\tCMADE4:\t Wagi niejednakowe, par<0.8lower,0.8upper>\t
      \tCMADE4,5:Wagi jednakowe, par<lower,upper>\t
      \tCMADE4,6:Wagi jednakowe, par<0.8lower,0.8upper>\t")
  sink()
  #capture.output(print(resmatrix, print.gap=8), file="C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/CMAES-DEv4/Cv3vsCv4vsCv45.txt")
  
  #fileConn<-file("C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/CMAES-DEv4/Cv3vsCv4vsCv45.txt")
  #writeLines(c("Hello","World"), fileConn)
  #close(fileConn)
}