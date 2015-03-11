results <- function(){
  
  cmade4 <- read.table("C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/CMAES-DEv4/Cresult.txt")  
  cmade3 <- read.table("C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/CMAES-DEv3/Cresult.txt")
  
  cmade4vs3 <- array(0,1000)
  cmade4vs3 <- cmade3[,4] - cmade4[,4]
  
  resmatrix <- matrix(0, nrow=1000, ncol=3)
  resmatrix[,1] <- cmade3[,4]
  resmatrix[,2] <- cmade4[,4]
  resmatrix[,3] <- cmade4vs3
  
  colnames(resmatrix) <- c("CMADEv3-MIN","CMADEv4-MIN","CMADEv3-CMADEv4")
  rownames(resmatrix) <- c(0:999)
  resmatrix <- as.table(resmatrix)
  
  write.table(resmatrix, "C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/CMAES-DEv4/Cv3vsCv4.txt", sep="\t\t")
  
  print(sum(cmade4vs3))
  print(mean(cmade4vs3))
  print(median(cmade4vs3))
  
  
}