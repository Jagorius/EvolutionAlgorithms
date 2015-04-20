benchmark <- function(){
  # 10 DIM
  cmade10     <- read.table("C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/RESULTS/cmade10Mean.csv",sep = ",",header = TRUE,colClasses=c("NULL",rep(NA,28)))  
  ipop10      <- read.table("C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/RESULTS/ipop10Mean.csv",sep = ",",header = TRUE,colClasses=c("NULL",rep(NA,28)))  
  bipop10     <- read.table("C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/RESULTS/bipop10Mean.csv",sep = ",",header = TRUE,colClasses=c("NULL",rep(NA,28)))  
  nipop10     <- read.table("C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/RESULTS/nipop10Mean.csv",sep = ",",header = TRUE,colClasses=c("NULL",rep(NA,28)))  
  options("scipen"=-100, "digits"=6)
  
  res10 <- matrix(0, nrow=28, ncol=4)
  res10[,1] <- unlist(lapply(cmade10, '[[', 1))
  res10[,2] <- unlist(lapply(ipop10, '[[', 1))
  res10[,3] <- unlist(lapply(bipop10, '[[', 1))
  res10[,4] <- unlist(lapply(nipop10, '[[', 1))
  colnames(res10) <- c("CMADE", "IPOP", "BIPOP", "NIPOP")
  write.csv(res10, file = "C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/RESULTS/Results10.csv")
  
  # 30 DIM
  cmade30     <- read.table("C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/RESULTS/cmade30Mean.csv",sep = ",",header = TRUE,colClasses=c("NULL",rep(NA,28)))  
  ipop30      <- read.table("C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/RESULTS/ipop30Mean.csv",sep = ",",header = TRUE,colClasses=c("NULL",rep(NA,28)))  
  bipop30     <- read.table("C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/RESULTS/bipop30Mean.csv",sep = ",",header = TRUE,colClasses=c("NULL",rep(NA,28)))  
  nipop30     <- read.table("C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/RESULTS/nipop30Mean.csv",sep = ",",header = TRUE,colClasses=c("NULL",rep(NA,28)))  
  options("scipen"=-100, "digits"=6)
  
  res30 <- matrix(0, nrow=28, ncol=4)
  res30[,1] <- unlist(lapply(cmade30, '[[', 1))
  res30[,2] <- unlist(lapply(ipop30, '[[', 1))
  res30[,3] <- unlist(lapply(bipop30, '[[', 1))
  res30[,4] <- unlist(lapply(nipop30, '[[', 1))
  colnames(res30) <- c("CMADE", "IPOP", "BIPOP", "NIPOP")
  write.csv(res30, file = "C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/RESULTS/Results30.csv")
  
  # 50 DIM
  cmade50     <- read.table("C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/RESULTS/cmade50Mean.csv",sep = ",",header = TRUE,colClasses=c("NULL",rep(NA,28)))  
  ipop50      <- read.table("C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/RESULTS/ipop50Mean.csv",sep = ",",header = TRUE,colClasses=c("NULL",rep(NA,28)))  
  bipop50     <- read.table("C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/RESULTS/bipop50Mean.csv",sep = ",",header = TRUE,colClasses=c("NULL",rep(NA,28)))  
  nipop50     <- read.table("C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/RESULTS/nipop50Mean.csv",sep = ",",header = TRUE,colClasses=c("NULL",rep(NA,28)))  
  options("scipen"=-100, "digits"=6)
  
  res50 <- matrix(0, nrow=28, ncol=4)
  res50[,1] <- unlist(lapply(cmade50, '[[', 1))
  res50[,2] <- unlist(lapply(ipop50, '[[', 1))
  res50[,3] <- unlist(lapply(bipop50, '[[', 1))
  res50[,4] <- unlist(lapply(nipop50, '[[', 1))
  colnames(res50) <- c("CMADE", "IPOP", "BIPOP", "NIPOP")
  write.csv(res50, file = "C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/RESULTS/Results50.csv")


}

prepareCMADE <- function(){
  cmade10     <- read.table("C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/RESULTS/cmade10.csv",sep = ",",header = TRUE,colClasses=c("NULL",rep(NA,28)))  
  cmade10     <- colMeans(cmade10, na.rm = FALSE)
  write.csv(t(cmade10), file = "C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/RESULTS/cmade10Mean.csv")
  
  cmade30     <- read.table("C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/RESULTS/cmade30.csv",sep = ",",header = TRUE,colClasses=c("NULL",rep(NA,28)))  
  cmade30     <- colMeans(cmade30, na.rm = FALSE)
  write.csv(t(cmade30), file = "C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/RESULTS/cmade30Mean.csv")
  
  cmade50     <- read.table("C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/RESULTS/cmade50.csv",sep = ",",header = TRUE,colClasses=c("NULL",rep(NA,28)))  
  cmade50     <- colMeans(cmade50, na.rm = FALSE)
  write.csv(t(cmade50), file = "C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/RESULTS/cmade50Mean.csv")
}