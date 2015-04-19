benchmark <- function(){

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