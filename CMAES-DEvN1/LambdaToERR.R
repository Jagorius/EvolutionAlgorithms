PlotALL <- function() {
  par(mfrow=c(2,2))
  
  cec10 <- read.table("C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/CMAES-DEvN1/res10.csv",header = TRUE,sep = ",")
  plot(cec10$V1, cec10$V2, main='Benchmarking on CEC2013\n N=10 Budget=100*N', xlab="Population size (lambda)", ylab="Averaged error")
  points(cec10$V1[46], cec10$V2[46], col="red", pch=19)
  abline(v = 55, col = "red")
  text( 55, 2e+07, "(4+sqrt(N)/2)*N", col = "red" )
  text( cec10$V1[13]+2, cec10$V2[13], "22", col = "blue")
  
  cec30 <- read.table("C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/CMAES-DEvN1/res30.csv",header = TRUE,sep = ",")
  plot(cec30$V1, cec30$V2, main='Benchmarking on CEC2013\n N=30 Budget=100*N', xlab="Population size (lambda)", ylab="Averaged error", xlim=c(30,210 ))
  points(202, 3.6e+09, col="red", pch=19)
  abline(v = 202, col = "red")
  text( 202, 4.2e+08, "(4+sqrt(N)/2)*N", col = "red" )
  text( cec30$V1[8]+4, cec30$V2[8], "37", col = "blue")
  
  cec50 <- read.table("C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/CMAES-DEvN1/res50.csv",header = TRUE,sep = ",")
  plot(cec50$V1, cec50$V2, main='Benchmarking on CEC2013\n N=50 Budget=100*N', xlab="Population size (lambda)", ylab="Averaged error", xlim=c(30,200 ))
  abline(v = 188, col = "red")
  text( 188, 1.2e+09, "(4+sqrt(N)/2)*N", col = "red" )
}

Plot2 <- function() {
  par(mfrow=c(2,2))
  cec10 <- read.table("C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/CMAES-DEvN1/res10-2.csv",header = TRUE,sep = ",")
  # drop the first two observations
  cec10 = cec10[-1,]
  cec10 = cec10[-1,]
  plot(cec10$V1, cec10$V2, main='Benchmarking on CEC2013: F2,F9,F21,F22\n N=10 Budget=100*N', xlab="Population size (lambda)", ylab="Averaged error")
  print(cec10$V1[which.min(cec10$V2)])
  
  cec30 <- read.table("C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/CMAES-DEvN1/res30-2.csv",header = TRUE,sep = ",")
  # drop the first three observations
  cec30 = cec30[-1,]
  cec30 = cec30[-1,] 
  cec30 = cec30[-1,]  
  plot(cec30$V1, cec30$V2, main='Benchmarking on CEC2013: F2,F9,F21,F22\n N=30 Budget=100*N', xlab="Population size (lambda)", ylab="Averaged error")
  print(cec30$V1[which.min(cec30$V2)])
  
  cec50 <- read.table("C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/CMAES-DEvN1/res50-2.csv",header = TRUE,sep = ",")
  cec50 = cec50[-1,]
  cec50 = cec50[-1,]  
  cec50 = cec50[-1,]
  cec50 = cec50[-1,]   
  plot(cec50$V1, cec50$V2, main='Benchmarking on CEC2013: F2,F9,F21,F22\n N=50 Budget=100*N', xlab="Population size (lambda)", ylab="Averaged error")
  print(cec50$V1[which.min(cec50$V2)])
  
}

Plot3 <- function() {
  par(mfrow=c(2,2))
  cec10 <- read.table("C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/CMAES-DEvN1/res10-3.csv",header = TRUE,sep = ",")
  cec10 <- head(cec10,-4)
  plot(cec10$V1, cec10$V2, main='Benchmarking on CEC2013: F2,F9,F21,F22\n N=10 Budget=10*N', xlab="Population size (lambda)", ylab="Averaged error")
  print(cec10$V1[which.min(cec10$V2)])
  
  cec30 <- read.table("C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/CMAES-DEvN1/res30-3.csv",header = TRUE,sep = ",")
  plot(cec30$V1, cec30$V2, main='Benchmarking on CEC2013: F2,F9,F21,F22\n N=30 Budget=10*N', xlab="Population size (lambda)", ylab="Averaged error")
  print(cec30$V1[which.min(cec30$V2)])
  
  cec50 <- read.table("C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/CMAES-DEvN1/res50-3.csv",header = TRUE,sep = ",")
  plot(cec50$V1, cec50$V2, main='Benchmarking on CEC2013: F2,F9,F21,F22\n N=50 Budget=10*N', xlab="Population size (lambda)", ylab="Averaged error")
  print(cec50$V1[which.min(cec50$V2)])
}

santanaROC <- function() {
  Roc  <-read.table("C:/Users/JS/Desktop/RFC.txt",header = TRUE)
  print(Roc$X.Sensitivity)
  
  p <- c(0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0)
  plot(p,p,col="blue",type="l",xlab="1-specificity",ylab="sensitivity",main='Tree ROC contours validation')
  points(Roc$X.1.Specificity,Roc$X.Sensitivity,col="red",type="l")
}