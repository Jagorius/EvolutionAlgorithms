results <- function(){
  cmadevN13  <-read.table("C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/CMAES-DEvN1/CResultsvN1-3.txt")
  cmadevN12  <-read.table("C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/CMAES-DEvN1/CResultsvN1-2.txt")
  cmadevN1    <-read.table("C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/CMAES-DEvN1/CResultsvN1.txt")
  
  cmade72     <-read.table("C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/CMAES-DEv7/Cresults72.txt",sep = ",",header = TRUE)  
  cmade71     <-read.table("C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/CMAES-DEv7/Cresults71.txt",sep = ",",header = TRUE)  
  cmade7      <-read.table("C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/CMAES-DEv7/Cresults7.txt")  
  
  cmade595l5ur<-read.table("C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/CMAES-DEv5/Cresult595l5ur.txt",sep = ",",header = TRUE)  
  cmade595l5<-  read.table("C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/CMAES-DEv5/Cresult595l5.txt",sep = ",",header = TRUE)  
  cmade595l4<-  read.table("C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/CMAES-DEv5/Cresult595l4.txt",sep = ",",header = TRUE)  
  cmade598<- read.table("C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/CMAES-DEv5/Cresult598.txt")  
  cmade597<- read.table("C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/CMAES-DEv5/Cresult597.txt")  
  cmade596<- read.table("C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/CMAES-DEv5/Cresult596.txt")  
  cmade595<- read.table("C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/CMAES-DEv5/Cresult595.txt")  
  cmade59 <- read.table("C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/CMAES-DEv5/Cresult59.txt")  
  cmade58 <- read.table("C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/CMAES-DEv5/Cresult58.txt")  
  cmade57 <- read.table("C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/CMAES-DEv5/Cresult57.txt")  
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
  
  cmade57vs4         <- array(0,1000)
  cmade57vs4         <- cmade4[,4] - cmade57[,4]
  cmade57vs4[1001]   <- sum(cmade57vs4)
  cmade57vs4[1002]   <- median(cmade57vs4)
  
  cmade58vs4         <- array(0,1000)
  cmade58vs4         <- cmade4[,4] - cmade58[,4]
  cmade58vs4[1001]   <- sum(cmade58vs4)
  cmade58vs4[1002]   <- median(cmade58vs4)
  
  cmade59vs4         <- array(0,1000)
  cmade59vs4         <- cmade4[,4] - cmade59[,4]
  cmade59vs4[1001]   <- sum(cmade59vs4)
  cmade59vs4[1002]   <- median(cmade59vs4)
  
  cmade595vs4         <- array(0,1000)
  cmade595vs4         <- cmade4[,4] - cmade595[,4]
  cmade595vs4[1001]   <- sum(cmade595vs4)
  cmade595vs4[1002]   <- median(cmade595vs4)
  
  cmade596vs4         <- array(0,1000)
  cmade596vs4         <- cmade4[,4] - cmade596[,4]
  cmade596vs4[1001]   <- sum(cmade596vs4)
  cmade596vs4[1002]   <- median(cmade596vs4)
  
  cmade597vs4         <- array(0,1000)
  cmade597vs4         <- cmade4[,4] - cmade597[,4]
  cmade597vs4[1001]   <- sum(cmade597vs4)
  cmade597vs4[1002]   <- median(cmade597vs4)
  
  cmade598vs4         <- array(0,1000)
  cmade598vs4         <- cmade4[,4] - cmade598[,4]
  cmade598vs4[1001]   <- sum(cmade598vs4)
  cmade598vs4[1002]   <- median(cmade598vs4)
  
  cmade595l4vs4       <- array(0,1000)
  cmade595l4vs4       <- cmade4[,4] - cmade595l4[,3]
  cmade595l4vs4[1001] <- sum(cmade595l4vs4)
  cmade595l4vs4[1002] <- median(cmade595l4vs4)
  
  cmade595l5vs4       <- array(0,1000)
  cmade595l5vs4       <- cmade4[,4] - cmade595l5[,3]
  cmade595l5vs4[1001] <- sum(cmade595l5vs4)
  cmade595l5vs4[1002] <- median(cmade595l5vs4)
  
  cmade595l5urvs4       <- array(0,1000)
  cmade595l5urvs4       <- cmade4[,4] - cmade595l5ur[,3]
  cmade595l5urvs4[1001] <- sum(cmade595l5urvs4)
  cmade595l5urvs4[1002] <- median(cmade595l5urvs4)
  
  cmade7vs4             <- array(0,1000)
  cmade7vs4             <- cmade4[,4] - cmade7[,4]
  cmade7vs4[1001]       <- sum(cmade7vs4)
  cmade7vs4[1002]       <- median(cmade7vs4)
    
  cmade71vs4            <- array(0,1000)
  cmade71vs4            <- cmade4[,4] - cmade71[,3]
  cmade71vs4[1001]      <- sum(cmade71vs4)
  cmade71vs4[1002]      <- median(cmade71vs4)
 
  cmade72vs4            <- array(0,1000)
  cmade72vs4            <- cmade4[,4] - cmade72[,3]
  cmade72vs4[1001]      <- sum(cmade72vs4)
  cmade72vs4[1002]      <- median(cmade72vs4)
  
  cmadevN1vs4           <- array(0,1000)
  cmadevN1vs4           <- cmade4[,4] - cmadevN1[,4]
  cmadevN1vs4[1001]     <- sum(cmadevN1vs4)
  cmadevN1vs4[1002]     <- median(cmadevN1vs4)
  
  cmadevN12vs4           <- array(0,1000)
  cmadevN12vs4           <- cmade4[,4] - cmadevN12[,4]
  cmadevN12vs4[1001]     <- sum(cmadevN12vs4)
  cmadevN12vs4[1002]     <- median(cmadevN12vs4)
  
  cmadevN13vs4           <- array(0,1000)
  cmadevN13vs4           <- cmade4[,4] - cmadevN13[,4]
  cmadevN13vs4[1001]     <- sum(cmadevN13vs4)
  cmadevN13vs4[1002]     <- median(cmadevN13vs4)
  
  resmatrix <- matrix(0, nrow=1002, ncol=22)
  #resmatrix[,1] <- cmade3[,4]
  #resmatrix[,2] <- cmade4[,4]
  #resmatrix[,3] <- cmade4vs3
  resmatrix[,1] <- cmade51vs4
  resmatrix[,2] <- cmade52vs4
  resmatrix[,3] <- cmade53vs4
  resmatrix[,4] <- cmade54vs4
  resmatrix[,5] <- cmade55vs4
  resmatrix[,6] <- cmade56vs4
  resmatrix[,7] <- cmade57vs4
  resmatrix[,8] <- cmade58vs4
  resmatrix[,9] <- cmade59vs4
  resmatrix[,10] <- cmade595vs4
  resmatrix[,11] <- cmade596vs4
  resmatrix[,12] <- cmade597vs4
  resmatrix[,13] <- cmade598vs4
  resmatrix[,14] <- cmade595l4vs4
  resmatrix[,15] <- cmade595l5vs4
  resmatrix[,16] <- cmade595l5urvs4
  resmatrix[,17] <- cmade7vs4
  resmatrix[,18] <- cmade71vs4
  resmatrix[,19] <- cmade72vs4
  resmatrix[,20] <- cmadevN1vs4
  resmatrix[,21] <- cmadevN12vs4
  resmatrix[,22] <- cmadevN13vs4
  
  
  rankMatrix <- resmatrix[1:1000,]
  resRank <- rep(0,22)
  for (n in 1:1000) {
    resRank <- resRank + rank(rankMatrix[n,])
  }
  resRank <- 22-(resRank/1000)
  resRank <- t(resRank)
  colnames(resRank) <- c("CMADEv51", "CMADEv52", "CMADEv53", "CMADEv54", "CMADEv55", "CMADEv56", "CMADEv57", "CMADEv58", "CMADEv59", "CMADEv595", "CMADEv596", "CMADEv597", "CMADEv598","CMADE595L4","CMADE595L5","CMADE595L5UR","CMADEv7","CMADEv71","CMADEv72","CMADEvN1","CMADEvN1-2","CMADEvN1-3")
  print(resRank)
  
  sink("C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/CMAES-DEv7/SummaryRanks.txt")
  print(resRank)
  cat("\n\nLEGENDA:\n\tCMADE5 :\t c_Ft=1/sqrt(N)
      \tCMADE51:\t c_Ft=2/sqrt(N)
      \tCMADE52:\t pathRatio<- stale, nowe c_Ft (wektor chi)
      \tCMADE53:\t pathLength=10
      \tCMADE54:\t pathLength=if(N<10) 10 else 5
      \tCMADE55:\t Monitorowanie Ft
      \tCMADE56:\t Ft*pc<10^-4
      \tCMADE57:\t Ft*pc<10^-4, Pathlength=6, rowMeans()
      \tCMADE58:\t Ft*pc<10^-4, rowMeans()
      \tCMADE59:\t Ft*pc<10^-4 Pathlength=6
      \tCMADE595:\t Ft*pc<10^-4 Pathlength=6, mueff=mu
      \tCMADE596:\t Ft*pc<10^-3 Pathlength=6, mueff=mu
      \tCMADE597:\t Ft*pc<10^-2 Pathlength=6, mueff=mu
      \tCMADE597:\t Ft*pc<10^-5 Pathlength=6, mueff=mu
      \tCMADE595L4:\t Ft*pc<10^-4 Pathlength=6, mueff=mu, Lambda=4N
      \tCMADE595L5:\t Ft*pc<10^-4 Pathlength=6, mueff=mu, Lambda=5N
      \tCMADE595L5ur:\t CMADE595L5 + history=5N, rozkl.jednostajny, sterowanie restart
      \tCMADE7:\t\t CMADE595L5ur + restart.length calculation (restart if bud_left>0.8restart)
      \tCMADE71:\t CMADE7 + sqrt-log
      \tCMADE72:\t CMADE7 + initLambda with tanh")
  sink()
  #capture.output(print(resmatrix, print.gap=8), file="C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/CMAES-DEv4/Cv3vsCv4vsCv45.txt")
  
  #capture.output(print(resmatrix, print.gap=8), file="C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/CMAES-DEv4/Cv3vsCv4vsCv45.txt")
  
  #fileConn<-file("C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/CMAES-DEv4/Cv3vsCv4vsCv45.txt")
  #writeLines(c("Hello","World"), fileConn)
  #close(fileConn)
}