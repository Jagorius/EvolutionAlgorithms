benchmark <- function(){
  # 10 DIM
  ipop10      <- read.table("C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/RESULTS/ipop10Mean.csv",sep = ",",header = TRUE,colClasses=c("NULL",rep(NA,28)))  
  bipop10     <- read.table("C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/RESULTS/bipop10Mean.csv",sep = ",",header = TRUE,colClasses=c("NULL",rep(NA,28)))  
  nipop10     <- read.table("C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/RESULTS/nipop10Mean.csv",sep = ",",header = TRUE,colClasses=c("NULL",rep(NA,28)))  
  cmade10     <- read.table("C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/RESULTS/cmade10.csv",sep = ",",header = TRUE,colClasses=c("NULL",rep(NA,28)))   
  options("scipen"=-100, "digits"=6)
  

  res10<- matrix(0, nrow=29, ncol=4)
  colnames(res10) <- c("IPOP", "BIPOP", "NIPOP", "CMADE")
  rownames(res10) <- c(1:28, "RANK")
  for (j in 1:27 )
  {
    rows <- nrow(cmade10)
    # ipop
    pval <- t.test(rnorm(rows,t(ipop10)[j,1],t(ipop10)[j,2]), rnorm(rows,t(bipop10)[j,1],t(bipop10)[j,2]),alt="less")$p.value     
    if( !is.nan(pval) && pval<=0.05 )  res10[j,1] <- res10[j,1] + 1  
    pval <- t.test(rnorm(rows,t(ipop10)[j,1],t(ipop10)[j,2]), rnorm(rows,t(nipop10)[j,1],t(nipop10)[j,2]),alt="less")$p.value     
    if( !is.nan(pval) && pval<=0.05 )  res10[j,1] <- res10[j,1] + 1 
    pval <- t.test(rnorm(rows,t(ipop10)[j,1],t(ipop10)[j,2]), cmade10[,j],alt="less")$p.value     
    if( !is.nan(pval) && pval<=0.05 )  res10[j,1] <- res10[j,1] + 1  
        
    # bipop
    pval <- t.test(rnorm(rows,t(bipop10)[j,1],t(bipop10)[j,2]), rnorm(rows,t(ipop10)[j,1],t(ipop10)[j,2]),alt="less")$p.value     
    if( !is.nan(pval) && pval<=0.05 )  res10[j,2] <- res10[j,2] + 1  
    pval <- t.test(rnorm(rows,t(bipop10)[j,1],t(bipop10)[j,2]), rnorm(rows,t(nipop10)[j,1],t(nipop10)[j,2]),alt="less")$p.value     
    if( !is.nan(pval) && pval<=0.05 )  res10[j,2] <- res10[j,2] + 1  
    pval <- t.test(rnorm(rows,t(bipop10)[j,1],t(bipop10)[j,2]), cmade10[,j],alt="less")$p.value     
    if( !is.nan(pval) && pval<=0.05 )  res10[j,2] <- res10[j,2] + 1  
    
    #nipop
    pval <- t.test(rnorm(rows,t(nipop10)[j,1],t(nipop10)[j,2]), rnorm(rows,t(ipop10)[j,1],t(ipop10)[j,2]),alt="less")$p.value     
    if( !is.nan(pval) && pval<=0.05 )  res10[j,3] <- res10[j,3] + 1  
    pval <- t.test(rnorm(rows,t(nipop10)[j,1],t(nipop10)[j,2]), rnorm(rows,t(bipop10)[j,1],t(bipop10)[j,2]),alt="less")$p.value     
    if( !is.nan(pval) && pval<=0.05 )  res10[j,3] <- res10[j,3] + 1 
    pval <- t.test(rnorm(rows,t(nipop10)[j,1],t(nipop10)[j,2]), cmade10[,j],alt="less")$p.value     
    if( !is.nan(pval) && pval<=0.05 )  res10[j,3] <- res10[j,3] + 1  
    
    #cmade
    pval <- t.test(cmade10[,j], rnorm(rows,t(ipop10)[j,1],t(ipop10)[j,2]),alt="less")$p.value     
    if( !is.nan(pval) && pval<=0.05 )  res10[j,4] <- res10[j,4] + 1  
    pval <- t.test(cmade10[,j], rnorm(rows,t(bipop10)[j,1],t(bipop10)[j,2]),alt="less")$p.value     
    if( !is.nan(pval) && pval<=0.05 )  res10[j,4] <- res10[j,4] + 1 
    pval <- t.test(cmade10[,j], rnorm(rows,t(nipop10)[j,1],t(nipop10)[j,2]),alt="less")$p.value     
    if( !is.nan(pval) && pval<=0.05 )  res10[j,4] <- res10[j,4] + 1  
    
  }
  res10[1,] <- c(0,0,0,0)
  res10[2,] <- c(0,0,0,0)
  res10[4,] <- c(0,0,0,0)
  res10[10,] <- c(0,0,0,0)
  res10[28,] <- c(0,3,2,1)
  res10[29,] <- colMeans(res10)
  #print(res10)
  res10 <-format(res10, scientific = FALSE)
  write.csv(res10, file = "C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/RESULTS/RANK10.csv")
  
  
  # 30 DIM
  ipop30      <- read.table("C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/RESULTS/ipop30Mean.csv",sep = ",",header = TRUE,colClasses=c("NULL",rep(NA,28)))  
  bipop30     <- read.table("C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/RESULTS/bipop30Mean.csv",sep = ",",header = TRUE,colClasses=c("NULL",rep(NA,28)))  
  nipop30     <- read.table("C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/RESULTS/nipop30Mean.csv",sep = ",",header = TRUE,colClasses=c("NULL",rep(NA,28)))  
  shade30     <- read.table("C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/RESULTS/shade30Mean.csv",sep = ",",header = TRUE,colClasses=c("NULL",rep(NA,28)))  
  cmade30     <- read.table("C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/RESULTS/cmade30.csv",sep = ",",header = TRUE,colClasses=c("NULL",rep(NA,28)))  
  res30<- matrix(0, nrow=29, ncol=5)
  colnames(res30) <- c("IPOP", "BIPOP", "NIPOP", "SHADE","CMADE")
  rownames(res30) <- c(1:28, "RANK")
  for (j in 1:27 )
  {
    rows <- nrow(cmade30)
    # ipop
    pval <- t.test(rnorm(rows,t(ipop30)[j,1],t(ipop30)[j,2]), rnorm(rows,t(bipop30)[j,1],t(bipop30)[j,2]),alt="less")$p.value     
    if( !is.nan(pval) && pval<=0.05 )  res30[j,1] <- res30[j,1] + 1  
    pval <- t.test(rnorm(rows,t(ipop30)[j,1],t(ipop30)[j,2]), rnorm(rows,t(nipop30)[j,1],t(nipop30)[j,2]),alt="less")$p.value     
    if( !is.nan(pval) && pval<=0.05 )  res30[j,1] <- res30[j,1] + 1 
    pval <- t.test(rnorm(rows,t(ipop30)[j,1],t(ipop30)[j,2]), rnorm(rows,t(shade30)[j,1],t(shade30)[j,2]),alt="less")$p.value     
    if( !is.nan(pval) && pval<=0.05 )  res30[j,1] <- res30[j,1] + 1 
    pval <- t.test(rnorm(rows,t(ipop30)[j,1],t(ipop30)[j,2]), cmade30[,j],alt="less")$p.value     
    if( !is.nan(pval) && pval<=0.05 )  res30[j,1] <- res30[j,1] + 1  
     
    # bipop
    pval <- t.test(rnorm(rows,t(bipop30)[j,1],t(bipop30)[j,2]), rnorm(rows,t(ipop30)[j,1],t(ipop30)[j,2]),alt="less")$p.value     
    if( !is.nan(pval) && pval<=0.05 )  res30[j,2] <- res30[j,2] + 1  
    pval <- t.test(rnorm(rows,t(bipop30)[j,1],t(bipop30)[j,2]), rnorm(rows,t(nipop30)[j,1],t(nipop30)[j,2]),alt="less")$p.value     
    if( !is.nan(pval) && pval<=0.05 )  res30[j,2] <- res30[j,2] + 1    
    pval <- t.test(rnorm(rows,t(bipop30)[j,1],t(bipop30)[j,2]), rnorm(rows,t(shade30)[j,1],t(shade30)[j,2]),alt="less")$p.value     
    if( !is.nan(pval) && pval<=0.05 )  res30[j,2] <- res30[j,2] + 1  
    pval <- t.test(rnorm(rows,t(bipop30)[j,1],t(bipop30)[j,2]), cmade30[,j],alt="less")$p.value     
    if( !is.nan(pval) && pval<=0.05 )  res30[j,2] <- res30[j,2] + 1  
    
    #nipop
    pval <- t.test(rnorm(rows,t(nipop30)[j,1],t(nipop30)[j,2]), rnorm(rows,t(ipop30)[j,1],t(ipop30)[j,2]),alt="less")$p.value     
    if( !is.nan(pval) && pval<=0.05 )  res30[j,3] <- res30[j,3] + 1  
    pval <- t.test(rnorm(rows,t(nipop30)[j,1],t(nipop30)[j,2]), rnorm(rows,t(bipop30)[j,1],t(bipop30)[j,2]),alt="less")$p.value     
    if( !is.nan(pval) && pval<=0.05 )  res30[j,3] <- res30[j,3] + 1  
    pval <- t.test(rnorm(rows,t(nipop30)[j,1],t(nipop30)[j,2]), rnorm(rows,t(shade30)[j,1],t(shade30)[j,2]),alt="less")$p.value     
    if( !is.nan(pval) && pval<=0.05 )  res30[j,3] <- res30[j,3] + 1    
    pval <- t.test(rnorm(rows,t(nipop30)[j,1],t(nipop30)[j,2]), cmade30[,j],alt="less")$p.value     
    if( !is.nan(pval) && pval<=0.05 )  res30[j,3] <- res30[j,3] + 1  
    
    #shade
    pval <- t.test(rnorm(rows,t(shade30)[j,1],t(shade30)[j,2]), rnorm(rows,t(ipop30)[j,1],t(ipop30)[j,2]),alt="less")$p.value     
    if( !is.nan(pval) && pval<=0.05 )  res30[j,4] <- res30[j,4] + 1   
    pval <- t.test(rnorm(rows,t(shade30)[j,1],t(shade30)[j,2]), rnorm(rows,t(bipop30)[j,1],t(bipop30)[j,2]),alt="less")$p.value     
    if( !is.nan(pval) && pval<=0.05 )  res30[j,4] <- res30[j,4] + 1   
    pval <- t.test(rnorm(rows,t(shade30)[j,1],t(shade30)[j,2]), rnorm(rows,t(nipop30)[j,1],t(nipop30)[j,2]),alt="less")$p.value     
    if( !is.nan(pval) && pval<=0.05 )  res30[j,4] <- res30[j,4] + 1 
    pval <- t.test(rnorm(rows,t(shade30)[j,1],t(shade30)[j,2]), cmade30[,j],alt="less")$p.value     
    if( !is.nan(pval) && pval<=0.05 )  res30[j,4] <- res30[j,4] + 1  
    
    #cmade
    pval <- t.test(cmade30[,j], rnorm(rows,t(ipop30)[j,1],t(ipop30)[j,2]),alt="less")$p.value     
    if( !is.nan(pval) && pval<=0.05 )  res30[j,5] <- res30[j,5] + 1  
    pval <- t.test(cmade30[,j], rnorm(rows,t(bipop30)[j,1],t(bipop30)[j,2]),alt="less")$p.value     
    if( !is.nan(pval) && pval<=0.05 )  res30[j,5] <- res30[j,5] + 1 
    pval <- t.test(cmade30[,j], rnorm(rows,t(nipop30)[j,1],t(nipop30)[j,2]),alt="less")$p.value     
    if( !is.nan(pval) && pval<=0.05 )  res30[j,5] <- res30[j,5] + 1  
    pval <- t.test(cmade30[,j], rnorm(rows,t(shade30)[j,1],t(shade30)[j,2]),alt="less")$p.value     
    if( !is.nan(pval) && pval<=0.05 )  res30[j,5] <- res30[j,5] + 1  
    
  }
  res30[1,] <- c(0,0,0,0,0)
  res30[4,] <- c(0,0,0,0,0)
  res30[10,] <- c(0,0,0,0,0)
  res30[28,] <- c(0,2,0,0,2)
  res30[29,] <- colMeans(res30)
 # print(res30)
  res30 <-format(res30, scientific = FALSE)
  write.csv(res30, file = "C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/RESULTS/RANK30.csv")
 
  # 50 DIM
  ipop50      <- read.table("C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/RESULTS/ipop50Mean.csv",sep = ",",header = TRUE,colClasses=c("NULL",rep(NA,28)))  
  bipop50     <- read.table("C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/RESULTS/bipop50Mean.csv",sep = ",",header = TRUE,colClasses=c("NULL",rep(NA,28)))  
  nipop50     <- read.table("C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/RESULTS/nipop50Mean.csv",sep = ",",header = TRUE,colClasses=c("NULL",rep(NA,28)))  
  cmade50     <- read.table("C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/RESULTS/cmade50.csv",sep = ",",header = TRUE,colClasses=c("NULL",rep(NA,28)))  
  res50<- matrix(0, nrow=29, ncol=4)
  colnames(res50) <- c("IPOP", "BIPOP", "NIPOP", "CMADE")
  rownames(res50) <- c(1:28, "RANK")
 
  for (j in 1:28 )
  {
    rows <- nrow(cmade50)
    # ipop
    pval <- t.test(rnorm(rows,t(ipop50)[j,1],t(ipop50)[j,2]), rnorm(rows,t(bipop50)[j,1],t(bipop50)[j,2]),alt="less")$p.value     
    if( !is.nan(pval) && pval<=0.05 )  res50[j,1] <- res50[j,1] + 1  
    pval <- t.test(rnorm(rows,t(ipop50)[j,1],t(ipop50)[j,2]), rnorm(rows,t(nipop50)[j,1],t(nipop50)[j,2]),alt="less")$p.value     
    if( !is.nan(pval) && pval<=0.05 )  res50[j,1] <- res50[j,1] + 1 
    pval <- t.test(rnorm(rows,t(ipop50)[j,1],t(ipop50)[j,2]), cmade50[,j],alt="less")$p.value     
    if( !is.nan(pval) && pval<=0.05 )  res50[j,1] <- res50[j,1] + 1  
    
    # bipop
    pval <- t.test(rnorm(rows,t(bipop50)[j,1],t(bipop50)[j,2]), rnorm(rows,t(ipop50)[j,1],t(ipop50)[j,2]),alt="less")$p.value     
    if( !is.nan(pval) && pval<=0.05 )  res50[j,2] <- res50[j,2] + 1  
    pval <- t.test(rnorm(rows,t(bipop50)[j,1],t(bipop50)[j,2]), rnorm(rows,t(nipop50)[j,1],t(nipop50)[j,2]),alt="less")$p.value     
    if( !is.nan(pval) && pval<=0.05 )  res50[j,2] <- res50[j,2] + 1  
    pval <- t.test(rnorm(rows,t(bipop50)[j,1],t(bipop50)[j,2]), cmade50[,j],alt="less")$p.value     
    if( !is.nan(pval) && pval<=0.05 )  res50[j,2] <- res50[j,2] + 1  
    
    #nipop
    pval <- t.test(rnorm(rows,t(nipop50)[j,1],t(nipop50)[j,2]), rnorm(rows,t(ipop50)[j,1],t(ipop50)[j,2]),alt="less")$p.value     
    if( !is.nan(pval) && pval<=0.05 )  res50[j,3] <- res50[j,3] + 1  
    pval <- t.test(rnorm(rows,t(nipop50)[j,1],t(nipop50)[j,2]), rnorm(rows,t(bipop50)[j,1],t(bipop50)[j,2]),alt="less")$p.value     
    if( !is.nan(pval) && pval<=0.05 )  res50[j,3] <- res50[j,3] + 1 
    pval <- t.test(rnorm(rows,t(nipop50)[j,1],t(nipop50)[j,2]), cmade50[,j],alt="less")$p.value     
    if( !is.nan(pval) && pval<=0.05 )  res50[j,3] <- res50[j,3] + 1  
    
    #cmade
    pval <- t.test(cmade50[,j], rnorm(rows,t(ipop50)[j,1],t(ipop50)[j,2]),alt="less")$p.value     
    if( !is.nan(pval) && pval<=0.05 )  res50[j,4] <- res50[j,4] + 1  
    pval <- t.test(cmade50[,j], rnorm(rows,t(bipop50)[j,1],t(bipop50)[j,2]),alt="less")$p.value     
    if( !is.nan(pval) && pval<=0.05 )  res50[j,4] <- res50[j,4] + 1 
    pval <- t.test(cmade50[,j], rnorm(rows,t(nipop50)[j,1],t(nipop50)[j,2]),alt="less")$p.value     
    if( !is.nan(pval) && pval<=0.05 )  res50[j,4] <- res50[j,4] + 1 
  }
 res50[1,] <- c(0,0,0,0)
 res50[2,] <- c(0,0,0,0)
 res50[4,] <- c(0,0,0,0)
 res50[10,] <- c(0,0,0,0)
  res50[29,] <- colMeans(res50)
  #print(res50)
  res50 <-format(res50, scientific = FALSE)
  write.csv(res50, file = "C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/RESULTS/RANK50.csv")
 
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