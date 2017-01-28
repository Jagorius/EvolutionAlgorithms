T0 <- function(){
  x= 0.55;
  for (i in 1:1000000) {
    x=x + x; x=x/2; x=x*x; x=sqrt(x); x=log(x); x=exp(x); x=x/(x+2);
  }
}

T1 <- function(d){
  for (i in 1:200000) {
    cec2017(18,rep(0,d))
  }
}

T2 <- function(d){
  for (i in 1:5){
    source('C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/CEC2017/CMADE-BBcomp4 cFT=0 Pop=4N/CMADEv2017.R')
    print(CMADE(rep(0,d),fn=function(x){cec2017(18,x)}, lower=-100, upper=100, control=list("Lamarckism"=FALSE, budget=200000) ) )
  }
}

TableResCEC2017 <- function(d){
  path  <- "C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/CEC2017/CMADE-BBcomp4 cFT=0 Pop=4N/N/"
  
  for(i in 1:30){
    options("scipen"=100, "digits"=4)
    resColumn <- read.table(paste(path,"N",i,"-D",d,sep=""), sep=",",header = TRUE)
    options("scipen"=-100, "digits"=4)
    resColumn <- t(resColumn)
    resColumn[resColumn < 1e-08] <- 0
    minP <- min(resColumn)
    maxP <- max(resColumn)
    medianP <- median(resColumn)
    meanP <- mean(resColumn)
    sdP <- sd(resColumn)
    print(paste("\textbf{",i,"} &",format(minP,digits=4)," & ",format(maxP,digits=4)," & ",format(medianP,digits=4)," & ",format(meanP,digits=4)," & ",format(sdP,digits=4)," \\ ",sep =""),quote=FALSE)
    }
  options("scipen"=100, "digits"=4)
  
}