Cigar <- function(x){
  res <- 0
  for(i in 2:length(x))
    res <- res + x[i]^2
  res <- res*10^6 + x[1]^2
  return(res)
}
Ellipsoid <- function(x){
  res <- 0
  for(i in 1:length(x))
    res <- res + 10^(6*(i-1)/(length(x)-1))*x[i]^2
  return(res)
}
Sphere <- function(x){
  return(sum(x^2))
}
RosenBrock = function(x){
  res <- 0
  for(i in 1:(length(x)-1))
    res <- res + 100*(x[i+1]-x[i]^2)^2 + (x[i]-1)^2
  return(res)
}

Histogram <- function(POPmatrix){
  library(gplots)
  library(MASS)
  h1 <- hist(POPmatrix[1,], breaks=100, plot=F)
  h2 <- hist(POPmatrix[2,], breaks=100, plot=F)
  top <- max(h1$counts, h2$counts)
  k <- kde2d(POPmatrix[1,], POPmatrix[2,], n=100)
  
  # margins
  oldpar <- par()
  par(mar=c(3,3,1,1))
  layout(matrix(c(2,0,1,3),2,2,byrow=T),c(3,1), c(1,3))
  image(k,col=heat.colors(100)) #plot the image
  par(mar=c(0,2,1,0))
  barplot(h1$counts, axes=F, ylim=c(0, top), space=0, col='red')
  par(mar=c(2,0,0.5,1))
  barplot(h2$counts, axes=F, xlim=c(0, top), space=0, col='red', horiz=T)  
  
}


N <- 2
lambda_DES <- 4+floor(3*sqrt(N))

source('C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/IEEEPlots/CMADEvsCMAesNOS/DESv2017_COVTEST.R')
source('C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/IEEEPlots/CMADEvsCMAesNOS/cmaesNoS_COVTEST.R')

histNumber <- 2

## Global variables
CMAESnosPOP11 <<- c()
CMADEPOP11 <<- c()

resCMAESnoS <- cma_esNos(rep(0,N),
                         fn=Cigar,
                         lower=-10^100, upper=10^100,
                         control=list("lambda"=lambda_DES,"diag.bestVal"=TRUE,diag.pop="TRUE","budget"=1500*N)
                        )

resCMADE <- DES(rep(0,N),first10pop=resCMAESnoS$diagnostic$pop[,,1:histNumber], fn=Cigar,
                   lower=-10^100, upper=10^100,
                   control=list( "lambda"=lambda_DES, "budget"=1500*N,"diag.Ft"=TRUE,diag.mean=TRUE,diag.pop="TRUE","diag.bestVal"=TRUE,"diag.worstVal"=TRUE)
               )

plot(CMAESnosPOP11[1,],CMAESnosPOP11[2,],pch=16,col="red",main="RED=CmaesNos, BLACK=CMADE")
points(CMADEPOP11[1,],CMADEPOP11[2,],pch=16,col="black")