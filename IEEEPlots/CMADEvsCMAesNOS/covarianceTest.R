Cigar <- function(x){
  res <- 0
  for(i in 2:length(x))
    res <- res + x[i]^2
  res <- res*10^6 + x[1]^2
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
N <- 3
lambda_DES <- 4+floor(3*sqrt(N))

source('C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/IEEEPlots/CMADEvsCMAesNOS/DESv2017_COVTEST.R')
source('C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/IEEEPlots/CMADEvsCMAesNOS/cmaesNoS_COVTEST.R')

## Global variables
CMAESnosPOP11 <<- c()
CMADEPOP11 <<- c()

resCMAESnoS <- cma_esNos(rep(0,N),
                         fn=Cigar,
                         lower=-10^100, upper=10^100,
                         control=list("lambda"=lambda_DES,"diag.bestVal"=TRUE,diag.pop="TRUE","budget"=1500*N)
                        )

resCMADE <- DES(rep(0,N),first10pop=resCMAESnoS$diagnostic$pop[,,1:10], fn=Cigar,
                   lower=-10^100, upper=10^100,
                   control=list( "lambda"=lambda_DES, "budget"=1500*N,"diag.Ft"=TRUE,diag.mean=TRUE,diag.pop="TRUE","diag.bestVal"=TRUE,"diag.worstVal"=TRUE)
               )