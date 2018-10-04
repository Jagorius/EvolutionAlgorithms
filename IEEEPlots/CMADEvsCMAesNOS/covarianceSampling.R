Ellipsoid <- function(x){
  res <- 0
  for(i in 1:length(x))
    res <- res + 10^(6*(i-1)/(length(x)-1))*x[i]^2
  return(res)
}

N <-10

source('C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/IEEEPlots/CMADEvsCMAesNOS/cmaesNoS_COVTEST.R')
resCMAESnoS <- cma_esNos(rep(0,N),
                         fn=Ellipsoid,
                         lower=-10^100, upper=10^100,
                         control=list("lambda"=lambda_DES,"diag.bestVal"=TRUE,"diag.pc"=TRUE,diag.pop="TRUE","budget"=1500*N)
)

####### C1
weightsC <- rep(1/N,10)^(seq_along(rep(1/N,10)))
weightsC <- weightsC/sum(weightsC)

C <- matrix(0,nrow=N,ncol=N)
for(i in 1:N)
  C <- C + rev(weightsC)[i]*resCMAESnoS$diagnostic$pc[i,] %*% t(resCMAESnoS$diagnostic$pc[i,])



####### C2
pop <- matrix(0,nrow=N,ncol=0)
for(p in 1:100){
  ind <- sample(1:N,1, replace = TRUE, prob = rev(weightsC))
  pop <- cbind(pop,resCMAESnoS$diagnostic$pc[ind,]*rnorm(1))
}
C2 <- cov(t(pop))


                                                             

