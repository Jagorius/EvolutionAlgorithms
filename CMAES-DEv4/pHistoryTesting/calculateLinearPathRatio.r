calculateLinearPathRatio <- function(N, pathLength) {
  
  linearCMAES <- function(N){   
    linFitness <- function(x){
      sum(x)
    }
    
    workpoint <- rep(0,N)
    m <-matrix(0, 10000, N)   
    for (i in 2:10000) {
      shifts    <-  replicate(3*N,rnorm(N)+workpoint)
      best      <-  order(apply(shifts, 2, linFitness))[1:(ceiling(3*N/2))]
      workpoint <-  drop(shifts[,best] %*% (rep(1/ceiling(3*N/2),ceiling(3*N/2))))
      m[i,]     <-  workpoint
      print(workpoint)
    }
    return (m)
  }
  
  
  m <- linearCMAES(N)
  steps <- matrix(0, 10000, N)
  for (i in 1:9999) {
    steps[i,] = m[i+1,] - m[i,]
  }
  
  a <- 0
  FtWeights <- (1-a)^(0:(pathLength-1))
  wSteps <- list()
  
  ratio <- rep(NA, 10000-pathLength)
  for (i in (pathLength+1):10000) {
    for (j in 1:pathLength) {
      wSteps[[j]] = steps[i+j-pathLength-1,] * FtWeights[j]
    }
    
    directPath <- rep(0,N)
    for (j in 1:pathLength) {
      directPath <- directPath + wSteps[[j]]
    }
    directPath <- norm(directPath)
    
    totalPath <- 0
    for (j in 1:pathLength) {
      totalPath <- totalPath + norm(wSteps[[j]])
    }
    
    ratio[i-pathLength] <- totalPath / directPath
  }
  return (sd(ratio))
  #return(sd(ratio)/mean(ratio))
}

pathRatioLinearPlot <- function(N) {
  results <<- matrix(nrow=2*N, ncol=2)
  for (i in 1:(2*N)) {
    results[i,1] <<- i
    ratio <-  calculatePathRatio(N, i)
    results[i,2] <<-ratio
    print(i)
  }
  plot(results[,1],results[,2], col="blue", ann=FALSE)
  title(xlab="History size", col.lab=rgb(0,0.5,0))
  title(ylab="Standard Deviation", col.lab=rgb(0,0.5,0))
  #title(ylab="Deviation/Mean", col.lab=rgb(0,0.5,0))
  title(main="Linear fitness function\nN=30", col.main="red", font.main=4)
  
}