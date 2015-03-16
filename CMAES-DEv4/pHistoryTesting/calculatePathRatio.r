## Function to calculate path length control reference value based on a problem
# dimensions and history buffer size
# @N - number of problem dimensions
# @pathLength - size of evolution path
# RETURN: new path ratio standart deviation or deviation/mean!
##
calculatePathRatio <- function(N, pathLength) {
  
  randomWalk <- function(N) {
    m <-matrix(0, 10000, N)
    for (i in 2:10000) {
      m[i,] = m[i-1,] + rnorm(N)
    }
    return (m)
  }
  
  m <- randomWalk(N)
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
  #return(mean(ratio))
  #return (sd(ratio))
  return(sd(ratio)/mean(ratio))
}

pathRatioPlot <- function(N) {
  results <<- matrix(nrow=2*N, ncol=2)
  for (i in 1:(2*N)) {
    results[i,1] <<- i
    ratio <-  calculatePathRatio(N, i)
    results[i,2] <<-ratio
    print(i)
  }
  plot(results[,1],results[,2], col="blue", ann=FALSE)
  title(xlab="History size", col.lab=rgb(0,0.5,0))
  #title(ylab="Mean", col.lab=rgb(0,0.5,0))
  #title(ylab="Standard Deviation", col.lab=rgb(0,0.5,0))
  title(ylab="Deviation/Mean", col.lab=rgb(0,0.5,0))
  title(main="N=30", col.main="red", font.main=4)
  
}

