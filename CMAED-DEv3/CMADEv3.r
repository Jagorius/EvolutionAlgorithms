CMADE3 <- function(par, fn, ..., lower, upper, control=list()) {
  
  library("ringbuffer")
  
  controlParam <- function(name, default) {
    v <- control[[name]]
    if (is.null(v))
      return (default)
    else
      return (v)
  }
  
  ## Inital solution:
  xmean <- par
  N <- length(xmean)
  
  ## Box constraints:
  if (missing(lower))
    lower <- rep(-100, N)
  else if (length(lower) == 1)  
    lower <- rep(lower, N)
  
  if (missing(upper))
    upper <- rep(100, N)
  else if (length(upper) == 1)  
    upper <- rep(upper, N)
  
  ## Parameters:
  stopfitness <- controlParam("stopfitness", -Inf)
  initsigma   <- controlParam("sigma", 5)
  keep.best   <- controlParam("keep.best", TRUE)
  
  ## Logging options:
  log.all    <- controlParam("diag", FALSE)
  log.sigma  <- controlParam("diag.sigma", log.all)
  log.value  <- controlParam("diag.value", log.all)
  log.mean   <- controlParam("diag.mean", log.all)
  log.pop    <- controlParam("diag.pop", log.all)
  
  ## Strategy parameter setting (defaults as recommended by Nicolas Hansen):
  lambda      <- controlParam("lambda", 3*N)
  initlambda  <- controlParam("lambda", 3*N)
  maxiter     <- controlParam("maxit", floor((10000*N)/(lambda+1))) 
  mu          <- controlParam("mu", floor(lambda/2))
  weights     <- controlParam("weights", log(mu+1) - log(1:mu))
  weights     <- controlParam("weights", (1:mu)*0+1)
  weights     <- weights/sum(weights)
  mueff       <- controlParam("mueff", sum(weights)^2/sum(weights^2))
  cc          <- controlParam("ccum", 4/(N+4))
  c_cov       <- controlParam("c_cov", 1/2)
  Ft_scale    <- controlParam("Ft_scale", ((mueff+2)/(N+mueff+3))/(1 + 2*max(0, sqrt((mueff-1)/(N+1))-1) + (mueff+2)/(N+mueff+3)))
  
  pathLength  <- controlParam("pathLength", 5)
  c_sigma     <- controlParam("c_sigma", 0.1)
  pathRatio   <- controlParam("pathRatio", 2.35)
  checkMiddle <- controlParam("checkMiddle", TRUE)
  histSize    <- controlParam("history", 0.5*N^2)
  tol         <- controlParam("tol", 10^-20)
  
  ## Distribution parameter for history sampling
  p           <- controlParam("p", 0.001)
  
  ## Safety checks:
  stopifnot(length(upper) == N)  
  stopifnot(length(lower) == N)
  stopifnot(all(lower < upper))
  stopifnot(length(initsigma) == 1)
  
  ## Bookkeeping variables for the best solution found so far:
  best.fit <- Inf
  best.par <- NULL
  
  ## Preallocate logging structures:
  if (log.sigma)
    sigma.log <- numeric(maxiter)
  if (log.value)
    value.log <- matrix(0, nrow=maxiter, ncol=lambda)
  if (log.mean)
    mean.log <- numeric(maxiter)
  if (log.pop)
    pop.log <- array(0, c(N, lambda, maxiter))
  
  counteval <- lambda ## Number of function evaluations
  
  cumMean=par
  
  while( counteval < 10000*N && best.fit > stopfitness)
  {
    ## Allocate buffers
    steps <- ringbuffer(size = pathLength*N)
    sigmaHistory <- array(0, histSize)
    history <- array(0, c(N, mu, histSize))
    histHead <- 0
    
    ## Initialize dynamic (internal) strategy parameters and constants
    iter <- 0L      ## Number of iterations
    msg <- NULL     ## Reason for terminating
    
    ## Preallocate work arrays:
    sigma <- initsigma
    if(counteval>lambda)
    {
      par=runif(N,0.8*lower,0.8*upper)
    }
    population <- par + sigma * replicate(lambda, rnorm(N))
    
    selection <- rep(0, mu)
    selectedPoints <- matrix(0, nrow=N, ncol=mu)
    fitness <- apply(population, 2, fn)
    oldMean <- numeric(N)
    newMean <- par
    pc <- rep(0.0, N)
    limit <- 0
    
    ## Matrices for creating diffs
    diffs <- matrix(0, N, lambda)
    x1sample <- numeric(lambda)
    x2sample <- numeric(lambda)
    
    chiN<-(sqrt(2)*gamma((N+1)/2)/gamma(N/2))  
    histNorm<-  sqrt(mu/(mu+1))/sqrt(2)  
    
    stoptol=F
    while (iter < maxiter && !stoptol) {
      iter <- iter + 1L
      histHead <- (histHead %% histSize) + 1
      
      if (log.sigma) sigma.log[iter] <- sigma
      if (log.value) value.log[iter,] <- fitness
      if (log.mean) mean.log[iter] <- fn(newMean)
      if (log.pop) pop.log[,,iter] <- population
      
      ## Selection
      selection <- order(fitness)[1:mu]
      selectedPoints <- population[,selection]
      history[,,histHead] <- selectedPoints * histNorm/sigma
      
      ## Calculate weighted mean of selected points
      oldMean <- newMean
      newMean <- drop(selectedPoints %*% weights)
      
      ## Write to buffers
      step <- (newMean - oldMean) / sigma
      steps$write(step)
      
      ## Update sigma
      sigmaHistory[histHead] = sigma
      oldSigma <- sigma
      if (iter > pathLength-1 && (sum(step == 0) == 0)) {
        sigma <- calculateSigma(steps, N, lambda, pathLength, sigma, c_sigma, pathRatio)
      }
      else {
        sigma <- sigma * exp((1/3)*((p_succ(drop(fn(oldMean)), fitness) - 0.2) / 0.8))
      }
      
      ## Update parameters
      pc = ((1 - cc) * pc + sqrt(cc*(2 - cc) * mueff) * step)
      if (norm(pc)*sigma<tol)
        stoptol=T
      
      ## Sample from history with geometric or uniform distribution
      limit <- ifelse(iter < histSize, histHead, histSize)
      historySample <- (rgeom(lambda, p) %% limit) + 1
      
      x1sample <- sample(1:mu, lambda, replace=TRUE)
      x2sample <- sample(1:mu, lambda, replace=TRUE)
      
      ## Make diffs    
      for (i in 1:lambda) {
        x1 <- history[, x1sample[i], historySample[i]]
        x2 <- history[, x2sample[i], historySample[i]]
        
        diffs[,i] <- (x1 - x2) +  
          sqrt(1-c_cov) * rnorm(N) * step +  
          sqrt(c_cov) * rnorm(N)/chiN  
        
      }
      
      ## New population
      population <- newMean + sigma * diffs
     
      # Check constraints violations
      # Repair the individual if necessary
      population <- ifelse(population > lower, 
                           ifelse(population < upper, population, 
                                  bounceBackBoundary(lower,upper,isLowerViolation=FALSE,population)),   ## upper bonduary violation
                           bounceBackBoundary(lower,upper,isLowerViolation=TRUE,population)             ## lower bonduary violation
      )   
      
      ## Evaluation
      fitness <- apply(population, 2, fn)
      
      ## break if fit:    
      wb <- which.min(fitness)
      if (fitness[wb] < best.fit) {
        best.fit <- fitness[wb]
        best.par <- population[,wb]
      }
      
      counteval <- counteval + lambda + 1 # +1 for the middle point
      
      ## Check if the middle point is the best found so far
      cumMean <- 0.9*cumMean+0.1*newMean
      fn_cumMean <- fn(cumMean)
      
      if (checkMiddle && fn_cumMean < best.fit) {
        best.fit <- drop(fn_cumMean)
        best.par <- cumMean
      }
      
      ## Escape from flat-land:
      if (min(fitness) == sort(fitness,partial=min(1+floor(lambda/2), 2+ceiling(lambda/4)))[min(1+floor(lambda/2), 2+ceiling(lambda/4))]) { 
        sigma <- sigma * exp(0.2*Ft_scale);
      }
      
      if (fitness[1] <= stopfitness) {
        msg <- "Stop fitness reached."
        break
      }
      
    }
    if (fn(cumMean) < best.fit) {
      best.fit <- drop(fn(cumMean))
      best.par <- cumMean
    }
    
    lambda <- lambda+initlambda * 0.5
  }
  cnt <- c(`function`=as.integer(counteval), gradient=NA)
  
  log <- list()
  ## Subset lognostic data to only include those iterations which
  ## where actually performed.
  if (log.sigma) log$sigma <- sigma.log[1:iter]
  if (log.value) log$value <- value.log[1:iter,]
  if (log.mean) log$mean <- mean.log[1:iter]
  if (log.pop)   log$pop   <- pop.log[,,1:iter]
  
  ## Drop names from value object
  names(best.fit) <- NULL
  res <- list(par=best.par,
              value=best.fit,
              counts=cnt,
              convergence=ifelse(iter >= maxiter, 1L, 0L),
              message=msg,
              diagnostic=log
  )
  class(res) <- "cmade.result"
  return(res)
}

norm <- function(x)
  drop(sqrt(crossprod(x)))

p_succ<-function(benchmarkFitness, popFitness) {
  return (sum(popFitness < benchmarkFitness) / length(popFitness))
}

bounceBackBoundary <- function(lowerBoundary, upperBoundary, isLowerViolation, individual) {
  if(isLowerViolation == TRUE)
    individual <- lowerBoundary + abs(lowerBoundary - individual)
  else
    individual <- upperBoundary - abs(upperBoundary - individual)
  
  return(ifelse((individual >= lowerBoundary) && (individual <= upperBoundary), 
                individual, 
                bounceBackBoundary(lowerBoundary, upperBoundary, !isLowerViolation, individual)
  ))
}

calculateSigma <- function(stepsBuffer, N, lambda, pathLength, currentSigma, c_sigma, pathRatio) {
  
  steps <- split(stepsBuffer$peek(), ceiling(seq_along(stepsBuffer$peek())/N))
  
  directPath <- rep(0,N)
  for (i in 1:pathLength) {
    directPath <- directPath + steps[[i]]
  }
  directPath <- norm(directPath)
  
  totalPath <- 0
  for (i in 1:pathLength) {
    totalPath <- totalPath + norm(steps[[i]])
  }
  return (currentSigma * exp(c_sigma * (pathRatio - (totalPath / directPath))))  
}
