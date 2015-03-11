CMADE4 <- function(par, fn, ..., lower, upper, control=list()) {
  
  library("ringbuffer")
  
  ## Function to check the presence of options in the arguments specified by the user
   # @name - argument name
   # @default - default value of the argument
   # RETURN: value specified by user if the given argument name found, default value otherwise
   ##
  controlParam <- function(name, default) {
    v <- control[[name]]
    if (is.null(v))
      return (default)
    else
      return (v)
  }
  
  N <- length(par)                                                    ## Number of dimensions 
  
  ## Check if user specified box constraints values. 
  ## If not accept the default values.
  if (missing(lower))
    lower <- rep(-100, N)
  else if (length(lower) == 1)  
    lower <- rep(lower, N)
  
  if (missing(upper))
    upper <- rep(100, N)
  else if (length(upper) == 1)  
    upper <- rep(upper, N)
  
  #############################
  ##  Algorithm parameters:  ##
  #############################
  Ft          <- controlParam("Ft", 0.5)                              ## Scaling factor of difference vectors (a variable!)
  initFt      <- controlParam("initFt", 5)
  stopfitness <- controlParam("stopfitness", -Inf)                    ## Fitness value after which the convergence is reached 
  stopvariance<- controlParam("stopvariance", 1e-12*Ft)               ## Genetic diversity minimum value(stop fitness variance)
  ## Strategy parameter setting:
  lambda      <- controlParam("lambda", 3*N)                          ## Population size
  initlambda  <- controlParam("lambda", 3*N)
  mu          <- controlParam("mu", floor(lambda/2))                  ## Selection size
  #weights     <- controlParam("weights", log(mu+1) - log(1:mu))       ## Weights to calculate mean from selected individuals
  weights     <- controlParam("weights", (1:mu)*0+1)
  weights     <- weights/sum(weights)                                 ##    \-> weights are normalized by the sum
  mueff       <- controlParam("mueff", sum(weights)^2/sum(weights^2)) ## Variance effectiveness factor
  cc          <- controlParam("ccum", 4/(N+4))                        ## Evolution Path decay factor
  cc_mueff    <- sqrt(cc*(2 - cc) * mueff)                            ## 'cc' and 'mueff' are constant so as this equation
  c_cov       <- controlParam("c_cov", 1/2)                           ## Mutation vectors weight constant
  pathLength  <- controlParam("pathLength", 5)                        ## Size of evolution path
  budget      <- controlParam("budget", 10000*N )                     ## The maximum number of fitness function calls
  maxiter     <- controlParam("maxit", floor(budget/(lambda+1)))      ## Maximum number of iterations after which algorithm stops
  c_Ft        <- controlParam("c_Ft", 0.2)                            ## Vatiance scaling constant
  pathRatio   <- controlParam("pathRatio",calculatePathRatio(N,pathLength)) ## Path Length Control reference value
  checkMiddle <- controlParam("checkMiddle", TRUE)                    ## Vatiable telling if algorithm should save best individual
  histSize    <- controlParam("history", 0.5*N^2)                     ## Size of the window of history - the step length history
  histSize    <- ceiling(histSize)                                    ##    \-> size should be integer
  p           <- controlParam("p", 0.001)                             ## Distribution parameter for history sampling (success prob)
  Ft_scale    <- controlParam("Ft_scale", ((mueff+2)/(N+mueff+3))/(1 + 2*max(0, sqrt((mueff-1)/(N+1))-1) + (mueff+2)/(N+mueff+3)))
  tol         <- controlParam("tol", 10^-20)
  
  ## Logging options:
  log.all     <- controlParam("diag", FALSE)                 
  log.Ft      <- controlParam("diag.Ft", log.all)
  log.value   <- controlParam("diag.value", log.all)
  log.mean    <- controlParam("diag.mean", log.all)
  log.pop     <- controlParam("diag.pop", log.all)  
  
  ## Asserts - safety checks:
  stopifnot(length(upper) == N)  
  stopifnot(length(lower) == N)
  stopifnot(all(lower < upper))
  stopifnot(length(Ft) == 1)
  
  ## Initialize variables for the best solution found so far:
  best.fit <- Inf
  best.par <- NULL
  
  ## According to user specification, preallocate logging structures:
  if (log.Ft)
    Ft.log <- numeric(maxiter)
  if (log.value)
    value.log <- matrix(0, nrow=maxiter, ncol=lambda)
  if (log.mean)
    mean.log <- numeric(maxiter)
  if (log.pop)
    pop.log <- array(0, c(N, lambda, maxiter))
  
  ## Allocate buffers:
  steps       <- ringbuffer(size = pathLength*N)                      ## Cyclical buffer containing last 'pathLength' steps of algorithm
  FtHistory   <- array(0, histSize)                                   ## Array buffer containing 'histSize' last values of 'Ft'
  
  ## Initialize internal strategy parameters
  counteval   <- 0                                                    ## Number of function evaluations
  msg         <- NULL                                                 ## Reason for terminating
  
  cumMean     <- par
  lambda      <- initlambda
  
  while( counteval < budget && best.fit > stopfitness)
  {
    histHead  <- 0                                                    ## Pointer to the history buffer head
    iter      <- 0L                                                   ## Number of iterations
    history   <- array(0, c(N, mu, histSize))                         ## Array stores best 'mu' individuals for 'hsize' recent iterations   
    Ft        <- initFt
    
    # Generate seed point
    if(counteval>0)
      par=runif(N,lower,upper)
    
    population <- par + Ft * replicate(lambda, rnorm(N))
    
    # Check constraints violations
    # Repair the individual if necessary
    population <- ifelse(population > lower, 
                         ifelse(population < upper, population, 
                                bounceBackBoundary(lower,upper,isLowerViolation=FALSE,population)),   ## upper bonduary violation
                         bounceBackBoundary(lower,upper,isLowerViolation=TRUE,population)             ## lower bonduary violation
    )   
    
    selection       <- rep(0, mu)
    selectedPoints  <- matrix(0, nrow=N, ncol=mu)
    fitness         <- apply(population, 2, fn)
    counteval       <- counteval + lambda
    oldMean         <- numeric(N)
    newMean         <- par
    pc              <- rep(0.0, N)
    limit           <- 0
    
    ## Matrices for creating diffs
    diffs     <- matrix(0, N, lambda)
    x1sample  <- numeric(lambda)
    x2sample  <- numeric(lambda)
    
    chiN      <- (sqrt(2)*gamma((N+1)/2)/gamma(N/2))  
    histNorm  <- sqrt(mu/(mu+1))/sqrt(2)  
    
    stoptol=F
    while (counteval < budget && !stoptol) {
      iter      <- iter + 1L
      histHead  <- (histHead %% histSize) + 1
      
      if (log.Ft) Ft.log[iter] <- Ft
      if (log.value) value.log[iter,] <- fitness
      if (log.mean) mean.log[iter] <- fn(newMean)
      if (log.pop) pop.log[,,iter] <- population
      
      ## Select best 'mu' individuals of population
      selection       <- order(fitness)[1:mu]
      selectedPoints  <- population[,selection]
      
      # Save selected population in the history buffer
      history[,,histHead] <- selectedPoints * histNorm/Ft
      
      ## Calculate weighted mean of selected points
      oldMean <- newMean
      newMean <- drop(selectedPoints %*% weights)
      
      ## Write to buffers
      step <- (newMean - oldMean) / Ft
      steps$write(step)
      
      ## Update Ft
      FtHistory[histHead] = Ft
      oldFt <- Ft
      if (iter > pathLength-1 && (sum(step == 0) == 0)) {
        Ft <- calculateFt(steps, N, lambda, pathLength, Ft, c_Ft, pathRatio)
      }
      else {
        Ft <- Ft * exp((1/3)*((p_succ(drop(fn(oldMean)), fitness) - 0.2) / 0.8))
        counteval <- counteval + 1
      }
      
      ## Update parameters
      pc = (1 - cc) * pc + cc_mueff * step
      ##                      \-> sqrt(cc*(2 - cc) * mueff)
      
      ## Check if cumulated step is greater than the tolerance
      if (norm(pc)*Ft<tol)
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
          sqrt(1-c_cov) * rnorm(1) * step* chiN + 
          sqrt(c_cov) * rnorm(N)/chiN  
        
      }
      
      ## New population
      population <- newMean + Ft * diffs
      
      # Check constraints violations
      # Repair the individual if necessary
      population <- ifelse(population > lower, 
                           ifelse(population < upper, population, 
                                  bounceBackBoundary(lower,upper,isLowerViolation=FALSE,population)),   ## upper bonduary violation
                           bounceBackBoundary(lower,upper,isLowerViolation=TRUE,population)             ## lower bonduary violation
      )   
      
      ## Evaluation
      fitness <- apply(population, 2, fn)
      
      ## Break if fit:    
      wb <- which.min(fitness)
      if (fitness[wb] < best.fit) {
        best.fit <- fitness[wb]
        best.par <- population[,wb]
      }
      
      counteval <- counteval + lambda
      
      ## Check if the middle point is the best found so far
      cumMean <- 0.9*cumMean+0.1*newMean
      
      ## Escape from flat-land:
      if (min(fitness) == sort(fitness,partial=min(1+floor(lambda/2), 2+ceiling(lambda/4)))[min(1+floor(lambda/2), 2+ceiling(lambda/4))]) { 
        Ft <- Ft * exp(0.2*Ft_scale);
      }
      
      if (fitness[1] <= stopfitness) {
        msg <- "Stop fitness reached."
        break
      }
      
    }
    
    fn_cum  <- fn(cumMean)
    if (fn_cum < best.fit) {
      best.fit <- drop(fn_cum)
      best.par <- cumMean
    }
    counteval <- counteval + 1
    
    lambda  <- round(lambda+initlambda * 0.5)
    mu      <- floor(lambda/2)
    #weights <- log(mu+1) - log(1:mu)
    weights <- (1:mu)*0+1
    weights <- weights/sum(weights)                                 
  }
  
  cnt <- c(`function`=as.integer(counteval), gradient=NA)
  
  log <- list()
  ## Subset lognostic data to only include those iterations which
  ## where actually performed.
  if (log.Ft) log$Ft <- Ft.log[1:iter]
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

## Function that repair individuals beyond the search range, using the modified idea 
 # of back bouncing.
 # @lowerBoundary - search space lower bonduary
 # @upperBoundary - search space upper bonduary
 # @isLowerViolation - logical value saying whether violation was lower or upper (lower=TRUE, upper=FALSE)
 # @individual - individual to repair
 # RETURN: fixed individual
 ##
bounceBackBoundary <- function(lowerBoundary, upperBoundary, isLowerViolation, individual) {
  if(isLowerViolation == TRUE)
    individual <- lowerBoundary + abs(lowerBoundary - individual)%% (upperBoundary- lowerBoundary)
  else
    individual <- upperBoundary - abs(upperBoundary - individual)%% (upperBoundary- lowerBoundary)
  
  return(ifelse((individual >= lowerBoundary) && (individual <= upperBoundary), 
                individual, 
                bounceBackBoundary(lowerBoundary, upperBoundary, !isLowerViolation, individual)
  ))
}

## Norm: function that assigns a strictly positive length to each vector in a vector space.
 # @vectorX - vector to norm
 # RETURN: euclidean norm of the given vector
 ##
norm <- function(vectorX)
  drop(sqrt(crossprod(vectorX)))

## Function calculate what proportion of the population has a better fitness
 # than its center.
 # @benchmarkFitness - fitness value of mean individual of population
 # @popFitness - actual population fitness array
 # RETURN: proportion of better fitted individuals to the whole population 
 ##
p_succ<-function(benchmarkFitness, popFitness) {
  return (sum(popFitness < benchmarkFitness) / length(popFitness))
}

## Function to calculate new scaling factor F(step size).
 # @arguments - according to their names
 # RETURN: new Ft value
 ##
calculateFt <- function(stepsBuffer, N, lambda, pathLength, currentFt, c_Ft, pathRatio) {
  
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
  return (currentFt * exp(c_Ft * (pathRatio / (totalPath / directPath)-1)))  
}

## Function to calculate path length control reference value based on a problem
 # dimensions and history buffer size
 # @N - number of problem dimensions
 # @pathLength - size of evolution path
 # RETURN: new path ratio value
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
    
    ratio[i-pathLength] <- directPath / totalPath
  }
  return (mean(ratio))
  
}