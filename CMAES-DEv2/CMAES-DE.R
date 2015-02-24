CMADE <- function(par, fn, ..., lower, upper, control=list()) {
  
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
  stopfitness <- controlParam("stopfitness", -Inf)                    ## Fitness value after which the convergence is reached 
  stopvariance<- controlParam("stopvariance", 1e-12*Ft)               ## Genetic diversity minimum value(stop fitness variance)
  ## Strategy parameter setting:
  lambda      <- controlParam("lambda", 4*N)                          ## Population size
  mu          <- controlParam("mu", floor(lambda/2))                  ## Selection size
  maxiter     <- controlParam("maxit", (10000*N)/(lambda+1)) - 1      ## Maximum number of iterations after which algorithm stops
  weights     <- controlParam("weights", log(mu+1) - log(1:mu))       ## Weights to calculate mean from selected individuals
  weights     <- weights/sum(weights)                                 ##    \-> weights are normalized by the sum
  mueff       <- controlParam("mueff", sum(weights)^2/sum(weights^2)) ## Variance effectiveness factor
  cc          <- controlParam("ccum", 4/(N+4))                        ## Evolution Path decay factor
  c_cov       <- controlParam("c_cov", 1/2)                           ## Mutation vectors weight constant
  pathLength  <- controlParam("pathLength", 5)                        ## Size of evolution path
  c_Ft        <- controlParam("c_Ft", 0.1)                            ## Vatiance scaling constant
  pathRatio   <- controlParam("pathRatio", 1/2.35)                    ## Path Length Control reference value
  checkMiddle <- controlParam("checkMiddle", TRUE)                    ## Vatiable telling if algorithm should save best individual
  histSize    <- controlParam("history", 0.5*N^2)                     ## Size of the window of history - the step length history
  histSize    <- ceiling(histSize)                                    ##    \-> size should be integer
  p           <- controlParam("p", 0.001)                             ## Distribution parameter for history sampling (success prob)
  Ft_scale    <- controlParam("Ft_scale", ((mueff+2)/(N+mueff+3))/(1 + 2*max(0, sqrt((mueff-1)/(N+1))-1) + (mueff+2)/(N+mueff+3)))
    
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
  FtHistory   <- ringbuffer(size =  histSize)                         ## Array buffer containing 'histSize' last values of 'Ft'
  history     <- array(0, c(N, lambda, histSize))                     ## Array stores whole population fraction for 'hsize' recent iterations
  histHead    <- 0                                                    ## Pointer to the history buffer head
  
  ## Initialize internal strategy parameters
  iter        <- 0L                                                   ## Number of iterations
  counteval   <- lambda                                               ## Number of function evaluations
  msg         <- NULL                                                 ## Reason for terminating
  population  <- matrix(par + Ft * replicate(lambda, rnorm(1)), nrow=N, ncol=lambda)
  selection   <- rep(0, mu)                                           ## Selection mask - only mu best individuals reproduces
  selectedPoints <- matrix(0, nrow=N, ncol=mu)                        ## Selected point matrix
  fitness     <- apply(population, 2, fn)                             ## Fitness array
  cc_mueff    <- sqrt(cc*(2 - cc) * mueff)                            ## 'cc' and 'mueff' are constant so as equation
  sqrt_1ccov  <- sqrt(1-c_cov)
  sqrt_ccov   <- sqrt(c_cov)
  oldMean     <- numeric(N)
  newMean     <- par
  pc          <- rep(0.0, N)
  limit       <- 0
  ## Matrices for creating diffs:
  diffs       <- matrix(0, N, lambda)
  x1sample    <- numeric(lambda)
  x2sample    <- numeric(lambda)
      
  ## Random pre-begin
  for (i in 1:histSize) {
    iter <- iter + 1L
    histHead <- (histHead %% histSize) + 1
    
    steps$write(runif(N, lower, upper))
    FtHistory$write(runif(1, lower, upper))
    
    history[,,histHead] <- (population - newMean)/sqrt(2)
    newMean <- runif(1, lower, upper)
    
    population <- matrix(par + Ft * replicate(lambda, rnorm(1)), nrow=N, ncol=lambda)
    par <- runif(N, lower, upper)
    
  }
  
  while (iter < maxiter) {
    iter <- iter + 1L

    histHead <- (histHead %% histSize) + 1          # Update pointer to the history head (finds the remainder after division(modulo)) 
    
    if (log.Ft) Ft.log[iter]        <- Ft
    if (log.value) value.log[iter,] <- fitness
    if (log.mean) mean.log[iter]    <- fn(newMean)
    if (log.pop) pop.log[,,iter]    <- population
    
    ## Select best 'mu' individuals of population
    selection       <- order(fitness)[1:mu]  
    selectedPoints  <- population[,selection]
    
    # Save whole population in the history buffer
    history[,,histHead] <- (population - newMean)/sqrt(2)
    
    ## Calculate weighted mean of selected points
    oldMean         <- newMean    
    newMean         <- drop(selectedPoints %*% weights)
      
    ## Write to buffers
    step            <- (newMean - oldMean) / Ft
    steps$write(step)
    
    ## Update Ft
    if (iter > pathLength-1 && (sum(step == 0) == 0)) {
      Ft            <- calculateFt(steps, N, lambda, pathLength, Ft, c_Ft, pathRatio)
    }
    else {
      Ft            <- Ft * exp((1/3)*((p_succ(drop(fn(oldMean)), fitness) - 0.2) / 0.8))
      counteval     <- counteval + 1  # +1 for the oldMean fitness calculation
    }
    FtHistory$write(Ft)
    
    
    ## Update parameters
    pc = (1 - cc) * pc + cc_mueff * step
    ##                      \-> sqrt(cc*(2 - cc) * mueff)
       
    ## Sample from history with geometric or uniform distribution
    limit <- ifelse(iter < histSize, histHead, histSize)
    historySample <- (rgeom(lambda, p) %% limit) + 1
    
    # Sample difference individuals with distribution resulting from weights
    # From selection vector get a new population length vector
    x1sample <- sample(selection, lambda, replace=TRUE, prob=weights)
    x2sample <- sample(selection, lambda, replace=TRUE, prob=weights)

    ## Make diffs
    for (i in 1:lambda) {
      x1 <- history[, x1sample[i], historySample[i]]
      x2 <- history[, x2sample[i], historySample[i]]
          
      sFt <- 0
      for( k in 0:(histSize-1)){
        sFt <- sFt + (1-c_cov)^k * FtHistory$peek()[histSize-k] * rnorm(1)
      }
      
      diffs[,i] <- (x1 - x2) + c_cov*sFt
        
      #diffs[,i] <- (x1 - x2) + sqrt_1ccov * rnorm(1) * step + sqrt_ccov * pc * rnorm(1)
      #                           \-> sqrt(1-c_cov)               \-> sqrt(c_cov)
      
      
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
    
    counteval <- counteval + lambda + 1 # +1 for the middle point fitness calculation
    
    ## Check if the middle point is the best found so far
    meanFit <- fn(newMean)
    if (checkMiddle && meanFit > best.fit) {
      best.fit <- drop(meanFit)
      best.par <- newMean  
    }
     
    ## Check if stop finess value reached
    if (min(fitness) <= stopfitness) {
      msg <- "Stop fitness reached."
      break
    }
        
    ## Escape from flat-land:
    if (min(fitness) == sort(fitness,partial=min(1+floor(lambda/2), 2+ceiling(lambda/4)))[min(1+floor(lambda/2), 2+ceiling(lambda/4))]) { 
      Ft <- Ft * exp(0.2*Ft_scale);
    }
   
    ## Check if population diversity reached stop value
    if(var(fitness) <= stopvariance && all(Ft * pc < stopvariance) ){
      msg <- "Stop variance reached. Too little genetic diversity."
      break
    }
    
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
  class(res) <- "cma_de.result"
  return(res)
}

## Function that repair individuals beyond the search range, using the idea 
 # of back bouncing.
 # @lowerBoundary - search space lower bonduary
 # @upperBoundary - search space upper bonduary
 # @isLowerViolation - logical value saying whether violation was lower or upper (lower=TRUE, upper=FALSE)
 # @individual - individual to repair
 # RETURN: fixed individual
 ##
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
  
  return (currentFt * exp(c_Ft * ((directPath / totalPath) - pathRatio)))  
}