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
  
  ###### LOG REPAIRED NUMBER
#  all_REP <- matrix(0, nrow = 0, ncol = 1)
  ###### LOG newMEAN
#  all_NEWMEAN <- matrix(0, nrow = 0, ncol = N)
  ###### LOG_PC
#  all_PC <- matrix(0, nrow = 0, ncol = N)

  bounceBackBoundary3 <- function(population,lower,upper){
    while (!( all(population > lower) && all(population < upper)))
    {
       population[population> upper] <-  upper - (population[population > upper] -  upper)%% ( upper- lower)
       population[population< lower] <-  lower + ( lower- population[population <  lower])%%( upper- lower)
    }
    return(population)

}
  bounceBackBoundary2 <- function(x){
    
    if(all(x >= cbind(lower)) && all(x <= cbind(upper)))
      return (x)
    else if(any(x < cbind(lower)))
      for(i in which(x < cbind(lower)) )
        x[i] <- lower[i] + abs(lower[i] - x[i])%% (upper[i]- lower[i])
    else if(any(x > cbind(upper)))
      for(i in which(x > cbind(upper)) )
        x[i] <- upper[i] - abs(upper[i] - x[i])%% (upper[i]- lower[i])
    return (bounceBackBoundary2(x))
    
  }
  
  #############################
  ##  Algorithm parameters:  ##
  #############################
  Ft          <- controlParam("Ft", sqrt(1/2))                        ## Scaling factor of difference vectors (a variable!)
  initFt      <- controlParam("initFt", sqrt(1/2))
  stopfitness <- controlParam("stopfitness", -Inf)                    ## Fitness value after which the convergence is reached 
  stopvariance<- controlParam("stopvariance", 1e-12*Ft)               ## Genetic diversity minimum value(stop fitness variance)
  ## Strategy parameter setting:
  budget      <- controlParam("budget", 100*N^2 )                     ## The maximum number of fitness function calls
  initlambda  <- controlParam("lambda", floor((4+sqrt(N)/2)*N))       ## Population starting size
  lambda      <- initlambda                                           ## Population size

  mu          <- controlParam("mu", floor(lambda/2))                  ## Selection size
  weights     <- controlParam("weights", log(mu+1) - log(1:mu))       ## Weights to calculate mean from selected individuals
  #weights     <- controlParam("weights", (1:mu)*0+1)
  weights     <- weights/sum(weights)                                 ##    \-> weights are normalized by the sum
  weightsSumS <- sum(weights^2)                                       ## weights sum square
  mueff       <- controlParam("mueff", mu)     	                      ## Variance effectiveness factor
  cc          <- controlParam("ccum", 4/(N+4))                        ## Evolution Path decay factor
  c_pc        <- controlParam("cpc", 1/2)                             ## Covariance deformation factor
  cc_mueff    <- sqrt(cc*(2 - cc) )#*sqrt( mueff)                     ## 'cc' and 'mueff' are constant so as this equation
  c_cov       <- controlParam("c_cov", 1/2)                           ## Mutation vectors weight constant
  pathLength  <- controlParam("pathLength",  6)                       ## Size of evolution path
  maxiter     <- controlParam("maxit", floor(budget/(lambda+1)))      ## Maximum number of iterations after which algorithm stops
  c_Ft        <- controlParam("c_Ft", 1/((sqrt(2)*gamma((N+1)/2)/gamma(N/2)) )) ## Vatiance scaling constant
  pathRatio   <- controlParam("pathRatio",sqrt(pathLength))           ## Path Length Control reference value
  checkMiddle <- controlParam("checkMiddle", TRUE)                    ## Vatiable telling if algorithm should save check middle point in every iteration
  histSize    <- controlParam("history", 6+ceiling(3*sqrt(N)))        ## Size of the window of history - the step length history
  #histSize    <- controlParam("history", 0.5*N^2)                    ## Size of the window of history - the step length history
  histSize    <- ceiling(histSize)                                    ##    \-> size should be integer
  Ft_scale    <- controlParam("Ft_scale", ((mueff+2)/(N+mueff+3))/(1 + 2*max(0, sqrt((mueff-1)/(N+1))-1) + (mueff+2)/(N+mueff+3)))
  tol         <- controlParam("tol", 10^-6)
  #rand_seed   <- controlParam("tol", get(".Random.seed", mode="numeric", envir=globalenv()))
  #set.seed(rand_seed)
  
  ## Logging options:
  log.all     <- controlParam("diag", FALSE)                 
  log.Ft      <- controlParam("diag.Ft", log.all)
  log.value   <- controlParam("diag.value", log.all)
  log.mean    <- controlParam("diag.mean", log.all)
  log.pop     <- controlParam("diag.pop", log.all)  
  
  ## Lamarckian approach allows individuals to violate boundaries. 
  ## Fitness value is estimeted by fitness of repaired individual.
  Lamarckism     <- controlParam("Lamarckism", FALSE)
  
  fn_p <- function(P, P_repaired) {
    if(all(P == P_repaired)){
      if(is.matrix(P) && is.matrix(P_repaired)){
          return ( apply(P, 2, fn) )
      }else{
          return ( fn(P) )
      }
    }
    else{
      if(is.matrix(P) && is.matrix(P_repaired)){
          compareResult <- apply(P==P_repaired,2,all)
          P_fit <-  apply(P_repaired, 2, fn)
          P_fit[compareResult] <- P_fit[compareResult] + exp(dist(cbind(P_repaired[compareResult],upper-(abs(lower)) )) )
          return(P_fit)
      }else{
          P_fit <- fn(P_repaired) + 10000
          return ( P_fit )
      }
    }
  }
  
  ## Asserts - safety checks:
  stopifnot(length(upper) == N)  
  stopifnot(length(lower) == N)
  stopifnot(all(lower < upper))
  stopifnot(length(Ft) == 1)
  
  ## Initialize variables for the best solution found so far:
  best.fit <- Inf
  best.par <- NULL
  
  last.restart <-0
  restart.length <-0
  restart.number <-0
  
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
  lambda      <- initlambda
  pc          <- rep(0.0, N)/sqrt(N)
  histHead  <- 0                                                      ## Pointer to the history buffer head
  iter      <- 0L                                                     ## Number of iterations
  history   <- array(0, c(N, mu, histSize))                           ## Array stores best 'mu' individuals for 'hsize' recent iterations   
  Ft        <- initFt
  
  ####### SAVE ALL FT
#  all_FT    <- Ft
#  all_NEWMEAN  <- 0
#  all_PC    <- pc
  
  # Create fisrt population
  population <- replicate(lambda, runif(N,lower,upper))
  cumMean=rowMeans(population)
  # Check constraints violations
  # Repair the individual if necessary
  #populationRepaired <- ifelse(population > lower, 
  #                     ifelse(population < upper, population, 
   #                           bounceBackBoundary(lower,upper,isLowerViolation=FALSE,population)),   ## upper bonduary violation
  #                     bounceBackBoundary(lower,upper,isLowerViolation=TRUE,population)             ## lower bonduary violation
  #)   
 #
  ###### SAVE REPARIRED IND. NUMBER
#  all_REP <- rbind(all_REP,sum(apply(population, 2, function(x) any(x > cbind(upper) || x < cbind(lower))),na.rm=TRUE))

      #populationRepaired <- bounceBackBoundary3(population,lower,upper)
  populationRepaired <- apply(population,2,bounceBackBoundary2)

  if(Lamarckism==FALSE){
    population <- populationRepaired
  }
  
  ###### SAVE ALL POPULATIONS
#  all_populations <- population
  
  selection       <- rep(0, mu)
  selectedPoints  <- matrix(0, nrow=N, ncol=mu)
  fitness         <- fn_p(population, populationRepaired)
  counteval       <- counteval + lambda
  oldMean         <- numeric(N)
  newMean         <- par
  pc              <- rep(0.0, N)/sqrt(N)
  limit           <- 0
    
#  all_NEWMEAN <- rbind(all_NEWMEAN, newMean)
  
  ## Matrices for creating diffs
  diffs     <- matrix(0, N, lambda)
  x1sample  <- numeric(lambda)
  x2sample  <- numeric(lambda)
    
  chiN      <- (sqrt(2)*gamma((N+1)/2)/gamma(N/2))  
  histNorm  <- sqrt(mu/(mu+1))/sqrt(2)  
  
  while (counteval < budget) {
      iter      <- iter + 1L
      histHead  <- (histHead %% histSize) + 1
      
      if (log.Ft) Ft.log[iter] <- Ft*norm(pc)
      if (log.value) value.log[iter,] <- fitness
      if (log.mean) mean.log[iter] <- fn_p(bounceBackBoundary2(newMean))
      if (log.pop) pop.log[,,iter] <- population
      
      ## Select best 'mu' individuals of population
      selection       <- order(fitness)[1:mu]
      selectedPoints  <- population[,selection]
      
      # Save selected population in the history buffer
      history[,,histHead] <- selectedPoints * histNorm/Ft
      
      ## Calculate weighted mean of selected points
      oldMean <- newMean
      newMean <- drop(selectedPoints %*% weights)
      
#      all_NEWMEAN <- rbind(all_NEWMEAN, newMean)
      ## Write to buffers
      step <- (newMean - oldMean) / Ft
      steps$write(step)
      
      ## Update Ft
      FtHistory[histHead] = Ft
      oldFt <- Ft
      if (iter > pathLength-1 && (sum(step == 0) == 0)) {
        Ft <- calculateFt(steps, N, lambda, pathLength, Ft, c_Ft, pathRatio)
      }
      
      ####### SAVE ALL FT
#      all_FT    <- rbind(all_FT,Ft)
      
      ## Update parameters
      pc = (1 - cc)* pc + cc* step
#      all_PC <- rbind(all_PC,pc)
      
      ## Sample from history with uniform distribution
      limit <- ifelse(iter < histSize, histHead, histSize)
      historySample <- sample(1:limit,lambda, T)
      
      x1sample <- sample(1:mu, lambda, replace=TRUE)#, weights)
      x2sample <- sample(1:mu, lambda, replace=TRUE)#, weights)
      ## Make diffs    
      for (i in 1:lambda) {
        x1 <- history[, x1sample[i], historySample[i]]
        x2 <- history[, x2sample[i], historySample[i]]
        
        diffs[,i] <- (x1 - x2) +
          sqrt(1-c_cov) * rnorm(1) * pc*chiN + 
          sqrt(c_cov) * rnorm(N)/chiN*tol  
        
      #  diffs[,i] <- sqrt(1-c_pc)*((x1 - x2)*(1/sqrt(2)) + rnorm(1)*step*chiN +
          #  diffs[,i] <- sqrt(1-c_pc)*((x1 - x2)*(1/sqrt(2)) + rnorm(1)*(newMean-oldMean)) +
          #   sqrt(c_pc) * rnorm(1) * pc * chiN +
        #  weightsSumS * rnorm(N)  
        
      }
      
      ## New population
      population <- newMean + Ft * diffs
      
      # Check constraints violations
      # Repair the individual if necessary
     # populationRepaired <- ifelse(population > lower, 
      #                     ifelse(population < upper, population, 
      #                            bounceBackBoundary(lower,upper,isLowerViolation=FALSE,population)),   ## upper bonduary violation
      #                     bounceBackBoundary(lower,upper,isLowerViolation=TRUE,population)             ## lower bonduary violation
      #)   
      ###### SAVE REPARIRED IND. NUMBER
      #all_REP <- rbind(all_REP,sum(apply(population, 2, function(x) any(x > cbind(upper) || x < cbind(lower))),na.rm=TRUE))
      populationTemp <- population
      #populationRepaired <- bounceBackBoundary3(population,lower,upper)
      populationRepaired <- apply(population,2,bounceBackBoundary2)
      
      counter=0
      for(tt in 1:ncol(populationTemp)){
        if(any(populationTemp[,tt] != populationRepaired[,tt]))
          counter = counter + 1
      }
#      all_REP <- rbind(all_REP,counter)
      
      if(Lamarckism==FALSE){
        population <- populationRepaired
      }
      
      # SAVE ALL POPULATIONS
#      all_populations <- rbind(all_populations,population)
      
      ## Evaluation
      fitness <- fn_p(population, populationRepaired)
  
      
      ## Break if fit:    
      wb <- which.min(fitness)
      if (fitness[wb] < best.fit) {
        best.fit <- fitness[wb]
        best.par <- population[,wb]
      }
      
      counteval <- counteval + lambda
      
      ## Check if the middle point is the best found so far
      cumMean <- 0.9*cumMean+0.1*newMean
      #cumMeanRepaired <- ifelse(cumMean > lower, 
       #                              ifelse(cumMean < upper, cumMean, 
        #                                    bounceBackBoundary(lower,upper,isLowerViolation=FALSE,cumMean)),   ## upper bonduary violation
        #                             bounceBackBoundary(lower,upper,isLowerViolation=TRUE,cumMean)             ## lower bonduary violation
      #) 
      cumMeanRepaired <-bounceBackBoundary2(cumMean)
      
      fn_cum  <- fn_p(cumMean, cumMeanRepaired)
      if (fn_cum < best.fit) {
        best.fit <- drop(fn_cum)
        best.par <- cumMean
      }
      counteval <- counteval + 1
      
      ## Escape from flat-land:
      if (min(fitness) == sort(fitness,partial=min(1+floor(lambda/2), 2+ceiling(lambda/4)))[min(1+floor(lambda/2), 2+ceiling(lambda/4))]) { 
        Ft <- Ft * exp(0.2*Ft_scale);
      }
      
      if (fitness[1] <= stopfitness) {
        msg <- "Stop fitness reached."
        break
      }
      
      #print(c(counteval,restart.length,best.fit, norm(step), norm(pc)*Ft, Ft),4) 
      
  }
  # Restart paramaters adaptation
  lambda  <- round(lambda+initlambda * 0.2)
  mu      <- floor(lambda/2)
  weights <- log(mu+1) - log(1:mu)
  #weights <- (1:mu)*0+1
  weights <- weights/sum(weights)                                 

    
  cnt <- c(`function`=as.integer(counteval))
  
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
  
#  all_populations <<- all_populations
#  all_FT <<- all_FT
#  all_REP <<- all_REP
#  all_NEWMEAN <<- all_NEWMEAN
#  all_PC <<- all_PC
  return(res)
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
    
    ratio[i-pathLength] <- totalPath / directPath
  }
  return (mean(ratio))
  
}
