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

  bounceBackBoundary2 <- function(x){
    if(all(x >= cbind(lower)) && all(x <= cbind(upper)))
      return (x)
    else if(any(x < cbind(lower)))
      for(i in which(x < cbind(lower)) )
        x[i] <- lower[i] + abs(lower[i] - x[i])%% (upper[i]- lower[i])
      else if(any(x > cbind(upper)))
        for(i in which(x > cbind(upper)) )
          x[i] <- upper[i] - abs(upper[i] - x[i])%% (upper[i]- lower[i])
        x <-deleteInfsNaNs(x)
        return (bounceBackBoundary2(x))
  }

  #############################
  ##  Algorithm parameters:  ##
  #############################
  Ft          <- controlParam("Ft", 1)                                ## Scaling factor of difference vectors (a variable!)
  initFt      <- controlParam("initFt", 1)
  stopfitness <- controlParam("stopfitness", -Inf)                    ## Fitness value after which the convergence is reached
  ## Strategy parameter setting:
  budget      <- controlParam("budget", 10000*N )                     ## The maximum number of fitness function calls
  initlambda  <- controlParam("lambda", 4*N)       		      ## Population starting size
  minlambda   <- controlParam("minlambda", 4*N)                       ## Population ending size
  lambda      <- initlambda                                           ## Population size
  mu          <- controlParam("mu", floor(lambda/2))                  ## Selection size
  weights     <- controlParam("weights", log(mu+1) - log(1:mu))       ## Weights to calculate mean from selected individuals
  weights     <- weights/sum(weights)                                 ##    \-> weights are normalized by the sum
  weightsSumS <- sum(weights^2)                                       ## weights sum square
  mueff       <- controlParam("mueff", sum(weights)^2/sum(weights^2)) ## Variance effectiveness factor
  cc          <- controlParam("ccum", 4/(N+4))                        ## Evolution Path decay factor
  pathLength  <- controlParam("pathLength",  6)                       ## Size of evolution path
  maxiter     <- controlParam("maxit", floor(budget/(lambda+1)))      ## Maximum number of iterations after which algorithm stops
  c_Ft        <- controlParam("c_Ft",  0)
  pathRatio   <- controlParam("pathRatio",sqrt(pathLength))           ## Path Length Control reference value
  histSize    <- controlParam("history",ceiling(6+ceiling(3*sqrt(N))))## Size of the window of history - the step length history
  Ft_scale    <- controlParam("Ft_scale", ((mueff+2)/(N+mueff+3))/(1 + 2*max(0, sqrt((mueff-1)/(N+1))-1) + (mueff+2)/(N+mueff+3)))
  tol         <- controlParam("tol", 10^-6)
  counteval   <- 0                                                    ## Number of function evaluations

  ## Logging options:
  log.all     <- controlParam("diag", FALSE)
  log.Ft      <- controlParam("diag.Ft", log.all)
  log.value   <- controlParam("diag.value", log.all)
  log.mean    <- controlParam("diag.mean", log.all)
  log.pop     <- controlParam("diag.pop", log.all)
  log.bestVal <- controlParam("diag.bestVal", log.all)

  ## nonLamarckian approach allows individuals to violate boundaries.
  ## Fitness value is estimeted by fitness of repaired individual.
  Lamarckism     <- controlParam("Lamarckism", TRUE)

  ## Fitness function wrapper
  fn_ <- function(x){
    if(all(x >= cbind(lower)) && all(x <= cbind(upper))){
      counteval <<- counteval + 1
      return (fn(x))
    }
    else
      return(.Machine$double.xmax)
  }

  ## Fitness function wrapper for Lamarcian approach
  fn_l <- function(P){
    if(is.matrix(P)){
      if(counteval + ncol(P) <= budget)
        return ( apply(P, 2, fn_) )
      else{
        ret <- c()
        budLeft <- budget-counteval
        for (i in 1:budLeft ) {
          ret <- c(ret,fn_(P[,i]))
        }
        return(c(ret,rep(.Machine$double.xmax,ncol(P)-budLeft)))
      }
    }else{
      if(counteval < budget)
        return ( fn_(P) )
      else
        return(.Machine$double.xmax)
    }
  }

  ## Fitness function wrapper for nonLamarckian approach
  fn_d <- function(P, P_repaired, fitness) {
    P <- deleteInfsNaNs(P)
    P_repaired <- deleteInfsNaNs(P_repaired)

    if(is.matrix(P) && is.matrix(P_repaired)){
      repairedInd <- apply(P!=P_repaired,2,all)
      P_fit <- fitness
      vecDist <- colSums((P - P_repaired)^2)
      P_fit[which(repairedInd)] <- worst.fit + vecDist[which(repairedInd)]
      P_fit <- deleteInfsNaNs(P_fit)
      return(P_fit)
    }else{
      P_fit <- fitness
      if (P!=P_repaired){
        P_fit <- worst.fit + (sum(P-P_repaired)^2)
        P_fit <- deleteInfsNaNs(P_fit)
      }
      return (P_fit)
    }

  }

  ## Asserts - safety checks:
  stopifnot(length(upper) == N)
  stopifnot(length(lower) == N)
  stopifnot(all(lower < upper))
  stopifnot(length(Ft) == 1)

  ## Initialize variables:
  best.fit        <- Inf                  ## The best fitness found so far
  best.par        <- NULL                 ## The best solution found so far
  worst.fit       <- NULL                 ## The worst solution found so far:
  last.restart    <-0
  restart.length  <-0
  restart.number  <-0

  ## According to user specification, preallocate logging structures:
  if (log.Ft)
    Ft.log <- numeric(maxiter)
  if (log.value)
    value.log <- matrix(0, nrow=0, ncol=lambda)
  if (log.mean)
    mean.log <- numeric(maxiter)
  if (log.pop)
    pop.log <- array(0, c(N, lambda, maxiter))
  if (log.bestVal)
    bestVal.log <-  matrix(0, nrow=0, ncol=1)

  ## Allocate buffers:
  steps       <- ringbuffer(size = pathLength*N)                      ## Cyclical buffer containing last 'pathLength' steps of algorithm
  FtHistory   <- array(0, histSize)                                   ## Array buffer containing 'histSize' last values of 'Ft'

  ## Initialize internal strategy parameters
  msg             <- NULL                                             ## Reason for terminating
  restart.number  <- -1

  while( counteval < budget){
    restart.number  <- restart.number+1
    lambda          <- round(((minlambda-initlambda)/budget)*counteval+initlambda)
    mu              <- floor(lambda/2)
    weights         <- log(mu+1) - log(1:mu)
    weights         <- weights/sum(weights)

    pc          <- rep(0.0, N)/sqrt(N)
    histHead    <- 0                                                      ## Pointer to the history buffer head
    iter        <- 0L                                                     ## Number of iterations
    history     <- list()                                                 ## List stores best 'mu'(variable) individuals for 'hsize' recent iterations
    Ft          <- initFt

    # Create fisrt population
    population <- replicate(lambda, runif(N,0.8*lower,0.8*upper))
    cumMean=(upper+lower)/2
    populationRepaired <- apply(population,2,bounceBackBoundary2)

    if(Lamarckism==TRUE){
      population <- populationRepaired
    }

    selection       <- rep(0, mu)
    selectedPoints  <- matrix(0, nrow=N, ncol=mu)
    fitness         <- fn_l(population)
    oldMean         <- numeric(N)
    newMean         <- par
    pc              <- rep(0.0, N)/sqrt(N)
    limit           <- 0
    worst.fit       <- max(fitness)

    ## Matrices for creating diffs
    diffs     <- matrix(0, N, lambda)
    x1sample  <- numeric(lambda)
    x2sample  <- numeric(lambda)

    chiN      <- (sqrt(2)*gamma((N+1)/2)/gamma(N/2))
    histNorm  <- 1/sqrt(2)
    counterRepaired <- 0

    stoptol=F
    while (counteval < budget && !stoptol) {
      iter      <- iter + 1L
      histHead  <- (histHead %% histSize) + 1

      lambda      <- round(((minlambda-initlambda)/budget)*counteval+initlambda)
      mu          <- floor(lambda/2)
      weights <- log(mu+1) - log(1:mu)
      weights <- weights/sum(weights)

      if (log.Ft) Ft.log[iter] <- Ft*norm(pc)
      if (log.value) value.log <- rbind(value.log,fitness)
      if (log.mean) mean.log[iter] <- fn_l(bounceBackBoundary2(newMean))
      if (log.pop) pop.log[,,iter] <- population
      if (log.bestVal) bestVal.log <- rbind(bestVal.log,min(suppressWarnings(min(bestVal.log)), min(fitness)))

      ## Select best 'mu' individuals of popu-lation
      selection       <- order(fitness)[1:mu]
      selectedPoints  <- population[,selection]

      # Save selected population in the history buffer
      history[[histHead]] <- array(0,dim=c(N,mu))
      history[[histHead]] <- selectedPoints * histNorm/Ft

      ## Calculate weighted mean of selected points
      oldMean <- newMean
      newMean <- drop(selectedPoints %*% weights)

      ## Write to buffers
      step <- (newMean - oldMean) / Ft
      steps$write(step)

      ## Update Ft
      FtHistory[histHead] = Ft
      oldFt <- Ft
      #if (iter > pathLength-1 && (sum(step == 0) == 0)&&counterRepaired<0.1*lambda) {
      #  Ft <- calculateFt(steps, N, lambda, pathLength, Ft, c_Ft, pathRatio, chiN, mueff)
      #}

      ## Update parameters
      pc = (1 - cc)* pc + cc* step

      ## Sample from history with uniform distribution
      limit <- ifelse(iter < histSize, histHead, histSize)
      historySample <- sample(1:limit,lambda, T)

      x1sample <- sampleFromHistory(history,historySample,lambda)
      x2sample <- sampleFromHistory(history,historySample,lambda)

      ## Make diffs
      for (i in 1:lambda) {
        x1 <- history[[historySample[i]]][,x1sample[i]]
        x2 <- history[[historySample[i]]][,x2sample[i]]

        diffs[,i] <- (x1 - x2) + rnorm(1)*pc*chiN
      }

      ## New population
      population <- newMean + Ft * diffs + tol*rnorm(diffs)/chiN
      population <- deleteInfsNaNs(population)

      # Check constraints violations
      # Repair the individual if necessary
      populationTemp <- population
      populationRepaired <- apply(population,2,bounceBackBoundary2)

      counterRepaired=0
      for(tt in 1:ncol(populationTemp)){
        if(any(populationTemp[,tt] != populationRepaired[,tt]))
          counterRepaired = counterRepaired + 1
      }

      if(Lamarckism==TRUE){
        population <- populationRepaired
      }

      ## Evaluation
      fitness <- fn_l(population)
      if(Lamarckism==FALSE){
        fitnessNonLamarcian <- fn_d(population, populationRepaired, fitness)
      }

      ## Break if fit :
      wb <- which.min(fitness)
      if (fitness[wb] < best.fit) {
        best.fit <- fitness[wb]
        if(Lamarckism==TRUE)
          best.par <- population[,wb]
        else
          best.par <- populationRepaired[,wb]
      }

      ## Check worst fit:
      ww <- which.max(fitness)
      if (fitness[ww] > worst.fit){
        worst.fit <- fitness[ww]
      }

      ## Fitness with penalty for nonLamarcian approach
      if(Lamarckism==FALSE){
        fitness <- fitnessNonLamarcian
      }


      ## Check if the middle point is the best found so far
      cumMean <- 0.8*cumMean+0.2*newMean
      cumMeanRepaired <-bounceBackBoundary2(cumMean)

      fn_cum  <- fn_l(cumMeanRepaired)
      if (fn_cum < best.fit) {
        best.fit <- drop(fn_cum)
        best.par <- cumMeanRepaired
      }

      ## Escape from flat-land:
      #if (min(fitness) == sort(fitness,partial=min(1+floor(lambda/2), 2+ceiling(lambda/4)))[min(1+floor(lambda/2), 2+ceiling(lambda/4))]) {
      #  Ft <- Ft * exp(0.2*Ft_scale);
      #}

      if (fitness[1] <= stopfitness) {
        msg <- "Stop fitness reached."
        break
      }

      if(abs(range(fitness)[2] - range(fitness)[1]) < tol)
      {
        if (counteval < 0.8*budget)
          stoptol=T
      }


    }
  }

  cnt <- c(`function`=as.integer(counteval))

  log <- list()
  ## Subset lognostic data to only include those iterations which
  ## where actually performed.
  if (log.Ft) log$Ft <- Ft.log[1:iter]
  if (log.value) log$value <- value.log[1:iter,]
  if (log.mean) log$mean <- mean.log[1:iter]
  if (log.pop)   log$pop   <- pop.log[,,1:iter]
  if (log.bestVal) log$bestVal <- bestVal.log

  ## Drop names from value object
  names(best.fit) <- NULL
  res <- list(par=best.par,
              value=best.fit,
              counts=cnt,
              resets=restart.number,
              convergence=ifelse(iter >= maxiter, 1L, 0L),
              message=msg,
              diagnostic=log
  )
  class(res) <- "cmade.result"

  return(res)
}

sampleFromHistory <- function(history,historySample,lambda){
  ret <- c()
  for(i in 1:lambda)
    ret <- c(ret,sample(1:ncol(history[[historySample[i]]]), 1))
  return(ret)
}

deleteInfsNaNs <- function(x){
  x[is.na(x)] <- .Machine$double.xmax
  x[is.infinite(x)] <- .Machine$double.xmax
  return(x)
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
calculateFt <- function(stepsBuffer, N, lambda, pathLength, currentFt, c_Ft, pathRatio, chiN, mueff) {

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
  return (currentFt * exp(1/(sqrt(N)+1) *(c_Ft * (chiN / (totalPath / directPath)-1))) )
  #return (currentFt * exp(c_Ft * (pathRatio / (totalPath / directPath)-1)))
  #return (currentFt * exp(1/(sqrt(N)+1) * ((chiN / (totalPath / directPath) - 1)*((mueff+2)/(N+mueff+3))/( 1 + 2*max(0, sqrt((mueff-1)/(N+1))-1) + ((mueff+2)/(N+mueff+3))))))
}
