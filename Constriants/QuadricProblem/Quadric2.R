Quadric2 <- function(){
  N <- 10
  reps <- 10
  #b <- seq(0,1,by=0.1)
  b <- c(0.2, 0.6, 0.8, 0.9, 0.95, 0.975, 0.987, 0.99375, 0.996875, 0.9984375, 0.9992188, 1)
  colors <- rainbow(16)
  linetype <- c(c(2:6),c(1:6),c(1:6))
  handlingMethods <- c("BounceBack","Darwinian","Drawing","ExpS","MidBase","MidTarget","RandBase","ScaledMutant","ScaledMutant2","ThrowOnLimit","Wrapping")

  ## TARGET LEVEL
  targetLevel <- NULL
  source('C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/Constriants/QuadricProblem/CMADEv2017.R')
  for (i in 1:reps) {
      set.seed(1)
      # Variant with virtually no constraints
      result <- CMADE(
        rep(0,N),
        fn=function(x){
          sum(x-0)^2
        },
        lower=-10^8,
        upper=10^8,
        control=list("Lamarckism"=FALSE,"diag.bestVal"=TRUE, "budget"=1000*N)
      )

      # In which population the level 10^-8 was reached for the first time
      targetLevel<-mean(c( targetLevel, which(result$diagnostic$bestVal <= 10^-8)[1] ))
  }

  ## THE LEVELS FOR CONSTRAINTED PROBLEM (-1,1) FOR EACH HANDLING METHOD
  resConstraints <- list()
  handlingMethodNum <- 0
  for(cc in handlingMethods){
      handlingMethodNum <- handlingMethodNum + 1
      print(cc)
      resMethod <- c()
      source(paste("C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/Constriants/DES/",cc,"/CMADEv2017.R",sep=""))
      for(bb in b){
        print(bb)
        level <- NULL
        for (i in 1:reps) {
              set.seed(1)
              if(cc=="Darwinian")
                  result <- CMADE(
                    rep(0,N),
                    fn=function(x){
                      sum(x-bb)^2
                    },
                    lower=-1,
                    upper=1,
                    control=list("Lamarckism"=FALSE,"diag.bestVal"=TRUE,"budget"=1000*N)
                  )
              else
                result <- CMADE(
                  rep(0,N),
                  fn=function(x){
                    sum(x-bb)^2
                  },
                  lower=-1,
                  upper=1,
                  control=list("Lamarckism"=TRUE,"diag.bestVal"=TRUE,"budget"=1000*N)
                )
              if(any(result$diagnostic$bestVal <= 10^-8))
                level<-mean(c( level, which(result$diagnostic$bestVal <= 10^-8)[1] ))
              else
                level<-mean(c( level, length(result$diagnostic$bestVal) ))
        }
        resMethod <- c(resMethod,level/targetLevel)
      }
      resConstraints[[handlingMethodNum]] <- resMethod
  }

  resConstraints <<- resConstraints

  plot(1-b,resConstraints[[2]], type="l",lwd=2, log="x", ylab="level / targetLevel",col=colors[2], lty=linetype[2])
  for(j in 1:handlingMethodNum )
    lines(1-b,resConstraints[[j]], type="l",lwd=2, col=colors[j], lty=linetype[j])



}
