Quadric2 <- function(){
  N <- 10
  reps <- 10
  b <- seq(0,1,by=0.01)
  colors <- rainbow(16)
  linetype <- c(c(2:6),c(1:6),c(1:6))
  handlingMethods <- c("BounceBack","Darwinian","Drawing","ExpS","MidBase","MidTarget","RandBase","ScaledMutant","ScaledMutant2","ThrowOnLimit","Wrapping")

  ## TARGET LEVEL
  targetLevel <- NULL
  source('C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/Constriants/QuadricProblem/CMADEv2017.R')
  for (i in 1:reps) {
      # Variant with virtually no constraints
      result <- CMADE(
        rep(0,N),
        fn=function(x){
          sum(x-0)^2
        },
        lower=-10^8,
        upper=10^8,
        control=list("Lamarckism"=FALSE,"diag.bestVal"=TRUE)
      )

      # In which population the level 10^-8 was reached for the first time
      targetLevel<-mean(c( targetLevel, which(result$diagnostic$bestVal < 10^-8)[1] ))
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
              result <- CMADE(
                rep(0,N),
                fn=function(x){
                  sum(x-bb)^2
                },
                lower=-1,
                upper=1,
                control=list("Lamarckism"=FALSE,"diag.bestVal"=TRUE)
              )

              level<-mean(c( level, which(result$diagnostic$bestVal < 10^-8)[1] ))
        }
        resMethod <- c(resMethod,targetLevel/level)
      }
      resConstraints[[handlingMethodNum]] <- resMethod
  }


  plot(b,resConstraints[[1]], type="l",lwd=2, ylim=c(0,1),ylab="targetLevel / level",col=colors[1], lty=linetype[1])
  for(j in 2:handlingMethodNum )
    lines(b,resConstraints[[j]], type="l",lwd=2, col=colors[j], lty=linetype[j])

  legend("topright",handlingMethods,text.font=2, cex=1.2, col=colors[1:handlingMethodNum], lty=linetype[1:handlingMethodNum], lwd=2, ncol=3)


}
