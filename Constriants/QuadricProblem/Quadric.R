quadric <- function(){
  source('C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/Constriants/QuadricProblem/CMADEv2017.R')
  colors <- rainbow(16)
  linetype <- c(c(2:6),c(1:6),c(1:6))
  N <- 10

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
  plot(result$diagnostic$bestVal,type="l",log="y",xlab="Population number", ylab="Error", col="black",
       lwd=3, main=expression(paste("Function: ", sum((x[i]-b)^2,i=1,N),"   N=",10   )))



  # Variants with constraints <-1,1>
  size_b = 5
  b_by = 0.2
  for(b in seq(0.2,1,b_by)){
        result <- CMADE(
              rep(0,N),
              fn=function(x){
                sum(x-b)^2
              },
              lower=-1,
              upper=1,
              control=list("Lamarckism"=FALSE,"diag.bestVal"=TRUE)
        )
        lines(result$diagnostic$bestVal,type="l",lwd=2,col=colors[b/b_by], lty=linetype[b/b_by])
  }

  legend("topright",c('b=0 (-Inf,Inf)','b=0.2 (-1,1)','b=0.4 (-1,1)','b=0.6 (-1,1)','b=0.8 (-1,1)','b=1.0 (-1,1)'),text.font=2, cex=1.2, col=c("black",colors[1:size_b]), lty=c(1,linetype[1:size_b]), lwd=c(3,rep(2,size_b)),ncol=2)
}
