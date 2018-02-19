bSeq=c(0)
startFromStep=0.0125/16
cumSum=startFromStep
while(cumSum<=1){
  bSeq = c( bSeq, cumSum )
  cumSum=2*cumSum
}
bSeq=rev(1-bSeq)


baseVect=c()
baseVect4b=c()
minY = Inf
maxY = -Inf
DATA_VERS = c( "rzut", "odb", "zawijanie", "losowanie", "DES",
               "scaledMutant","midTarget", "scaledMutant2", "randBase", "midBase" )

myDATA_VERS = c("ThrowOnLimit","BounceBack", "Wrapping","Drawing","Darwinian",
                "ScaledMutant","MidTarget","ScaledMutant2","RandBase","MidBase")

NMBR_OF_RUNS = 10
N <- 10

for( DATA_VER in myDATA_VERS){
  print(DATA_VER)
  baseVect4b=c()
  for( b in bSeq ){

    numberOfSucc = 0
    for( run in 1:NMBR_OF_RUNS){
            if(DATA_VER=="Darwinian")
              result <- CMADE(
                rep(0,N),
                fn=function(x){
                  sum(x-b)^2
                },
                lower=-1,
                upper=1,
                control=list("Lamarckism"=FALSE,"diag.bestVal"=TRUE,"budget"=1000*N)
              )
            else
              result <- CMADE(
                rep(0,N),
                fn=function(x){
                  sum(x-b)^2
                },
                lower=-1,
                upper=1,
                control=list("Lamarckism"=TRUE,"diag.bestVal"=TRUE,"budget"=1000*N)
              )
            whichIters = which(result$diagnostic$bestVal<=10^-8)
      if(length(whichIters)>0){
        whichIter=whichIters[1]
        numberOfSucc = numberOfSucc+1
      }else{
        whichIter = MAX_GEN

      }
      whichEval=whichIter*Evals4iter+Evals4iter#+ocena pop poczatkowej
      baseVect[run] = whichEval
    }
    if(numberOfSucc>0){
      curLevel=sum(baseVect)/numberOfSucc #ERT
    }else{
      curLevel=sum(baseVect)/0.5 #ERT
    }
    baseVect4b=c(baseVect4b, curLevel/targetLevel)
  }
  res4Constraints[[DATA_VER]] <- baseVect4b
  minY=min(minY, baseVect4b)
  maxY=max(maxY, baseVect4b)
}
#colors <- c(rep("black",10),rep("red",10))
colors <- rainbow(length(DATA_VERS))
linetype <- c(c(1:6),c(1:6),c(1:6),c(1:2))
plotchar <- c( 0:10, 0:10)

setEPS()
postscript( paste0('wykresy/D', DIM, '/', ALG_NAME, '/kwadrat/', TEST_SUIT_NAME, '_dim:', DIM, '_alg:', ALG_NAME,  '_Kw.eps'),paper='special', family="Courier", width=4.2,height=4.2,horizontal=F)
par(mar=c(3,4,1,1)+0.1)#bottom left top right

DATA_VER = DATA_VERS[1]
maxY=3
plot(res4Constraints[[DATA_VER]], ylab="level/targetLevel",  type="l", lwd=2, col=colors[1], ylim=c(minY, maxY), xaxt = "n" )
points(length(bSeq), res4Constraints[[DATA_VER]][length(bSeq)], col=colors[1], pch=plotchar[1])
points(1, res4Constraints[[DATA_VER]][1], col=colors[1], pch=plotchar[1])

axis(1, at=1:length(bSeq), labels=round(bSeq, digits=3) )


#odstep w ramach jednej jednostki
odstJednX=1/length(DATA_VERS)

for( dataVerIndx in 2:length(DATA_VERS) ){
  DATA_VER=DATA_VERS[dataVerIndx]
  lines( res4Constraints[[DATA_VER]], type="l", lwd=2, col=colors[dataVerIndx])
  odstJednY = (res4Constraints[[DATA_VER]][length(bSeq)]-res4Constraints[[DATA_VER]][length(bSeq)-1])/length(DATA_VERS)
  yCoord=res4Constraints[[DATA_VER]][length(bSeq)]-dataVerIndx*odstJednY +odstJednY
  if( yCoord >maxY && (res4Constraints[[DATA_VER]][length(bSeq)]<maxY || res4Constraints[[DATA_VER]][length(bSeq)-1]<maxY)){
    yCoord =maxY
  }
  points(length(bSeq)-dataVerIndx*odstJednX +odstJednX , yCoord, col=colors[dataVerIndx], pch=plotchar[dataVerIndx])
  odstJednY = (res4Constraints[[DATA_VER]][2]-res4Constraints[[DATA_VER]][1])/length(DATA_VERS)
  yCoord=res4Constraints[[DATA_VER]][1]+dataVerIndx*odstJednY -odstJednY
  if( yCoord >maxY && (res4Constraints[[DATA_VER]][2]<maxY || res4Constraints[[DATA_VER]][1]<maxY)){
    yCoord =maxY
  }
  points(1+dataVerIndx*odstJednX -odstJednX , yCoord, col=colors[dataVerIndx], pch=plotchar[dataVerIndx])

}
legend("topleft", DATA_VERS, text.font=2, cex=0.6, col=colors[1:length(DATA_VERS)],pch=plotchar[1:length(DATA_VERS)] )
dev.off()
