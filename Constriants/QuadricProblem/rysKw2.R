rbQuadricfunction <- function(){
  
DATA_VERS_NICE_NAMES=c("Reinitialization", "Lamarckian projection" , "Darwinian projection", "Lamarckian reflection", "Darwinian reflection", "Lamarckian wrapping", 
                       "Darwinian wrapping", "Scaled mutant", "Death penalty", 
                       "Quadratic penalty", "Substitution penalty", "Resampling",
                       "Rand base", "Midpoint base", "Midpoint target", "Scaled to base", "Conservatism" )

ALG_NAME = "DES"
NMBR_OF_RUNS = 2
DIM = 10
DESIRED_LEVEL = 10^-8
Evals4iter = 4*DIM+1
TEST_SUIT_NAME = "kw"

if(ALG_NAME == "DES"){
  targetLevel = 29*Evals4iter
}

if(ALG_NAME == "CMAES"){#, "expS", "expC"
  DATA_VERS = c( "rzut", "odb", "zawijanie", "losowanie", "powtMut", "genPhenSepRzut", "genPhenSepOdb", "genPhenzawijanie", 
                 "DES", "penSq", "scaledMutant", "scaledMutant2", "multByPun", "randBase", "midBase", "conserv", "DeathPen")
}else if(ALG_NAME == "BBDE"){
  DATA_VERS = c("rzut", "odb", "zawijanie", "losowanie", "powtMut", "genPhenSepRzut", "genPhenSepOdb", 
                "genPhenzawijanie", "DES", "scaledMutant", "penSq", "midTarget", "scaledMutant2", "randBase" )
  DATA_VERS_NAMES_MAPPING = list("Reinitialization"="losowanie", "Lamarckian projection"="rzut" , "Darwinian projection"="genPhenSepRzut", 
                                 "Lamarckian reflection"="odb", "Darwinian reflection"="genPhenSepOdb", "Lamarckian wrapping"="zawijanie", 
                                 "Darwinian wrapping"="genPhenzawijanie", "Scaled mutant"="scaledMutant", "Death penalty"="DES", 
                                 "Quadratic penalty"="penSq", "Substitution penalty"="NONE", "Resampling"="powtMut",
                                 "Rand base"="randBase", "Midpoint base"="NONE", "Midpoint target"="midTarget", "Scaled to base"="scaledMutant2", 
                                 "Conservatism"="NONE" )
  
}else if(ALG_NAME == "DES"){
  DATA_VERS = c( "Lamarckian projection", "Lamarckian reflection", "Lamarckian wrapping", "Reinitialization", 
                 "Resampling", "Darwinian projection", "Darwinian reflection", 
                 "Darwinian wrapping", "Substitution penalty", "Scaled mutant", 
                 "Quadratic penalty", "Midpoint target", "Scaled to base", "Rand base", "Conservatism", "Midpoint base" ) 
  DATA_VERS_NAMES_MAPPING = list("Reinitialization"="Reinitialization", "Lamarckian projection"="Lamarckian projection" , "Darwinian projection"="Darwinian projection", "Lamarckian reflection"="Lamarckian reflection", "Darwinian reflection"="Darwinian reflection", "Lamarckian wrapping"="Lamarckian wrapping", 
                                 "Darwinian wrapping"="Darwinian wrapping", "Scaled mutant"="Scaled mutant", "Death penalty"="NONE", 
                                 "Quadratic penalty"="Quadratic penalty", "Substitution penalty"="Substitution penalty", "Resampling"="Resampling",
                                 "Rand base"="Rand base", "Midpoint base"="Midpoint base", "Midpoint target"="Midpoint target", "Scaled to base"="Scaled to base", "Conservatism"="Conservatism" )
  

}else{
  #komplet
  DATA_VERS = c( "rzut", "odb", "zawijanie", "losowanie", 
                 "powtMut", "genPhenSepRzut", "genPhenSepOdb", 
                 "genPhenzawijanie", "DES", "scaledMutant", 
                 "penSq", "midTarget", "scaledMutant2", "randBase", "conserv", "midBase" ) 
  DATA_VERS_NAMES_MAPPING = list("Reinitialization"="losowanie", "Lamarckian projection"="rzut" , "Darwinian projection"="genPhenSepRzut", "Lamarckian reflection"="odb", "Darwinian reflection"="genPhenSepOdb", "Lamarckian wrapping"="zawijanie", 
                                 "Darwinian wrapping"="genPhenzawijanie", "Scaled mutant"="scaledMutant", "Death penalty"="DES", 
                                 "Quadratic penalty"="penSq", "Substitution penalty"="NONE", "Resampling"="powtMut",
                                 "Rand base"="randBase", "Midpoint base"="midBase", "Midpoint target"="midTarget", "Scaled to base"="scaledMutant2", "Conservatism"="conserv" )
  
}


bSeq=c(0)
startFromStep=0.0125/16
cumSum=startFromStep
while(cumSum<=1){
  bSeq = c( bSeq, cumSum )
  cumSum=2*cumSum
}
bSeq=rev(1-bSeq)

res4Constraints = list()
baseVect=c()
baseVect4b=c()
minY = Inf
maxY = -Inf
for( DATA_VER in DATA_VERS){
  print(DATA_VER)
  baseVect4b=c()
  for( b in bSeq ){
    
    numberOfSucc = 0
    for( run in 1:NMBR_OF_RUNS){
      if(ALG_NAME == "DES"){
        if(DATA_VER == "Darwinian projection" || DATA_VER == "Darwinian reflection" || 
           DATA_VER == "Darwinian wrapping" || DATA_VER == "Quadratic penalty" || 
           DATA_VER == "Substitution penalty")
          isDarw = TRUE
        else
          isDarw= FALSE
        source(paste("C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/Constriants/DES/",DATA_VER,"/CMADEv2017.R",sep=""))
        wyniki <- CMADE(
          rep(0,DIM),
          fn=function(x){
            sum(x-b)^2
          },
          lower=-1,
          upper=1,
          control=list("Lamarckism"=isDarw,"diag.bestVal"=TRUE)
        )
        whichIters = which(wyniki$diagnostic$bestVal<=DESIRED_LEVEL)
      }else{
        load(paste0( 'bin/', TEST_SUIT_NAME, '_alg:', ALG_NAME, '_dim:', DIM, '_b:', b, '_ver:', DATA_VER, '.bin') )
        whichIters = which(wyniki[[run]]$bestSoFarFitHist<=DESIRED_LEVEL)
      }
      if(length(whichIters)>0){
        whichIter=whichIters[1]
        numberOfSucc = numberOfSucc+1
      }else{
        whichIter = length(wyniki$diagnostic$bestVal)
        
      }
      whichEval=whichIter*Evals4iter #+ocena pop poczatkowej
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

res4Constraints <<- res4Constraints
colors <- c("black", "black", "grey", "black", "grey", "black", "grey", rep("black",11))
plotchar <- c( 4, 0, 15, 1, 16, 2, 17, 3, 5:14)

setEPS()
postscript( paste0( TEST_SUIT_NAME, 'dim', DIM, 'alg', ALG_NAME,  '.eps'),paper='special', family="Courier", width=4.2,height=4.2,horizontal=F)
par(mar=c(3,4,1,1)+0.1)#bottom left top right


DATA_VER = DATA_VERS_NAMES_MAPPING[[1]]
LWD = 1
#maxY=3
plot(res4Constraints[[DATA_VER]], ylab="ERT/reference ERT",  type="n", lwd=LWD, col=colors[1], ylim=c(minY, maxY), xaxt = "n" )
axis(1, at=1:length(bSeq), labels=round(bSeq, digits=3) )
title(xlab="b", mgp=c(2,1,0) )
      
#odstep w ramach jednej jednostki
MAX_SPLITS=9
odstJednX=1/MAX_SPLITS

plottingDataVerIndx = 0
for( dataVerIndx in 1:length(DATA_VERS_NICE_NAMES) ){
  DATA_VER=DATA_VERS_NAMES_MAPPING[[dataVerIndx]]
  if(DATA_VER!="NONE"){
    lines( res4Constraints[[DATA_VER]], type="l", lwd=LWD, col=colors[dataVerIndx])
    for(shift in seq(0, 10, by=2)){
      
      if( plottingDataVerIndx < MAX_SPLITS ){ #ostatnia podzialka
        odstJednY = (res4Constraints[[DATA_VER]][length(bSeq)-shift]-res4Constraints[[DATA_VER]][length(bSeq)-1-shift])/MAX_SPLITS
        yCoord=res4Constraints[[DATA_VER]][length(bSeq)-shift] - plottingDataVerIndx*odstJednY 
        if( yCoord >maxY && (res4Constraints[[DATA_VER]][length(bSeq)-shift]<maxY || res4Constraints[[DATA_VER]][length(bSeq)-1-shift]<maxY)){
          if(res4Constraints[[DATA_VER]][length(bSeq)-1-shift]<maxY){#od lewej widac
            fractYdone = (maxY-res4Constraints[[DATA_VER]][length(bSeq)-1-shift])/(res4Constraints[[DATA_VER]][length(bSeq)-shift]-res4Constraints[[DATA_VER]][length(bSeq)-1-shift])
            yCoord =maxY
            points( length(bSeq)-shift-1+fractYdone, yCoord, col=colors[dataVerIndx], pch=plotchar[dataVerIndx])
          }else{#od prawej widac
            fractYdone = (maxY-res4Constraints[[DATA_VER]][length(bSeq)-shift])/(res4Constraints[[DATA_VER]][length(bSeq)-shift-1]-res4Constraints[[DATA_VER]][length(bSeq)-shift])
            yCoord =maxY
            points( length(bSeq)-shift-fractYdone, yCoord, col=colors[dataVerIndx], pch=plotchar[dataVerIndx])
          }
        }else{
          points(length(bSeq)-shift-plottingDataVerIndx*odstJednX, yCoord, col=colors[dataVerIndx], pch=plotchar[dataVerIndx])
        }
      }else{
        if(length(bSeq)-shift-plottingDataVerIndx*odstJednX >1){
          odstJednY = (res4Constraints[[DATA_VER]][length(bSeq)-shift-1]-res4Constraints[[DATA_VER]][length(bSeq)-shift-2])/MAX_SPLITS
          yCoord=res4Constraints[[DATA_VER]][length(bSeq)-shift-1]-(plottingDataVerIndx-MAX_SPLITS)*odstJednY 
          if( yCoord >maxY && (res4Constraints[[DATA_VER]][length(bSeq)-shift-1]<maxY || res4Constraints[[DATA_VER]][length(bSeq)-shift-2]<maxY)){
            if(res4Constraints[[DATA_VER]][length(bSeq)-2-shift]<maxY){#od lewej widac
              fractYdone = (maxY-res4Constraints[[DATA_VER]][length(bSeq)-2-shift])/(res4Constraints[[DATA_VER]][length(bSeq)-1-shift]-res4Constraints[[DATA_VER]][length(bSeq)-2-shift])
              yCoord =maxY
              points( length(bSeq)-shift-2+fractYdone, yCoord, col=colors[dataVerIndx], pch=plotchar[dataVerIndx])
            }else{#od prawej widac
              fractYdone = (maxY-res4Constraints[[DATA_VER]][length(bSeq)-1-shift])/(res4Constraints[[DATA_VER]][length(bSeq)-shift-2]-res4Constraints[[DATA_VER]][length(bSeq)-1-shift])
              yCoord =maxY
              points( length(bSeq)-shift-1-fractYdone, yCoord, col=colors[dataVerIndx], pch=plotchar[dataVerIndx])
            }
          }else{
            points(length(bSeq)-shift-plottingDataVerIndx*odstJednX, yCoord, col=colors[dataVerIndx], pch=plotchar[dataVerIndx])
          }
        }
        
      }
    }
    plottingDataVerIndx = plottingDataVerIndx + 1
  }
}
dev.off()

}