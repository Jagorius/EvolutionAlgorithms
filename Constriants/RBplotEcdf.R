ecdfplot <- function(DIM){
  
MU_EA=100
MU_ES=25

NMBR_OF_RUNS = 51

#ALG_NAME = "DE"
#ALG_NAME = "DE_L2BEST"
#ALG_NAME = "JADE"
#ALG_NAME = "EA"
#ALG_NAME = "EsMuPlLam"
#ALG_NAME = "B6e6rl"
#ALG_NAME = "SADE"
#ALG_NAME = "CMAES"
#ALG_NAME = "BBDE"
ALG_NAME = "jSO"
#ALG_NAME = "DES"

#TEST_SUIT_NAME = 'cec05'
#TEST_SUIT_NAME = 'cec13'
TEST_SUIT_NAME = 'cec17'

#DIM=10
#DIM=30
#DIM=50
#DIM=100

if( TEST_SUIT_NAME == 'cec05'){
  MAX_FUN_NMBR = 25
}else if( TEST_SUIT_NAME == 'cec13'){
  MAX_FUN_NMBR = 28
}else if( TEST_SUIT_NAME == 'cec17'){
  MAX_FUN_NMBR = 30
}

if( ALG_NAME == "DE" ||ALG_NAME == "BBDE" || ALG_NAME == "DE_L2BEST" || ALG_NAME == "EA" || ALG_NAME == "JADE"|| ALG_NAME == "B6e6rl"|| ALG_NAME == "SADE" || ALG_NAME == "DES" || ALG_NAME == "jSO"){
  MU=MU_EA
}else if(ALG_NAME == "CMAES"||ALG_NAME == "EsMuPlLam"){
  MU=MU_ES
}else{
  stop("Nieznany algorytm")
}

if(ALG_NAME == "CMAES"){
  LAMBDA = 4+floor(3*log(10))#CMAES
  MU = floor(LAMBDA/2) #CMAES
}

MaxFES = 10000 * DIM
if( ALG_NAME == "DE" ||ALG_NAME == "BBDE" ||ALG_NAME == "DE_L2BEST"|| ALG_NAME == "EA" || ALG_NAME == "JADE"||ALG_NAME == "B6e6rl" || ALG_NAME == "SADE" || ALG_NAME == "DES" || ALG_NAME == "jSO"){
  MAX_GEN = MaxFES/MU
}else if(ALG_NAME == "CMAES"||ALG_NAME == "EsMuPlLam"){
  MAX_GEN = MaxFES/LAMBDA
}else{
  stop("Nieznany algorytm")
}




stateDumpAt = 1:MAX_GEN
if( ALG_NAME == "DE" || ALG_NAME == "BBDE" ||ALG_NAME == "DE_L2BEST" || ALG_NAME == "EA" || ALG_NAME == "JADE"||ALG_NAME == "B6e6rl" || ALG_NAME == "SADE"){
  stateDumpAt = stateDumpAt*MU
}else if(ALG_NAME == "CMAES"||ALG_NAME == "EsMuPlLam"){
  stateDumpAt = stateDumpAt*LAMBDA
}else if(ALG_NAME == "DES"||ALG_NAME == "jSO"){
  stateDumpAt = stateDumpAt*MU
}else{
  stop("Nieznany algorytm")
}


if( ALG_NAME == "DE" ||ALG_NAME == "BBDE" ||ALG_NAME == "DE_L2BEST"|| ALG_NAME == "EA" || ALG_NAME == "JADE"||ALG_NAME == "B6e6rl" || ALG_NAME == "SADE" || ALG_NAME == "DES" || ALG_NAME == "jSO"){
  rowNames = stateDumpAt + MU
}else if(ALG_NAME == "CMAES"||ALG_NAME == "EsMuPlLam"){
  rowNames = stateDumpAt + LAMBDA
}else{
  stop("Nieznany algorytm")
}

if(ALG_NAME == "DES"){
  MAX_GEN = 100
}else if(ALG_NAME == "jSO"){
  if(DIM==10)
    MAX_GEN = 96
  else if(DIM==30)
    MAX_GEN = 95
  else if(DIM==50)
    MAX_GEN = 99
  else if(DIM==100)
    MAX_GEN = 99
}

if(ALG_NAME == "DES" || ALG_NAME == "jSO"){
  rowNames = seq(0.01, 1, length.out = MAX_GEN)*10000
}else{
  rowNames = rowNames/DIM
}

colNames = 1:NMBR_OF_RUNS

DATA_VERS_NICE_NAMES=c("Reinitialization", "Lamarckian projection" , "Darwinian projection", "Lamarckian reflection", "Darwinian reflection", "Lamarckian wrapping", 
                       "Darwinian wrapping", "Scaled mutant", "Death penalty", 
                       "Quadratic penalty", "Substitution penalty", "Resampling",
                       "Rand base", "Midpoint base", "Midpoint target", "Scaled to base", "Conservatism" )




if(ALG_NAME == "CMAES"){
  DATA_VERS = c( "rzut", "odb", "zawijanie", "losowanie", "powtMut", "genPhenSepRzut", "genPhenSepOdb", "genPhenzawijanie", "expS", "expC", 
               "DES", "penSq", "multByPun")
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
  DATA_VERS = c( "Lamarckian projection", "Lamarckian reflection", "Lamarckian wrapping", "Reinitialization", "Substitution penalty",
                 "Scaled mutant","Midpoint target", "Scaled to base", "Rand base", "Midpoint base" )
  
  DATA_VERS_NAMES_MAPPING = list("Reinitialization"="Reinitialization", "Lamarckian projection"="Lamarckian projection" , "Darwinian projection"="NONE", 
                                 "Lamarckian reflection"="Lamarckian reflection", "Darwinian reflection"="NONE", "Lamarckian wrapping"="Lamarckian wrapping", 
                                 "Darwinian wrapping"="NONE", "Scaled mutant"="Scaled mutant", "Death penalty"="NONE", 
                                 "Quadratic penalty"="NONE", "Substitution penalty"="Substitution penalty", "Resampling"="NONE",
                                 "Rand base"="Rand base", "Midpoint base"="Midpoint base", "Midpoint target"="Midpoint target", "Scaled to base"="Scaled to base", 
                                 "Conservatism"="NONE" )
}else if(ALG_NAME == "jSO"){
  DATA_VERS = c( "Lamarckian projection", "Lamarckian reflection", "Lamarckian wrapping", "Reinitialization", 
                 "Resampling", "Darwinian projection", "Darwinian reflection", 
                 "Darwinian wrapping", "Substitution penalty", "Scaled mutant", 
                 "Quadratic penalty", "Midpoint target", "Scaled to base", "Rand base", "Conservatism" ) 
  
  DATA_VERS_NAMES_MAPPING = list("Reinitialization"="Reinitialization", "Lamarckian projection"="Lamarckian projection" , "Darwinian projection"="Darwinian projection", 
                                 "Lamarckian reflection"="Lamarckian reflection", "Darwinian reflection"="Darwinian reflection", "Lamarckian wrapping"="Lamarckian wrapping", 
                                 "Darwinian wrapping"="Darwinian wrapping", "Scaled mutant"="Scaled mutant", "Death penalty"="NONE", 
                                 "Quadratic penalty"="Quadratic penalty", "Substitution penalty"="Substitution penalty", "Resampling"="Resampling",
                                 "Rand base"="Rand base", "Midpoint base"="NONE", "Midpoint target"="Midpoint target", "Scaled to base"="Scaled to base", 
                                 "Conservatism"="Conservatism" )
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


progi <- list()
 
lastDumpIndx = MAX_GEN


#lista list. skladowe $rzut=list(), $odb=list() 
results<- list()
correctionResults = list()

for( DATA_VER in DATA_VERS){
  results[[DATA_VER]] = list()
  correctionResults[[DATA_VER]] = list()
}

csvRowNames =  DATA_VERS 

csvColNames = 1:MAX_FUN_NMBR
csvMeanMatrix = matrix(0, length(csvRowNames), length(csvColNames), dimnames=list(row=csvRowNames, column=csvColNames) )
csvSdMatrix = matrix(0, length(csvRowNames), length(csvColNames), dimnames=list(row=csvRowNames, column=csvColNames) )
csvMinMatrix = matrix(0, length(csvRowNames), length(csvColNames), dimnames=list(row=csvRowNames, column=csvColNames) )
csvMedMatrix = matrix(0, length(csvRowNames), length(csvColNames), dimnames=list(row=csvRowNames, column=csvColNames) )

for( funNmbr in 1:MAX_FUN_NMBR ){
  print(funNmbr)
  minAtEnd = Inf
  maxAtStart = -Inf
  kompletnyPoczatekJakosc = c()
  resMatrix = matrix(0, length(rowNames), length(colNames), dimnames=list(row=rowNames, column=colNames) )
  corVector = vector("double", NMBR_OF_RUNS )
  dataVerIndx=1
  for( DATA_VER in DATA_VERS){
    if(ALG_NAME == "DES"){
      des_res_path = paste0("C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/Constriants/DES/",DATA_VER,"/M/")
      resMatrix <- read.table(file = paste(  des_res_path,"DES_",funNmbr,"_",DIM,".txt",sep=""),sep = ",")
      results[[DATA_VER ]][[funNmbr]] = resMatrix
    }else if(ALG_NAME == "jSO"){
      print(DATA_VER)
      jso_res_path = paste0("C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/Constriants/jSO/Results/",DATA_VER,"/M/")
      resMatrix <- read.table(file = paste(  jso_res_path,"jSO_",funNmbr,"_",DIM,".txt",sep=""),sep = " ", nrow=51)
      resMatrix <- t(resMatrix)
      results[[DATA_VER ]][[funNmbr]] = resMatrix
    }else{
        load( paste0( 'bin/', TEST_SUIT_NAME, '_alg:', ALG_NAME, '_dim:', DIM, '_fun:', funNmbr, '_ver:', DATA_VER, '.bin')  ) 
        for(runNmbr in 1:NMBR_OF_RUNS){
          resMatrix[,runNmbr] = wyniki[[runNmbr]]$bestSoFarFitHist#-funNmbr*100
          corVector[runNmbr] = wyniki[[runNmbr]]$corrections
          for( dumpIndx in 1: lastDumpIndx){
            if( resMatrix[dumpIndx, runNmbr]== 0 ){
              resMatrix[dumpIndx, runNmbr]= wyniki[[runNmbr]]$bestSoFarFit#-funNmbr*100
            }
          }
          resMatrix[,runNmbr] = resMatrix[,runNmbr]-funNmbr*100
        }
      results[[DATA_VER ]][[funNmbr]] = resMatrix
      correctionResults[[DATA_VER ]][[funNmbr]] = corVector
    }
    
    csvMeanMatrix[dataVerIndx, funNmbr] = mean( unlist(resMatrix[lastDumpIndx,]) )
    csvMedMatrix[dataVerIndx, funNmbr] = median( unlist(resMatrix[lastDumpIndx,]) )
    csvSdMatrix[dataVerIndx, funNmbr] = sd( unlist(resMatrix[lastDumpIndx,]) )
    csvMinMatrix[dataVerIndx, funNmbr] = min( unlist(resMatrix[lastDumpIndx,]) )
    
    minAtEnd = min(minAtEnd,  unlist(resMatrix[lastDumpIndx,]) )
    maxAtStart = max(maxAtStart,  unlist(resMatrix[1,]) )
    kompletnyPoczatekJakosc = c(kompletnyPoczatekJakosc, unlist(resMatrix[1,]))
    
    dataVerIndx = dataVerIndx +1
  }

  #ToDo:inaczej liczysz progi
  length.out=51
  progi[[funNmbr]]<-  10^seq( log10( median(kompletnyPoczatekJakosc)), log10( max(minAtEnd, 10^-8)  ), length.out= length.out) 
  #progi[[funNmbr]] <- rev(c(1 %o% (10)^(0.2*((log10( max(minAtEnd, 10^-8))/0.2):(log10(median(kompletnyPoczatekJakosc))/0.2) ))))
  progi[[funNmbr]][length.out] = max(minAtEnd, 10^-8)
} 

progi<<- progi

#outFileName = paste0( 'csv/', TEST_SUIT_NAME, '_dim:', DIM, '_alg:', ALG_NAME, 'PorSr.csv')

#fileHandle = file( outFileName , open = 'wt') #aby wykasowac stara zawartosc

#cat(ALG_NAME, ", Medians - fun. numbers in columns\n",file=fileHandle)
#write.csv( csvMedMatrix, file=fileHandle )
#cat("\n Min:\n",file=fileHandle)
#write.csv( csvMinMatrix, file=fileHandle )
#cat("\n Mean\n",file=fileHandle)
#write.csv( csvMeanMatrix, file=fileHandle )
#cat("\n Sd\n",file=fileHandle)
#write.csv( csvSdMatrix, file=fileHandle )
#close(fileHandle)

 
plotSequence=list(list(FunFrom = 1, FunTo = 3), list(FunFrom = 4, FunTo = 30), list(FunFrom = 1, FunTo = 30))
#plotSequence=list(list(FunFrom = 4, FunTo = 30))



for(plotSeqEl in plotSequence ){
  FunFrom = plotSeqEl$FunFrom
  FunTo = plotSeqEl$FunTo
  print(paste(FunFrom,FunTo) )


  budgetSteps=1:MAX_GEN

  #lista wektorow. skladowe $rzut=c(), $odb=c() 
  minCount<- list()
  for( DATA_VER in DATA_VERS){
    minCount[[DATA_VER]] = rep(0,length(budgetSteps))
  }

  ecdfMaxSucess <- 0

  for( funNmbr in FunFrom:FunTo ){
    print(paste("Calculating for function: ",funNmbr))
    for( budStep in 1:length( budgetSteps ) ){
      for( progIndx in 1:length(progi[[funNmbr]])){
        for( DATA_VER in DATA_VERS){
          minCount[[DATA_VER]][ budStep ] <- minCount[[DATA_VER]][ budStep ] + sum(results[[DATA_VER]][[funNmbr]][budStep,] <= progi[[funNmbr]][progIndx])
        }
      }
    }
    ecdfMaxSucess <- ecdfMaxSucess + length(progi[[funNmbr]])*NMBR_OF_RUNS
  }
 # ecdfMaxSucess <<- ecdfMaxSucess
  #minCount <<-  minCount
  
  isXaxt="s"
  isYaxt="s"

  colors <- c("black", "black", "grey", "black", "grey", "black", "grey", rep("black",11))
  plotchar <- c( 4, 0, 15, 1, 16, 2, 17, 3, 5:14)


  setEPS()
  postscript( paste0( TEST_SUIT_NAME, 'dim', DIM, 'alg', ALG_NAME,  'funs', FunFrom, 'to', FunTo,  '.eps'), paper='special', family="Courier", width=4.2,height=4.2,horizontal=F)
  par(mar=c(4,4,1.5,1)+0.1)#bottom left top right
  LWD = 1
  plot( rowNames, 1:length(rowNames), xlab="f-evals / dimension",ylab="Proportion of function + target pairs",ylim=c(0, 1),type="n", log="x")  
  interPointsSpace=5
  plottingDataVerIndx = 0
  pointSepStep = 0.02
  atEndValues = matrix(NA,ncol=2, nrow=length(DATA_VERS))
  aucValues = matrix(NA,ncol=2, nrow=length(DATA_VERS))
  for( dataVerIndx in 1:length(DATA_VERS_NICE_NAMES) ){
    DATA_VER = DATA_VERS_NAMES_MAPPING[[dataVerIndx]]
    if(DATA_VER!="NONE"){
      lines(rowNames, minCount[[DATA_VER]]/ecdfMaxSucess, type="l", lwd=LWD, col=colors[dataVerIndx], pch=plotchar[dataVerIndx])
      plotAtPowers = seq( 0.5-plottingDataVerIndx*pointSepStep, log10(MAX_GEN)-plottingDataVerIndx*pointSepStep, length.out = 10)
      plotAtIndx = round( 10^plotAtPowers )
      plotAtIndx = c(1, 2, plotAtIndx)
      
      points( rowNames[plotAtIndx], minCount[[DATA_VER]][plotAtIndx]/ecdfMaxSucess, col=colors[dataVerIndx], pch=plotchar[dataVerIndx])

      atEndValues[plottingDataVerIndx+1,]=c(DATA_VERS_NICE_NAMES[dataVerIndx], minCount[[DATA_VER]][length(minCount[[DATA_VER]])]/ecdfMaxSucess)
      aucValues[plottingDataVerIndx+1,]=c(DATA_VERS_NICE_NAMES[dataVerIndx], sum(minCount[[DATA_VER]]/ecdfMaxSucess)/length(minCount[[DATA_VER]]) )
      
      plottingDataVerIndx = plottingDataVerIndx+1
    }
  }
  write.csv( atEndValues, file=paste0(TEST_SUIT_NAME, 'dim', DIM, 'alg', ALG_NAME,  'funs', FunFrom, 'to', FunTo,  'AtEndVals.csv' ))
  write.csv( aucValues, file=paste0(TEST_SUIT_NAME, 'dim', DIM, 'alg', ALG_NAME,  'funs', FunFrom, 'to', FunTo,  'AUCVals.csv' ))
  
  dev.off()
}

}

#ecdfplot(10)
#ecdfplot(30)
#ecdfplot(50)
ecdfplot(100)