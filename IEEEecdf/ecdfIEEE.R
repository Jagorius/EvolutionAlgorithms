library(ggplot2)

ecdfIEEE<- function(){
  N       <- 30         
  F_from  <- 1
  F_to    <- 30
  
  des_path = "C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/CEC2017/CMADE-BBComp5 HistSize=3sqrt(N) init(-80,80) FINALTEST/M/"
  rb_ipop_cma_es_path = "C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/IEEEecdf/Results for all papers/paper ID  E-17343/RB-IPOP-CMA-ES/"
  cmaes_pure = "C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/CEC2017/CMAES/M/"
  des_pathv2 = "C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/DESv2/DES + dMean custom1 (DESv2.5)/M/"
  jsopath = "C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/IEEEecdf/Results for all papers/Paper ID   17315/jSO/"
  lshadeSpacma_path = "C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/IEEEecdf/Results for all papers/Paper ID 17051/LSHADE_SPACMA/"
  cmaesNos_path = "C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/CEC2017/CMAES noSigma/M/"
  
  ecdfValues <- list()
  budgetSteps <- c(0.01, 0.02, 0.03, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)*log10(10000)
  colors <- rainbow(11) 
  colors[3] <- colors[4]
  colors[4] <- colors[11]
  linetype <- c(1:10) 
  plotchar <- seq(18,18+10,1)

  ecdfMaxSucess <- 0
  resultsDES <- list()
  resultsRB_IPOP_CMA_ES <- list()
  resultsCmaes_pure <- list()
  resultsDESv2 <- list()
  resultsjso <- list()
  resultslshadeSpacma <- list()
  resultsCmaesNos <- list()
  for(p in 1:30){
    resultsDES[[p]] <- read.table(file = paste(des_path,"DES_",p,"_",N,".txt",sep=""),sep = ",")
    resultsRB_IPOP_CMA_ES[[p]] <- read.table(file = paste(rb_ipop_cma_es_path,"RB-IPOP-CMA-ES_",p,"_",N,".txt",sep=""),sep = " ")
    resultsCmaes_pure[[p]] <- read.table(file = paste(cmaes_pure,"CMAES_",p,"_",N,".txt",sep=""),sep = ",",header = TRUE)
    resultsDESv2[[p]] <- read.table(file = paste(des_pathv2,"DES_",p,"_",N,".txt",sep=""),sep = ",",header = TRUE)
    resultsjso[[p]] <- read.table(file = paste(jsopath,"jSO_",p,"_",N,".txt",sep=""))
    resultslshadeSpacma[[p]] <- read.table(file = paste(lshadeSpacma_path,"LSHADE_SPACMA_",p,"_",N,".txt",sep=""))
    resultsCmaesNos[[p]] <- read.table(file = paste(cmaesNos_path,"CMAES_",p,"_",N,".txt",sep=""),sep = ",",header = TRUE)
    
    ecdfValues[[p]] <- rev(c(1 %o% (10)^(0.2*((log10(max(min(
                                                        min(resultsDES[[p]][14,]),
                                                        min(resultsRB_IPOP_CMA_ES[[p]][14,]),
                                                        min(resultsCmaes_pure[[p]][14,]),
                                                        min(resultsDESv2[[p]][14,]),
                                                        min(resultsjso[[p]][14,]),
                                                        min(resultslshadeSpacma[[p]][14,]),
                                                        min(resultsCmaesNos[[p]][14,])
                                                            ),10^-8)  )/0.2):(log10(max(
                                                                 max(resultsDES[[p]][1,]),
                                                                 max(resultsRB_IPOP_CMA_ES[[p]][1,]),
                                                                 max(resultsCmaes_pure[[p]][1,]),
                                                                 max(resultsDESv2[[p]][1,]),
                                                                 max(resultsjso[[p]][1,]),
                                                                 max(resultslshadeSpacma[[p]][1,]),
                                                                 max(resultsCmaesNos[[p]][1,])
                                                                 )  )/0.2) ))))
      }
  
  minCountDES <- rep(0,length(budgetSteps))
  minCountRB_IPOP_CMA_ES <- rep(0,length(budgetSteps))
  minCountCmaes_pure <- rep(0,length(budgetSteps))
  minCountDESv2 <- rep(0,length(budgetSteps))
  minCountjso <- rep(0,length(budgetSteps))
  minCountshadeSpacma <- rep(0,length(budgetSteps))
  minCountCmaes_nos <- rep(0,length(budgetSteps))
  for(p in F_from:F_to){
    print(paste("Calculating for function: ",p))
    for(b in 1:length(budgetSteps)){
      for(e in 1:length(ecdfValues[[p]])){
        minCountDES[b] <- minCountDES[b] + sum(resultsDES[[p]][b,]<ecdfValues[[p]][e])
        minCountRB_IPOP_CMA_ES[b] <- minCountRB_IPOP_CMA_ES[b] + sum(resultsRB_IPOP_CMA_ES[[p]][b,]<ecdfValues[[p]][e])
        minCountCmaes_pure[b] <- minCountCmaes_pure[b] + sum(resultsCmaes_pure[[p]][b,]<ecdfValues[[p]][e])
        minCountDESv2[b] <- minCountDESv2[b] + sum(resultsDESv2[[p]][b,]<ecdfValues[[p]][e])
        minCountjso[b] <- minCountjso[b] + sum(resultsjso[[p]][b,]<ecdfValues[[p]][e])
        minCountshadeSpacma[b] <- minCountshadeSpacma[b] + sum(resultslshadeSpacma[[p]][b,]<ecdfValues[[p]][e])
        minCountCmaes_nos[b] <- minCountCmaes_nos[b] + sum(resultsCmaesNos[[p]][b,]<ecdfValues[[p]][e])
        
      }
    }
    ecdfMaxSucess <- ecdfMaxSucess + length(ecdfValues[[p]])*51
  }
  
  setEPS()
  postscript( paste("Problems",F_from,"-",F_to,",N=",N,".eps",sep=""), width = 8, height = 8)
  
  plot(budgetSteps,minCountDES/(ecdfMaxSucess),xlab="log10 of (f-evals / dimension)",ylab="Proportion of function + target pairs",ylim=c(0, 1),type="b", lwd=2,lty=linetype[1], col=colors[1], pch=plotchar[1])
  lines(budgetSteps,minCountRB_IPOP_CMA_ES/(ecdfMaxSucess),type="b", lwd=2,lty=linetype[2], col=colors[2], pch=plotchar[2])
  lines(budgetSteps,minCountCmaes_pure/(ecdfMaxSucess),type="b", lwd=2,lty=linetype[3], col=colors[3], pch=plotchar[3])
  lines(budgetSteps,minCountDESv2/(ecdfMaxSucess),type="b", lwd=2,lty=linetype[4], col=colors[4], pch=plotchar[4])
  lines(budgetSteps,minCountjso/(ecdfMaxSucess),type="b", lwd=2,lty=linetype[5], col=colors[5], pch=plotchar[5])
  lines(budgetSteps,minCountshadeSpacma/(ecdfMaxSucess),type="b", lwd=2,lty=linetype[6], col=colors[6], pch=plotchar[6])
  lines(budgetSteps,minCountCmaes_nos/(ecdfMaxSucess),type="b", lwd=2,lty=linetype[7], col=colors[7], pch=plotchar[7])
  
  legend(-0.08, 1.03, c('DESv1', 'RB_IPOP_CMA_ES', 'CMAES','DESv2','jSO','LSHADE_SPACMA','CMAES w/o Sigma'), cex=0.8, col=colors[1:7],pch=plotchar[1:7], lty=linetype[1:7] )
  
  dev.off()
  
}