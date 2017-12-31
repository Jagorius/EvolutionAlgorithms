library(ggplot2)

ecdfIEEE<- function(n,f_from,f_to){
  N       <- n         
  F_from  <- f_from
  F_to    <- f_to
  
  print(paste("Dimension: ",n,sep=''))
  
  #des_path = "C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/CEC2017/CMADE-BBComp5 HistSize=3sqrt(N) init(-80,80) FINALTEST/M/"
  rb_ipop_cma_es_path = "C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/IEEEecdf/Results for all papers/paper ID  E-17343/RB-IPOP-CMA-ES/"
  cmaes_pure = "C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/CEC2017/CMAES/M/"
  des_pathv2 = "C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/DESv2/DES + dMean custom1 (DESv2.5)/M/"
  jsopath = "C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/IEEEecdf/Results for all papers/Paper ID   17315/jSO/"
  lshadeSpacma_path = "C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/IEEEecdf/Results for all papers/Paper ID 17051/LSHADE_SPACMA/"
  #cmaesNos_path = "C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/CEC2017/CMAES noSigma/M/"
  ebopath = "C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/IEEEecdf/Results for all papers/EBOwithCMAR/data for email/"
  idepath = "C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/IEEEecdf/Results for all papers/Paper ID  E-17322/IDEbestNsize/"
  ppsopath = "C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/IEEEecdf/Results for all papers/paper ID E-17447/cec2017PPSO/"
  mmoedpath = "C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/IEEEecdf/Results for all papers/Paper ID  17321/Multi-method based Orthogonal Experimental Design Algorithm for Solving CEC2017 Competition Problems results/"
  dyypopath = "C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/IEEEecdf/Results for all papers/Paper ID 17543/DYYPO/"
  tlboflpath = "C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/IEEEecdf/Results for all papers/Paper ID 17544/TLBO-FL/"

  ecdfValues <- list()
  budgetSteps <- c(0.01, 0.02, 0.03, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)*log10(10000)
  #colors <- rainbow(16) 
  #colors[3] <- colors[16]
  colors <- c(rep("black",3),rep("grey",10))
  linetype <- c(c(1:6),c(1:6))
  plotchar <- seq(15,15+10,1)

  ecdfMaxSucess <- 0
  #resultsDES <- list()
  resultsRB_IPOP_CMA_ES <- list()
  resultsCmaes_pure <- list()
  resultsDESv2 <- list()
  resultsjso <- list()
  resultslshadeSpacma <- list()
  #resultsCmaesNos <- list()
  resultsEbo <- list()
  resultsIDE <- list()
  resultsPPSO <- list()
  resultsMMOED <- list()
  resultsDYYPO <- list()
  resultsTLBOFL <- list()
  
  for(p in 1:30){
    #resultsDES[[p]] <- read.table(file = paste(des_path,"DES_",p,"_",N,".txt",sep=""),sep = ",")
    resultsRB_IPOP_CMA_ES[[p]] <- read.table(file = paste(rb_ipop_cma_es_path,"RB-IPOP-CMA-ES_",p,"_",N,".txt",sep=""),sep = " ")
    resultsCmaes_pure[[p]] <- read.table(file = paste(cmaes_pure,"CMAES_",p,"_",N,".txt",sep=""),sep = ",",header = TRUE)
    resultsDESv2[[p]] <- read.table(file = paste(des_pathv2,"DES_",p,"_",N,".txt",sep=""),sep = ",",header = TRUE)
    resultsjso[[p]] <- read.table(file = paste(jsopath,"jSO_",p,"_",N,".txt",sep=""))
    resultslshadeSpacma[[p]] <- read.table(file = paste(lshadeSpacma_path,"LSHADE_SPACMA_",p,"_",N,".txt",sep=""))
    #resultsCmaesNos[[p]] <- read.table(file = paste(cmaesNos_path,"CMAES_",p,"_",N,".txt",sep=""),sep = ",",header = TRUE)
    resultsEbo[[p]] <- read.table(file = paste(ebopath,"EBOwithCMAR_",p,"_",N,".dat",sep=""))
    resultsIDE[[p]] <- read.table(file = paste(idepath,"IDEbestNsize_",N,"_",p,".txt",sep=""))
    resultsPPSO[[p]] <- read.table(file = paste(ppsopath,"PPSO_",p,"_",N,".txt",sep=""))
    resultsMMOED[[p]] <- read.table(file = paste(mmoedpath,"MM_OED_",p,"_",N,".txt",sep=""))
    resultsDYYPO[[p]] <- read.table(file = paste(dyypopath,"DYYPO_",p,"_",N,".txt",sep=""))
    resultsTLBOFL[[p]] <- read.table(file = paste(tlboflpath,"TLBO-FL_",p,"_",N,".txt",sep=""))
    
    ecdfValues[[p]] <- rev(c(1 %o% (10)^(0.2*((log10(max(min(
                                                        #min(resultsDES[[p]][14,]),
                                                        min(resultsRB_IPOP_CMA_ES[[p]][14,]),
                                                        min(resultsCmaes_pure[[p]][14,]),
                                                        min(resultsDESv2[[p]][14,]),
                                                        min(resultsjso[[p]][14,]),
                                                        min(resultslshadeSpacma[[p]][14,]),
                                                        #min(resultsCmaesNos[[p]][14,]),
                                                        min(resultsEbo[[p]][14,]),
                                                        min(resultsIDE[[p]][14,]),
                                                        min(resultsPPSO[[p]][14,]),
                                                        min(resultsMMOED[[p]][14,]),
                                                        min(resultsDYYPO[[p]][14,]),
                                                        min(resultsTLBOFL[[p]][14,])
                                                            ),10^-8)  )/0.2):(log10(max(
                                                                 #max(resultsDES[[p]][1,]),
                                                                 max(resultsRB_IPOP_CMA_ES[[p]][1,]),
                                                                 max(resultsCmaes_pure[[p]][1,]),
                                                                 max(resultsDESv2[[p]][1,]),
                                                                 max(resultsjso[[p]][1,]),
                                                                 max(resultslshadeSpacma[[p]][1,]),
                                                                 #max(resultsCmaesNos[[p]][1,]),
                                                                 max(resultsEbo[[p]][1,]),
                                                                 max(resultsIDE[[p]][1,]),
                                                                 max(resultsPPSO[[p]][1,]),
                                                                 max(resultsMMOED[[p]][1,]),
                                                                 max(resultsDYYPO[[p]][1,]),
                                                                 max(resultsTLBOFL[[p]][1,])
                                                                 )  )/0.2) ))))
      }
  
  #minCountDES <- rep(0,length(budgetSteps))
  minCountRB_IPOP_CMA_ES <- rep(0,length(budgetSteps))
  minCountCmaes_pure <- rep(0,length(budgetSteps))
  minCountDESv2 <- rep(0,length(budgetSteps))
  minCountjso <- rep(0,length(budgetSteps))
  minCountshadeSpacma <- rep(0,length(budgetSteps))
  #minCountCmaes_nos <- rep(0,length(budgetSteps))
  minCountEbo <- rep(0,length(budgetSteps))
  minCountIde <- rep(0,length(budgetSteps))
  minCountPpso <- rep(0,length(budgetSteps))
  minCountMmoed <- rep(0,length(budgetSteps))
  minCountDyypo <- rep(0,length(budgetSteps))
  minCountTlbofl <- rep(0,length(budgetSteps))
  
  for(p in F_from:F_to){
    print(paste("Calculating for function: ",p))
    for(b in 1:length(budgetSteps)){
      for(e in 1:length(ecdfValues[[p]])){
        #minCountDES[b] <- minCountDES[b] + sum(resultsDES[[p]][b,]<ecdfValues[[p]][e])
        minCountRB_IPOP_CMA_ES[b] <- minCountRB_IPOP_CMA_ES[b] + sum(resultsRB_IPOP_CMA_ES[[p]][b,]<ecdfValues[[p]][e])
        minCountCmaes_pure[b] <- minCountCmaes_pure[b] + sum(resultsCmaes_pure[[p]][b,]<ecdfValues[[p]][e])
        minCountDESv2[b] <- minCountDESv2[b] + sum(resultsDESv2[[p]][b,]<ecdfValues[[p]][e])
        minCountjso[b] <- minCountjso[b] + sum(resultsjso[[p]][b,]<ecdfValues[[p]][e])
        minCountshadeSpacma[b] <- minCountshadeSpacma[b] + sum(resultslshadeSpacma[[p]][b,]<ecdfValues[[p]][e])
        #minCountCmaes_nos[b] <- minCountCmaes_nos[b] + sum(resultsCmaesNos[[p]][b,]<ecdfValues[[p]][e])
        minCountEbo[b] <- minCountEbo[b] + sum(resultsEbo[[p]][b,]<ecdfValues[[p]][e])
        minCountIde[b] <- minCountIde[b] + sum(resultsIDE[[p]][b,]<ecdfValues[[p]][e])
        minCountPpso[b] <- minCountPpso[b] + sum(resultsPPSO[[p]][b,]<ecdfValues[[p]][e])
        minCountMmoed[b] <- minCountMmoed[b] + sum(resultsMMOED[[p]][b,]<ecdfValues[[p]][e])
        minCountDyypo[b] <- minCountDyypo[b] + sum(resultsDYYPO[[p]][b,]<ecdfValues[[p]][e])
        minCountTlbofl[b] <- minCountTlbofl[b] + sum(resultsTLBOFL[[p]][b,]<ecdfValues[[p]][e])
        
      }
    }
    ecdfMaxSucess <- ecdfMaxSucess + length(ecdfValues[[p]])*51
  }
  
  isXaxt <- if(N==100) "s" else "n"
  isYaxt <- if(F_from==1) "s" else "n"
  #setEPS()
  #postscript( paste("Problems",F_from,"-",F_to,",N=",N,".eps",sep=""), width = 5, height = 5)
                                              #xlab="log10 of (f-evals / dimension)",ylab="Proportion of function + target pairs",
  plot(budgetSteps,minCountjso/(ecdfMaxSucess),xlab="",ylab="",ylim=c(0, 1),type="b", lwd=2,lty=linetype[4], col=colors[4], pch=plotchar[4], xaxt=isXaxt,  yaxt=isYaxt)
  lines(budgetSteps,minCountshadeSpacma/(ecdfMaxSucess),type="b", lwd=2,lty=linetype[5], col=colors[5], pch=plotchar[5])
  lines(budgetSteps,minCountEbo/(ecdfMaxSucess),type="b", lwd=2,lty=linetype[6], col=colors[6], pch=plotchar[6])
  lines(budgetSteps,minCountIde/(ecdfMaxSucess),type="b", lwd=2,lty=linetype[7], col=colors[7], pch=plotchar[7])
  lines(budgetSteps,minCountPpso/(ecdfMaxSucess),type="b", lwd=2,lty=linetype[8], col=colors[8], pch=plotchar[8])
  lines(budgetSteps,minCountMmoed/(ecdfMaxSucess),type="b", lwd=2,lty=linetype[9], col=colors[9], pch=plotchar[9])
  lines(budgetSteps,minCountDyypo/(ecdfMaxSucess),type="b", lwd=2,lty=linetype[10], col=colors[10], pch=plotchar[10])
  lines(budgetSteps,minCountTlbofl/(ecdfMaxSucess),type="b", lwd=2,lty=linetype[11], col=colors[11], pch=plotchar[11])
  
  lines(budgetSteps,minCountRB_IPOP_CMA_ES/(ecdfMaxSucess),type="b", lwd=2,lty=linetype[3], col=colors[3], pch=plotchar[3])
  lines(budgetSteps,minCountCmaes_pure/(ecdfMaxSucess),type="b", lwd=2,lty=linetype[2], col=colors[2], pch=plotchar[2])
  lines(budgetSteps,minCountDESv2/(ecdfMaxSucess),type="b", lwd=2,lty=linetype[1], col=colors[1], pch=plotchar[1])
  
  #legend(-0.08, 1.03, c('DESv2', 'CMAES','RB_IPOP_CMA_ES','jSO','LSHADE_SPACMA','EBOwithCMAR','IDEbestNsize','PPSO','MM_OED','DYYPO','TLBO-FL'), text.font=2, cex=0.8, col=colors[1:11],pch=plotchar[1:11], lty=linetype[1:11] )
  
  #dev.off()
  #return(rplot)
}

combinedPlot <- function(){
  #colors <- rainbow(16) 
  #colors[3] <- colors[16]
  colors <- c(rep("black",3),rep("grey",10))
  linetype <- c(c(1:6),c(1:6))
  plotchar <- seq(15,15+10,1)
  
  setEPS()
  postscript( "combinedPlot.eps", width = 20, height = 18)
  layout(mat = matrix(1:16,nrow = 4,ncol = 4,byrow = TRUE),heights = c(1,1,1,1,0.2))
  par(oma=c(10, 6, 0, 6), mar=4*c(.1,.1,.1,.1), cex=1, las=1)
  ecdfIEEE(10,1,3)
  ecdfIEEE(10,4,10)
  ecdfIEEE(10,11,20)
  ecdfIEEE(10,21,30)

  ecdfIEEE(30,1,3)
  ecdfIEEE(30,4,10)
  ecdfIEEE(30,11,20)
  ecdfIEEE(30,21,30)

  ecdfIEEE(50,1,3)
  ecdfIEEE(50,4,10)
  ecdfIEEE(50,11,20)
  ecdfIEEE(50,21,30)

  ecdfIEEE(100,1,3)
  ecdfIEEE(100,4,10)
  ecdfIEEE(100,11,20)
  ecdfIEEE(100,21,30)
  mtext("log10 of (f-evals / dimension)", 1, 0, outer=TRUE,padj=3,cex=2)
  mtext("Proportion of function + target pairs", 2, 3, outer=TRUE, las=0,cex=2)
  
  par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
  plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
  legend("bottom", c('DESv2', 'CMAES','RB_IPOP_CMA_ES','jSO','LSHADE_SPACMA','EBOwithCMAR','IDEbestNsize','PPSO','MM_OED','DYYPO','TLBO-FL'), text.font=2, cex=1.5, col=colors[1:11],pch=plotchar[1:11], lty=linetype[1:11], ncol=6)
  
  dev.off()
}

if(FALSE){
ecdfIEEE(10,1,3)
ecdfIEEE(10,4,10)
ecdfIEEE(10,11,20)
ecdfIEEE(10,21,30)
ecdfIEEE(10,1,30)

ecdfIEEE(30,1,3)
ecdfIEEE(30,4,10)
ecdfIEEE(30,11,20)
ecdfIEEE(30,21,30)
ecdfIEEE(30,1,30)

ecdfIEEE(50,1,3)
ecdfIEEE(50,4,10)
ecdfIEEE(50,11,20)
ecdfIEEE(50,21,30)
ecdfIEEE(50,1,30)

ecdfIEEE(100,1,3)
ecdfIEEE(100,4,10)
ecdfIEEE(100,11,20)
ecdfIEEE(100,21,30)
ecdfIEEE(100,1,30)
}