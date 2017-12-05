library(ggplot2)

ecdfIEEE<- function(){
  
  des_path = "C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/CEC2017/CMADE-BBComp5 HistSize=3sqrt(N) init(-80,80) FINALTEST/M/"
  rb_ipop_cma_es_path = "C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/IEEEecdf/Results for all papers/paper ID  E-17343/RB-IPOP-CMA-ES/"
  cmaes_pure = "C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/CEC2017/CMAES/M/"
  #ecdfValues <- rev(c(1 %o% 10^(-8:5)))
  #ecdfValues <-  rev(c(1 %o% 10^(-1:4)))
  ecdfValues <- rev(c(1 %o% (10)^(0.2*(-40:20))))
  N <- 10         
  F_from <- 21
  F_to <- 30
  budgetSteps <- c(0.01, 0.02, 0.03, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)*log10(10000)
  colors <- rainbow(11) 
  colors[3] <- colors[5]
  linetype <- c(1:10) 
  plotchar <- seq(18,18+10,1)
  
  
  resultsDES <- list()
  resultsRB_IPOP_CMA_ES <- list()
  resultsCmaes_pure <- list()
  for(p in 1:30){
    resultsDES[[p]] <- read.table(file = paste(des_path,"DES_",p,"_",N,".txt",sep=""),sep = ",")
    resultsRB_IPOP_CMA_ES[[p]] <- read.table(file = paste(rb_ipop_cma_es_path,"RB-IPOP-CMA-ES_",p,"_",N,".txt",sep=""),sep = " ")
    resultsCmaes_pure[[p]] <- read.table(file = paste(cmaes_pure,"CMAES_",p,"_",N,".txt",sep=""),sep = ",",header = TRUE)
  }
  
  minCountDES <- rep(0,length(budgetSteps))
  minCountRB_IPOP_CMA_ES <- rep(0,length(budgetSteps))
  minCountCmaes_pure <- rep(0,length(budgetSteps))
  for(p in F_from:F_to){
    print(paste("Calculating for function: ",p))
    for(b in 1:length(budgetSteps)){
      for(e in 1:length(ecdfValues)){
        minCountDES[b] <- minCountDES[b] + sum(resultsDES[[p]][b,]<ecdfValues[e])
        minCountRB_IPOP_CMA_ES[b] <- minCountRB_IPOP_CMA_ES[b] + sum(resultsRB_IPOP_CMA_ES[[p]][b,]<ecdfValues[e])
        minCountCmaes_pure[b] <- minCountCmaes_pure[b] + sum(resultsCmaes_pure[[p]][b,]<ecdfValues[e])
  
      }
    }
  }
  
  setEPS()
  postscript( paste("Problems",F_from,"-",F_to,",N=",N,".eps",sep=""), width = 8, height = 8)
  
  plot(budgetSteps,minCountDES/(51*(F_to-F_from+1)*length(ecdfValues)),xlab="log10 of (f-evals / dimension)",ylab="Proportion of function + target pairs",ylim=c(0, 1),type="b", lwd=2,lty=linetype[1], col=colors[1], pch=plotchar[1])
  lines(budgetSteps,minCountRB_IPOP_CMA_ES/(51*(F_to-F_from+1)*length(ecdfValues)),type="b", lwd=2,lty=linetype[2], col=colors[2], pch=plotchar[2])
  lines(budgetSteps,minCountCmaes_pure/(51*(F_to-F_from+1)*length(ecdfValues)),type="b", lwd=2,lty=linetype[3], col=colors[3], pch=plotchar[3])
  
  legend(0, 1, c('DESv1', 'RB_IPOP_CMA_ES', 'CMAES'), cex=0.8, col=colors[1:3],pch=plotchar[1:3], lty=linetype[1:3], )
  
  dev.off()
  
  #for(e in 1:length(ecdfValues)){
  #  for(f in e:e){
  #    for(r in 1:30){
  #      minCountDES[e] <- minCountDES[e] + sum(resultsDES[[r]][e,]<ecdfValues[f])
  #      minCountRB_IPOP_CMA_ES[e] <-  minCountRB_IPOP_CMA_ES[e] + sum(resultsRB_IPOP_CMA_ES[[r]][e,]<ecdfValues[f])
  #      minCountCmaes_pure[e] <-  minCountCmaes_pure[e] + sum(resultsCmaes_pure[[r]][e,]<ecdfValues[f])
  #      
  #    }
  #  }
  #}
  
  #budgetDES <- c()
  #budgetRB_IPOP_CMA_ES <- c()
  #budgetCMAES_PURE <- c()
  #for(m in 1:length(minCountDES)){
  #  budgetDES <- c(budgetDES,rep(budgetSteps[m],minCountDES[m]))
  #  budgetRB_IPOP_CMA_ES <- c(budgetRB_IPOP_CMA_ES,rep(budgetSteps[m],minCountRB_IPOP_CMA_ES[m]))
  #  budgetCMAES_PURE <- c(budgetCMAES_PURE,rep(budgetSteps[m],minCountCmaes_pure[m]))
    
  #}
  #minCountDES <<- minCountDES
  #budgetDES <<- budgetDES
  #budgetRB_IPOP_CMA_ES  <<- -budgetRB_IPOP_CMA_ES

  
#  DF <- data.frame(method=rep(c('DESv1', 'RB_IPOP_CMA_ES', 'CMAES'), c(length(budgetDES),length(budgetRB_IPOP_CMA_ES),length(budgetCMAES_PURE))), value=c(budgetDES, budgetRB_IPOP_CMA_ES, budgetCMAES_PURE))
  #DF <- data.frame(method=rep(c('DES', 'RB_IPOP_CMA_ES'), c(length(budgetDES),length(budgetRB_IPOP_CMA_ES))), value=c(budgetDES, budgetRB_IPOP_CMA_ES))
  
#  ggplot(DF) + stat_ecdf(aes(value, color=method),geom = "line") + xlab("log10 of (f-evals / dimension)") +
 #   ylab("Proportion of function + target pairs") + theme_bw()# + scale_x_continuous(trans='log10')
  
  #ggsave(paste("Problems1-3,N=",N,".eps",sep=""), width = 10, height = 8, units = "in")
  
}