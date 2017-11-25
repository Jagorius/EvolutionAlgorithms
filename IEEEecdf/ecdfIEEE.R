library(ggplot2)

ecdfIEEE<- function(){
  
  des_path = "C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/CEC2017/CMADE-BBComp5 HistSize=3sqrt(N) init(-80,80) FINALTEST/M/"
  rb_ipop_cma_es_path = "C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/IEEEecdf/Results for all papers/paper ID  E-17343/RB-IPOP-CMA-ES/"
  ecdfValues <- rev(c(1 %o% 10^(-8:5)))
  N <- 10
  budgetSteps <- c(0.01, 0.02, 0.03, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)*log10(10000)
  
  
  resultsDES <- list()
  resultsRB_IPOP_CMA_ES <- list()
  for(p in 1:30){
    resultsDES[[p]] <- read.table(file = paste(des_path,"DES_",p,"_",N,".txt",sep=""),sep = ",")
    resultsRB_IPOP_CMA_ES[[p]] <- read.table(file = paste(rb_ipop_cma_es_path,"RB-IPOP-CMA-ES_",p,"_",N,".txt",sep=""),sep = " ")
    
  }
  
  minCountDES <- rep(0,length(ecdfValues))
  minCountRB_IPOP_CMA_ES <- rep(0,length(ecdfValues))
  for(e in 1:length(ecdfValues)){
    for(f in e:e){
      for(r in 1:30){
        minCountDES[e] <- minCountDES[e] + sum(resultsDES[[r]][e,]<ecdfValues[f])
        minCountRB_IPOP_CMA_ES[e] <-  minCountRB_IPOP_CMA_ES[e] + sum(resultsRB_IPOP_CMA_ES[[r]][e,]<ecdfValues[f])
      
      }
    }
  }

  budgetDES <- c()
  budgetRB_IPOP_CMA_ES <- c()
  for(m in 1:length(minCountDES)){
    budgetDES <- c(budgetDES,rep(budgetSteps[m],minCountDES[m]))
    budgetRB_IPOP_CMA_ES <- c(budgetRB_IPOP_CMA_ES,rep(budgetSteps[m],minCountRB_IPOP_CMA_ES[m]))
    
  }
  budgetDES <<- budgetDES
  DF <- data.frame(method=rep(c('DES', 'RB_IPOP_CMA_ES'), c(length(budgetDES),length(budgetRB_IPOP_CMA_ES))), value=c(budgetDES, budgetRB_IPOP_CMA_ES))
   
  ggplot(DF) + stat_ecdf(aes(value, color=method),geom = "line") + xlab("log10 of (f-evals / dimension)") +
    ylab("Proportion of function + target pairs") + theme_bw()#+ scale_x_continuous(trans='log2')
  
}