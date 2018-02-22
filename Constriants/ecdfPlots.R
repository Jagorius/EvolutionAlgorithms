library(ggplot2)

ecdfIEEE<- function(n,f_from,f_to){
  N       <- n         
  F_from  <- f_from
  F_to    <- f_to
  
  DATA_VERS_NICE_NAMES=c("Reinitialization", "Lamarckian projection" , "Darwinian projection", "Lamarckian reflection", "Darwinian reflection", "Lamarckian wrapping", 
                         "Darwinian wrapping", "Scaled mutant", "Death penalty", 
                         "Quadratic penalty", "Substitution penalty", "Resampling",
                         "Rand base", "Midpoint base", "Midpoint target", "Scaled to base", "Conservatism")
  
  print(paste("Dimension: ",n,sep=''))
  
  des_throw_path = "C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/Constriants/DES/Lamarckian projection/M/"
  des_bback_path = "C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/Constriants/DES/Lamarckian reflection/M/"
  des_wrapp_path = "C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/Constriants/DES/Lamarckian wrapping/M/"
  des_draw_path =  "C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/Constriants/DES/Reinitialization/M/"
  des_ddes_path =  "C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/Constriants/DES/Substitution penalty/M/"
  des_smut_path =  "C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/Constriants/DES/Scaled mutant/M/"
  des_midt_path =  "C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/Constriants/DES/Midpoint target/M/"
  des_smut2_path =  "C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/Constriants/DES/Scaled to base/M/"
  des_rbase_path =  "C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/Constriants/DES/Rand base/M/"
  des_mbase_path =  "C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/Constriants/DES/Midpoint base/M/"
  des_dthrow_path =  "C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/Constriants/DES/Darwinian projection/M/"
  des_dbback_path =  "C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/Constriants/DES/Darwinian reflection/M/"
  des_dwrapp_path =  "C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/Constriants/DES/Darwinian wrapping/M/"
  des_qpenalty_path =  "C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/Constriants/DES/Quadratic penalty/M/"
  des_resampl_path =  "C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/Constriants/DES/Resampling/M/"
  des_conserv_path =  "C:/Users/JS/Desktop/Doktorat/EvolutionAlgorithms/Constriants/DES/Conservatism/M/"
  
  
  ecdfValues <- list()
  budgetSteps <- seq(0.01,1,by=0.01)*log10(10000)
  
  colors <- c("black", "black", "grey", "black", "grey", "black", "grey", rep("black",11))
  plotchar <- c( 4, 0, 15, 1, 16, 2, 17, 3, 5:14)

  ecdfMaxSucess <- 0
  resultsDES_THROW <- list()
  resultsDES_BBACK <- list()
  resultsDES_WRAPP <- list()
  resultsDES_DRAW <- list()
  resultsDES_DDES <- list()
  resultsDES_SMUT <- list()
  resultsDES_MIDT <- list()
  resultsDES_SMUT2 <- list()
  resultsDES_RBASE <- list()
  resultsDES_MBASE <- list()
  resultsDES_DTHROW <- list()
  resultsDES_DBBACK <- list()
  resultsDES_DWRAPP <- list()
  resultsDES_QPENAL <- list()
  resultsDES_RESAMPL <- list()
  resultsDES_CONSERV <- list()
  
  for(p in 1:30){
    resultsDES_THROW[[p]] <- read.table(file = paste( des_throw_path,"DES_",p,"_",N,".txt",sep=""),sep = ",")
    resultsDES_BBACK[[p]] <- read.table(file = paste( des_bback_path,"DES_",p,"_",N,".txt",sep=""),sep = ",")
    resultsDES_WRAPP[[p]] <- read.table(file = paste( des_wrapp_path,"DES_",p,"_",N,".txt",sep=""),sep = ",")
    resultsDES_DRAW[[p]] <- read.table(file = paste( des_draw_path,"DES_",p,"_",N,".txt",sep=""),sep = ",")
    resultsDES_DDES[[p]] <- read.table(file = paste( des_ddes_path,"DES_",p,"_",N,".txt",sep=""),sep = ",")
    resultsDES_SMUT[[p]] <- read.table(file = paste( des_smut_path,"DES_",p,"_",N,".txt",sep=""),sep = ",")
    resultsDES_MIDT[[p]] <- read.table(file = paste( des_midt_path,"DES_",p,"_",N,".txt",sep=""),sep = ",")
    resultsDES_SMUT2[[p]] <- read.table(file = paste( des_smut2_path,"DES_",p,"_",N,".txt",sep=""),sep = ",")
    resultsDES_RBASE[[p]] <- read.table(file = paste( des_rbase_path,"DES_",p,"_",N,".txt",sep=""),sep = ",")
    resultsDES_MBASE[[p]] <- read.table(file = paste( des_mbase_path,"DES_",p,"_",N,".txt",sep=""),sep = ",")
    resultsDES_DTHROW[[p]] <- read.table(file = paste( des_dthrow_path,"DES_",p,"_",N,".txt",sep=""),sep = ",")
    resultsDES_DBBACK[[p]] <- read.table(file = paste( des_dbback_path,"DES_",p,"_",N,".txt",sep=""),sep = ",")
    resultsDES_DWRAPP[[p]] <- read.table(file = paste( des_dwrapp_path,"DES_",p,"_",N,".txt",sep=""),sep = ",")
    resultsDES_QPENAL[[p]] <- read.table(file = paste( des_qpenalty_path,"DES_",p,"_",N,".txt",sep=""),sep = ",")
    resultsDES_RESAMPL[[p]] <- read.table(file = paste( des_resampl_path,"DES_",p,"_",N,".txt",sep=""),sep = ",")
    resultsDES_CONSERV[[p]] <- read.table(file = paste( des_conserv_path,"DES_",p,"_",N,".txt",sep=""),sep = ",")
    
    ecdfValues[[p]] <- rev(c(1 %o% (10)^(0.2*((log10(max(min(
                                                        min(resultsDES_THROW[[p]][100,]),
                                                        min(resultsDES_BBACK[[p]][100,]),
                                                        min(resultsDES_WRAPP[[p]][100,]),
                                                        min(resultsDES_DRAW[[p]][100,]),
                                                        min(resultsDES_DDES[[p]][100,]),
                                                        min(resultsDES_SMUT[[p]][100,]),
                                                        min(resultsDES_MIDT[[p]][100,]),
                                                        min(resultsDES_SMUT2[[p]][100,]),
                                                        min(resultsDES_RBASE[[p]][100,]),
                                                        min(resultsDES_MBASE[[p]][100,]),
                                                        min(resultsDES_DTHROW[[p]][100,]),
                                                        min(resultsDES_DBBACK[[p]][100,]),
                                                        min(resultsDES_DWRAPP[[p]][100,]),
                                                        min(resultsDES_QPENAL[[p]][100,]),
                                                        min(resultsDES_RESAMPL[[p]][100,]),
                                                        min(resultsDES_CONSERV[[p]][100,])
                                                        
                                                            ),10^-8)  )/0.2):(log10(median(
                                                                 max(resultsDES_THROW[[p]][1,]),
                                                                 max(resultsDES_BBACK[[p]][1,]),
                                                                 max(resultsDES_WRAPP[[p]][1,]),
                                                                 max(resultsDES_DRAW[[p]][1,]),
                                                                 max(resultsDES_DDES[[p]][1,]),
                                                                 max(resultsDES_SMUT[[p]][1,]),
                                                                 max(resultsDES_MIDT[[p]][1,]),
                                                                 max(resultsDES_SMUT2[[p]][1,]),
                                                                 max(resultsDES_RBASE[[p]][1,]),
                                                                 max(resultsDES_MBASE[[p]][1,]),
                                                                 max(resultsDES_DTHROW[[p]][1,]),
                                                                 max(resultsDES_DBBACK[[p]][1,]),
                                                                 max(resultsDES_DWRAPP[[p]][1,]),
                                                                 max(resultsDES_QPENAL[[p]][1,]),
                                                                 max(resultsDES_RESAMPL[[p]][1,]),
                                                                 max(resultsDES_CONSERV[[p]][1,])
                                                                 
                                                                 )  )/0.2) ))))
      }
  
  minCountDES_THROW <- rep(0,length(budgetSteps))
  minCountDES_BBACK <- rep(0,length(budgetSteps))
  minCountDES_WRAPP <- rep(0,length(budgetSteps))
  minCountDES_DRAW <- rep(0,length(budgetSteps))
  minCountDES_DDES <- rep(0,length(budgetSteps))
  minCountDES_SMUT <- rep(0,length(budgetSteps))
  minCountDES_MIDT <- rep(0,length(budgetSteps))
  minCountDES_SMUT2 <- rep(0,length(budgetSteps))
  minCountDES_RBASE <- rep(0,length(budgetSteps))
  minCountDES_MBASE <- rep(0,length(budgetSteps))
  minCountDES_DTHROW <- rep(0,length(budgetSteps))
  minCountDES_DBACK <- rep(0,length(budgetSteps))
  minCountDES_DWRAPP <- rep(0,length(budgetSteps))
  minCountDES_QPENAL <- rep(0,length(budgetSteps))
  minCountDES_RESAMPL <- rep(0,length(budgetSteps))
  minCountDES_CONSERV <- rep(0,length(budgetSteps))
  
  for(p in F_from:F_to){
    print(paste("Calculating for function: ",p))
    for(b in 1:length(budgetSteps)){
      for(e in 1:length(ecdfValues[[p]])){
        minCountDES_THROW[b] <- minCountDES_THROW[b] + sum(resultsDES_THROW[[p]][b,]<ecdfValues[[p]][e])
        minCountDES_BBACK[b] <- minCountDES_BBACK[b] + sum(resultsDES_BBACK[[p]][b,]<ecdfValues[[p]][e])
        minCountDES_WRAPP[b] <- minCountDES_WRAPP[b] + sum(resultsDES_WRAPP[[p]][b,]<ecdfValues[[p]][e])
        minCountDES_DRAW[b] <- minCountDES_DRAW[b] + sum(resultsDES_DRAW[[p]][b,]<ecdfValues[[p]][e])
        minCountDES_DDES[b] <- minCountDES_DDES[b] + sum(resultsDES_DDES[[p]][b,]<ecdfValues[[p]][e])
        minCountDES_SMUT[b] <- minCountDES_SMUT[b] + sum(resultsDES_SMUT[[p]][b,]<ecdfValues[[p]][e])
        minCountDES_MIDT[b] <- minCountDES_MIDT[b] + sum(resultsDES_MIDT[[p]][b,]<ecdfValues[[p]][e])
        minCountDES_SMUT2[b] <- minCountDES_SMUT2[b] + sum(resultsDES_SMUT2[[p]][b,]<ecdfValues[[p]][e])
        minCountDES_RBASE[b] <- minCountDES_RBASE[b] + sum(resultsDES_RBASE[[p]][b,]<ecdfValues[[p]][e])
        minCountDES_MBASE[b] <- minCountDES_MBASE[b] + sum(resultsDES_MBASE[[p]][b,]<ecdfValues[[p]][e])
        minCountDES_DTHROW[b] <- minCountDES_DTHROW[b] + sum(resultsDES_DTHROW[[p]][b,]<ecdfValues[[p]][e])
        minCountDES_DBACK[b] <- minCountDES_DBACK[b] + sum(resultsDES_DBBACK[[p]][b,]<ecdfValues[[p]][e])
        minCountDES_DWRAPP[b] <- minCountDES_DWRAPP[b] + sum(resultsDES_DWRAPP[[p]][b,]<ecdfValues[[p]][e])
        minCountDES_QPENAL[b] <- minCountDES_QPENAL[b] + sum(resultsDES_QPENAL[[p]][b,]<ecdfValues[[p]][e])
        minCountDES_RESAMPL[b] <- minCountDES_RESAMPL[b] + sum(resultsDES_RESAMPL[[p]][b,]<ecdfValues[[p]][e])
        minCountDES_CONSERV[b] <- minCountDES_CONSERV[b] + sum(resultsDES_CONSERV[[p]][b,]<ecdfValues[[p]][e])
        
      }
    }
    ecdfMaxSucess <- ecdfMaxSucess + length(ecdfValues[[p]])*51
  }
  isXaxt <- "s"
  isYaxt <- "s"
  #isXaxt <- if(N==100) "s" else "n"
  #isYaxt <- if(F_from==1) "s" else "n"
  #setEPS()
  #postscript( paste("Problems",F_from,"-",F_to,",N=",N,".eps",sep=""), width = 8, height = 8)
  pdf(paste("Problems",F_from,"-",F_to,",N=",N,".pdf",sep=""), width = 8, height = 8) 
                                             #xlab="log10 of (f-evals / dimension)",ylab="Proportion of function + target pairs",
  rplot <- plot(budgetSteps,minCountDES_THROW/(ecdfMaxSucess),xlab="log10 of (f-evals / dimension)",ylab="Proportion of function + target pairs",ylim=c(0, 1),type="l", lwd=2,lty=linetype[1], col=colors[1], pch=plotchar[1], xaxt=isXaxt,  yaxt=isYaxt)
  points(budgetSteps[seq(1,length(budgetSteps),by=10)],(minCountDES_THROW/(ecdfMaxSucess))[seq(1,length(budgetSteps),by=10)],col=colors[1], pch=plotchar[1])
  
  lines(budgetSteps,minCountDES_BBACK/(ecdfMaxSucess),type="l", lwd=2,lty=linetype[2], col=colors[2], pch=plotchar[2])
  points(budgetSteps[seq(2,length(budgetSteps),by=10)],(minCountDES_BBACK/(ecdfMaxSucess))[seq(2,length(budgetSteps),by=10)],col=colors[2], pch=plotchar[2])
 
  lines(budgetSteps,minCountDES_WRAPP/(ecdfMaxSucess),type="l", lwd=2,lty=linetype[3], col=colors[3], pch=plotchar[3])
  points(budgetSteps[seq(3,length(budgetSteps),by=10)],(minCountDES_WRAPP/(ecdfMaxSucess))[seq(3,length(budgetSteps),by=10)],col=colors[3], pch=plotchar[3])
 
  lines(budgetSteps,minCountDES_DRAW/(ecdfMaxSucess),type="l", lwd=2,lty=linetype[4], col=colors[4], pch=plotchar[4])
  points(budgetSteps[seq(4,length(budgetSteps),by=10)],(minCountDES_DRAW/(ecdfMaxSucess))[seq(4,length(budgetSteps),by=10)],col=colors[4], pch=plotchar[4])
   
  lines(budgetSteps,minCountDES_DDES/(ecdfMaxSucess),type="l", lwd=2,lty=linetype[5], col=colors[5], pch=plotchar[5])
  points(budgetSteps[seq(5,length(budgetSteps),by=10)],(minCountDES_DDES/(ecdfMaxSucess))[seq(5,length(budgetSteps),by=10)],col=colors[5], pch=plotchar[5])
  
  lines(budgetSteps,minCountDES_SMUT/(ecdfMaxSucess),type="l", lwd=2,lty=linetype[6], col=colors[6], pch=plotchar[6])
  points(budgetSteps[seq(6,length(budgetSteps),by=10)],(minCountDES_SMUT/(ecdfMaxSucess))[seq(6,length(budgetSteps),by=10)],col=colors[6], pch=plotchar[6])
  
  lines(budgetSteps,minCountDES_MIDT/(ecdfMaxSucess),type="l", lwd=2,lty=linetype[7], col=colors[7], pch=plotchar[7])
  points(budgetSteps[seq(7,length(budgetSteps),by=10)],(minCountDES_MIDT/(ecdfMaxSucess))[seq(7,length(budgetSteps),by=10)],col=colors[7], pch=plotchar[7])
  
  lines(budgetSteps,minCountDES_SMUT2/(ecdfMaxSucess),type="l", lwd=2,lty=linetype[8], col=colors[8], pch=plotchar[8])
  points(budgetSteps[seq(8,length(budgetSteps),by=10)],(minCountDES_SMUT2/(ecdfMaxSucess))[seq(8,length(budgetSteps),by=10)],col=colors[8], pch=plotchar[8])
  
  lines(budgetSteps,minCountDES_RBASE/(ecdfMaxSucess),type="l", lwd=2,lty=linetype[9], col=colors[9], pch=plotchar[9])
  points(budgetSteps[seq(9,length(budgetSteps),by=10)],(minCountDES_RBASE/(ecdfMaxSucess))[seq(9,length(budgetSteps),by=10)],col=colors[9], pch=plotchar[9])
  
  lines(budgetSteps,minCountDES_MBASE/(ecdfMaxSucess),type="l", lwd=2,lty=linetype[10], col=colors[10], pch=plotchar[10])
  points(budgetSteps[seq(10,length(budgetSteps),by=10)],(minCountDES_MBASE/(ecdfMaxSucess))[seq(10,length(budgetSteps),by=10)],col=colors[10], pch=plotchar[10])
  
  lines(budgetSteps,minCountDES_EXPS/(ecdfMaxSucess),type="l", lwd=2,lty=linetype[11], col=colors[11], pch=plotchar[11])
  points(budgetSteps[seq(11,length(budgetSteps),by=10)],(minCountDES_EXPS/(ecdfMaxSucess))[seq(11,length(budgetSteps),by=10)],col=colors[11], pch=plotchar[11])
  
  legend("topleft",DATA_VERS_NICE_NAMES, text.font=2, cex=1, col=colors[1:11],pch=plotchar[1:11], lty=linetype[1:11] )
  
  dev.off()
  return(rplot)
}

combinedPlots <- function(){
  ecdfIEEE(10,1,3)
  ecdfIEEE(10,4,30)
  ecdfIEEE(10,1,30)
  
  ecdfIEEE(30,1,3)
  ecdfIEEE(30,4,30)
  ecdfIEEE(30,1,30)
  
  ecdfIEEE(50,1,3)
  ecdfIEEE(50,4,30)
  ecdfIEEE(50,1,30)
  
  ecdfIEEE(100,1,3)
  ecdfIEEE(100,4,30)
  ecdfIEEE(100,1,30)
}



