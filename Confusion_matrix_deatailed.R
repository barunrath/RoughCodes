O_cutoff_TOT<-function(X,Tr,Cut_off){
  
  for(i in 1:ncol(Tr)){
    if (Tr[,i]==0||Tr[,i]==1){
      Tr1<-Tr[,-i]
      Tr2<-Tr[,i]
    }
  }  
  predicted<-predict(X,Tr1,type="response")
  predicted<-ifelse(predicted>Cut_off,1,0)
  
  confusion<-table(Tr2,predicted)#confusion matrix
  Accuracy<-sum(diag(confusion))/sum(confusion)
  #True Positive Rate(tpr),Recall,Probability of Detection
  Sensitivity<-sum(confusion[1,1])/sum(confusion[1,])
  Specificity<-sum(confusion[2,2])/sum(confusion[2,])#True Negative Rate
  #FalsePositiveRate(FPR),Probability of false alarm,(1-specificity)
  Fall_out<-sum(confusion[2,1])/sum(confusion[2,])
  #falsenegativerate(FNR),(1-Sensitivity)
  missRate<-sum(confusion[1,2])/sum(confusion[1,])
  #positive liklihood Ratio(LR+)
  PLR<-Sensitivity/(1-Specificity)
  #Negative Likihood Ratio(LR-)
  NLR<-missRate/Specificity
  #Diagonastic Odd Rate(DOR)
  DOR<-PLR/NLR
  #Positive Predictive value(PPV)
  PPV<-sum(confusion[1,1])/sum(confusion[,1])
  #Negative Predictive Value(NPV)
  NPV<-sum(confusion[2,2])/sum(confusion[,2])
  #False Discovery Rate(FDR)
  FDR<-sum(confusion[2,1])/sum(confusion[,1])
  #False Ommision Rate(FOR)
  FOR<-sum(confusion[1,2])/sum(confusion[,2])
  
  Result<-data.frame(cbind(Cut_off,Accuracy,Sensitivity,
                           Specificity,Fall_out,missRate,PLR,NLR,DOR,PPV,NPV,FDR,FOR))
  return(Result)
}
#funcation to find Details from a confusion matrix
O_cutoff_TOT(Model,Testdata,Cut_off = )