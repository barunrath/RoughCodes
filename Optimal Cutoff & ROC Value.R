#Function to give optimal cut_off & plot the ROC Curve
O_cutoff<-function(X,Tr){
  
  for(i in 1:ncol(Tr)){
    if (Tr[,i]==0||Tr[,i]==1){
      Tr1<-Tr[,-i]
      Tr2<-Tr[,i]
    }
  }
  Cut_off<-seq(.01,.99,length=1000)
  Result<-NULL
  Accuracy<-NULL
  Sensitivity<-NULL
  Specificity<-NULL
  Fall_out<-NULL
  for(i in 1:1000){
    predicted<-predict(X,Tr1,type="response")
    predicted<-ifelse(predicted>Cut_off[i],1,0)
    
    confusion<-table(Tr2,predicted)#confusion matrix
    Accuracy<-c(Accuracy,sum(diag(confusion))/sum(confusion))
    #True Positive Rate(tpr),Recall,Probability of Detection
    Sensitivity<-c(Sensitivity,sum(confusion[1,1])/sum(confusion[1,]))
    Specificity<-c(Specificity,sum(confusion[2,2])/sum(confusion[2,]))#True Negative Rate
    #FalsePositiveRate(FPR),Probability of false alarm,(1-specificity)
    Fall_out<-c(Fall_out,sum(confusion[2,1])/sum(confusion[2,]))
  }
  Result<-data.frame(cbind(Cut_off,Accuracy,Sensitivity,
                           Specificity,Fall_out))
  
  plot(Result$Fall_out,Result$Sensitivity,col=c("red","green"))
  abline(a=0,b=1)
  
  optimal_cutOff<-Result$Cut_off[which.max(Result$Accuracy)]
  View(Result)
  return(optimal_cutOff)
}

O_cutoff(logit.fit1,My_Adult_IncomeTest)
