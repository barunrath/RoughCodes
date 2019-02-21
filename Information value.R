#simple binning function:
binner <- function(x) {
  cut(x, breaks=c(seq(0, 100, by = 10)), include.lowest=T)
}

#IV fuction: 

iv_score<-function(predit,target){
  
  if(class(target)%in% c("numeric", "integer")){
    target<- sapply(target, binner)
  }
  data<- data.frame(table(predit,target))
  Bins<-unique(data$target)
  
  
  NoEvent<-data$Freq[data$predit==0]
  Event<-data$Freq[data$predit==1]
  NoEvent_pct<-NoEvent/sum(NoEvent)
  Event_pct<-Event/sum(Event)
  
  #calculation of woe
  woe<-log(NoEvent_pct/Event_pct)
  plot(woe)
  iv<-(NoEvent_pct-Event_pct)*woe
  iv_value<-sum(iv)
  Combined_iv<-data.frame(Bins,NoEvent,Event,NoEvent_pct,Event_pct,woe,iv)
  print(Combined_iv)
  return(print(paste("The IV_Value is",iv_value)))
}

iv_score(M_inputDataTrain$ABOVE50K, M_inputDataTrain$HOURSPERWEEK)

