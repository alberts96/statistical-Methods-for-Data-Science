myspecificity <- function(pred,obs, positive = NULL){
  FP = 0;
  TP = 0;
  FN = 0;
  TN = 0;
  for (i in c(1:length(pred))){
    if (pred[i] == positive){
      if (obs[i] == positive){
        TP = TP +1;
      }
      else{
        FP =FP+1;
      }
    }
    else{
      if (obs[i] == positive){
        FN = FN +1;
      }
      else{
        TN =TN+1;
      }
    }
    
  }
  return( TN / (TN + FP))
}





spec <- function (data,
                        lev = NULL,
                        model = NULL) {
  out <- myspecificity(data$obs - data$pred,
             positive="active")
  names(out) <- "SPEC"
  out
}




}
