
itemProb = function(response, sample, itemAttribute, examineeProfile, itemIntercepts, itemMainEffects){

  if (is.null(response)){
    prob = 1
  } else {
    logit = min(itemIntercepts + itemMainEffects * examineeProfile %*% itemAttribute, log(.Machine$double.xmax))
    prob = exp(logit) / (1 + exp(logit))
    if (response == 0) prob = 1 - prob
  }
  return(prob)
}
