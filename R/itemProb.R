
itemProb = function(response, itemNumber, sample, itemAttribute, examineeProfile, itemIntercepts, itemMainEffects){

  if (is.null(response)){
    prob = 1
  } else {
    logit = min(itemIntercepts[paste0("intercept", itemNumber)] +
      itemMainEffects[paste0("lambda", itemNumber)] * examineeProfile %*% itemAttribute, log(.Machine$double.xmax))

    prob = exp(logit) / (1 + exp(logit))
    if (response == 0) prob = 1 - prob
  }
  return(prob)
}
