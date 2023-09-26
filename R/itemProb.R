
itemProb = function(response, itemNumber, itemAttribute, examineeProfile, itemIntercepts, itemMainEffects){

  if (is.null(response)){
    prob = 1
  } else {
    logit = itemIntercepts[paste0("intercept[", itemNumber, "]")] +
      itemMainEffects[paste0("lambda[", itemNumber, "]")] * examineeProfile[itemAttribute]

    prob = exp(logit) / (1 + exp(logit))
    if (response == 0) prob = 1 - prob
  }
  return(prob)
}
