adaptiveSimulation = function(nProfiles, abilityQ, itemcovs, nItems, profileMatrix, startingProfileProbablity, itemPool,
                              trueParameters, trueProfiles, itemProbArray,
                              maxItems, itemUpdateFunction, itemSummaryFunction, nItemSamples, stopCriterion, calculateSHE){


  responseVector = NULL
  itemAdministered = NULL

  currentProfileProbablity = startingProfileProbablity

  poolQ = abilityQ
  rownames(poolQ) = paste0("[", gsub("\\D", "", rownames(abilityQ)), "]")
  rownames(itemcovs) = paste0("[", gsub("\\D", "", rownames(itemcovs)), "]")

  itemNum = 1
  stop = FALSE

# eventual loop for adaptive algorithms =========================================
  while(itemNum <= maxItems && stop == FALSE){

  print(itemNum)
  itemNum = itemNum +1
browser()

  # select next item based on SHE algorithm
  selectedItem = selectNewItem(
    itemPool = itemPool,
    itemProbArray = itemProbArray,
    currentProfileProbablity = currentProfileProbablity,
    profileMatrix = profileMatrix,
    itemAbilityQ = poolQ,
    nProfiles = nProfiles,
    itemUpdateFunction = itemUpdateFunction,
    calculateSHE = calculateSHE
  )

  itemName = itemPool[selectedItem]
  itemAdministered = c(itemAdministered, itemName)




  # administer item -- get item response from examinee (via simulation)
  selectedItemcovs = itemcovs[itemName,]
  selectedCovs = which(selectedItemcovs ==1)


  betaInterceptSum = sum(trueParameters$beta_intercept[which(names(trueParameters$beta_intercept) %in% paste0("beta_intercept[", selectedCovs,"]"))])
  betaLambdaSum = sum(trueParameters$beta_lambda[which(names(trueParameters$beta_lambda) %in% paste0("beta_lambda[", selectedCovs,"]"))])

  interceptError = rnorm(1, mean =0, sd = sqrt(trueParameters$var_intercept))
  lambdaError = rnorm(1, mean =0, sd = sqrt(trueParameters$var_lambda))

  trueIntercept = betaInterceptSum + interceptError
  trueLambda = betaLambdaSum + lambdaError



  # find the set of Q matrix entries for item i (q_i)
  itemQ = poolQ[itemName,]
  # logit = intercept_i,0 + lambda_i * h(alpha_c, q_i) p.155
  logit = trueIntercept + trueLambda*profileMatrix[trueProfiles,]%*%itemQ
  prob = exp(logit)/(1+exp(logit))

  responseVector = c(responseVector, rbinom(n =1, size =1, prob = prob))
  names(responseVector)[length(responseVector)] = itemAdministered[length(itemAdministered)]




  # save current item probability for updating profile probability
  currentItemProbArray = itemSummaryFunction(itemProbArray = itemProbArray,
                                             itemName = itemName,
                                             nItemSamples = nItemSamples)

  # update current profile probability
  if (nItemSamples ==1) {
   currentProfileProbablity =
     currentProfileProbablity*
     currentItemProbArray[dim(currentItemProbArray)[1], , responseVector[length(responseVector)]+1]
  } else {
    # replicate currentProfileProbability nItemSamples times
    currentProfileProbablity = matrix(replicate(nItemSamples,currentProfileProbablity), nrow = nItemSamples, byrow = TRUE)

    for (profile in 1:nProfiles){
      currentProfileProbablity[1:nItemSamples, profile] =
        currentProfileProbablity[1:nItemSamples, profile] *
        currentItemProbArray[1:nItemSamples, profile, responseVector[length(responseVector)]+1]
    }
    currentProfileProbablity = apply(currentProfileProbablity, 2, mean, na.rm = TRUE)
  }

   currentProfileProbablity = currentProfileProbablity/sum(currentProfileProbablity)




  # adapt termination rule
  if (max(currentProfileProbablity) > stopCriterion) {stop = TRUE}



  # remove all items that have been administered from consideration
  itemProbArray = itemProbArray[,,,which(!dimnames(itemProbArray)[[4]] %in% itemName), drop=FALSE]
  itemPool = itemPool[which(!itemPool %in% itemName)]
  nItemsInPool = length(itemPool)
  poolQ = poolQ[which(!rownames(poolQ) %in% itemName), , drop=FALSE]





}
  # return key information
  return(list(currentProfileProbablity = currentProfileProbablity,
              currentProfile = which.max(currentProfileProbablity),
              currentItemProbArray = currentItemProbArray,
              responseVector = as.matrix(responseVector)))
}

