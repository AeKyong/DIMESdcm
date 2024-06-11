
simulateEDCMfromChains = function(nObs, nItems, itemParameterChains){

  # get location of beta intercept and beta lambda parameters
  betaInterceptCols = grep(pattern = "^beta_intercept\\[", x = colnames(itemParameterChains))
  betaLambdaCols = grep(pattern = "^beta_lambda\\[", x = colnames(itemParameterChains))
  interceptCols = grep(pattern = "^intercept\\[", x = colnames(itemParameterChains))
  lambdaCols = grep(pattern = "^lambda\\[", x = colnames(itemParameterChains))

  # original item pool size
  originalItemsInPool = length(grep(pattern = "^lambda\\[", x = colnames(itemParameterChains)))
  originalCovariatesInPool = length(grep(pattern = "^beta_lambda\\[", x = colnames(itemParameterChains)))


  # sample random number from the chain
  samplechain = NULL
  for (nTimes in 1:ceiling(nItems/originalItemsInPool)) {
    samplechain = c(samplechain, sample(x = 1:nrow(itemParameterChains), size = 1, replace = TRUE))
  }

  # sample item parameters
  interceptSamples = NULL
  lambdaSamples = NULL
  betaInterceptSamples = NULL
  betaLambdaSamples = NULL
  itemMat = matrix(0, nrow=1, ncol=originalItemsInPool)
  covMat = matrix(0, nrow=1, ncol=originalCovariatesInPool)

  for (sample in 1:length(samplechain)){

    if (sample == length(samplechain)){
      if (nItems %% originalItemsInPool == 0) {
        interceptSample =  itemParameterChains[samplechain[sample], interceptCols[1:originalItemsInPool]]
        lambdaSample = itemParameterChains[samplechain[sample], lambdaCols[1:originalItemsInPool]]
        betaInterceptSample = itemParameterChains[samplechain[sample], betaInterceptCols[1:originalCovariatesInPool]]
        betaLambdaSample = itemParameterChains[samplechain[sample], betaLambdaCols[1:originalCovariatesInPool]]

      }else{
        itemSample = sample(x = 1:originalItemsInPool, size = nItems %% originalItemsInPool, replace = FALSE)
        interceptSample =  itemParameterChains[samplechain[sample], interceptCols[itemSample]]
        lambdaSample =  itemParameterChains[samplechain[sample], lambdaCols[itemSample]]

        # create a matrix marking the sampled items
        itemMat[,itemSample] = 1
        # create a matrix marking the items properties associated with sampled items
        covMat = itemMat %*% itemcovsMat
        # draw the sampled item properties
        covSample = which(covMat>0)

        betaInterceptSample =  itemParameterChains[samplechain[sample], betaInterceptCols[covSample]]
        betaLambdaSample =  itemParameterChains[samplechain[sample], betaLambdaCols[covSample]]
      }

    } else {
      interceptSample =  itemParameterChains[samplechain[sample], interceptCols[1:originalItemsInPool]]
      lambdaSample = itemParameterChains[samplechain[sample], lambdaCols[1:originalItemsInPool]]
      betaInterceptSample = itemParameterChains[samplechain[sample], betaInterceptCols[1:originalCovariatesInPool]]
      betaLambdaSample = itemParameterChains[samplechain[sample], betaLambdaCols[1:originalCovariatesInPool]]
    }

    interceptSamples = c(interceptSamples,interceptSample)
    lambdaSamples = c(lambdaSamples, lambdaSample)
    betaInterceptSamples = c(betaInterceptSamples, betaInterceptSample)
    betaLambdaSamples = c(betaLambdaSamples, betaLambdaSample)

  }


  # generate random profiles from categorical distribution
  profiles = rcat(nObs, rep(1/nProfiles, nProfiles))

  profileMatrix = NULL
  for (i in 0:(nProfiles - 1)){
    profileMatrix = rbind(
      profileMatrix,
      dec2bin(decimal_number = i, nattributes = nAttributes, basevector = rep(2, nAttributes))
    )
  }

  attributeProfiles = matrix(NA, nrow = nObs, ncol = nAttributes)
  for(obs in 1:nObs){
    attributeProfiles[obs,1:nAttributes] = profileMatrix[profiles[obs], 1:nAttributes]
  }


  Xic = NULL
  for (i in 1:nItems){
    # get the item number
    itemN = as.numeric(gsub('\\D', '', x = names(lambdaSamples[i])))

    # find the ability profile for the item
    itemQ = abilityQre[itemN,]

    logit = interceptSamples[i]+lambdaSamples[i]*attributeProfiles%*%itemQ
    prob = exp(logit)/(1+exp(logit))

    Xc = rbinom(nObs, 1, prob = prob)
    Xic = cbind(Xic, Xc)
  }

  colnames(Xic) = paste0("item",  as.numeric(gsub('\\D', '', x = names(lambdaSamples))))



 return(list(
        simData = Xic,
        intercept = interceptSamples,
        lambda = lambdaSamples,
        betaIntercept = betaInterceptSamples,
        betaLambda = betaLambdaSamples,
        attributeProfiles = attributeProfiles
        ))


}

# loop for calibration --starts with pilot sample ==============================






















