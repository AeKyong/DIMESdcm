
simulateEDCMfromChains = function(nObs, nItems, itemParameterChains, itemcovs, abilityQ){

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
      # when sampled items N == original items N
      if (nItems %% originalItemsInPool == 0) {
        interceptSamples =  itemParameterChains[samplechain, interceptCols[1:originalItemsInPool]]
        lambdaSamples = itemParameterChains[samplechain, lambdaCols[1:originalItemsInPool]]
        betaInterceptSamples = itemParameterChains[samplechain, betaInterceptCols[1:originalCovariatesInPool]]
        betaLambdaSamples = itemParameterChains[samplechain, betaLambdaCols[1:originalCovariatesInPool]]
        simItemCovs = itemcovs

      # when sampled items N != original items N
      }else{
        itemSamples = sample(x = 1:originalItemsInPool, size = nItems %% originalItemsInPool, replace = FALSE)
        interceptSamples =  itemParameterChains[samplechain, interceptCols[itemSamples]]
        lambdaSamples =  itemParameterChains[samplechain, lambdaCols[itemSamples]]

        # create a matrix marking the sampled items
        itemMat = matrix(0, nrow=1, ncol=originalItemsInPool)
        itemMat[,itemSamples] = 1
        # create a matrix marking the items properties associated with sampled items
        covMat = itemMat %*% itemcovs
        # draw the sampled item properties
        covSamples = which(covMat>0)

        betaInterceptSamples =  itemParameterChains[samplechain, betaInterceptCols[covSamples]]
        betaLambdaSamples =  itemParameterChains[samplechain, betaLambdaCols[covSamples]]

        simItemCovsI = itemcovs[itemSamples,]
        simItemCovs = simItemCovsI[, which(apply(simItemCovsI, 2, sum)>0)]

      }

    # attach new item name
    rownames(simItemCovs) = paste0("item", 1:nrow(simItemCovs))
    colnames(simItemCovs) = paste0("cov", 1:ncol(simItemCovs))



  # generate random profiles from categorical distribution for respondents =================
  nAttributes = ncol(abilityQ)
  nProfiles = 2^nAttributes

  profiles = rcat(nObs, rep(1/nProfiles, nProfiles))

  profileMatrix = NULL
  for (i in 0:(nProfiles - 1)){
    profileMatrix = rbind(
      profileMatrix,
      dec2bin(decimal_number = i, nattributes = nAttributes, basevector = rep(2, nAttributes))
    )
  }

  # change profiles to attribute profiles (alpha_c)
  attributeProfiles = matrix(NA, nrow = nObs, ncol = nAttributes)
  for(obs in 1:nObs){
    attributeProfiles[obs,1:nAttributes] = profileMatrix[profiles[obs], 1:nAttributes]
  }
  colnames(attributeProfiles) = colnames(abilityQ)


  # generate responses ======================================================================
  Xic = NULL
  simAbilityQ = NULL
  for (i in 1:nItems){
    # get the item number
    itemN = as.numeric(gsub('\\D', '', x = names(lambdaSamples[i])))

    # find the set of Q matrix entries for item i (q_i)
    itemQ = abilityQ[itemN,]

    # logit = intercept_i,0 + lambda_i * h(alpha_c, q_i) p.155
    logit = interceptSamples[i]+lambdaSamples[i]*attributeProfiles%*%itemQ
    prob = exp(logit)/(1+exp(logit))

    Xc = rbinom(nObs, 1, prob = prob)
    Xic = cbind(Xic, Xc)

    simAbilityQ = rbind(simAbilityQ, abilityQ[itemN,])


  }

  # attach new item number
  colnames(Xic) = rownames(simItemCovs)
  rownames(simAbilityQ) = rownames(simItemCovs)


 return(list(
        simData = Xic,
        simItemCovs = simItemCovs,
        simAbilityQ = simAbilityQ,
        intercept = interceptSamples,
        lambda = lambdaSamples,
        betaIntercept = betaInterceptSamples,
        betaLambda = betaLambdaSamples,
        attributeProfiles = attributeProfiles
        ))


}

