
simulateEDCMfromChains = function(nObs, nItems, itemParameterChains, itemcovs, abilityQ,
                                  itemParameterVariance){

  # get location of beta intercept and beta lambda parameters
  betaInterceptCols = grep(pattern = "^beta_intercept\\[", x = colnames(itemParameterChains))
  betaLambdaCols = grep(pattern = "^beta_lambda\\[", x = colnames(itemParameterChains))
  varInterceptCols =  grep(pattern = "^var_intercept", x = colnames(itemParameterChains))
  varLambdaCols =  grep(pattern = "^var_lambda", x = colnames(itemParameterChains))

  # original item pool size
  originalItemsInPool = length(grep(pattern = "^lambda\\[", x = colnames(itemParameterChains)))
  originalCovariatesInPool = length(grep(pattern = "^beta_lambda\\[", x = colnames(itemParameterChains)))


  # sample random number from the chain
  samplechain = sample(x = 1:nrow(itemParameterChains), size = 1, replace = FALSE)


  betaInterceptSamples = NULL
  betaLambdaSamples = NULL
  simItemCovs = NULL
  sampledItems = NULL

  # sample item parameters ======================================================
    sampledItems = sample(x = 1:originalItemsInPool, size = nItems, replace = FALSE)

    # create a matrix marking the sampled items
    itemMat = matrix(0, nrow=1, ncol=originalItemsInPool)
    itemMat[,sampledItems] = 1

    # create a matrix marking the items covariates associated with sampled items
    covMat = itemMat %*% itemcovs
    # draw the sampled item covariates
    covSamples = which(covMat>0)

    betaInterceptSamples =  itemParameterChains[samplechain, betaInterceptCols[covSamples]]
    betaLambdaSamples =  itemParameterChains[samplechain, betaLambdaCols[covSamples]]

    simItemCovsMat = itemcovs[sampledItems,]
    simItemCovs = simItemCovsMat[, which(apply(simItemCovsMat, 2, sum)>0)]


    # attach new item name on item and covariate parameters
    names(betaInterceptSamples) = paste0("beta_intercept[",1:length(betaInterceptSamples),"]")
    names(betaLambdaSamples) = paste0("beta_lambda[",1:length(betaLambdaSamples),"]")

    # attach new item name
    originalCovName = colnames(simItemCovs)
    rownames(simItemCovs) = paste0("item", 1:nrow(simItemCovs))
    colnames(simItemCovs) = paste0("cov", 1:ncol(simItemCovs))

    # sampled item's abilityQ[item, AbilityQ]
    simAbilityQ = abilityQ[sampledItems,]
    rownames(simAbilityQ) = paste0("item", 1:nrow(simAbilityQ))

    # sample variance===========================================================
    varIntercept = itemParameterChains[samplechain, varInterceptCols]
    varLambda = itemParameterChains[samplechain, varLambdaCols]



  # generate respondents' profiles  =============================================
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
  examineeProfile = matrix(NA, nrow = nObs, ncol = nAttributes)
  for(obs in 1:nObs){
    examineeProfile[obs,1:nAttributes] = profileMatrix[profiles[obs], 1:nAttributes]
  }
  colnames(examineeProfile) = colnames(abilityQ)



  # generate responses ======================================================================
  Xic = NULL
  for (i in 1:nItems){

    # find the set of Q matrix entries for item i (q_i)
    itemQ = simAbilityQ[i, ]

    # find the set of item covariates' location
    itemCovariatesNum = which(simItemCovs[i, ]==1)

    # sample an error for the item
    interceptError = rnorm(1, mean = 0, sd = sqrt(varIntercept))
    lambdaError = rnorm(1, mean = 0, sd = sqrt(varLambda))

    # attach random error to the item parameter
    trueIntercept = sum(betaInterceptSamples[itemCovariatesNum]) + interceptError
    trueLambda = sum(betaLambdaSamples[itemCovariatesNum]) + lambdaError

    # logit = intercept_i + lambda_i * h(alpha_c, q_i) p.155
    logit = trueIntercept + trueLambda*examineeProfile%*%itemQ
    prob = exp(logit)/(1+exp(logit))

    Xc = rbinom(nObs, 1, prob = prob)
    Xic = cbind(Xic, Xc)

  }

  # attach new item number on response
  colnames(Xic) = rownames(simItemCovs)



 return(list(
        simData = Xic,
        simItemCovs = simItemCovs,
        simAbilityQ = simAbilityQ,
        betaIntercept = betaInterceptSamples,
        betaLambda = betaLambdaSamples,
        varIntercept =varIntercept,
        varLambda = varLambda,
        originalCovName = originalCovName
        ))


}


