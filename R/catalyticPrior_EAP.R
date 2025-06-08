catalyticPrior_EAP = function(betaInterceptPosterior, betaLambdaPosterior, varInterceptPosterior, varLambdaPosterior,
                              etaPosterior, itemcovs, abilityQ, nSyntheticData){


# browser()
  nItems = nrow(itemcovs)


  # sampling parameter
  betaIntercept = apply(betaInterceptPosterior, 2, mean, na.rm=T)
  betaLambda = apply(betaInterceptPosterior, 2, mean, na.rm=T)
  varIntercept = mean(varInterceptPosterior, na.rm=T)
  varLambda = mean(varLambdaPosterior, na.rm=T)
  eta = apply(etaPosterior, 2, mean, na.rm=T)


  # generate respondents' profiles  =============================================
  nAttributes = ncol(abilityQ)
  nProfiles = 2^nAttributes

  profiles = rcat(nSyntheticData, eta)

  profileMatrix = NULL
  for (i in 0:(nProfiles - 1)){
    profileMatrix = rbind(
      profileMatrix,
      dec2bin(decimal_number = i, nattributes = nAttributes, basevector = rep(2, nAttributes))
    )
  }

  # change profiles to attribute profiles (alpha_c)
  examineeProfile = matrix(NA, nrow = nSyntheticData, ncol = nAttributes)
  examineeProfile[,1:nAttributes] = profileMatrix[profiles, 1:nAttributes]
  colnames(examineeProfile) = colnames(abilityQ)



  # generate responses ======================================================================
  Xic = NULL
  for (i in 1:nItems){

    # find the set of Q matrix entries for item i (q_i)
    itemQ = abilityQ[i, ]

    # find the set of item covariates' location
    itemCovariatesNum = which(itemcovs[i, ]==1)

    # sample an error for the item
    interceptError = rnorm(1, mean = 0, sd = sqrt(varIntercept))
    lambdaError = rnorm(1, mean = 0, sd = sqrt(varLambda))

    # attach random error to the item parameter
    trueIntercept = sum(betaIntercept[itemCovariatesNum]) + interceptError
    trueLambda = sum(betaLambda[itemCovariatesNum]) + lambdaError

    # logit = intercept_i + lambda_i * h(alpha_c, q_i) p.155
    logit = trueIntercept + trueLambda*examineeProfile%*%itemQ
    prob = exp(logit)/(1+exp(logit))

    Xc = rbinom(nSyntheticData, 1, prob = prob)
    Xic = cbind(Xic, Xc)

  }

  # attach new item number on response
  colnames(Xic) = rownames(itemcovs)

  return(Xic)

  }
