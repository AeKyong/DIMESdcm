estimateJagsEDCM = function(itemcovs, abilityQ, modelData, seed,
                            betaInterceptMean=3, betaInterceptSD=.1, betaLambdaMean=3, betaLambdaSD=.1,
                            varHyperParaA, varHyperParaB, varInterceptMean, varInterceptSD, varLambdaMean, varLambdaSD) {

  # count attributes and profiles
  nAttributes = ncol(abilityQ)
  nProfiles = 2^nAttributes

  # make profile matrix
  profileMatrix = NULL
  for (i in 0:(nProfiles - 1)){
    profileMatrix = rbind(
      profileMatrix,
      dec2bin(decimal_number = i, nattributes = nAttributes, basevector = rep(2, nAttributes))
    )
  }


  # Generate vector of total observed data per item
  nObserved = colSums(!is.na(modelData))

  # Generate placeholder matrix for observed data indices
  observed = matrix(nrow = nrow(modelData), ncol = ncol(modelData), data = -9999)

  # Add missing data indices to placeholder matrix (-9999 = missing)
  for(i in 1:ncol(modelData)){
    for(obs in 1:nrow(modelData)){
      observed[1:nObserved[i], i] <- which(!is.na(modelData[,i]))
    }
  }


  # save which ability is needed for which item as vector
  itemAbilityQ = unlist(
    lapply(
      X = 1:nrow(abilityQ),
      FUN = function(x) return(which(abilityQ[x,] == 1))
    )
  )



  moddata = list(
    nItems = nrow(itemcovs),
    nItemCovs = ncol(itemcovs),
    itemcovs = itemcovs,
    itemAbilityQ = itemAbilityQ,
    nProfiles = nProfiles,
    etaPrior = rep(10, nProfiles),
    nObs = nrow(modelData),
    Y = modelData,
    nFactors = nAttributes,
    observed = observed,
    nObserved = nObserved,
    profileMatrix = profileMatrix,
    varHyperParaA = varHyperParaA,
    varHyperParaB = varHyperParaB
  )


  edcm_params_full = c("beta_intercept", 'beta_lambda', "var_lambda", "var_intercept", "lambda", "intercept", "eta")


  jags.inits = function(){
    list("beta_intercept" = rnorm(ncol(itemcovs), mean = betaInterceptMean, sd = betaInterceptSD),
         "beta_lambda" = rnorm(ncol(itemcovs), mean = betaLambdaMean, sd = betaLambdaSD),
         "var_intercept" = rnorm(1, mean = varInterceptMean, sd = varInterceptSD),
         "var_lambda" = rnorm(1, mean = varLambdaMean, sd = varLambdaSD))

  }

  edcm_full_run = jags.parallel(
    data = moddata,
    parameters.to.save = edcm_params_full,
    model.file = edcm_full,
    n.chains = 4,
    n.iter = 4000,
    n.thin = 1,
    n.burnin = 2000,
    n.cluster = 4,
    inits = jags.inits,
    jags.seed = seed
  )



  return(edcm_full_run)


  }
