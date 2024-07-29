##################################################################
##            Explanatory Model Syntax w/ Covariates            ##
##################################################################

edcm_full = function(){

  # item response model: same
  for(i in 1:nItems){
    for(obs in observed[1:nObserved[i],i]){
      Y[obs, i] ~ dbern(prob[obs,i])
      logit(prob[obs,i]) <- theta[obs,itemAbilityQ[i]]*lambda[i] + intercept[i]
    }
  }

  # prior distribution for class probabilities:
  eta[1:nProfiles] ~ ddirich(etaPrior[1:nProfiles])

  itemcov_intercept[1:nItems] <- itemcovs[1:nItems, 1:nItemCovs]%*%beta_intercept[1:nItemCovs]
  itemcov_lambda[1:nItems] <- itemcovs[1:nItems, 1:nItemCovs]%*%beta_lambda[1:nItemCovs]


  for(i in 1:nItems){
    intercept[i] ~ dnorm(itemcov_intercept[i], inVar_intercept)
    lambda[i] ~ dnorm(itemcov_lambda[i], inVar_lambda);T(0,)
  }


  # # VAR ~ INV_GAMMA
  # inVar_intercept ~ dgamma(1e-3,1e-3)
  # inVar_lambda ~ dgamma(1e-3,1e-3)
  #
  # var_intercept <- 1/inVar_intercept
  # var_lambda <- 1/inVar_lambda

  # # VAR ~ unif
  # inVar_intercept <- 1/var_intercept
  # inVar_lambda <- 1/var_lambda
  #
  # var_intercept ~ dunif(0,100)
  # var_lambda ~ dunif(0,100)

  # # VAR ~ logNormal
  # inVar_intercept <- 1/var_intercept
  # inVar_lambda <- 1/var_lambda
  #
  # var_intercept ~ dlnorm(-0.8, 0.1)
  # var_lambda ~ dlnorm(0.1, 0.1)

  # VAR ~ GAMMA
  inVar_intercept <- 1/var_intercept
  inVar_lambda <- 1/var_lambda

  var_intercept ~ dgamma(hyperParaA, hyperParaB)
  var_lambda ~ dgamma(hyperParaA, hyperParaB)




  for(covi in 1:nItemCovs){
    beta_intercept[covi] ~ dnorm(0,3)
    beta_lambda[covi] ~ dnorm(0,3)
  }

  for(obs in 1:nObs){
    # sample class of each observation
    profile[obs] ~ dcat(eta[1:nProfiles])

    # convert class to profile for item response model
    theta[obs, 1:nFactors] = profileMatrix[profile[obs], 1:nFactors]
  }



}
