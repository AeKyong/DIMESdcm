rm(list = ls())

library(R2jags)
library(tidyverse)
library(tibble)
library(truncnorm)
library(reshape2)
library(readxl)
library(bayesplot)

# seed used for analysis =  1690392934
seed = as.integer(Sys.time())

############################################################################

#################################################################
##                     Generate Q-Matrices                     ##
#################################################################

itemFile = read_xlsx(path = "drafts/FINAL_FOR_ANALYSIS_DIMES_DEIDENTIFIED_CONSENTED_Data_Scored_6.30.22.xlsx", sheet = "Items")

# remove tibble as we cannot use which() for selecting cases
itemFile = as.data.frame(itemFile)

# in process checks:
# check to see if items have more than one entry
# any(table(itemFile[,1]) >1) none do

dataFile = read.csv(file = "drafts/formTot_notduplic.csv")
itemStartCol = 16 # items start at column 16
itemStopCol = 120

# grab item list from data file
itemNamesRaw = names(dataFile)[itemStartCol:itemStopCol]

# remove X
itemNamesNoX = substr(x = itemNamesRaw, start = 2, stop = nchar(itemNamesRaw))

# remove any leading periods from names
itemNames = unlist(
  lapply(
    X = strsplit(x = itemNamesNoX, split = "\\."),
    FUN = function(x) return(x[length(x)])
  )
)

# add leading zeros to itemNames to order numerically
itemNamesZeros = sprintf("%03.f", as.integer(itemNames))

# add "item" stem to each itemNamesZeros
newItemNames = paste0("item", itemNamesZeros)

# rename in data file
names(dataFile)[itemStartCol:itemStopCol] = newItemNames

# convert itemNames to itemNumbers
itemNumbers = as.numeric(itemNames)

# select only items from itemFile that are in data:
itemData = itemFile[which(itemFile[,1] %in% itemNumbers),]
itemData$newItemName = paste0("item", sprintf("%03.f",itemData[,1]))

# reorder itemData by itemName order
itemData = itemData[order(itemData$newItemName),]

# remove R2 type items
itemDataNoR2 = itemData[which(itemData$`Ev. Model Task Code` != "R2"),]

# build Q-matrix for ability
abilityQ = matrix(data = 0, nrow = nrow(itemDataNoR2), ncol = 3)
colnames(abilityQ) = c("Recognition", "Comprehension", "ProblemSolving")
rownames(abilityQ) = itemDataNoR2$newItemName

# populate ability Q-matrix
itemTaskCode = substr(x = itemDataNoR2$`Ev. Model Task Code`, start = 1, stop = 1)
qAbilities = substr(x = colnames(abilityQ), start = 1, stop = 1)
for (ability in 1:ncol(abilityQ)){
  abilityQ[which(qAbilities[ability] == itemTaskCode),ability] = 1
}

# save which ability is needed for which item as vector
itemAbilityQ = unlist(
  lapply(
    X = 1:nrow(abilityQ),
    FUN = function(x) return(which(abilityQ[x,] == 1))
  )
)


modelData = dataFile[rownames(abilityQ)]

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

# BUILD PREDICTORS FOR ITEM PARAMETERS IN EXPLANATORY MODEL:

# note: should likely put back R2 items

# build Q-matrix for ability
abilityQ = matrix(data = 0, nrow = nrow(itemData), ncol = 3)
colnames(abilityQ) = c("Recognition", "Comprehension", "ProblemSolving")
rownames(abilityQ) = itemData$newItemName

# populate ability Q-matrix
itemTaskCode = substr(x = itemData$`Ev. Model Task Code`, start = 1, stop = 1)
qAbilities = substr(x = colnames(abilityQ), start = 1, stop = 1)
for (ability in 1:ncol(abilityQ)){
  abilityQ[which(qAbilities[ability] == itemTaskCode),ability] = 1
}

# save which ability is needed for which item as vector
itemAbilityQ = unlist(
  lapply(
    X = 1:nrow(abilityQ),
    FUN = function(x) return(which(abilityQ[x,] == 1))
  )
)

modelData = dataFile[rownames(abilityQ)]

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

# Ev. Model Task Code (column B; factor variable)
table(itemData$`Ev. Model Task Code`) # note: c2/c6 and c4/c6 need recoded to ensure all appear

# Task Type (column X; factor variable)
table(itemData$`Task Type`)

# Word Difficulty (column H; factor variable)
table(itemData[,"Word\r\nDifficulty"])

# Word Type (column G; factor variable)
table(itemData$`Word \r\ntype`)

# Task Difficulty (column I; factor variable)
table(itemData$`Task\r\nDifficulty`)

# Morphology (column F; factor variable)
table(itemData$Morphology)

# create data frame with only these variables
itemModelData = itemData[,
                         c(names(itemData)[1],
                           "newItemName",
                           "Ev. Model Task Code",
                           "Task Type",
                           "Word\r\nDifficulty",
                           "Word \r\ntype",
                           "Task\r\nDifficulty",
                           "Morphology"
                         )
]

# rename data columns to ensure access to data without Excel characters
names(itemModelData) = c(
  "itemNumberOriginal",
  "itemNumberNew",
  "evModelTaskCode",
  "taskType",
  "wordDifficulty",
  "wordType",
  "taskDifficulty",
  "morphology"
)

# add in column to allow for C2/C6 and C4/C6
itemModelData$taskCode =
  unlist(
    lapply(
      X = strsplit(itemModelData$evModelTaskCode, split = "/"),
      FUN = function(x) return(x[1])
    )
  )
itemModelData$taskCodeC6 =
  unlist(
    lapply(
      X = strsplit(itemModelData$evModelTaskCode, split = "/"),
      FUN = function(x) return(x[2])
    )
  )


# note: morphology has NA values for some items (needs changed)
itemModelData$morphology[which(is.na(itemModelData$morphology))] = "none"


# change variables to factors for easy model matrix building (but some cannot be)

# morphology (factor; reference "none")
itemModelData$morphology = relevel(factor(itemModelData$morphology), ref = "none")

# task code (factor; reference "C1")
itemModelData$taskCode = relevel(factor(itemModelData$taskCode), ref = "C1")

# C6 tasks (not a factor; reference == 0: no C6 on item)
itemModelData$taskC6 = NA
itemModelData$taskC6[which(is.na(itemModelData$taskCodeC6))] = 0
itemModelData$taskC6[which(!is.na(itemModelData$taskCodeC6))] = 1

# "taskType" (factor; reference "MC")
itemModelData$taskType = relevel(factor(itemModelData$taskType), ref = "MC")

# word difficulty (factor; reference "1")
itemModelData$wordDifficulty = relevel(factor(itemModelData$wordDifficulty), ref = 1)

# word type (factor; reference "suffix")
itemModelData$wordType = relevel(factor(itemModelData$wordType), ref = "suffix")

# task difficulty (factor; reference "E")
itemModelData$taskDifficulty =  relevel(factor(itemModelData$taskDifficulty), ref = "E")

itemModelFormula = formula(x =
                             itemNumberNew ~ morphology + taskCode + taskC6 + taskType + wordDifficulty + wordType + taskDifficulty
                           , data = itemModelData)


itemcovs <- model.matrix(
  object = itemModelFormula,
  data = itemModelData)

dec2bin = function(decimal_number, nattributes, basevector){
  dec = decimal_number
  profile = matrix(NA, nrow = 1, ncol = nattributes)
  for (i in nattributes:1){
    profile[1,i] =  dec %% basevector[i]
    dec = (dec - dec %% basevector[i])/basevector[i]
  }
  return(profile)
}

nAttributes = 3
nProfiles = 2^nAttributes
profileMatrix = NULL

for (i in 0:(nProfiles - 1)){
  profileMatrix = rbind(
    profileMatrix,
    dec2bin(decimal_number = i, nattributes = nAttributes, basevector = rep(2, nAttributes))
  )
}



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
    lambda[i] ~ dnorm(itemcov_lambda[i], inVar_lambda); T(0,)
  }

  inVar_intercept ~ dgamma(1e-3,1e-3)
  inVar_lambda ~ dgamma(1e-3,1e-3)

  var_intercept <- 1/inVar_intercept
  var_lambda <- 1/inVar_lambda




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

moddata <- list(
  nItems = nrow(itemcovs),
  nItemCovs = ncol(itemcovs),
  itemcovs = itemcovs,
  itemAbilityQ = itemAbilityQ,
  nProfiles = nProfiles,
  etaPrior = rep(1, nProfiles),
  nObs = nrow(modelData),
  Y = modelData,
  nFactors = nAttributes,
  observed = observed,
  nObserved = nObserved,
  profileMatrix = profileMatrix
)

edcm_params_full <- c("beta_intercept", 'beta_lambda', "var_lambda", "var_intercept", "lambda", "intercept", "eta")



edcm_full_run = jags.parallel(
  data = moddata,
  parameters.to.save = edcm_params_full,
  model.file = edcm_full,
  n.chains = 4,
  n.iter = 4e3,
  n.thin = 1,
  n.burnin = 2e3,
  n.cluster = 4,
  jags.seed = seed
)

# check max rhat
maxRhat = max(edcm_full_run$BUGSoutput$summary[,"Rhat"])

# item parameter chains
itemParameterChains = edcm_full_run[["BUGSoutput"]][["sims.matrix"]]

# # item intercept variance, item lambda variance
# summary = as.data.frame(edcm_full_run$BUGSoutput$summary)
# itemParameterVariance = as.matrix(summary[c("var_intercept","var_lambda"),"mean"])
# colnames(itemParameterVariance) = "variance"
# rownames(itemParameterVariance) = c("intercept","lambda")


save(itemParameterChains, file = "itemParameterChains.rda", row.names = F)
# save(itemParameterVariance, file = "itemParameterVariance.rda", row.names = F)
