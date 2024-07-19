rm(list = ls())
devtools::load_all(".")
library(DIMESdcm)
library(parallel)
library(R2jags)
library(tidyverse)
library(tibble)
library(truncnorm)
library(reshape2)
library(readxl)
library(bayesplot)
library(extraDistr)

# grab command line arguments
#arrayNumber = as.numeric(commandArgs(trailingOnly = TRUE)[1])
arrayNumber = 1

set.seed(arrayNumber)



# Generate Q-Matrices for simulation data =======================================

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

# # ability Q-matrix rename the items
# abilityQre = abilityQ
# rownames(abilityQre) = 1:nrow(abilityQ)


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


itemcovs = model.matrix(
  object = itemModelFormula,
  data = itemModelData)





# load item parameter data
data("itemParameterChains")
# data("itemParameterVariance")

# implement condition values in simulation
simulationSpecs = conditionInformation(arrayNumber= arrayNumber, nReplicationsPerCondition = 1, nCores = 4)


# generate simulation data =====================================================
simDataList = simulateEDCMfromChains(nObs = simulationSpecs$initialPilotSampleSize,
                                     nItems = simulationSpecs$nItemsInPool,
                                     itemParameterChains = itemParameterChains,
                                     itemcovs=itemcovs,
                                     abilityQ = abilityQ,
                                     itemParameterVariance = itemParameterVariance)


# loop for calibration --starts with pilot sample ==============================
calibration = 1
calibrationData = list()
runningData = simDataList$simData
itemcovs = simDataList$simItemCovs
abilityQ = simDataList$simAbilityQ
estimatedParameters = NULL
estimatedProfileProbability= NULL
simItemParameterChains = NULL



while (calibration <= simulationSpecs$nCalibrations ){
print(calibration)
  # save previously converged parameters
  simItemParameterChainsCnvg = simItemParameterChains

  jagsEDCMestimates = estimateJagsEDCM(itemcovs = itemcovs,
                                       abilityQ = abilityQ,
                                       modelData = runningData,
                                       betaInterceptMean = 3,
                                       betaInterceptSD = .1,
                                       betaLambdaMean = 3,
                                       betaLambdaSD = .1,
                                       seed = .Random.seed[1])


  # max Rhat
  maxRhat = max(jagsEDCMestimates$BUGSoutput$summary[,"Rhat"])

  # parameter Estimates df
  summary = as.data.frame(jagsEDCMestimates$BUGSoutput$summary)
  parameterEstimates = tibble::rownames_to_column(summary, "variable")
  # rm(summary)

  # simulated item parameter chains matrix
  variables = c("^beta_intercept\\[","^beta_lambda\\[","^intercept\\[","^lambda\\[", "var_intercept", "var_lambda")
  sims.matrix = jagsEDCMestimates$BUGSoutput$sims.matrix
  simItemParameterChains = sims.matrix[,grep(pattern =paste(variables, collapse ="|"), x = colnames(sims.matrix)) ]
  # rm(sims.matrix)

  betaInterceptCols = grep("^beta_intercept", colnames(simItemParameterChains))
  betaLambdaCols = grep("^beta_lambda", colnames(simItemParameterChains))
  interceptCols = grep("^intercept", colnames(simItemParameterChains))
  lambdaCols = grep("^lambda", colnames(simItemParameterChains))
  varInterceptCols = grep("^var_intercept", colnames(simItemParameterChains))
  varLambdaCols = grep("^var_lambda", colnames(simItemParameterChains))



  # if items have not converged, replace non-converged parameters with previously converged ones
  if (maxRhat > 1.1){
    # get bad item numbers
    badParams = parameterEstimates$variable[which(parameterEstimates$Rhat > 1.1)]

    # grab item/covariate number having bad parameters
    betaInterceptBad = as.numeric(gsub('\\D', '', badParams[grep("^beta_intercept", badParams)]))
    betaLambdaBad = as.numeric(gsub('\\D', '', badParams[grep("^beta_lambda", badParams)]))
    interceptBad = as.numeric(gsub('\\D', '', badParams[grep("^intercept", badParams)]))
    lambdaBad = as.numeric(gsub('\\D', '', badParams[grep("^lambda", badParams)]))
    varInterceptBad = length(badParams[grep("^var_intercept", badParams)])
    varLambdaBad = length(badParams[grep("^var_lambda", badParams)])


    # grab item/covariate column numbers having bad parameters from simItemParameterChains
    betaInterceptBadCols = betaInterceptCols[betaInterceptBad]
    betaLambdaBadCols = betaLambdaCols[betaLambdaBad]
    interceptBadCols = interceptCols[interceptBad]
    lambdaBadCols = lambdaCols[lambdaBad]
    varInterceptBadCols = varInterceptCols[varInterceptBad]
    varLambdaBadCols = varLambdaCols[varLambdaBad]

    # if bad parameters do not contain main variables, assign main's original columns to bad columns
    if (length(grep(pattern = paste(variables, collapse ="|"), x = badParams)) == 0) {
        betaInterceptBadCols = betaInterceptCols
        betaLambdaBadCols = betaLambdaCols
        interceptBadCols = interceptCols
        lambdaBadCols = lambdaCols
        varInterceptBadCols = varInterceptCols
        varLambdaBadCols = varLambdaCols
    }


    # replace bad parameters with regular parameters
    if (is.null(simItemParameterChainsCnvg)){

      # if no parameter has converged yet, generate beta_intercept, beta_lambda from uniform distribution
      simItemParameterChains[, betaInterceptCols] = runif(8000*length(betaInterceptCols), min = -0.5, max = 1.0)
      simItemParameterChains[, betaLambdaCols] = runif(8000*length(betaLambdaCols), min = 0, max = 1.0)

      for (i in 1:length(interceptCols)){
        interceptCov = betaInterceptCols[which(simDataList$simItemCovs[i,]==1)]
        simItemParameterChains[, interceptCols[i]] = apply(simItemParameterChains[,interceptCov], 1, sum)+
          rnorm(n=nrow(simItemParameterChains), mean = 0, sd = 1)
      }
      for (i in 1:length(lambdaCols)){
       lambdaCov = betaLambdaCols[which(simDataList$simItemCovs[i,]==1)]
       simItemParameterChains[, lambdaCols[i]] = apply(simItemParameterChains[,lambdaCov], 1, sum)+
        rgamma(n=nrow(simItemParameterChains), 1e-3, 1e-3) # might need to change to normal distribution
      }

      simItemParameterChains[, varInterceptCols] = runif(8000*length(varInterceptCols), min = 0, max = 1.0)
      simItemParameterChains[, varLambdaCols] = runif(8000*length(varLambdaCols), min = 0, max = 1.0)

    }else{

      # if there were converged parameters, replace non-converged parameters with previously converged ones
      simItemParameterChains[, betaInterceptBadCols] = simItemParameterChainsCnvg[, betaInterceptBadCols]
      simItemParameterChains[, betaLambdaBadCols] = simItemParameterChainsCnvg[, betaLambdaBadCols]
      simItemParameterChains[, interceptBadCols] = simItemParameterChainsCnvg[, interceptBadCols]
      simItemParameterChains[, lambdaBadCols] = simItemParameterChainsCnvg[, lambdaBadCols]
      simItemParameterChains[, varInterceptBadCols] = simItemParameterChainsCnvg[, varInterceptBadCols]
      simItemParameterChains[, varLambdaBadCols] = simItemParameterChainsCnvg[, varLambdaBadCols]


    }


  } else {
    badParams = NULL
  }



  trueParameters = list(beta_intercept = simDataList$betaIntercept,
                        beta_lambda = simDataList$betaLambda,
                        var_intercept = simDataList$varIntercept,
                        var_lambda = simDataList$varLambda
                      )
  calibrationData[[calibration]] = list()
  calibrationData[[calibration]]$maxRhat = maxRhat
  calibrationData[[calibration]]$badParams = badParams
  calibrationData[[calibration]]$nBadParams = length(badParams)


  # get parameter summaries
  itemQuantiles = t(
    apply(simItemParameterChains, 2, FUN = quantile, probs = c(.01, .05, .1, .25, .5, .75, .9, .95, .99))
  )
  itemQuantiles = data.frame(itemQuantiles)
  names(itemQuantiles) = c("Q01", "Q05", "Q10", "Q25", "Q50", "Q75", "Q90", "Q95", "Q99")
  itemQuantiles$parameter = rownames(itemQuantiles)
  itemQuantiles$eap = apply(X = simItemParameterChains, MARGIN = 2, FUN = mean)
  itemQuantiles$sd = apply(X = simItemParameterChains, MARGIN = 2, FUN = sd)
  itemQuantiles$calibration = calibration
  itemQuantiles$trueValues = c(trueParameters$beta_intercept, trueParameters$beta_lambda, rep(0, 2*nrow(itemcovs)),
                               trueParameters$var_intercept, trueParameters$var_lambda)



  estimatedParameters = rbind(
    estimatedParameters,
    itemQuantiles
  )

  #for adding each person
  tempEstimatedParameters =
    data.frame(matrix(data = NA, nrow = 1, ncol = ncol(estimatedParameters)))
  names(tempEstimatedParameters) = names(estimatedParameters)



  posteriorParameters = simItemParameterChains


  betaInterceptPosterior = posteriorParameters[, grep("^beta_intercept", colnames(posteriorParameters))]
  betaLambdaPosterior = posteriorParameters[, grep("^beta_lambda", colnames(posteriorParameters))]
  interceptPosterior = posteriorParameters[, grep("^intercept", colnames(posteriorParameters))]
  lambdaPosterior = posteriorParameters[, grep("^lambda", colnames(posteriorParameters))]

  # item pool information
  nItems = length(grep(pattern = "^intercept\\[", x = colnames(posteriorParameters)))
  itemPool =  paste0("[", as.numeric(gsub("\\D", '', x = colnames(interceptPosterior))), "]")


  # set a true theta value -- used for providing responses to items in the simulation
  nAttributes = ncol(simDataList$simAbilityQ)
  nProfiles = 2^nAttributes
  trueProfiles = rcat(simulationSpecs$nNewStudents, rep(1/nProfiles, nProfiles))

  profileMatrix = NULL
  for (i in 0:(nProfiles - 1)){
    profileMatrix = rbind(
      profileMatrix,
      dec2bin(decimal_number = i, nattributes = nAttributes, basevector = rep(2, nAttributes))
    )
  }


  # set an initial value of theta -- used for selecting items ==================
  currentProfileProbablity = rep(1/nProfiles, nProfiles)

  # # adaptive algorithm for all new students and all items
  # profileDetails = NULL


  # calculate item probabilities
  nSamples = nrow(interceptPosterior)
  itemProbArray = array(data = NA, dim = c(nSamples, nProfiles, 2, nItems), dimnames = list(c(paste0("draw", 1:nSamples)), c(paste0("profile", 1:nProfiles)),
                                                                                            c(paste0("resp", 0:1)),c(paste0("[", 1:nItems, "]"))))
  # create item response probability array for each item, class, and response
  for (item in 1:nItems){
    for(sample in 1:nSamples) {
      for (profile in 1:nProfiles){
        for (response in 0:1){
          itemProbArray[sample, profile, (response+1), item] = itemProb(
            response = response,
            itemNumber = itemPool[item],
            itemAttribute = abilityQ[item,],
            examineeProfile = profileMatrix[profile,],
            itemIntercepts = interceptPosterior[sample, item],
            itemMainEffects = lambdaPosterior[sample, item]
          )
        }
      }
    }
  }




  # i= 1
  # testDraw = adaptiveSimulation(nProfiles = nProfiles,
  #                               abilityQ = simDataList$simAbilityQ,
  #                               itemcovs = simDataList$simItemCovs,
  #                               nItems = nItems,
  #                               profileMatrix= profileMatrix,
  #                               startingProfileProbablity = currentProfileProbablity,
  #                               itemPool = itemPool,
  #                               trueParameters = trueParameters,
  #                               trueProfiles = trueProfiles[i],
  #                               itemQuantiles = itemQuantiles,
  #                               itemProbArray= itemProbArray,
  #                               maxItems = simulationSpecs$maxItems,
  #                               itemUpdateFunction = simulationSpecs$itemUpdateFunction,
  #                               itemSummaryFunction = simulationSpecs$itemSummaryFunction,
  #                               nItemSamples = simulationSpecs$nItemSamples,
  #                               stopCriterion = simulationSpecs$stopCriterion,
  #                               calculateSHE = simulationSpecs$calculateSHE)


   # result = NULL
   # result[[1]] = parallelAdaptiveSimulation(thread = 1, threadIDs = simulationSpecs$threadIDs,
   #                                          tempEstimatedParameters = tempEstimatedParameters, calibration = calibration,
   #                                          nProfiles = nProfiles,
   #                                          abilityQ = simDataList$simAbilityQ,
   #                                          itemcovs = simDataList$simItemCovs,
   #                                          nItems = nItems,
   #                                          profileMatrix= profileMatrix,
   #                                          startingProfileProbablity = currentProfileProbablity,
   #                                          itemPool = itemPool,
   #                                          trueParameters = trueParameters,
   #                                          trueProfiles = trueProfiles,
   #                                          itemProbArray= itemProbArray,
   #                                          itemQuantiles = itemQuantiles,
   #                                          maxItems = simulationSpecs$maxItems,
   #                                          itemUpdateFunction = simulationSpecs$itemUpdateFunction,
   #                                          itemSummaryFunction = simulationSpecs$itemSummaryFunction,
   #                                          nItemSamples = simulationSpecs$nItemSamples,
   #                                          stopCriterion = simulationSpecs$stopCriterion,
   #                                          calculateSHE = simulationSpecs$calculateSHE)
   #
   # result[[2]] = parallelAdaptiveSimulation(thread = 2, threadIDs = simulationSpecs$threadIDs,
   #                                          tempEstimatedParameters = tempEstimatedParameters, calibration = calibration,
   #                                          nProfiles = nProfiles,
   #                                          abilityQ = simDataList$simAbilityQ,
   #                                          itemcovs = simDataList$simItemCovs,
   #                                          nItems = nItems,
   #                                          profileMatrix= profileMatrix,
   #                                          startingProfileProbablity = currentProfileProbablity,
   #                                          itemPool = itemPool,
   #                                          trueParameters = trueParameters,
   #                                          trueProfiles = trueProfiles,
   #                                          itemProbArray= itemProbArray,
   #                                          itemQuantiles = itemQuantiles,
   #                                          maxItems = simulationSpecs$maxItems,
   #                                          itemUpdateFunction = simulationSpecs$itemUpdateFunction,
   #                                          itemSummaryFunction = simulationSpecs$itemSummaryFunction,
   #                                          nItemSamples = simulationSpecs$nItemSamples,
   #                                          stopCriterion = simulationSpecs$stopCriterion,
   #                                          calculateSHE = simulationSpecs$calculateSHE)
   #
   # result[[3]] = parallelAdaptiveSimulation(thread = 3, threadIDs = simulationSpecs$threadIDs,
   #                                          tempEstimatedParameters = tempEstimatedParameters, calibration = calibration,
   #                                          nProfiles = nProfiles,
   #                                          abilityQ = simDataList$simAbilityQ,
   #                                          itemcovs = simDataList$simItemCovs,
   #                                          nItems = nItems,
   #                                          profileMatrix= profileMatrix,
   #                                          startingProfileProbablity = currentProfileProbablity,
   #                                          itemPool = itemPool,
   #                                          trueParameters = trueParameters,
   #                                          trueProfiles = trueProfiles,
   #                                          itemProbArray= itemProbArray,
   #                                          itemQuantiles = itemQuantiles,
   #                                          maxItems = simulationSpecs$maxItems,
   #                                          itemUpdateFunction = simulationSpecs$itemUpdateFunction,
   #                                          itemSummaryFunction = simulationSpecs$itemSummaryFunction,
   #                                          nItemSamples = simulationSpecs$nItemSamples,
   #                                          stopCriterion = simulationSpecs$stopCriterion,
   #                                          calculateSHE = simulationSpecs$calculateSHE)
   #
   # result[[4]] = parallelAdaptiveSimulation(thread = 4, threadIDs = simulationSpecs$threadIDs,
   #                                          tempEstimatedParameters = tempEstimatedParameters, calibration = calibration,
   #                                          nProfiles = nProfiles,
   #                                          abilityQ = simDataList$simAbilityQ,
   #                                          itemcovs = simDataList$simItemCovs,
   #                                          nItems = nItems,
   #                                          profileMatrix= profileMatrix,
   #                                          startingProfileProbablity = currentProfileProbablity,
   #                                          itemPool = itemPool,
   #                                          trueParameters = trueParameters,
   #                                          trueProfiles = trueProfiles,
   #                                          itemProbArray= itemProbArray,
   #                                          itemQuantiles = itemQuantiles,
   #                                          maxItems = simulationSpecs$maxItems,
   #                                          itemUpdateFunction = simulationSpecs$itemUpdateFunction,
   #                                          itemSummaryFunction = simulationSpecs$itemSummaryFunction,
   #                                          nItemSamples = simulationSpecs$nItemSamples,
   #                                          stopCriterion = simulationSpecs$stopCriterion,
   #                                          calculateSHE = simulationSpecs$calculateSHE)




   cl = parallel::makeCluster(simulationSpecs$nCores, outfile = "", setup_strategy = "sequential")

   #set random seed
   seed =.Random.seed[1]

   parallel::clusterSetRNGStream(cl = cl, iseed = seed)

   parallel::clusterExport(
     cl = cl,
     varlist = c("adaptiveSimulation",
                 "currentProfileProbablity",
                 "simDataList",
                 "simulationSpecs",
                 "selectNewItem",
                 "itemQuantiles",
                 "nProfiles"),
     envir = environment()
   )


   result = parallel::parLapply(
     cl = cl,
     X = 1:4,
     fun = parallelAdaptiveSimulation,
     threadIDs = simulationSpecs$threadIDs,
     tempEstimatedParameters = tempEstimatedParameters,
     calibration = calibration,
     nProfiles = nProfiles,
     abilityQ = simDataList$simAbilityQ,
     itemcovs = simDataList$simItemCovs,
     nItems = nItems,
     profileMatrix= profileMatrix,
     startingProfileProbablity = currentProfileProbablity,
     itemPool = itemPool,
     maxItems = simulationSpecs$maxItems,
     trueParameters = trueParameters,
     trueProfiles = trueProfiles,
     itemProbArray= itemProbArray,
     itemQuantiles = itemQuantiles,
     itemUpdateFunction = simulationSpecs$itemUpdateFunction,
     itemSummaryFunction = simulationSpecs$itemSummaryFunction,
     stopCriterion = simulationSpecs$stopCriterion,
     calculateSHE = simulationSpecs$calculateSHE
   )

   parallel::stopCluster(cl = cl)


   runningData = rbind(
     runningData,
     result[[1]]$runningData,
     result[[2]]$runningData,
     result[[3]]$runningData,
     result[[4]]$runningData
   )


   estimatedProfileProbability = rbind(
     estimatedProfileProbability,
     result[[1]]$estimatedProfileProbability,
     result[[2]]$estimatedProfileProbability,
     result[[3]]$estimatedProfileProbability,
     result[[4]]$estimatedProfileProbability
   )


   calibration = calibration + 1

}

save(estimatedParameters, estimatedProfileProbability, runningData, calibrationData, simDataList, file= paste0("rep_", arrayNumber,".RData"))







#[NEXT] coding summary of the results







