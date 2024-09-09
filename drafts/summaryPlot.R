rm(list=ls())
library(ggplot2)

# setwd("drafts/result")

# list files in directory
directoryFiles = dir()
nCondition = 96
nReplicationsPerCondition = 5


# list only simulation results files
# repFiles = directoryFiles[grep(pattern = "rep\\_", x = directoryFiles)]
repFiles = paste0("rep_", 1:(nCondition*nReplicationsPerCondition),".RData")


# check incomplete files
total = c(1:480)
completeFiles = as.numeric(gsub("\\D", '', repFiles))
incompleteFiles = total[which(!total %in% completeFiles)]


# calculate measurement accuracy =================================================
# calculate bias, rmse, exposure rate, profile recovery rate (list by arrayNumber)
nAttributes = 3
nProfiles = 2^nAttributes

profileMatrix = NULL
for (i in 0:(nProfiles - 1)){
  profileMatrix = rbind(
    profileMatrix,
    dec2bin(decimal_number = i, nattributes = nAttributes, basevector = rep(2, nAttributes))
  )
}


results = list()
for (file in 1:length(repFiles)){


  fileName = repFiles[[file]]
  # fileName = repFiles[grep(paste0("rep_", file, ".RData"), repFiles)]
  load(file = fileName)



  # get the number of calibration
  calibrationF = as.factor(estimatedParameters$calibration)
  calibration = as.numeric(levels(calibrationF))

  # extract nStudents
  nStudents = nrow(simDataList$simData)

  # extract runningData except simulated response
  running = runningData[(nStudents+1):nrow(runningData), ]

  # extract nNewStudents
  nNewStudents =  max(estimatedProfileProbability[,"student"])


  betaInterceptBias = NULL
  betaLambdaBias = NULL
  varInterceptBias = NULL
  varLambdaBias = NULL
  betaInterceptRMSE = NULL
  betaLambdaRMSE = NULL
  varInterceptRMSE = NULL
  varLambdaRMSE = NULL
  exposure = NULL
  exposureRateMean = NULL
  # exposureRateMax = NULL
  # unUsedItemN = NULL
  chiSquare = NULL
  profileRecoveryRate = NULL
  attributeRecoveryRate = NULL

  for (nCalibration in 1:length(calibration)) {


    # extract assigned estimatedParameter data with nCalibration
    dat = estimatedParameters[which(estimatedParameters$calibration == nCalibration), ]

    # grep a parameter row
    betaInterceptRow = grep("^beta_intercept\\[", dat$parameter)
    betaLambdaRow = grep("^beta_lambda\\[", dat$parameter)
    varInterceptRow = grep("^var_intercept", dat$parameter)
    varLambdaRow = grep("^var_lambda", dat$parameter)



    # BIAS and RMSE ============================================================
    # get deviation: eap - trueValues
    deviation = dat$eap- dat$trueValues

    # # get bias
    # betaInterceptBias[nCalibration] = mean(deviation[betaInterceptRow])
    # betaLambdaBias[nCalibration] = mean(deviation[betaLambdaRow])
    # varInterceptBias[nCalibration] = mean(deviation[varInterceptRow])
    # varLambdaBias[nCalibration] = mean(deviation[varLambdaRow])

    # get absolute bias
    betaInterceptBias[nCalibration] = mean(abs(deviation[betaInterceptRow]))
    betaLambdaBias[nCalibration] = mean(abs(deviation[betaLambdaRow]))
    varInterceptBias[nCalibration] = mean(abs(deviation[varInterceptRow]))
    varLambdaBias[nCalibration] = mean(abs(deviation[varLambdaRow]))

    # get rmse
    betaInterceptRMSE[nCalibration] = sqrt(mean(deviation[betaInterceptRow]^2))
    betaLambdaRMSE[nCalibration] = sqrt(mean(deviation[betaLambdaRow]^2))
    varInterceptRMSE[nCalibration] = sqrt(mean(deviation[varInterceptRow]^2))
    varLambdaRMSE[nCalibration] = sqrt(mean(deviation[varLambdaRow]^2))




    # Exposure Rate ============================================================
    first = nNewStudents * (nCalibration-1) + 1
    last = nNewStudents * nCalibration

    # extract response data for the nCalibration
    response = running[first:last, ]

    # get exposure rate
    expRate = colSums(!is.na(response))/nNewStudents
    expRateMean = mean(expRate)

    exposureRateMean[nCalibration] = expRateMean
    # exposureRateMax[nCalibration] = max(expRate)
    # unUsedItemN[nCalibration] = length(which(expRate == 0))
    chiSquare[nCalibration] = sum((expRate - expRateMean)^2/expRateMean) # Lin & Chang (2019)



    # Recovery Rate =============================================
    profiles = estimatedProfileProbability[which(estimatedProfileProbability$calibration == nCalibration), ]

    # 1. profile recovery rate
    nSame = length(which(profiles$profile == profiles$trueProfile))
    profileRecoveryRate[nCalibration] = nSame/nNewStudents


    # 2. attribute pattern recovery rate
    estimatedPattern = profileMatrix[profiles$profile,]
    truePattern = profileMatrix[profiles$trueProfile,]

    nSamePattern = matrix(NA, ncol= ncol(estimatedPattern), nrow = nrow(estimatedPattern))
    for (i in 1:ncol(estimatedPattern)){
      nSamePattern[,i] = estimatedPattern[,i] - truePattern[,i]
    }

    nSameAttribute = length(which(nSamePattern == 0))
    attributeRecoveryRate[nCalibration] = nSameAttribute / length(nSamePattern)




  }

  results[[file]] = data.frame(betaInterceptBias = betaInterceptBias, betaLambdaBias = betaLambdaBias,
                               varInterceptBias = varInterceptBias, varLambdaBias = varLambdaBias,
                               betaInterceptRMSE = betaInterceptRMSE, betaLambdaRMSE = betaLambdaRMSE,
                               varInterceptRMSE = varInterceptRMSE, varLambdaRMSE = varLambdaRMSE,
                               exposureRateMean = exposureRateMean,  chiSquare = chiSquare,
                               # unUsedItemN = unUsedItemN,exposureRateMax = exposureRateMax,
                               profileRecoveryRate = profileRecoveryRate, attributeRecoveryRate = attributeRecoveryRate)

}

betaInterceptBiasL = NULL
betaLambdaBiasL = NULL
varInterceptBiasL = NULL
varLambdaBiasL = NULL

betaInterceptRmseL = NULL
betaLambdaRmseL = NULL
varInterceptRmseL = NULL
varLambdaRmseL = NULL

exposureRateL = NULL
exposureChisquareL = NULL
profileRecoveryL = NULL
attributeRecoveryL = NULL

for (arrayNumber in 1:length(repFiles)){

  conditionN = ceiling(arrayNumber/nReplicationsPerCondition)
  replicationN = arrayNumber %% nReplicationsPerCondition
  replicationN[which(replicationN %in% 0)] = nReplicationsPerCondition

  # Bias =======================
  betaInterceptB = cbind(results[[arrayNumber]]$betaInterceptBias, conditionN, replicationN, arrayNumber, as.numeric(rownames(results[[arrayNumber]])))
  betaLambdaB = cbind(results[[arrayNumber]]$betaLambdaBias, conditionN, replicationN, arrayNumber, as.numeric(rownames(results[[arrayNumber]])))
  varInterceptB = cbind(results[[arrayNumber]]$varInterceptBias, conditionN, replicationN, arrayNumber, as.numeric(rownames(results[[arrayNumber]])))
  varLambdaB = cbind(results[[arrayNumber]]$varLambdaBias, conditionN, replicationN, arrayNumber, as.numeric(rownames(results[[arrayNumber]])))

  # Bias long data
  betaInterceptBiasL = rbind(betaInterceptBiasL, betaInterceptB)
  betaLambdaBiasL = rbind(betaLambdaBiasL, betaLambdaB)
  varInterceptBiasL = rbind(varInterceptBiasL, varInterceptB)
  varLambdaBiasL = rbind(varLambdaBiasL, varLambdaB)


  # Rmse =======================
  betaInterceptR = cbind(results[[arrayNumber]]$betaInterceptRMSE, conditionN, replicationN, arrayNumber, as.numeric(rownames(results[[arrayNumber]])))
  betaLambdaR = cbind(results[[arrayNumber]]$betaLambdaRMSE, conditionN, replicationN, arrayNumber, as.numeric(rownames(results[[arrayNumber]])))
  varInterceptR = cbind(results[[arrayNumber]]$varInterceptRMSE, conditionN, replicationN, arrayNumber, as.numeric(rownames(results[[arrayNumber]])))
  varLambdaR = cbind(results[[arrayNumber]]$varLambdaRMSE, conditionN, replicationN, arrayNumber, as.numeric(rownames(results[[arrayNumber]])))

  # Rmse long data
  betaInterceptRmseL = rbind(betaInterceptRmseL, betaInterceptR)
  betaLambdaRmseL = rbind(betaLambdaRmseL, betaLambdaR)
  varInterceptRmseL = rbind(varInterceptRmseL, varInterceptR)
  varLambdaRmseL = rbind(varLambdaRmseL, varLambdaR)


  # exposure rate ==============
  expMean = cbind(results[[arrayNumber]]$exposureRateMean, conditionN, replicationN, arrayNumber, as.numeric(rownames(results[[arrayNumber]])))
  expChisquare = cbind(results[[arrayNumber]]$chiSquare, conditionN, replicationN, arrayNumber, as.numeric(rownames(results[[arrayNumber]])))

  # exposure rate long data
  exposureRateL = rbind(exposureRateL, expMean)
  exposureChisquareL = rbind(exposureChisquareL, expChisquare)



  # Recovery Rate ==============
  profileR = cbind(results[[arrayNumber]]$profileRecoveryRate, conditionN, replicationN, arrayNumber, as.numeric(rownames(results[[arrayNumber]])))
  attributeR = cbind(results[[arrayNumber]]$attributeRecoveryRate, conditionN, replicationN, arrayNumber, as.numeric(rownames(results[[arrayNumber]])))

  profileRecoveryL = rbind(profileRecoveryL, profileR)
  attributeRecoveryL = rbind(attributeRecoveryL, attributeR)


}


# change to data frame
betaInterceptBiasL = as.data.frame(betaInterceptBiasL)
betaLambdaBiasL  = as.data.frame(betaLambdaBiasL)
varInterceptBiasL  = as.data.frame(varInterceptBiasL)
varLambdaBiasL   = as.data.frame(varLambdaBiasL)

betaInterceptRmseL = as.data.frame(betaInterceptRmseL)
betaLambdaRmseL   = as.data.frame(betaLambdaRmseL)
varInterceptRmseL  = as.data.frame(varInterceptRmseL)
varLambdaRmseL   = as.data.frame(varLambdaRmseL)

exposureRateL  = as.data.frame(exposureRateL)
exposureChisquareL   = as.data.frame(exposureChisquareL)
profileRecoveryL  = as.data.frame(profileRecoveryL)
attributeRecoveryL   = as.data.frame(attributeRecoveryL)


colnames(betaInterceptBiasL) = c("value", "conditionN", "replicationN", "arrayNumber", "calibrationN")
colnames(betaLambdaBiasL) = c("value", "conditionN", "replicationN", "arrayNumber", "calibrationN")
colnames(varInterceptBiasL) = c("value", "conditionN", "replicationN", "arrayNumber", "calibrationN")
colnames(varLambdaBiasL) = c("value", "conditionN", "replicationN", "arrayNumber", "calibrationN")

colnames(betaInterceptRmseL) = c("value", "conditionN", "replicationN", "arrayNumber", "calibrationN")
colnames(betaLambdaRmseL) = c("value", "conditionN", "replicationN", "arrayNumber", "calibrationN")
colnames(varInterceptRmseL) = c("value", "conditionN", "replicationN", "arrayNumber", "calibrationN")
colnames(varLambdaRmseL) = c("value", "conditionN", "replicationN", "arrayNumber", "calibrationN")

colnames(exposureRateL) = c("value", "conditionN", "replicationN", "arrayNumber", "calibrationN")
colnames(exposureChisquareL) = c("value", "conditionN", "replicationN", "arrayNumber", "calibrationN")
colnames(profileRecoveryL) = c("value", "conditionN", "replicationN", "arrayNumber", "calibrationN")
colnames(attributeRecoveryL) = c("value", "conditionN", "replicationN", "arrayNumber", "calibrationN")


# draw plots & get summary statistics ===================================================================
criteria = list(betaInterceptBiasL, betaLambdaBiasL, varInterceptBiasL, varLambdaBiasL, betaInterceptRmseL ,betaLambdaRmseL,
                varInterceptRmseL, varLambdaRmseL, exposureRateL, exposureChisquareL, profileRecoveryL, attributeRecoveryL)
criteriaName = c("betaInterceptBias", "betaLambdaBias", "varInterceptBias", "varLambdaBias", "betaInterceptRmse", "betaLambdaRmse",
                 "varInterceptRmse", "varLambdaRmse", "exposureRate", "exposureChisquare", "profileRecovery", "attributeRecovery")

pdf("plots.pdf")
descriptiveStatistics = NULL
for (cri in 1:length(criteria)) {

  # choose one criterion among criteriaN
  criterion = criteria[[cri]]

  for (condition in 1:nCondition) {

    # choose one condition in the criteria
    criterionCond = criterion[which(criterion$conditionN == condition),]



    if (cri %in% c(4, 8)){
      # varLambda bias, rmse
      boxplot(value ~ calibrationN,
              data = criterionCond,
              xlab = "# of Calibration",
              ylab = criteriaName[cri],
              ylim = c(0, 12),
              main = paste0("condition", condition)
      )
    } else if (cri %in% c(10)){
      # exposure chi square
      boxplot(value ~ calibrationN,
              data = criterionCond,
              xlab = "# of Calibration",
              ylab = criteriaName[cri],
              ylim = c(0, 30),
              main = paste0("condition", condition)
      )
    } else {
      # draw a box plot
      boxplot(value ~ calibrationN,
              data = criterionCond,
              xlab = "# of Calibration",
              ylab = criteriaName[cri],
              ylim  = c(0, 1),
              main = paste0("condition", condition)
      )
    }



     # summary statistics
    for (nCalibration in 1:max(criterionCond$calibrationN)){

      # choose one calibration
      caliN = criterionCond[criterionCond$calibrationN==nCalibration,]
      quan = as.data.frame(t(quantile(caliN$value)))
      quan$mean = mean(caliN$value)
      quan$sd = sd(caliN$value)
      quan$variable = criteriaName[cri]
      quan$conditionN = condition
      quan$calibrationN = nCalibration

      descriptiveStatistics = rbind(descriptiveStatistics, quan)

    }
  }
}

dev.off()




# Summary Statistics by Factors ==============================================
# create conditions list
conditions = list(
  nItemsInPool = c(40, 105),
  initialPilotSampleSize = c(30, 300, 2000),
  nNewStudents = c(30),
  itemUpdateFunction = c(
    "Sample 1",
    "Sample 10",
    "EAP",
    "Mode"
  ),
  itemSummaryFunction = c(
    "Sample 1",
    "Sample 10",
    "EAP",
    "Mode"
  ),
  stopCriterion = c(.7, .8)
)

# number of conditions
nConditions = prod(unlist(lapply(X = conditions, FUN = length)))

conditionsMatrix = matrix(NA, nrow = nConditions, ncol = length(conditions))
colnames(conditionsMatrix) = names(conditions)

cond = 1
for (cond in 1:nConditions){
  conditionsMatrix[cond,] = dec2bin(
    decimal_number = cond - 1,
    nattributes = length(conditions),
    basevector = unlist(lapply(X = conditions, FUN = length))
  ) + 1
}

# Function for summarizing results by factor ===================================
summaryByFactor = function (criteria, conditionsMatrix) {

  criteria = as.data.frame(criteria)
  statistics = NULL
  for (calibrationN in 1:40) {
    nCalibration = criteria[which(criteria$calibrationN == calibrationN), c("value","conditionN","arrayNumber","calibrationN")]

    # stop criteria
    stop1 = which(conditionsMatrix[,"stopCriterion"] == 1)
    stop2 = which(conditionsMatrix[,"stopCriterion"] == 2)
    stop1.mean = mean(nCalibration[which(nCalibration$conditionN %in% stop1),"value"])
    stop2.mean = mean(nCalibration[which(nCalibration$conditionN %in% stop2),"value"])
    stop = cbind(stop1.mean, stop2.mean)

    # item summary function
    summary1 = which(conditionsMatrix[,"itemSummaryFunction"] == 1)
    summary2 = which(conditionsMatrix[,"itemSummaryFunction"] == 2)
    summary3 = which(conditionsMatrix[,"itemSummaryFunction"] == 3)
    summary4 = which(conditionsMatrix[,"itemSummaryFunction"] == 4)
    summary1.mean = mean(nCalibration[which(nCalibration$conditionN %in% summary1), "value"])
    summary2.mean = mean(nCalibration[which(nCalibration$conditionN %in% summary2), "value"])
    summary3.mean = mean(nCalibration[which(nCalibration$conditionN %in% summary3), "value"])
    summary4.mean = mean(nCalibration[which(nCalibration$conditionN %in% summary4), "value"])
    summary = cbind(summary1.mean, summary2.mean, summary3.mean, summary4.mean)

    # item update function
    update1 = which(conditionsMatrix[,"itemUpdateFunction"] == 1)
    update2 = which(conditionsMatrix[,"itemUpdateFunction"] == 2)
    update3 = which(conditionsMatrix[,"itemUpdateFunction"] == 3)
    update4 = which(conditionsMatrix[,"itemUpdateFunction"] == 4)
    update1.mean = mean(nCalibration[which(nCalibration$conditionN %in% update1), "value"])
    update2.mean = mean(nCalibration[which(nCalibration$conditionN %in% update2), "value"])
    update3.mean = mean(nCalibration[which(nCalibration$conditionN %in% update3), "value"])
    update4.mean = mean(nCalibration[which(nCalibration$conditionN %in% update4), "value"])
    update = cbind(update1.mean, update2.mean, update3.mean, update4.mean)

    # initial pilot sample size
    size1 = which(conditionsMatrix[,"initialPilotSampleSize"] == 1)
    size2 = which(conditionsMatrix[,"initialPilotSampleSize"] == 2)
    size3 = which(conditionsMatrix[,"initialPilotSampleSize"] == 3)
    size1.mean = mean(nCalibration[which(nCalibration$conditionN %in% size1), "value"])
    size2.mean = mean(nCalibration[which(nCalibration$conditionN %in% size2), "value"])
    size3.mean = mean(nCalibration[which(nCalibration$conditionN %in% size3), "value"])
    size = cbind(size1.mean, size2.mean, size3.mean)

    stat = cbind(stop, summary, update, size)
    statistics = rbind(statistics, stat)
  }

  return(statistics)

}

# Functions for plotting by factor =============================================
plotByFactor = function(criteria, criteriaName, conditionsMatrix) {

  # get statistics by factor
  statistics = summaryByFactor(criteria = criteria, conditionsMatrix = conditionsMatrix)

  par(mfrow = c(2,2))
  # stop criteria =======================
  stop = statistics[,c("stop1.mean","stop2.mean")]
  matplot(stop,
          type="l",
          ylab=criteriaName,
          main="Stop Criteria",
          col=1:2,
          lty=1
          )

  legend("topright",
         legend=c("0.7","0.8"),
         col=1:2,
         lty=1
         )


  # item summary function ====================
  summary = statistics[,c("summary1.mean","summary2.mean","summary3.mean","summary4.mean")]
  matplot(summary,
          type="l",
          ylab=criteriaName,
          main="Item Summary Function",
          col=1:4,
          lty=1
          )

  legend("topright",
         legend=c("Sample 1","Sample 10", "Mean", "Mode"),
         col = 1:4,
         lty = 1
         )


  # item update function =====================
  update = statistics[,c("update1.mean","update2.mean","update3.mean","update4.mean")]
  matplot(update,
          type="l",
          ylab=criteriaName,
          main="Item Update Function",
          col=1:4,
          lty=1
          )

  legend("topright",
         legend=c("Sample 1","Sample 10", "Mean", "Mode"),
         col = 1:4,
         lty = 1
         )


  # pilot sample size function ================
  pilot = statistics[,c("size1.mean","size2.mean","size3.mean")]
  matplot(pilot,
          type="l",
          ylab=criteriaName,
          main="Pilot Sample Size",
          col=1:3,
          lty=1
          )

  legend("topright",
         legend=c("30","300", "2000"),
         col = 1:3,
         lty = 1
         )


}

pdf("plotsByFactor.pdf")
for (i in 1:length(criteria)){
  plotByFactor(criteria = criteria[i],criteriaName = criteriaName[i], conditionsMatrix=conditionsMatrix)
}
dev.off()




# # check maxRhat
# maxrhat = NULL
# for (i in 1:40) {
#   maxrhat= c(maxrhat, calibrationData[[i]]$maxRhat)
# }
# plot(maxrhat)
# abline(a=1.1, b=0)




