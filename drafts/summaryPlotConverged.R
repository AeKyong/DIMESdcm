rm(list=ls())
library(ggplot2)

setwd("drafts/result")

# list files in directory
directoryFiles = dir()
nCondition = 160
nReplicationsPerCondition = 5


# list only simulation results files
# repFiles = directoryFiles[grep(pattern = "rep\\_", x = directoryFiles)]
repFiles = paste0("rep_", 1:(0+nCondition*nReplicationsPerCondition),".RData")


# # check incomplete files
# total = c(1:480)
# completeFiles = as.numeric(gsub("\\D", '', repFiles))
# incompleteFiles = total[which(!total %in% completeFiles)]


nAttributes = 3
nProfiles = 2^nAttributes

profileMatrix = NULL
for (i in 0:(nProfiles - 1)){
  profileMatrix = rbind(
    profileMatrix,
    dec2bin(decimal_number = i, nattributes = nAttributes, basevector = rep(2, nAttributes))
  )
}




# calculate measurement accuracy =================================================
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
  unconvg = NULL
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



    # maxRhat ===================================================
    if(calibrationData[[nCalibration]]$maxRhat>1.1){
      unconvg[nCalibration] = 1
    }else{
      unconvg[nCalibration] = 0
    }

  }


  resultsDF = data.frame(betaInterceptBias = betaInterceptBias, betaLambdaBias = betaLambdaBias,
                varInterceptBias = varInterceptBias, varLambdaBias = varLambdaBias,
                betaInterceptRMSE = betaInterceptRMSE, betaLambdaRMSE = betaLambdaRMSE,
                varInterceptRMSE = varInterceptRMSE, varLambdaRMSE = varLambdaRMSE,
                exposureRateMean = exposureRateMean,  chiSquare = chiSquare,
                # unUsedItemN = unUsedItemN,exposureRateMax = exposureRateMax,
                profileRecoveryRate = profileRecoveryRate, attributeRecoveryRate = attributeRecoveryRate,
                unconvg = unconvg)

  resultsDfconvg = resultsDF

  # remove value of unconverged calibration
  resultsDfconvg[which(resultsDF$unconvg == 1), !colnames(resultsDF) %in% c("unconvg")] = NA

  results[[file]] = resultsDfconvg

}



# transform results in a long format============================================
resultsLong= NULL
for (arrayNumber in 1:length(results)){

    result = results[[arrayNumber]]
    conditionN = ceiling(arrayNumber/nReplicationsPerCondition)
    replicationN = arrayNumber %% nReplicationsPerCondition
    replicationN[which(replicationN %in% 0)] = nReplicationsPerCondition
    calibrationN = as.numeric(rownames(result))

    result = cbind(result, conditionN, replicationN, arrayNumber, calibrationN)
    resultsLong = rbind(resultsLong, result)
}



# separate long format by criteria =============================================
criteriaName = c("betaInterceptBias", "betaLambdaBias", "varInterceptBias", "varLambdaBias",
                 "betaInterceptRMSE", "betaLambdaRMSE", "varInterceptRMSE", "varLambdaRMSE",
                 "exposureRateMean", "chiSquare", "profileRecoveryRate", "attributeRecoveryRate")

criteria = list()
for (i in 1:length(criteriaName)){
  criterion = resultsLong[,c(criteriaName[i],"conditionN","replicationN","arrayNumber","calibrationN")]
  colnames(criterion) = c("value","conditionN","replicationN","arrayNumber","calibrationN")

  criteria[[i]] = as.data.frame(criterion)
}





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
summaryByFactor = function (criterion, conditionsMatrix) {

  criterion = as.data.frame(criterion)
  statistics = NULL
  for (calibrationN in 1:40) {
    nCalibration = criterion[which(criterion$calibrationN == calibrationN), c("value","conditionN")]

    # stop criterion
    stop1 = which(conditionsMatrix[,"stopCriterion"] == 1)
    stop2 = which(conditionsMatrix[,"stopCriterion"] == 2)
    stop1.mean = mean(nCalibration[which(nCalibration$conditionN %in% stop1),"value"], na.rm=T)
    stop2.mean = mean(nCalibration[which(nCalibration$conditionN %in% stop2),"value"], na.rm=T)
    stop = cbind(stop1.mean, stop2.mean)

    # item summary function
    summary1 = which(conditionsMatrix[,"itemSummaryFunction"] == 1)
    summary2 = which(conditionsMatrix[,"itemSummaryFunction"] == 2)
    summary3 = which(conditionsMatrix[,"itemSummaryFunction"] == 3)
    summary4 = which(conditionsMatrix[,"itemSummaryFunction"] == 4)
    summary1.mean = mean(nCalibration[which(nCalibration$conditionN %in% summary1), "value"], na.rm=T)
    summary2.mean = mean(nCalibration[which(nCalibration$conditionN %in% summary2), "value"], na.rm=T)
    summary3.mean = mean(nCalibration[which(nCalibration$conditionN %in% summary3), "value"], na.rm=T)
    summary4.mean = mean(nCalibration[which(nCalibration$conditionN %in% summary4), "value"], na.rm=T)
    summary = cbind(summary1.mean, summary2.mean, summary3.mean, summary4.mean)

    # item update function
    update1 = which(conditionsMatrix[,"itemUpdateFunction"] == 1)
    update2 = which(conditionsMatrix[,"itemUpdateFunction"] == 2)
    update3 = which(conditionsMatrix[,"itemUpdateFunction"] == 3)
    update4 = which(conditionsMatrix[,"itemUpdateFunction"] == 4)
    update1.mean = mean(nCalibration[which(nCalibration$conditionN %in% update1), "value"], na.rm=T)
    update2.mean = mean(nCalibration[which(nCalibration$conditionN %in% update2), "value"], na.rm=T)
    update3.mean = mean(nCalibration[which(nCalibration$conditionN %in% update3), "value"], na.rm=T)
    update4.mean = mean(nCalibration[which(nCalibration$conditionN %in% update4), "value"], na.rm=T)
    update = cbind(update1.mean, update2.mean, update3.mean, update4.mean)

    # initial pilot sample size
    size1 = which(conditionsMatrix[,"initialPilotSampleSize"] == 1)
    size2 = which(conditionsMatrix[,"initialPilotSampleSize"] == 2)
    size3 = which(conditionsMatrix[,"initialPilotSampleSize"] == 3)
    size1.mean = mean(nCalibration[which(nCalibration$conditionN %in% size1), "value"], na.rm=T)
    size2.mean = mean(nCalibration[which(nCalibration$conditionN %in% size2), "value"], na.rm=T)
    size3.mean = mean(nCalibration[which(nCalibration$conditionN %in% size3), "value"], na.rm=T)
    size = cbind(size1.mean, size2.mean, size3.mean)

    # item pool size
    pool1 = which(conditionsMatrix[,"nItemsInPool"] == 1)
    pool2 = which(conditionsMatrix[,"nItemsInPool"] == 2)
    pool1.mean = mean(nCalibration[which(nCalibration$conditionN %in% pool1), "value"], na.rm=T)
    pool2.mean = mean(nCalibration[which(nCalibration$conditionN %in% pool2), "value"], na.rm=T)
    pool = cbind(pool1.mean, pool2.mean)

    stat = cbind(stop, summary, update, size, pool)
    statistics = rbind(statistics, stat)
  }

  return(statistics)

}

# Functions for plotting by factor =============================================
plotByFactor = function(criterion, criterionName, conditionsMatrix) {

  # get statistics by factor
  statistics = summaryByFactor(criterion = criterion, conditionsMatrix = conditionsMatrix)

  par(mfrow = c(2,3))
  # stop criteria =======================
  stop = statistics[,c("stop1.mean","stop2.mean")]
  matplot(stop,
          type="l",
          ylab=criterionName,
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
          ylab=criterionName,
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
          ylab=criterionName,
          main="Item Selction Function",
          col=1:4,
          lty=1
  )

  legend("topright",
         legend=c("Sample 1","Sample 10", "Mean", "Mode"),
         col = 1:4,
         lty = 1
  )


  # pilot sample size function ================
  size = statistics[,c("size1.mean","size2.mean","size3.mean")]
  matplot(size,
          type="l",
          ylab=criterionName,
          main="Pilot Sample Size",
          col=1:3,
          lty=1
  )

  legend("topright",
         legend=c("30","300", "2000"),
         col = 1:3,
         lty = 1
  )


  # item pool size function ================
  pool = statistics[,c("pool1.mean","pool2.mean")]
  matplot(pool,
          type="l",
          ylab=criterionName,
          main="Item Pool Size",
          col=1:2,
          lty=1
  )

  legend("topright",
         legend=c("40", "105"),
         col = 1:2,
         lty = 1
  )

}



pdf("plots.pdf")
for (i in 1:length(criteria)){
  plotByFactor(criterion = criteria[i],criterionName = criteriaName[i], conditionsMatrix=conditionsMatrix)
}
dev.off()






# # check maxRhat
# maxrhat = NULL
# for (i in 1:40) {
#   maxrhat= c(maxrhat, calibrationData[[i]]$maxRhat)
# }
# plot(maxrhat)
# abline(a=1.1, b=0)

# copied rep: 313 391 397 434 463 477

