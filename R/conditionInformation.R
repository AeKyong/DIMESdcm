conditionInformation = function(arrayNumber, nReplicationsPerCondition = 100, nCores = 4) {
  # convert array number to condition number
  conditionNumber = floor((arrayNumber - 1)/nReplicationsPerCondition) + 1

  # create conditions list
  conditions = list(
    nItemsInPool = c(40, 105),
    initialPilotSampleSize = c(30, 300, 2000),
    nNewStudents = c(30),
    itemUpdateFunction = c(
      "Sample 1",
      "Sample 10",
      "Sample 8000",
      "EAP",
      "Mode"
    ),
    itemSummaryFunction = c(
      "Sample 1",
      "Sample 10",
      "Sample 8000",
      "EAP",
      "Mode"
    ),
    stopCriterion = c(.6, .8)
  )


  stopSampleSize = 1200


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

  # populate condition values
  nItemsInPool = conditions$nItemsInPool[conditionsMatrix[conditionNumber,1]]
  initialPilotSampleSize = conditions$initialPilotSampleSize[conditionsMatrix[conditionNumber,2]]
  nNewStudents = conditions$nNewStudents[conditionsMatrix[conditionNumber,3]]
  itemUpdateFunction = conditions$itemUpdateFunction[conditionsMatrix[conditionNumber,4]]
  itemSummaryFunction = conditions$itemSummaryFunction[conditionsMatrix[conditionNumber,5]]
  stopCriterion = conditions$stopCriterion[conditionsMatrix[conditionNumber,6]]


  # number of calibrations needed
  nCalibrations = stopSampleSize/nNewStudents





  # Non-standard evaluation itemUpdateFunction
  if (itemUpdateFunction == "Sample 1"){
    itemUpdateFunction = eval(quote(itemUpdateProfileProbability_singleDraw))
    calculateSHE = eval(quote(calculateSHEsingle))
    nUpdateSamples = 1
  } else if (itemUpdateFunction == "Sample 10"){
    itemUpdateFunction = eval(quote(itemUpdateProfileProbability_multipleDraws))
    calculateSHE = eval(quote(calculateSHEmultiple))
    nUpdateSamples = 10
  } else if (itemUpdateFunction == "Sample 8000"){
    itemUpdateFunction = eval(quote(itemUpdateProfileProbability_multipleDraws))
    calculateSHE = eval(quote(calculateSHEmultiple))
    nUpdateSamples = 8000
  } else if (itemUpdateFunction == "EAP"){
    itemUpdateFunction = eval(quote(itemUpdateProfileProbability_EAP))
    calculateSHE = eval(quote(calculateSHEsingle))
    nUpdateSamples = 1
  } else if (itemUpdateFunction == "Mode"){
    itemUpdateFunction = eval(quote(itemUpdateProfileProbability_Mode))
    calculateSHE = eval(quote(calculateSHEsingle))
    nUpdateSamples = 1
  }

  # Non-standard evaluation itemSummaryFunction
  if (itemSummaryFunction == "Sample 1"){
    itemSummaryFunction =
      eval(quote(itemSummary_singleDraw))
    nItemSamples = 1
  } else if (itemSummaryFunction ==  "Sample 10"){
    itemSummaryFunction =
      eval(quote(itemSummary_multipleDraws))
    nItemSamples = 10
  } else if (itemSummaryFunction ==  "Sample 8000"){
    itemSummaryFunction =
      eval(quote(itemSummary_multipleDraws))
    nItemSamples = 8000
  } else if (itemSummaryFunction ==  "EAP"){
    itemSummaryFunction =
      eval(quote(itemSummary_EAP))
    nItemSamples = 1
  } else if (itemSummaryFunction ==  "Mode"){
    itemSummaryFunction =
      eval(quote(itemSummary_Mode))
    nItemSamples = 1
  }


  # maxItem
  if (nItemsInPool < 105){
    maxItems = 30
  } else if (nItemsInPool >= 105) {
    maxItems = 30
  }





  threadIDs = list()
  leftOver = nNewStudents %% 4
  nPerThread =  (nNewStudents - leftOver)/4
  for (i in 1:4){
    threadIDs[[i]] = ((i - 1) * nPerThread + 1):((i - 1) * nPerThread + nPerThread)
  }
  if (leftOver > 0){
    for (i in 1:leftOver){
      threadIDs[[i]] = c(threadIDs[[i]], nNewStudents - leftOver + i)
    }
  }



  return(list(conditionNumber = conditionNumber, conditions = conditions, stopSampleSize = stopSampleSize,
              nItemsInPool = nItemsInPool, initialPilotSampleSize = initialPilotSampleSize,
              nNewStudents = nNewStudents, itemUpdateFunction = itemUpdateFunction,
              itemSummaryFunction = itemSummaryFunction, calculateSHE = calculateSHE, stopCriterion = stopCriterion,
              nCalibrations = nCalibrations, maxItems = maxItems, nUpdateSamples = nUpdateSamples, nItemSamples = nItemSamples,
              nCores = nCores, threadIDs = threadIDs)

  )

}
