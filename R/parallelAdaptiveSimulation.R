parallelAdaptiveSimulation = function(thread, threadIDs,tempEstimatedParameters, calibration,
                                      nProfiles,abilityQ, itemcovs, nItems, profileMatrix, startingProfileProbablity, itemPool, maxItems,
                                      trueParameters, trueProfiles, itemProbArray, itemUpdateFunction, itemSummaryFunction,
                                      nItemSamples, nUpdateSamples, stopCriterion, calculateSHE){


  nStudents = length(threadIDs[[thread]])
  runningData = NULL
  estimatedProfiles = NULL

  profileMatrix = NULL
  for (i in 0:(nProfiles - 1)){
    profileMatrix = rbind(
      profileMatrix,
      dec2bin(decimal_number = i, nattributes = nAttributes, basevector = rep(2, nAttributes))
    )
  }


  for (student in 1:nStudents){
    pctComplete = round((student/nStudents)*100, 1)
    cat(paste0("Core ", thread, ": ", pctComplete, "% complete\n"))


    testDraw = adaptiveSimulation(nProfiles = nProfiles,
                                  abilityQ = simDataList$simAbilityQ,
                                  itemcovs = simDataList$simItemCovs,
                                  nItems = nItems,
                                  profileMatrix= profileMatrix,
                                  startingProfileProbablity = currentProfileProbablity,
                                  itemPool = itemPool,
                                  maxItems = simulationSpecs$maxItems,
                                  trueParameters = trueParameters,
                                  trueProfiles = trueProfiles[threadIDs[[thread]][student]],
                                  itemProbArray = itemProbArray,
                                  itemUpdateFunction = simulationSpecs$itemUpdateFunction,
                                  itemSummaryFunction = simulationSpecs$itemSummaryFunction,
                                  nItemSamples = simulationSpecs$nItemSamples,
                                  nUpdateSamples = simulationSpecs$nUpdateSamples,
                                  stopCriterion = simulationSpecs$stopCriterion,
                                  calculateSHE = simulationSpecs$calculateSHE)



    responses = testDraw$responseVector
    items = rownames(testDraw$responseVector)

    # add response vector to running data
    responses = responses[1:nItems]

    locations = as.numeric(substr(items, start = 2, stop = nchar(items)-1))
    runningData = rbind(runningData, matrix(data = NA, nrow = 1, ncol = nItems))
    for (loc in 1:length(locations)){
      runningData[nrow(runningData), locations[loc]] = t(responses)[loc]
    }


    # change estimated profiles to attributes
    profile2attribute = as.data.frame(profileMatrix)
    profile2attribute$jointProb = testDraw$currentProfileProbablity
    attributeMAPs = matrix(NA, ncol = ncol(profileMatrix))
    for (att in 1:ncol(profileMatrix)){
      attribute = sum(profile2attribute[profile2attribute[,att]==1,"jointProb"])
      attribute = ifelse(attribute>=.5, 1, 0)
      attributeMAPs[,att] = attribute
    }
    colnames(attributeMAPs) = paste0("attributeMAP", 1:ncol(profileMatrix))

    # change true profile to attributes
    trueAttributes = matrix(NA, ncol = 3)
    for(att in 1:ncol(profileMatrix)){
      trueAttributes[,att] = profileMatrix[trueProfiles[threadIDs[[thread]][student]],att]
    }
    colnames(trueAttributes) = paste0("trueAttribute", 1:ncol(profileMatrix))


    # the respondent's profile and attribute
    profileTemp = as.data.frame(t(testDraw$currentProfileProbablity))
    colnames(profileTemp) = paste0("profileEAP", 1:length(testDraw$currentProfileProbablity))

    profileTemp$profileMAP = testDraw$currentProfile
    profileTemp = cbind(profileTemp, as.data.frame(attributeMAPs))
    profileTemp$student = threadIDs[[thread]][student]
    profileTemp$calibration = calibration
    profileTemp$trueProfile = trueProfiles[threadIDs[[thread]][student]]
    profileTemp = cbind(profileTemp, trueAttributes)

    estimatedProfiles = rbind(estimatedProfiles, profileTemp)
  }


return(list(estimatedProfiles = estimatedProfiles,
            runningData = runningData))


}
