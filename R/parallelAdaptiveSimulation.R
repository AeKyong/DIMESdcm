parallelAdaptiveSimulation = function(thread, threadIDs,tempEstimatedParameters, calibration,
                                      nProfiles,abilityQ, itemcovs, nItems, profileMatrix, startingProfileProbablity,itemPool,maxItems,
                                      trueParameters, trueProfiles,itemProbArray,itemQuantiles,
                                      itemUpdateFunction, itemSummaryFunction, nItemSamples, stopCriterion, calculateSHE){


  nStudents = length(threadIDs[[thread]])
  runningData = NULL
  estimatedProfileProbability = NULL


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
                                  itemQuantiles = itemQuantiles,
                                  itemUpdateFunction = simulationSpecs$itemUpdateFunction,
                                  itemSummaryFunction = simulationSpecs$itemSummaryFunction,
                                  nItemSamples = simulationSpecs$nItemSamples,
                                  stopCriterion = simulationSpecs$stopCriterion,
                                  calculateSHE = simulationSpecs$calculateSHE)



    responses = testDraw$responseVector
    items = rownames(testDraw$responseVector)

    # add response vector to data frame
    responses = responses[1:nItems]

    locations = as.numeric(substr(items, start = 2, stop = nchar(items)-1))
    runningData = rbind(runningData, matrix(data = NA, nrow = 1, ncol = nItems))
    for (loc in 1:length(locations)){
      runningData[nrow(runningData), locations[loc]] = t(responses)[loc]
    }



    profileProbabilityTemp = as.data.frame(t(testDraw$currentProfileProbablity))
    colnames(profileProbabilityTemp) = paste0("profile", 1:length(testDraw$currentProfileProbablity))

    profileProbabilityTemp$profile = testDraw$currentProfile
    profileProbabilityTemp$student = threadIDs[[thread]][student]
    profileProbabilityTemp$calibration = calibration
    profileProbabilityTemp$trueProfile = trueProfiles[threadIDs[[thread]][student]]

    estimatedProfileProbability = rbind(estimatedProfileProbability, profileProbabilityTemp)



  }


return(list(estimatedProfileProbability = estimatedProfileProbability,
            runningData = runningData))


}
