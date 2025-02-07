itemUpdateProfileProbability_EAP = function(response, itemNumber, itemProbArray, currentProfileProbablity, nProfiles,
                                            nUpdateSamples, itemSample){

  updatedProfileProbability = matrix(NA, nrow = 1, ncol = nProfiles)

  for (profile in 1:nProfiles){
    updatedProfileProbability[profile] =
      currentProfileProbablity[profile] * mean(itemProbArray[, profile, (response + 1), itemNumber, drop=FALSE], na.rm=TRUE)
  }

  return(list(updatedProfileProbability = updatedProfileProbability,
              itemSample = itemSample)
  )
}
