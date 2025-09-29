itemUpdateProfileProbability_multipleDraws = function(response, itemNumber, itemProbArray, currentProfileProbablity, nProfiles,
                                                      nUpdateSamples, itemSample){
# browser()
  if (response == 0) {
    itemSample = sample(x = 1:nrow(itemProbArray), size = nUpdateSamples, replace = FALSE)
  } else {
    itemSample = itemSample
  }


  updatedProfileProbability = matrix(NA, nrow = nUpdateSamples, ncol = nProfiles)

  if(nUpdateSamples != nrow(itemProbArray)){
    for (sample in 1:nUpdateSamples) {
      for (profile in 1:nProfiles){
        updatedProfileProbability[sample, profile] =
          currentProfileProbablity[profile] * itemProbArray[itemSample[sample], profile, (response + 1), itemNumber, drop=FALSE]
      }
    }
  } else {
    for (profile in 1:nProfiles){
      updatedProfileProbability[, profile] =
        currentProfileProbablity[profile] * itemProbArray[, profile, (response + 1), itemNumber, drop=FALSE]
    }
  }


  return(list(updatedProfileProbability = updatedProfileProbability,
              itemSample = itemSample)
  )


}
