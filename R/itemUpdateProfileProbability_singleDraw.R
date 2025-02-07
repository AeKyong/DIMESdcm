itemUpdateProfileProbability_singleDraw = function(response, itemNumber, itemProbArray, currentProfileProbablity, nProfiles,
                                                   nUpdateSamples, itemSample){

  if (response ==0) {
    itemSample = sample(x = 1:nrow(itemProbArray), size = 1, replace = FALSE)
  } else {
    itemSample = itemSample
  }

  updatedProfileProbability = matrix(NA, nrow = 1, ncol = nProfiles)

  for (profile in 1:nProfiles){
    updatedProfileProbability[profile] =
      currentProfileProbablity[profile] * itemProbArray[itemSample, profile, (response + 1), itemNumber, drop=FALSE]
  }

   return(list(updatedProfileProbability = updatedProfileProbability,
              itemSample = itemSample)
         )
}
