itemUpdateProfileProbability_Mode = function(response, itemNumber, itemProbArray, currentProfileProbablity, nProfiles,
                                             nUpdateSamples, itemSample){

  updatedProfileProbability = matrix(NA, nrow = 1, ncol = nProfiles)

  for (profile in 1:nProfiles){
    itemMode = modeest::mlv(itemProbArray[, profile, (response + 1), itemNumber, drop=FALSE], method="naive")
    if (length(itemMode)>1){
      choose = sample(1:length(itemMode), size = 1, replace =FALSE)
      itemMode = itemMode[choose]
    }
    updatedProfileProbability[profile] = currentProfileProbablity[profile] * itemMode
  }
  return(list(updatedProfileProbability = updatedProfileProbability,
              itemSample = itemSample)
  )
}
