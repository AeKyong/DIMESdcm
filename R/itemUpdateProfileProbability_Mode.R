itemUpdateProfileProbability_Mode = function(response, itemNumber, itemProbArray, currentProfileProbablity, nProfiles){

  updatedProfileProbability = currentProfileProbablity

  for (profile in 1:nProfiles){
    updatedProfileProbability[profile] =
      currentProfileProbablity[profile] * modeest::mlv(itemProbArray[, profile, (response + 1), itemNumber, drop=FALSE], method="naive")
  }
  return(updatedProfileProbability)
}
