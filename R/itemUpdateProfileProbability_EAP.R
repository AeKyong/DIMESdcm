itemUpdateProfileProbability_EAP = function(response, itemNumber, itemProbArray, currentProfileProbablity, nProfiles){

  updatedProfileProbability = currentProfileProbablity

  for (profile in 1:nProfiles){
    updatedProfileProbability[profile] =
      currentProfileProbablity[profile] * mean(itemProbArray[, profile, (response + 1), itemNumber, drop=FALSE], na.rm=TRUE)
  }
  return(updatedProfileProbability)
}
