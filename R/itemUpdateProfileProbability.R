
itemUpdateProfileProbability = function(response, itemNumber, itemProbArray, currentProfileProbablity, nProfiles){
  updatedProfileProbability = currentProfileProbablity

  for (profile in 1:nProfiles){
    updatedProfileProbability[profile] =
      currentProfileProbablity[profile] * itemProbArray[itemNumber, profile, (response + 1)]
  }
  return(updatedProfileProbability)
}
