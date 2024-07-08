itemUpdateProfileProbability_singleDraw = function(response, itemNumber, itemProbArray, currentProfileProbablity, nProfiles){

  itemSample = sample(x = 1:nrow(itemProbArray), size = 1, replace = FALSE)

  updatedProfileProbability = currentProfileProbablity

  for (profile in 1:nProfiles){
    updatedProfileProbability[profile] =
      currentProfileProbablity[profile] * itemProbArray[itemSample, profile, (response + 1), itemNumber, drop=FALSE]
  }
  return(updatedProfileProbability)

}
