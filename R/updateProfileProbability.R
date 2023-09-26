
updateProfileProbability = function(
    responseVector, itemProbArray, currentProfileProbablity, profileMatrix, itemAbilityQ, nProfiles
){

  if (is.null(responseVector)){
    newProfileProbability = currentProfileProbablity
  } else {
    # update profile probability with new response
    newProfileProbability = itemUpdateProfileProbability(
      response = responseVector[length(responseVector)],
      itemNumber = length(responseVector),
      itemProbArray = itemProbArray,
      currentProfileProbablity = currentProfileProbablity,
      nProfiles = nProfiles
    )

    newProfileProbability = newProfileProbability / sum(newProfileProbability)
  }

  return(newProfileProbability)

}
