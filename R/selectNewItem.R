selectNewItem = function(
    itemPool, itemProbArray, currentProfileProbablity, profileMatrix, itemAbilityQ, nProfiles,
    itemUpdateFunction, calculateSHE, nUpdateSamples
){

  # calculate SHE for each item remaining in pool
  poolSHE = calculateSHE(
    itemPool = itemPool,
    itemProbArray = itemProbArray,
    currentProfileProbablity = currentProfileProbablity,
    profileMatrix = profileMatrix,
    nProfiles = nProfiles,
    itemUpdateFunction = itemUpdateFunction,
    nUpdateSamples = nUpdateSamples
  )

  newItem = which.min(poolSHE)
  return(newItem)
}
