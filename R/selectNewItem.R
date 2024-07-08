selectNewItem = function(
    itemPool, itemProbArray, currentProfileProbablity, profileMatrix, itemAbilityQ, nProfiles, itemUpdateFunction, calculateSHE
){

  # calculate SHE for each item remaining in pool
  poolSHE = calculateSHE(
    itemPool = itemPool,
    itemProbArray = itemProbArray,
    currentProfileProbablity = currentProfileProbablity,
    profileMatrix = profileMatrix,
    nProfiles = nProfiles,
    itemUpdateFunction = itemUpdateFunction
  )

  newItem = which.min(poolSHE)
  return(newItem)
}
