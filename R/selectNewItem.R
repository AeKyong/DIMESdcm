selectNewItem = function(
    itemPool, itemProbArray, currentProfileProbablity, profileMatrix, itemAbilityQ, nProfiles
){

  # calculate SHE for each item remaining in pool
  poolSHE = calculateSHE(
    itemPool = itemPool,
    itemProbArray = itemProbArray,
    currentProfileProbablity = currentProfileProbablity,
    profileMatrix = profileMatrix,
    itemAbilityQ = itemAbilityQ,
    nProfiles = nProfiles
  )

  newItem = itemPool[which.min(poolSHE)]
  return(newItem)
}
