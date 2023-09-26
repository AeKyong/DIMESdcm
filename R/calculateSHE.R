
calculateSHE = function(itemPool, itemProbArray, currentProfileProbablity, profileMatrix, itemAbilityQ, nProfiles){


  SHE = rep(0, length(itemPool))

  for (item in 1:length(itemPool)){
    # browser()
    # print(item)
    tempProfileProbability0 =
      itemUpdateProfileProbability(
        response = 0,
        itemNumber = item,
        itemProbArray = itemProbArray,
        currentProfileProbablity = currentProfileProbablity,
        nProfiles = nProfiles
      )
    p0 = sum(tempProfileProbability0)
    p0profileProb = tempProfileProbability0 / p0
    H0 = -1*sum(p0profileProb*log(p0profileProb))

    tempProfileProbability1 =
      itemUpdateProfileProbability(
        response = 1,
        itemNumber = item,
        itemProbArray = itemProbArray,
        currentProfileProbablity = currentProfileProbablity,
        nProfiles = nProfiles
      )
    p1 = sum(tempProfileProbability1)
    p1profileProb = tempProfileProbability1 / p1
    H1 = -1*sum(p1profileProb*log(p1profileProb))

    SHE[item] = p0 * H0 + p1 * H1
  }
  names(SHE) = itemPool
  return(SHE)
}
