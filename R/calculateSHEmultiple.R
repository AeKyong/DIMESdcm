calculateSHEmultiple = function(itemPool, itemProbArray, currentProfileProbablity, profileMatrix, nProfiles,
                                    itemUpdateFunction,  nSamples=10){


  SHEmatrix = matrix(0, nrow = nSamples, ncol = length(itemPool))


  for (item in 1:length(itemPool)){
    # browser()
    # print(item)
    tempProfileProbability0 =
      itemUpdateFunction(
        response = 0,
        itemNumber = item,
        itemProbArray = itemProbArray,
        currentProfileProbablity = currentProfileProbablity,
        nProfiles = nProfiles,
        nSamples = nSamples
      )
    p0 = apply(tempProfileProbability0, 1, sum)
    p0profileProb = tempProfileProbability0 / p0
    H0 = -1*apply(p0profileProb*log(p0profileProb), 1, sum)

    tempProfileProbability1 =
      itemUpdateFunction(
        response = 1,
        itemNumber = item,
        itemProbArray = itemProbArray,
        currentProfileProbablity = currentProfileProbablity,
        nProfiles = nProfiles,
        nSamples = nSamples
      )
    p1 = apply(tempProfileProbability1, 1, sum)
    p1profileProb = tempProfileProbability1 / p1
    H1 = -1*apply(p1profileProb*log(p1profileProb), 1, sum)

    SHEmatrix[1:nSamples, item] = p0 * H0 + p1 * H1
  }

  SHE = apply(SHEmatrix, 2, mean, na.rm=TRUE)

  names(SHE) = itemPool
  return(SHE)
}
