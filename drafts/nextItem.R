# environment setup for plumber
library(DIMESadaptive)
data("nProfiles")
data("itemAbilityQ")
data("profileMatrix")
data("itemInterceptEAP")
data("itemInterceptVariance")
data("itemMainEffectEAP")
data("itemMainEffectVariance")
data("itemMapping")


#*
#* API wrapper for package
#* @param responseVector A vector of 0/1 responses to items
#* @param itemList A vector of item IDs corresponding to responseVector
#* @param currentProfileProbs A vector (length 8) of profile probabilities
#* @param currentItemProbArray An three-dimensional array of size length(responseVector), 8, 2; returned from function
#* @post /nextItem
function(currentProfileProbablity, responseVector, itemList, currentItemProbArray){

  # convert all characters to numeric
  currentProfileProbablity = as.numeric(currentProfileProbablity)
  responseVector = as.numeric(responseVector)
  itemList = as.numeric(itemList)

  # browser()
  # when responseVector, itemList, and itemProbArray are NULL, include for all items
  # when they have data, include only items that remain
  if (is.null(responseVector) & is.null(itemList) & is.null(currentItemProbArray)){
    interceptMeans = itemInterceptEAP
    interceptVars = itemInterceptVariance
    mainEffectMeans = itemMainEffectEAP
    mainEffectVars = itemMainEffectVariance
    nItemsInPool = length(itemMapping$itemNumber)
    poolItems = itemMapping$itemNumber
    poolQ = itemAbilityQ
  } else {
    # update profile probabilities
    for (profile in 1:nProfiles){
      currentProfileProbablity[profile] =
        currentProfileProbablity[profile] *
          currentItemProbArray[dim(currentItemProbArray)[1], profile, responseVector[length(responseVector)]+1]
    }
    currentProfileProbablity = currentProfileProbablity/sum(currentProfileProbablity)

    nItemsInPool = length(itemMapping$itemNumber[-itemList])
    poolItems = itemMapping$itemNumber[-itemList]

    # remove all items that have been administered from consideration
    interceptMeans =
      itemInterceptEAP[which(!names(itemInterceptEAP) %in% paste0("intercept[", itemList, "]"))]
    interceptVars = itemInterceptVariance
    mainEffectMeans =
      itemMainEffectEAP[which(!names(itemMainEffectEAP) %in% paste0("lambda[", itemList, "]"))]
    mainEffectVars = itemMainEffectVariance
    poolQ = itemAbilityQ[which(!names(itemAbilityQ) %in% paste0("item", itemList))]
    # remove from itemInterceptEAP, nItemsInPool, itemInterceptVariance, itemMainEffectEAP, itemMainEVariance
  }

  # add random error to remaining items using temporary arrays built from package data
  poolIntercepts = interceptMeans + rnorm(nItemsInPool, 0, interceptVars)
  poolMainEffects = mainEffectMeans + rnorm(nItemsInPool, 0, mainEffectVars)

  # calculate item probabilities
  # create item response probability array for each item, class, and response
  itemProbArray = array(data = NA, dim = c(nItemsInPool, nProfiles, 2))

  for (item in 1:nItemsInPool){
    for (profile in 1:nProfiles){
      for (response in 0:1){
        itemProbArray[item, profile, (response+1)] = itemProb(
          response = response,
          itemNumber = poolItems[item],
          itemAttribute = poolQ[item],
          examineeProfile = profileMatrix[profile,],
          itemIntercepts = poolIntercepts,
          itemMainEffects = poolMainEffects
        )
      }
    }
  }

  # select next item based on SHE algorithm
  newItem = selectNewItem(
    itemPool = poolItems,
    itemProbArray = itemProbArray,
    currentProfileProbablity = currentProfileProbablity,
    profileMatrix = profileMatrix,
    itemAbilityQ = poolQ,
    nProfiles = nProfiles
  )

  if (is.null(responseVector)) {
    outputItemProbArray = array(itemProbArray[newItem, , ], c(1, 8, 2))
  } else {
    outputItemProbArray = abind::abind(currentItemProbArray, itemProbArray[newItem, , ], along = 1)
  }

  # return key information
  return(list(nextItem = newItem, currentProfileProbablity = currentProfileProbablity,
              outputItemProbArray = outputItemProbArray))
}

#* @post /test
function(a, b){
  print(req$body)
  return(list(thing1 = NULL, thing2=nchar(a)))
}


#* @param msg the message to echo
#* @get /echo
function(msg = ""){
list(
  msg = paste0("the message is: '", msg, "'")
)
}

#* @plumber
function(pr){
  pr %>%
    pr_set_api_spec(function(spec) { spec$paths[["/echo"]]$get$summary <-
      "Echo back the input"
    spec })
}
