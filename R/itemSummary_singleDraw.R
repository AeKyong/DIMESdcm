itemSummary_singleDraw = function(itemProbArray, itemName, nItemSamples){


  itemSample = sample(x = 1:nrow(itemProbArray), size = 1, replace = FALSE)
  currentItemProbArray = array(NA, dim = c(1, ncol(itemProbArray), length(c(0, 1))))

  for (profile in 1:nProfiles){
    for (response in 0:1){
      currentItemProbArray[1, profile, (response+1)] = itemProbArrayNew[itemSample, profile, (response+1), itemName, drop=FALSE]
    }
  }

  return(currentItemProbArray)

}
