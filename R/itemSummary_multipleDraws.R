itemSummary_multipleDraws = function(itemProbArray, itemName, nItemSamples){

  itemSamples = sample(x = 1:nrow(itemProbArray), size = nItemSamples, replace = FALSE)
  currentItemProbArray = array(NA, dim = c(nItemSamples, ncol(itemProbArray), length(c(0, 1))))

  for (profile in 1:nProfiles){
    for (response in 0:1){
         currentItemProbArray[1:nItemSamples, profile, (response+1)] = itemProbArray[itemSamples, profile, (response+1), itemName, drop=FALSE]
    }
  }

  return(currentItemProbArray)

}





