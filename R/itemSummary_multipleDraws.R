itemSummary_multipleDraws = function(itemProbArray, itemName, nItemSamples){

  itemSamples = sample(x = 1:nrow(itemProbArray), size = nItemSamples, replace = FALSE)
  currentItemProbArray = array(NA, dim = c(nItemSamples, ncol(itemProbArray), length(c(0, 1))))

  # select 'itemSample'th samples from the currentItemProbArray
  currentItemProbArray[1:nItemSamples, , ] = itemProbArray[itemSamples, , , itemName, drop=FALSE]


  return(currentItemProbArray)

}





