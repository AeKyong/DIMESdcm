itemSummary_singleDraw = function(itemProbArray, itemName, nItemSamples){


  itemSample = sample(x = 1:nrow(itemProbArray), size = 1, replace = FALSE)
  currentItemProbArray = array(NA, dim = c(1, ncol(itemProbArray), length(c(0, 1))))

  # select 'itemSample'th sample from the currentItemProbArray
  currentItemProbArray[1, , ] = itemProbArray[itemSample, , , itemName, drop=FALSE]

  return(currentItemProbArray)

}
