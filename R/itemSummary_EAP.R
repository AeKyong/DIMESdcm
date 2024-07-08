itemSummary_EAP = function(itemProbArray, itemName, nItemSamples){


  itemProbArrayNew = itemProbArray[,,, itemName]
  currentItemProbArray = array(NA, dim = c(1, ncol(itemProbArray), length(c(0, 1))))

    for (profile in 1:nProfiles){
      for (response in 0:1){
        currentItemProbArray[1, profile, (response+1)] = mean(itemProbArrayNew[, profile, (response+1)], na.rm=TRUE)
      }
    }

  return(currentItemProbArray)

  }

