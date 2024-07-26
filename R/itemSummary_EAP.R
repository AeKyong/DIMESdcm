itemSummary_EAP = function(itemProbArray, itemName, nItemSamples){


  currentItemProbArray = array(NA, dim = c(1, ncol(itemProbArray), length(c(0, 1))))

    for (profile in 1:nProfiles){
      for (response in 0:1){

        currentItemProbArray[1, profile, (response+1)] = mean(itemProbArray[, profile, (response+1), itemName], na.rm=TRUE)
      }
    }

  return(currentItemProbArray)

  }

