itemSummary_Mode = function(itemProbArray, itemName, nItemSamples){


 currentItemProbArray = array(NA, dim = c(1, ncol(itemProbArray), length(c(0, 1))))

  for (profile in 1:nProfiles){
    for (response in 0:1){
      itemMode = modeest::mlv(itemProbArray[, profile, (response+1), itemName], method="naive")
      if (length(itemMode)>1){
        choose = sample(1:length(itemMode), size =1, replace = FALSE)
        itemMode = itemMode[choose]
      }
      currentItemProbArray[1, profile, (response+1)] = itemMode
    }
  }

  return(currentItemProbArray)

}

