itemUpdateProfileProbability_multipleDraws = function(response, itemNumber, itemProbArray, currentProfileProbablity, nProfiles, nSamples){

  itemSample = sample(x = 1:nrow(itemProbArray), size = nSamples, replace = FALSE)

  updatedProfileProbability = matrix(0, nrow = nSamples, ncol = nProfiles)

for (sample in 1:nSamples) {
  for (profile in 1:nProfiles){
    updatedProfileProbability[sample, profile] =
      currentProfileProbablity[profile] * itemProbArray[itemSample[sample], profile, (response + 1), itemNumber, drop=FALSE]
  }
}

  return(updatedProfileProbability)

}
