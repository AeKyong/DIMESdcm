calculateSHE_termP = function(response, itemNumber, itemProbArray, currentProfileProbablity, nProfiles){
  SHE_termP = 0
  for (profile in 1:nProfiles){
    SHE_termP = SHE_termP + currentProfileProbablity[profile] * itemProbArray[itemNumber, profile, (response + 1)]
  }
  return(SHE_termP)
}
