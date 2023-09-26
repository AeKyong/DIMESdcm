# tests nextItem function (to be exposed with API)
rm(list = ls())
source("drafts/nextItem.R")

# initialize for storage
profileProbs = list()

# starting student with equal probabilities of attribute profiles
currentProfileProbablity = rep(1/nProfiles, nProfiles)

# response vector (make names() the item number that was administered)
responseVector = NULL

# itemList
itemList = NULL

currentItemProbArray = NULL

upNext1 = nextItem(responseVector = responseVector,
                  itemList = itemList,
                  currentProfileProbablity = currentProfileProbablity,
                  currentItemProbArray = currentItemProbArray)

responseVector = c(responseVector, 1)
itemList = c(itemList, upNext1$nextItem)
currentProfileProbablity = upNext1$currentProfileProbablity
currentItemProbArray = upNext1$outputItemProbArray


upNext2 =
  nextItem(responseVector = responseVector,
           itemList = itemList,
           currentProfileProbablity = upNext1$currentProfileProbablity,
           currentItemProbArray = upNext1$outputItemProbArray)

