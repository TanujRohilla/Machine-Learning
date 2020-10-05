# 1. K-Means #
# K = as.Integer(readline(prompt="Enter value of K : "))
K = 3 
data = iris 
rows = dim(data)[1]
columns = dim(data)[2]
randomRows = sample(1:rows, K)
# change it accoring to the K 
# setosa versicolor  virginica 
allRows = c()
index=1
for(i in c(1:K)){
  for(j in c(1:4)){
    allRows[index] = iris[randomRows[i], j]
    index=index+1
  }
}
means = matrix(allRows, nrow = 3, ncol = 4, byrow = TRUE) #row is k and column is attributes




K1Indexes = c()
K2Indexes = c()
K3Indexes = c()


K1Indexes[length(K1Indexes)+1] = randomRows[1]
K2Indexes[length(K2Indexes)+1] = randomRows[2]
K3Indexes[length(K3Indexes)+1] = randomRows[3]



noMeanChange = FALSE

while(noMeanChange == FALSE){
  noMeanChange = TRUE
  cat("Before mean 1 ", calculateMean(iris, K1Indexes), "\n")
  cat("Before mean 2 ", calculateMean(iris, K2Indexes), "\n")
  cat("Before mean 3 ", calculateMean(iris, K3Indexes), "\n")
  oldMean = means
  K1Indexes = c()
  K2Indexes = c()
  K3Indexes = c()
  for(i in c(1:150)){
    assignmentMean = assignNearestMean(iris, means, i)
    if(assignmentMean == 1){
      K1Indexes[length(K1Indexes)+1] = i
    }
    if(assignmentMean == 2){
      K2Indexes[length(K2Indexes)+1] = i
    }
    if(assignmentMean == 3){
      K3Indexes[length(K3Indexes)+1] = i
    }
  }
  cat(head(K1Indexes), "\n")
  cat(head(K2Indexes), "\n")
  cat(head(K3Indexes), "\n")
  
  # calculate new mean
  newMeans = c()
  for( i in 1:4){
    newMeans[length(newMeans)+1] = mean(iris[K1Indexes, i])
  }
  for( i in 1:4){
    newMeans[length(newMeans)+1] = mean(iris[K2Indexes, i])
  }
  for( i in 1:4){
    newMeans[length(newMeans)+1] = mean(iris[K3Indexes, i])
  }
  newMeans = matrix(newMeans, nrow = 3, ncol = 4, byrow = TRUE) 
  changed = FALSE
  for(i in 1:dim(newMeans)[1])
  {
    for(j in 1:dim(newMeans)[2]){
      if(newMeans[i,j] != means[i, j]){
        noMeanChange = FALSE
        break
      }
    }
  }
  means = newMeans
  means
}


assignNearestMean <- function(data, means, ithRow){
  nearestMeanValue = .Machine$integer.max
  nearestMean = NULL
  for(ithMean in c(1:dim(means)[1])){
    distanceVector = c()
    for(j in c(1:(dim(data)[2]-1))){
      distanceVector[j] = sqrt(
        (means[ithMean, j]-data[ithRow, j])*(means[ithMean, j]-data[ithRow, j])
        )
    }
    if(sum(distanceVector)<nearestMeanValue){
      nearestMean = ithMean
      nearestMeanValue = sum(distanceVector)
    }
    # cat(sum(distanceVector),"\n")
  }
 # cat("Nearest is - ", nearestMeanValue, " and mean name is ", nearestMean)
  return <- (nearestMean)
}


columnNumber=1
calculateMean<-function(data, rows){
  m = c()
  for(i in c(1:4)){
    m[i] = mean(iris[rows, i])
  }
  return <- m
}

accurate = 0
error = 0
for(i in c(K1Indexes)){
  if(iris[i, 5] == "setosa")
    accurate=accurate+1
  else 
    error = error + 1
}
for(i in c(K2Indexes)){
  if(iris[i, 5] == "versicolor")
    accurate=accurate+1
  else 
    error = error + 1
}
for(i in c(K3Indexes)){
  if(iris[i, 5] == "virginica")
    accurate=accurate+1
  else 
    error = error + 1
}
cat("Accuracy is - ", accurate/150)
cat("Error is - ", error/150)
