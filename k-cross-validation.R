library(readr)
library(ECoL)

folders <- 10
repeats <- 10
times <- 10

data = read.table("diabetes_clean.csv", header=TRUE, sep=",")

result <- data.frame()

for (time in 1:times) {
for (k in 3:folders) {
  n <- 0
  
while (n < repeats) {
  print(paste("K =", k, ", n = ", n, ", time = ", time))
  if (n == repeats) break
  
  yourData<-data[sample(nrow(data)),]
  
  #Create k equally size folds
  folds <- cut(seq(1, nrow(yourData)), breaks=k, labels=FALSE)

  #Perform k fold cross validation
  for(i in 1:k){
    print(paste("i =", i, "n = ", n))
  
    if (n == repeats) break
  
    #Segement your data by fold using the which() function 
    testIndexes <- which(folds==i, arr.ind=TRUE)
    testData <- yourData[testIndexes, ]
    trainData <- yourData[-testIndexes, ]
    #Use the test and train data partitions however you desire...
  
    res <- complexity(Unspecified.blood.glucose.measurement ~ ., trainData, type="regr")
    res <- as.data.frame(t(res))
    res$k <- k
    res$i <- i
    res$times <- time
    result <- rbind(result, res)
    
    n <- n + 1
  }
  if (n == repeats) {
    n <- 0
    break
  }
  }
}
}

write.csv(result, "result.csv")





