
install.packages("forecast")
install.packages("fpp")


library(forecast)
library(fpp)



stockVector = list()
BestPrediction = vector()

stockVector[[1]] = read.table("./input/A.csv", sep=",", header=TRUE)


for(i in 1:length(stockVector))
{
  cat("\n*** predicting stock number: ",i) ;
  BestPrediction[i] = fb(stockVector[[i]])
  
}


cat("\n\n!!! winning models:", BestPrediction);






