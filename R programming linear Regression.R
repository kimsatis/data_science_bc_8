### Machine learning created 
### linear regression
install.packages("caret")
library(caret)
## See the Data
head(mtcars)
colnames(mtcars)
View(mtcars)

## Train test sprit data

set.seed(55) # fix random process
n = nrow(mtcars) # count the number of row
id = sample(n,size = 0.8*n) # sample 80% of n and save to id parameter
train_data = mtcars[id,] # set row that number  same as id to train data 
test_data = mtcars[-id,] # set row that number  not same as id to test data

# create train,test funtion 
train_test_data = function(data){
  set.seed(55)
  n = nrow(data)
  id = sample(n,size = 0.8*n)
  train = data[id,]
  test = data[-id,]
  return(list(train,test)) # save train test data to list
}
split_data = train_test_data(mtcars)# call funtion and save result to split_data
train_data = split_data[[1]]
test_data = split_data[[2]]


## train model by caret library

lm_model = train(mpg~hp,
                 data = train_data,
                 method = "lm")

# scoring model

p = predict(lm_model,newdata = test_data)
p # prediction of test data result

# evaluation model

error = test_data$mpg - p
RMSE = sqrt(mean(error**2))
