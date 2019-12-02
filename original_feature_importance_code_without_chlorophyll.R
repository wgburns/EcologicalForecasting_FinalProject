
#Combined feature importance abd Time series forecasting 
# I got this information from here:

#https://rpubs.com/mattBrown88/TimeSeriesMachineLearning

#install.packages("Ckmeans.1d.dp")


# libraries we're going to use
library(xgboost) # for xgboost
library(tidyverse) # general utility functions
library(data.table)
library(dplyr)
library(Matrix)
library(zoo)


# Here i combine all the data

multmerge = function(mypath){
  filenames=list.files(path=mypath, full.names=TRUE)
  datalist = lapply(filenames, function(x){read.csv(file=x,header=T)})
  Reduce(function(x,y) {merge(x,y,all = TRUE)}, datalist)
}

data = multmerge("/Users/chege/Desktop/quantitative reasoning final project/csv data")


#Here i check how my data looks
head(data)

#here i check the format of my variables
str(data)

##########################################################################################################
##TESTING FOR REAL TIME LAG PCMean1
########################################################################################################

# get training labels i.e our bloom variable
bloom_variable <-data%>%
  select(PCMean1) # get the column with the bloom measurements


# check out the first few lines
head(bloom_variable) # of our target variable

#Just have a dataframe with the target variable and lags removed

without_bloom_indicator <- data %>%
  select(-starts_with("PCMean"))

#Here i remove redundant columns and keep away whatever is not numeric

without_bloom_numeric <- without_bloom_indicator %>%
  #select(-ID) %>% # the case id shouldn't contain useful information
  select(-c(date,X,TempMean, TurbMean, DeltaDOMean,ChlMean )) %>% # Here i have removed Ph and timestamp
  select_if(is.numeric) # select remaining numeric columns

# make sure that our dataframe is all numeric
str(without_bloom_numeric)


#convert the dataframe into a matrix
without_bloom_matrix <- data.matrix(without_bloom_numeric)



# get the numb 70/30 training test split

numberOfTrainingSamples <- round(nrow(bloom_variable)*.7)

# training data
train_data <- without_bloom_matrix[1:numberOfTrainingSamples,]
train_labels <- bloom_variable[1:numberOfTrainingSamples,]

# testing data
test_data <- without_bloom_matrix[-(1:numberOfTrainingSamples),]
test_labels <- bloom_variable[-(1:numberOfTrainingSamples),]


# put our testing & training data into two seperates Dmatrixs objects
dtrain <- xgb.DMatrix(data = train_data, label= train_labels)
dtest <- xgb.DMatrix(data = test_data, label= test_labels)

# train a model using our training data
model <- xgboost(data = dtrain, # the data   
                 nround = 2, # max number of boosting iterations
                 objective = "reg:linear")  
                 # the objective function

# generate predictions for our held-out testing data
pred <- predict(model, dtest)

# get information on how important each feature is
importance <- xgb.importance(names(without_bloom_matrix), model = model)


xgb.ggplot.importance(importance_matrix = importance)





xgb.plot.importance(importance)
# To save the ggplot as png
ggsave("nocl_0_days_lag.png")

dev.off()
# and your plot is in your files as feature_importances.tiff and publication ready!

##########################################################################################################
##TEND OF REAL TIME LAG PCMean1
########################################################################################################


##########################################################################################################
##TESTING FOR REAL TIME LAG PCMean2
########################################################################################################

# get training labels i.e our bloom variable
bloom_variable <-data%>%
  select(PCMean2) # get the column with the bloom measurements


# check out the first few lines
head(bloom_variable) # of our target variable

#Just have a dataframe with the target variable and lags removed

without_bloom_indicator <- data %>%
  select(-starts_with("PCMean"))

#Here i remove redundant columns and keep away whatever is not numeric

without_bloom_numeric <- without_bloom_indicator %>%
  #select(-ID) %>% # the case id shouldn't contain useful information
  select(-c(date,X,TempMean, TurbMean, DeltaDOMean,ChlMean)) %>% # Here i have removed Ph and timestamp
  select_if(is.numeric) # select remaining numeric columns

# make sure that our dataframe is all numeric
str(without_bloom_numeric)


#convert the dataframe into a matrix
without_bloom_matrix <- data.matrix(without_bloom_numeric)



# get the numb 70/30 training test split

numberOfTrainingSamples <- round(nrow(bloom_variable)*.7)

# training data
train_data <- without_bloom_matrix[1:numberOfTrainingSamples,]
train_labels <- bloom_variable[1:numberOfTrainingSamples,]

# testing data
test_data <- without_bloom_matrix[-(1:numberOfTrainingSamples),]
test_labels <- bloom_variable[-(1:numberOfTrainingSamples),]


# put our testing & training data into two seperates Dmatrixs objects
dtrain <- xgb.DMatrix(data = train_data, label= train_labels)
dtest <- xgb.DMatrix(data = test_data, label= test_labels)

# train a model using our training data
model <- xgboost(data = dtrain, # the data   
                 nround = 2, # max number of boosting iterations
                 objective = "reg:linear")  
# the objective function

# generate predictions for our held-out testing data
pred <- predict(model, dtest)

# get information on how important each feature is
importance <- xgb.importance(names(without_bloom_matrix), model = model)


xgb.ggplot.importance(importance_matrix = importance)



#Here is code to generate cute graphs for publication


xgb.plot.importance(importance)
ggsave("nocl_1_days_lag.png")

dev.off()
# and your plot is in your files as feature_importances.tiff and publication ready!

##########################################################################################################
##END OF REAL TIME LAG PCMean2
########################################################################################################
##########################################################################################################
##TESTING FOR REAL TIME LAG PCMean3
########################################################################################################

# get training labels i.e our bloom variable
bloom_variable <-data%>%
  select(PCMean3) # get the column with the bloom measurements


# check out the first few lines
head(bloom_variable) # of our target variable

#Just have a dataframe with the target variable and lags removed

without_bloom_indicator <- data %>%
  select(-starts_with("PCMean"))

#Here i remove redundant columns and keep away whatever is not numeric

without_bloom_numeric <- without_bloom_indicator %>%
  #select(-ID) %>% # the case id shouldn't contain useful information
  select(-c(date,X,TempMean, TurbMean, DeltaDOMean,ChlMean)) %>% # Here i have removed Ph and timestamp
  select_if(is.numeric) # select remaining numeric columns

# make sure that our dataframe is all numeric
str(without_bloom_numeric)


#convert the dataframe into a matrix
without_bloom_matrix <- data.matrix(without_bloom_numeric)



# get the numb 70/30 training test split

numberOfTrainingSamples <- round(nrow(bloom_variable)*.7)

# training data
train_data <- without_bloom_matrix[1:numberOfTrainingSamples,]
train_labels <- bloom_variable[1:numberOfTrainingSamples,]

# testing data
test_data <- without_bloom_matrix[-(1:numberOfTrainingSamples),]
test_labels <- bloom_variable[-(1:numberOfTrainingSamples),]


# put our testing & training data into two seperates Dmatrixs objects
dtrain <- xgb.DMatrix(data = train_data, label= train_labels)
dtest <- xgb.DMatrix(data = test_data, label= test_labels)

# train a model using our training data
model <- xgboost(data = dtrain, # the data   
                 nround = 2, # max number of boosting iterations
                 objective = "reg:linear")  
# the objective function

# generate predictions for our held-out testing data
pred <- predict(model, dtest)

# get information on how important each feature is
importance <- xgb.importance(names(without_bloom_matrix), model = model)


xgb.ggplot.importance(importance_matrix = importance)



#Here is code to generate cute graphs for publication


xgb.plot.importance(importance)
ggsave("nocl_2_days_lag.png")

dev.off()
# and your plot is in your files as feature_importances.tiff and publication ready!

##########################################################################################################
##TEND OF REAL TIME LAG PCMean3
########################################################################################################
##########################################################################################################
##TESTING FOR REAL TIME LAG PCMean4
########################################################################################################

# get training labels i.e our bloom variable
bloom_variable <-data%>%
  select(PCMean4) # get the column with the bloom measurements


# check out the first few lines
head(bloom_variable) # of our target variable

#Just have a dataframe with the target variable and lags removed

without_bloom_indicator <- data %>%
  select(-starts_with("PCMean"))

#Here i remove redundant columns and keep away whatever is not numeric

without_bloom_numeric <- without_bloom_indicator %>%
  #select(-ID) %>% # the case id shouldn't contain useful information
  select(-c(date,X,TempMean, TurbMean, DeltaDOMean,ChlMean)) %>% # Here i have removed Ph and timestamp
  select_if(is.numeric) # select remaining numeric columns

# make sure that our dataframe is all numeric
str(without_bloom_numeric)


#convert the dataframe into a matrix
without_bloom_matrix <- data.matrix(without_bloom_numeric)



# get the numb 70/30 training test split

numberOfTrainingSamples <- round(nrow(bloom_variable)*.7)

# training data
train_data <- without_bloom_matrix[1:numberOfTrainingSamples,]
train_labels <- bloom_variable[1:numberOfTrainingSamples,]

# testing data
test_data <- without_bloom_matrix[-(1:numberOfTrainingSamples),]
test_labels <- bloom_variable[-(1:numberOfTrainingSamples),]


# put our testing & training data into two seperates Dmatrixs objects
dtrain <- xgb.DMatrix(data = train_data, label= train_labels)
dtest <- xgb.DMatrix(data = test_data, label= test_labels)

# train a model using our training data
model <- xgboost(data = dtrain, # the data   
                 nround = 2, # max number of boosting iterations
                 objective = "reg:linear")  
# the objective function

# generate predictions for our held-out testing data
pred <- predict(model, dtest)

# get information on how important each feature is
importance <- xgb.importance(names(without_bloom_matrix), model = model)


xgb.ggplot.importance(importance_matrix = importance)



#Here is code to generate cute graphs for publication


xgb.plot.importance(importance)
ggsave("nocl_3_days_lag.png")

dev.off()
# and your plot is in your files as feature_importances.tiff and publication ready!

##########################################################################################################
##END OF REAL TIME LAG PCMean4
########################################################################################################
##########################################################################################################
##TESTING FOR REAL TIME LAG PCMean5
########################################################################################################

# get training labels i.e our bloom variable
bloom_variable <-data%>%
  select(PCMean5) # get the column with the bloom measurements


# check out the first few lines
head(bloom_variable) # of our target variable

#Just have a dataframe with the target variable and lags removed

without_bloom_indicator <- data %>%
  select(-starts_with("PCMean"))

#Here i remove redundant columns and keep away whatever is not numeric

without_bloom_numeric <- without_bloom_indicator %>%
  #select(-ID) %>% # the case id shouldn't contain useful information
  select(-c(date,X,TempMean, TurbMean, DeltaDOMean,ChlMean)) %>% # Here i have removed Ph and timestamp
  select_if(is.numeric) # select remaining numeric columns

# make sure that our dataframe is all numeric
str(without_bloom_numeric)


#convert the dataframe into a matrix
without_bloom_matrix <- data.matrix(without_bloom_numeric)



# get the numb 70/30 training test split

numberOfTrainingSamples <- round(nrow(bloom_variable)*.7)

# training data
train_data <- without_bloom_matrix[1:numberOfTrainingSamples,]
train_labels <- bloom_variable[1:numberOfTrainingSamples,]

# testing data
test_data <- without_bloom_matrix[-(1:numberOfTrainingSamples),]
test_labels <- bloom_variable[-(1:numberOfTrainingSamples),]


# put our testing & training data into two seperates Dmatrixs objects
dtrain <- xgb.DMatrix(data = train_data, label= train_labels)
dtest <- xgb.DMatrix(data = test_data, label= test_labels)

# train a model using our training data
model <- xgboost(data = dtrain, # the data   
                 nround = 2, # max number of boosting iterations
                 objective = "reg:linear")  
# the objective function

# generate predictions for our held-out testing data
pred <- predict(model, dtest)

# get information on how important each feature is
importance <- xgb.importance(names(without_bloom_matrix), model = model)


xgb.ggplot.importance(importance_matrix = importance)



#Here is code to generate cute graphs for publication


xgb.plot.importance(importance)
ggsave("nocl_4_days_lag.png")

dev.off()
# and your plot is in your files as feature_importances.tiff and publication ready!

##########################################################################################################
##TEND OF REAL TIME LAG PCMean5
########################################################################################################
##########################################################################################################
##TESTING FOR REAL TIME LAG PCMean6
########################################################################################################

# get training labels i.e our bloom variable
bloom_variable <-data%>%
  select(PCMean6) # get the column with the bloom measurements


# check out the first few lines
head(bloom_variable) # of our target variable

#Just have a dataframe with the target variable and lags removed

without_bloom_indicator <- data %>%
  select(-starts_with("PCMean"))

#Here i remove redundant columns and keep away whatever is not numeric

without_bloom_numeric <- without_bloom_indicator %>%
  #select(-ID) %>% # the case id shouldn't contain useful information
  select(-c(date,X,TempMean, TurbMean, DeltaDOMean,ChlMean)) %>% # Here i have removed Ph and timestamp
  select_if(is.numeric) # select remaining numeric columns

# make sure that our dataframe is all numeric
str(without_bloom_numeric)


#convert the dataframe into a matrix
without_bloom_matrix <- data.matrix(without_bloom_numeric)



# get the numb 70/30 training test split

numberOfTrainingSamples <- round(nrow(bloom_variable)*.7)

# training data
train_data <- without_bloom_matrix[1:numberOfTrainingSamples,]
train_labels <- bloom_variable[1:numberOfTrainingSamples,]

# testing data
test_data <- without_bloom_matrix[-(1:numberOfTrainingSamples),]
test_labels <- bloom_variable[-(1:numberOfTrainingSamples),]


# put our testing & training data into two seperates Dmatrixs objects
dtrain <- xgb.DMatrix(data = train_data, label= train_labels)
dtest <- xgb.DMatrix(data = test_data, label= test_labels)

# train a model using our training data
model <- xgboost(data = dtrain, # the data   
                 nround = 2, # max number of boosting iterations
                 objective = "reg:linear")  
# the objective function

# generate predictions for our held-out testing data
pred <- predict(model, dtest)

# get information on how important each feature is
importance <- xgb.importance(names(without_bloom_matrix), model = model)


xgb.ggplot.importance(importance_matrix = importance)



#Here is code to generate cute graphs for publication


xgb.plot.importance(importance)
ggsave("nocl_5_days_lag.png")

dev.off()
# and your plot is in your files as feature_importances.tiff and publication ready!

##########################################################################################################
##TEND OF REAL TIME LAG PCMean6
########################################################################################################
##########################################################################################################
##TESTING FOR REAL TIME LAG PCMean7
########################################################################################################

# get training labels i.e our bloom variable
bloom_variable <-data%>%
  select(PCMean7) # get the column with the bloom measurements


# check out the first few lines
head(bloom_variable) # of our target variable

#Just have a dataframe with the target variable and lags removed

without_bloom_indicator <- data %>%
  select(-starts_with("PCMean"))

#Here i remove redundant columns and keep away whatever is not numeric

without_bloom_numeric <- without_bloom_indicator %>%
  #select(-ID) %>% # the case id shouldn't contain useful information
  select(-c(date,X,TempMean, TurbMean, DeltaDOMean,ChlMean)) %>% # Here i have removed Ph and timestamp
  select_if(is.numeric) # select remaining numeric columns

# make sure that our dataframe is all numeric
str(without_bloom_numeric)


#convert the dataframe into a matrix
without_bloom_matrix <- data.matrix(without_bloom_numeric)



# get the numb 70/30 training test split

numberOfTrainingSamples <- round(nrow(bloom_variable)*.7)

# training data
train_data <- without_bloom_matrix[1:numberOfTrainingSamples,]
train_labels <- bloom_variable[1:numberOfTrainingSamples,]

# testing data
test_data <- without_bloom_matrix[-(1:numberOfTrainingSamples),]
test_labels <- bloom_variable[-(1:numberOfTrainingSamples),]


# put our testing & training data into two seperates Dmatrixs objects
dtrain <- xgb.DMatrix(data = train_data, label= train_labels)
dtest <- xgb.DMatrix(data = test_data, label= test_labels)

# train a model using our training data
model <- xgboost(data = dtrain, # the data   
                 nround = 2, # max number of boosting iterations
                 objective = "reg:linear")  
# the objective function

# generate predictions for our held-out testing data
pred <- predict(model, dtest)

# get information on how important each feature is
importance <- xgb.importance(names(without_bloom_matrix), model = model)


xgb.ggplot.importance(importance_matrix = importance)



#Here is code to generate cute graphs for publication


xgb.plot.importance(importance)
ggsave("nocl_6_days_lag.png")

dev.off()
# and your plot is in your files as feature_importances.tiff and publication ready!

##########################################################################################################
##TEND OF REAL TIME LAG PCMean6
########################################################################################################