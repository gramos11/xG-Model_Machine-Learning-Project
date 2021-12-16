library(xgboost)
library(gmodels)  
library(mltools) #for one-hot encoding
library(data.table) #for one-hot encoding
library(lime)
library(ROSE)

#Import the data
mydata=read.csv("C://Users//gramos1//Desktop//Football_Data//myData.csv")


#Type "C" for classification and "R" for regression

model.type="C"

#Number of rounds. This can be set to a lower or higher value, if you wish, example: 150 or 250 or 300  
cv.nround = 5000


#END OF DATA IMPORT

#START OF RESPONSE REDEFINITION

mydata$myresponse=mydata$goal #Substitute "Status" with the name of your response variable
mydata$goal=NULL #Substitute "Status" with the name of your response variable

if (model.type=="C") (table(mydata$myresponse))

#The code below redefines the levels of categorical outcome to 0,1,...
if (model.type=="C"){
  
  if (is.character(mydata$myresponse)){
    mydata$myresponse=as.factor(mydata$myresponse)}
  
  mydata$myresponse=as.numeric(mydata$myresponse)
  
  if (min(mydata$myresponse)!=0){ 
    if (min(mydata$myresponse)>0)
      (mydata$myresponse=mydata$myresponse-min(mydata$myresponse)) else 
        (mydata$myresponse=mydata$myresponse+abs(min(mydata$myresponse)))}
  
  
  unique.vals=unique(mydata$myresponse)
  unique.vals=unique.vals[order(unique.vals)]
  
  if (sum(unique.vals==seq(0,length(unique.vals)-1))!=length(unique(unique.vals))){
    
    j=0
    
    for (i in unique.vals){
      mydata$myresponse[mydata$myresponse==i]=j
      j=j+1
    }
  }
}

if (model.type=="C") (table(mydata$myresponse))


#END OF RESPONSE REDEFINITION

#In the following statements substitute the names after "$" sign with the names of predictors
#in your data that are categorical but are read into R in a different format. If there are no such 
#variables in your data, then ignore.

#START OF PREDICTOR TRANSFORMATION

mydata$bodypartused=as.factor(mydata$bodypartused)
mydata$counter=as.factor(mydata$counter)
mydata$matchPeriod=as.factor(mydata$matchPeriod)

##MESSI CODE##
#messi <- mydata[(mydata$playerId==3359 & mydata$teamId==676),]
#mydata <- mydata[!(mydata$playerId==3359 & mydata$teamId==676),]

##RONNY CODE##
#ronaldo <- mydata[(mydata$playerId==3322 & mydata$teamId==675),]
#mydata <- mydata[!(mydata$playerId==3322 & mydata$teamId==675),]

#add statements similar to above as needed
#messi <- messi[-c(2,5:6,9)]
#ronaldo <- ronaldo[-c(2,5:6,9)]
mydata <- mydata[-c(2,5:6,9)]

#END OF PREDICTOR TRANSFORMATION

#The statements below remove all the variables that will not be passed to the tree algorithm
#as predictors. If no such redundant variables exist in your dataset, then the statements
#in the "REDUNDANT VARIABLE REMOVAL" section should be ignored.

#START OF REDUNDANT VARIABLE REMOVAL

#mydata$xyz=NULL    #Substitute "xyz" with the name of the variable in your data that 
#will not be passed to the tree algorithm. Add as many statements similar 
#to this as needed.

#END OF REDUNDANT VARIABLE REMOVAL

#############################################################################################
#####################################ATTENTION###############################################
#############################################################################################
#######################IF THE ABOVE MODIFICATIONS ARE MADE CORRECTLY,########################
####AT THIS POINT "MYDATA" DATA FRAME SHOULD CONTAIN ONLY THE PREDICTORS AND THE OUTCOME.#### 
####IN CASE IT CONTAINS ANYTHING MORE OR LESS, THE CODE BELOW WILL NOT FUNCTION PROPERLY.####
#############################################################################################

str(mydata) #make sure the structure of your data reflects all the modifications made above

#Start 80-20 partition
numpredictors=dim(mydata)[2]-1

numfac=0

for (i in 1:numpredictors) {
  if ((is.factor(mydata[,i]))){
    numfac=numfac+1} 
}

#End finding the number of categorical predictors 

nobs=dim(mydata)[1]


if (model.type=="R") {
  
  #Below is the setup for stratified 80-20 holdout sampling for a Regression Tree
  
  train_size=floor(0.8*nobs)
  test_size=nobs-train_size
  
} else {
  
  #Below is the setup for stratified 80-20 holdout sampling for a Classification Tree
  
  prop = prop.table(table(mydata$myresponse))
  length.vector = round(nobs*0.8*prop)
  train_size=sum(length.vector)
  test_size=nobs-train_size
  class.names = as.data.frame(prop)[,1]
  numb.class = length(class.names)}


resample=1
RNGkind(sample.kind = "Rejection")
set.seed(1) #sets the seed for random sampling

while (resample==1) {
  
  
  if (model.type=="C") {
    
    train_index = c()
    
    for(i in 1:numb.class){
      index_temp = which(mydata$myresponse==class.names[i])
      train_index_temp = sample(index_temp, length.vector[i], replace = F)
      train_index = c(train_index, train_index_temp)
    }} else {
      train_index=sample(nobs,train_size, replace=F)
    }
  
  mydata_train=mydata[train_index,] #randomly select the data for training set using the row numbers generated above
  mydata_test=mydata[-train_index,]#everything not in the training set should go into testing set
  
  right_fac=0 #denotes the number of factors with "right" distributions (i.e. - the unique levels match across mydata, test, and train data sets)
  
  
  for (i in 1:numpredictors) {
    if (is.factor(mydata_train[,i])) {
      if (sum(as.vector(unique(mydata_test[,i])) %in% as.vector(unique(mydata_train[,i])))==length(unique(mydata_test[,i])))
        right_fac=right_fac+1
    }
  }
  
  if (right_fac==numfac) (resample=0) else (resample=1)
  
}

dim(mydata_test) #confirms that testing data has only 20% of observations
dim(mydata_train) #confirms that training data has 80% of observations

#End 80-20 partition

sum(mydata_train$myresponse)

mydata_train <- ovun.sample(myresponse ~ ., data = mydata_train, method = "both")$data


#One-hot encoding of factor predictors for training and test sets separately

if (sum(sapply(mydata_train, is.factor))>0) {

only.fac.train=mydata_train[,sapply(mydata_train, is.factor), drop=FALSE]
only.fac.test=mydata_test[,sapply(mydata_test, is.factor), drop=FALSE]
#only.fac.messi=messi[,sapply(messi, is.factor), drop=FALSE]
#only.fac.ronaldo=ronaldo[,sapply(ronaldo, is.factor), drop=FALSE]


non.fac.train=mydata_train[,setdiff(colnames(mydata_train), colnames(only.fac.train))]
non.fac.test=mydata_test[,setdiff(colnames(mydata_test), colnames(only.fac.test))]
#non.fac.messi=messi[,setdiff(colnames(messi), colnames(only.fac.messi))]
#non.fac.ronaldo=ronaldo[,setdiff(colnames(ronaldo), colnames(only.fac.ronaldo))]


if (length(only.fac.train)>0) {
  dummy.train <- one_hot(as.data.table(only.fac.train))
  dummy.test  <- one_hot(as.data.table(only.fac.test))
  mydata_train=cbind(non.fac.train, dummy.train)
  mydata_test=cbind(non.fac.test, dummy.test)}
  #dummy.messi <- one_hot(as.data.table(only.fac.messi))
  #messi=cbind(non.fac.messi, dummy.messi)}
  #dummy.ronaldo <- one_hot(as.data.table(only.fac.ronaldo))
  #ronaldo=cbind(non.fac.ronaldo, dummy.ronaldo)}
  }
  

#Set the parameters for cross-validation and xgboost.
#You can try different values for nthread, max_depth, eta, gamma, etc., and see if you get lower prediction error.

#Start of Hyperparameter Setup

if ((model.type=="C") & (length(unique(mydata$myresponse))==2)) {
  cl.obj="binary:logistic"
  cl.eval.metric="error"} else {
  if ((model.type=="C") & (length(unique(mydata$myresponse))>2)) {
    
    cl.obj="multi:softmax"
    cl.eval.metric="merror"}}
    
if (model.type=="C") {
  
  if ((length(unique(mydata$myresponse))==2)) {

## eta changed from 0.3 to 0.1, max_depth changed from 1 to 3        

param.cl       = list("objective" = cl.obj, 
                   #"num_class"= length(unique(mydata$myresponse)),
                   "eval_metric" = cl.eval.metric, 
                   "nthread" = 8,   			 # number of threads to be used 
                   "max_depth" = c(1,4,7,10,13,16,20),    		 # maximum depth of tree 
                   "eta" = c(0.01, 0.05, 0.1, 0.2,0.3),    			 # step size shrinkage 
                   "gamma" = 0,    			 # minimum loss reduction 
                   "subsample" = 1,    		 # part of data instances to grow tree 
                   "colsample_bytree" = 1, 		 # subsample ratio of columns when constructing each tree 
                   "min_child_weight" = 5,  		 # minimum sum of instance weight needed in a child 
                   "early_stopping_rounds"=10
)
  } else {
  
  param.cl       = list("objective" = cl.obj, 
                        "num_class"= length(unique(mydata$myresponse)),
                        "eval_metric" = cl.eval.metric, 
                        "nthread" = 8,   			 # number of threads to be used 
                        "max_depth" = 1,    		 # maximum depth of tree 
                        "eta" = 0.3,    			 # step size shrinkage 
                        "gamma" = 0,    			 # minimum loss reduction 
                        "subsample" = 1,    		 # part of data instances to grow tree 
                        "colsample_bytree" = 1, 		 # subsample ratio of columns when constructing each tree 
                        "min_child_weight" = 5,  		 # minimum sum of instance weight needed in a child 
                        "early_stopping_rounds"=10
  )
}
  } else {

param.reg       = list("objective" = "reg:squarederror", 
                      "eval_metric" = "mape", 
                      "nthread" = 8,   			 # number of threads to be used 
                      "max_depth" = 1,    		 # maximum depth of tree 
                      "eta" = 0.3,    			 # step size shrinkage 
                      "gamma" = 0,    			 # minimum loss reduction 
                      "subsample" = 1,    		 # part of data instances to grow tree 
                      "colsample_bytree" = 1, 		 # subsample ratio of columns when constructing each tree 
                      "min_child_weight" = 5,  		 # minimum sum of instance weight needed in a child 
                      "early_stopping_rounds"=10
)
}

#End of Hyperparameter Setup


#Identify the Predictors and the dependent variable, aka label.
predictors = setdiff(colnames(mydata_train), "myresponse")

#Save the training output variable into "label"
label=mydata_train$myresponse

#########################################################################################################
# Step 1: Run a Cross-Validation to identify the round with the minimum loss or error.
#         Note: xgboost expects the data in the form of a numeric matrix.
##############################################################################################################################

set.seed(100)

if (model.type=="C"){
bst.cv = xgb.cv(
  param=param.cl,
  data = as.matrix(mydata_train[,predictors]),
  label = label,
  nfold = 10,
  nrounds=cv.nround,
  prediction=T,
  verbose=F)
} else {
  
  bst.cv = xgb.cv(
    param=param.reg,
    data = as.matrix(mydata_train[,predictors]),
    label = label,
    nfold = 10,
    nrounds=cv.nround,
    prediction=T,
    verbose=F)}
  
#Find where the minimum loss occurred

if (model.type=="C"){
  if ((length(unique(mydata$myresponse))==2)) {
  
min.error.idx = bst.cv$evaluation_log$iter[which.min(bst.cv$evaluation_log$test_error_mean)]} else {
  min.error.idx = bst.cv$evaluation_log$iter[which.min(bst.cv$evaluation_log$test_merror_mean)]}} else {
    
    min.error.idx = bst.cv$evaluation_log$iter[which.min(bst.cv$evaluation_log$test_mape_mean)]}
  
cat ("Minimum error occurred in round : ", min.error.idx, "\n")

# Minimum error
print(min(bst.cv$evaluation_log[,4]))

#Plot the CV error vs tree order

dd=as.data.frame(bst.cv$evaluation_log)
ylim.min=min(c(dd[,2], dd[,4]))
ylim.max=max(c(dd[,2], dd[,4]))

plot(dd[,2], col="blue", type="l", ylab="Error", xlab="Tree Number", main="Cross Validation Error vs Trees", ylim=c(ylim.min, ylim.max))
lines(dd[,4], col="red")
abline(v=min.error.idx, col="black", lwd=1, lty=2)


##############################################################################################################################
# Step 2: Train the xgboost model using min.error.idx found above.
#         Note, we have to stop at the round where we get the minimum error.

##############################################################################################################################

set.seed(100)

if (model.type=="C"){

bst = xgboost(
  param=param.cl,
  data =as.matrix(mydata_train[,predictors]),
  label = label,
  #early_stopping_rounds=10,
  nrounds=min.error.idx,
  verbose=F)} else {
    
    bst = xgboost(
      param=param.reg,
      data =as.matrix(mydata_train[,predictors]),
      label = label,
      #early_stopping_rounds=10,
      nrounds=min.error.idx,
      verbose = F)} 


# Make prediction on the testing data.
mydata_test$prediction = predict(bst, as.matrix(mydata_test[,predictors]))

if ((model.type=="C") & length(unique(mydata$myresponse))==2) {
  mydata_test$prediction=0*(mydata_test$prediction<=0.11)+1*(mydata_test$prediction>0.11)
}


#LIME plots below
#For details see resources below for excellent explanations:
#https://uc-r.github.io/lime 
#https://christophm.github.io/interpretable-ml-book/lime.html 


#Below provide the row number of the testing set which you would like LIME to explain
row.to.explain=2

set.seed(100)
lime_to_explain <-mydata_test[row.to.explain,predictors] #Explaining the row stored in 'row.to.explain'
explainer <- suppressWarnings(lime(mydata_train, model = bst)) #specifying that the training model will be used for explanations


if (model.type=="R"){
explanation <- explain(
                       lime_to_explain, #the explanations will be given for these observations
                       explainer, 
                       n_features=5, #the top 5 features based on forward selection will be used in explanations
                       feature_select = "forward_selection", #see above
                       
                       dist_fun = "euclidean",
                       
                       kernel_width = 0.5,
                       
                       
                       n_permutations = 500 #for each obs in "lime_to_explain" there will be 5,00 permutations created
                                             #based on the data contained in "explainer", i.e. based on the variables
                                             #of training set. In other words, for each test set observation, 5K obs
                                             #are created using the distributions of the training data columns. Those then
                                             #are used to fit the local model in the vicinity of the test set obs in question.
                       )
                                                       
} else {
  
  explanation <- explain(
                        lime_to_explain, #the explanations will be given for these observations
                        explainer,
                        n_labels = length(unique(mydata_test$myresponse)),
                        n_features=5, #the top 5 features based on forward selection will be used in explanations
                        feature_select = "forward_selection", #see above
                        
                        dist_fun = "euclidean",
                        
                        kernel_width = 0.5,
                        
                        
                        n_permutations = 500 #for each obs in "lime_to_explain" there will be 5,00 permutations created
                                              #based on the data contained in "explainer", i.e. based on the variables
                                              #of training set. In other words, for each test set observation, 5K obs
                                              #are created using the distributions of the training data columns. Those then
                                              #are used to fit the local model in the vicinity of the test set obs in question.
  )
  

}  

plot_features(explanation)


#Summarize the predictive accuracy

if (model.type=="C"){
#Compute the accuracy of predictions.
CrossTable(mydata_test$myresponse,mydata_test$prediction,prop.chisq=F,prop.t=F)} else {
  
  rmse=sqrt(mean((mydata_test$myresponse-mydata_test$prediction)^2))
  mape=100*mean(abs(mydata_test$myresponse-mydata_test$prediction)/mydata_test$myresponse)
  
  print(paste("RMSE: ", round(rmse,2)))
  print(paste("MAPE: ", round(mape,2)))
}

mydata_test$prob <- predict(bst, as.matrix(mydata_test[,predictors]))

#messi$prob <- predict(bst, as.matrix(messi[,predictors]))

#ronaldo$prob <- predict(bst, as.matrix(ronaldo[,predictors]))



