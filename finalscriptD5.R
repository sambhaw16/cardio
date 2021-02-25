#Importing the required libraries
library(ggplot2) 
require(ggiraph)
require(ggiraphExtra)
require(plyr)
library(ROCR)
library(Hmisc)
library(tidyverse)
library(caret)
library(caretEnsemble)
library(psych)
library(Amelia)
library(mice)
library(GGally)
libr?ry(rpart)
library(randomForest)
library(class)
library(e1071)
library(gmodels)
library(randomForest)
library (ISLR)
library (boot)
#Printing the current R version
R.version.string 

#Importing the data from csv into a data frame using read.csv() method

da?a <- read.csv('C:\\Users\\Sambhaw\\Desktop\\777 fall 2020\\project\\cardio_train.csv', header = TRUE,
                 sep = ";", quote = "\"", dec = ".")


#Displaying the top few rows of the data
head(data)

#Statement to check if its a dataframe
print(i?.data.frame(data))

#Displaying the number of columns in the dataframe
print(ncol(data))

#Displaying the number of rows in the dataframe
print(nrow(data))

data <- data[-c(1)]

genderT <- table(data$gender) # 1 represent 

#prop function is used to comput? proportions and percentages
#Displaying the proportion of different genders
prop.table( genderT )

#collecting all the numeric values in a seperate dataframe
numtab <- data[c(1,3:6)] # data with numeric values 

#collecting all the cartegorical data in a ?eperate dataframe
catdata <- data[-c(1,3:6)] # data with catagorical values 

#Similar to the usual contingency tables, displaying the counts of each combination of the levels of the variables (factors) involved. 
ftable(catdata) # it shows distrubution of?values into catgories.

#Displaying the summary of the numerical data
summary(numtab) # 5 number summery of numric data.

#This data requires some data cleaning 
#From the summary data above we can see that we have few outliers in the data 
#Removing outli?rs
data <- subset(data,ap_lo >= 20 & ap_lo <= 190 & ap_hi >= 60 & ap_hi <= 230 & height >= 120 & height <= 210 & weight >= 40 & weight <= 200 & ap_lo < ap_hi)


#We can observe that all the data is represented as integers and numeric but we have few catego?ical data
#solving data type inconsistency issues

data[c(2,7:12)] <- lapply(data[c(2,7:12)],factor)
nrow(data)

#This function determines whether the variable is character, factor, category, binary, discrete numeric, and continuous numeric, and prints a c?ncise statistical summary according to each.
Hmisc::describe(data)



#The datatypes of columns currently present in the dataframe 
data_types <- function(frame) {
  res <- lapply(frame, class)
  res_frame <- data.frame(unlist(res))
  barplot(table(res_fra?e), main="Data Types", col="steelblue", ylab="Number of Features")
}

#This displays a plot of the datatype of features 
#Reference - https://stackoverflow.com/questions/21125222/determine-the-data-types-of-a-data-frames-columns
data_types(data)




data_t?pes(data)


summary(data)
#Plotting histograms for numeric data to display their distribution
hist(data$age)

hist(data$height)

hist(data$weight)

hist(data$ap_hi)

hist(data$ap_lo)

#Displaying the bar plots for categorical data representing the number o? observations in each category
barplot(table(data$gender),main="Gender",
        xlab="Gender",
        ylab="Count")

barplot(table(data$cholesterol),main="Cholestrol",
        xlab="Cholestrol",
        ylab="Count")

barplot(table(data$gluc),main="Gluco?e",
        xlab="Glucose",
        ylab="Count")

barplot(table(data$smoke),main="Smoking",
        xlab="Smoking",
        ylab="Count")

barplot(table(data$alco),main="Alcohol",
        xlab="Alcohol",
        ylab="Count")

barplot(table(data$active),m?in="Physical Activity",
        xlab="Physical Activity",
        ylab="Count")

barplot(table(data$cardio),main="Cardio Vascular Disease",
        xlab="Cardio Vascular Disease",
        ylab="Count")

#printing the correlation matrix

str(data[c(1,3:6)])?
cor(data[c(1,3:6)], use="pairwise.complete.obs")

#KNN
library(caret)
data1 <- data[c(1:11)]
ncol(data1)
#converting the categorical variables to multiple variables with values 0 and 1 only for KNN
dummy <- dummyVars(" ~ .", data=data1)
newdata <- data.fr?me(predict(dummy, newdata = data1))
newdata$cardio <- data[12]
ncol(newdata)
head(newdata)
head(data)
#Splitting the dataset
ran <- sample(1:nrow(newdata), 0.5 * nrow(newdata))

trainX <- newdata[c(1:19)][ran,]
nrow(trainX)

testX <- newdata[c(1:19)][-ran,?
nrow(testX)

trainY <- newdata[ran,20]
trainY <- trainY$cardio
class(trainY)
testY <- newdata[-ran,20]
head(testY)
testY <- testY$cardio
class(testY)
head(trainX)
head(trainY)
head(testX)
head(testY)
#fitting the model using k=5
pred <- knn(trainX,testX,t?ainY,k=5)
#confusion matrix
table(pred,testY)
#acuracy
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}

accuracy(table(pred,testY))


i=1
k.optm=1
while (i <= 26){
  knn.mod <- knn(train = trainX, test = testX, cl = trainY, k=i)
  knn.res <- ?able(knn.mod,testY)
  k.optm[i] <- accuracy(knn.res)
  cat('k[', i, ']', '=', k.optm[i],'\n')
  i = i+5
}
plot(k.optm, type="b", xlab="K- Value",ylab="Accuracy level")



# logistic regression 

train1 <- data[c(1:12)][ran,]
test1 <- data[c(1:12)][-ran,]

?glm.fits=glm(cardio~ ., data = train1 ,family =binomial )
summary (glm.fits)


anova(glm.fits , test = "Chisq")


fitted.results <- predict(glm.fits, newdata=subset(test1,select=c(1:11)),type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
m?sClasificError <- mean(fitted.results != train1$cardio)
print(paste('Accuracy',1-misClasificError))


p <- predict(glm.fits, newdata=subset(test1,select=c(1:11)),type='response')
pr <- prediction(p , test1$cardio)
prf <- performance(pr, measure = "tpr", x.?easure = "fpr")
plot(prf)


auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc



# naive bayes 
# dividing the data in 75% and 25%

indxTrain <- createDataPartition(y = data$cardio ,p = 0.75,list = FALSE)

training <- data[indxTrain,]
te?ting <- data[-indxTrain,]

prop.table(table(training$cardio))

#create objects x which holds the predictor variables and y which holds the response variables
x = training[1:11]
y = training$cardio

model <- naiveBayes(x , y)

Predict <- predict(model,newda?a = testing )
misClasificError <- mean(Predict != testing$cardio)
print(paste('Accuracy',1-misClasificError))

Predict <- predict(model,newdata = training )
misClasificError <- mean(Predict != training$cardio)
print(paste('Accuracy',1-misClasificError))


?onfusionMatrix(Predict , training$cardio)
----------------------------------------------------------------------------------------------------------------------


#rendom forest 

x = training[1:11]
y = training$cardio

fitRF <- randomForest(y~., x)
fittin? <- predict(fitRF,newdata = testing)

confusionMatrix(fitting , testing$cardio)

##------------------------------ rought work code add here ---------------------------------------------

##-------------------------------------d4----------------------------?-----------------------------------
## logistic regression with whole dataset 
data

i=1
acc= list()
while (i <= 10){
  lr.mod <- glm(cardio~ poly(age, i)+gender+poly(height,i)+
                  poly(weight,i)+poly(ap_hi,i)+cholesterol+gluc+smoke+alco+act?ve ,
                data = data ,family =binomial )
  print(summary(lr.mod))
  fitted.results <- predict(lr.mod, newdata=subset(data,select=c(1:11)),type='response')
  fitted.results <- ifelse(fitted.results > 0.5,1,0)
  misClasificError <- mean(fitted.re?ults == data$cardio)
  print(misClasificError)
  acc[[i]] <- misClasificError
  i = i+1
}

acc
plot(unlist(acc))

#----------------------------------------------------------------------------------
#---------logistic regression with - validation set approc? ----------------------------


train1 <- data[c(1:12)][ran,]
test1 <- data[c(1:12)][-ran,]


i=1
acc= list()
while (i <= 10){
  lr.mod <- glm(cardio~ poly(age, i)+gender+poly(height,i)+
                  poly(weight,i)+poly(ap_hi,i)+cholesterol+gluc+smoke?alco+active ,
                data = train1 ,family =binomial )
  print(summary(lr.mod))
  fitted.results <- predict(lr.mod, newdata=subset(test1,select=c(1:11)),type='response')
  fitted.results <- ifelse(fitted.results > 0.5,1,0)
  misClasificError <- me?n(fitted.results == test1$cardio)
  print(misClasificError)
  acc[[i]] <- misClasificError
  i = i+1
}
acc
plot(unlist(acc))                
      

#----------------------------------------------------------------------------------
#---------logistic regr?ssion with - leave one out approch (do not run takes hours)----------------------------

data

i=1
acc= list()
while (i <= 1){
  lr.mod <- glm(cardio~ poly(age, i)+gender+poly(height,i)+
                  poly(weight,i)+poly(ap_hi,i)+cholesterol+gluc+smoke?alco+active ,
                data = data ,family =binomial )
  cv.err =cv.glm(data ,lr.mod)
  print(cv.err$delta)
  acc[[i]] <- cv.err$delta
  i = i+1
}

acc
plot(unlist(acc))

#-----------------------------------------------------------------------------?---------------- 
#----------------------------------------------------------------------------------
#---------logistic regression with - k- fold----------------------------


i=1
acc= list()
while (i <= 10){
  lr.mod <- glm(cardio~ poly(age, i)+gender+po?y(height,i)+
                  poly(weight,i)+poly(ap_hi,i)+cholesterol+gluc+smoke+alco+active ,
                data = data ,family =binomial )
  cv.err =cv.glm(data ,lr.mod , K = 10 )
  print(cv.err$delta[1])
  acc[[i]] <- 1- cv.err$delta[1]
  i = i+1
}
?acc
plot( unlist(acc))


#------------------------------------------------------------------------------
#------------------------------NAIVE BAYS WITH WHOLE dataset ---------------------------------

data
model <- naiveBayes(data$cardio~ .,
              ?     data = data)

Predict <- predict(model,newdata = data )
misClasificError <- mean(Predict != data$cardio)
print(paste('Accuracy',1-misClasificError))



#---------------------------------------------------------------------------------------------
  #-?----------------------------NAIVE BAYS WITH vaidation set dataset ---------------------------------
indxTrain <- createDataPartition(y = data$cardio ,p = 0.75,list = FALSE)

training <- data[indxTrain,]
testing <- data[-indxTrain,]

prop.table(table(traini?g$cardio))

#create objects x which holds the predictor variables and y which holds the response variables
x = training[1:11]
y = training$cardio

model <- naiveBayes(x , y)

Predict <- predict(model,newdata = testing )
misClasificError <- mean(Predict != ?esting$cardio)
print(paste('Accuracy',1-misClasificError))

Predict <- predict(model,newdata = training )
misClasificError <- mean(Predict != training$cardio)
print(paste('Accuracy',1-misClasificError))


confusionMatrix(Predict , training$cardio)

#------?-------------------------------------------------------------------------------------

x = data[1:11]
y = data$cardio


train_control <- trainControl(
  method = "cv", 
  number = 10)

nb.m1 <- train(
  x = x,
  y = y,
  method = "nb",
  trControl = train_?ontrol
)

Predict <- predict(nb.m1,newdata = data )
misClasificError <- mean(Predict != data$cardio)
print(paste('Accuracy',1-misClasificError))
confusionMatrix(nb.m1)




#------------------------------------------------------

#### Cross Validation for K?N 
trainData <- newdata[c(1:19)]
trainLabel <- newdata[20]
set.seed(1)
#5 fold cross validation
y <- newdata[[20]]
y <- y$cardio
idx <- createFolds(y, k=5)
sapply(idx, length)
error5Fold <- c()
accuracy5Fold <- c()
iteration <- c(1:5)
for (i in 1:5) {
  pr?d <- knn(train=trainData[ -idx[[i]] , ], test=trainData[ idx[[i]], ], cl=trainLabel$cardio[ -idx[[i]] , ], k=26)
  print(paste0(i,") error rate: ", round(mean(trainLabel$cardio[ idx[[i]], ] != pred),3)))
  error5Fold[i] <- round(mean(trainLabel$cardio[ idx?[i]], ] != pred),3)
  print(accuracy(table(pred,trainLabel$cardio[ idx[[i]], ])))
  accuracy5Fold[i] <- accuracy(table(pred,trainLabel$cardio[ idx[[i]], ]))
}
error5Fold
accuracy5Fold
Fold5 <- data.frame(iteration,error5Fold,accuracy5Fold)
Fold5
ggplot(Fol?5, aes(x = iteration, y = accuracy5Fold)) +
  geom_line()
ggplot(Fold5, aes(x = iteration, y = error5Fold)) +
  geom_line()




#10 fold cross validation 
idx <- createFolds(y, k=10)
sapply(idx, length)
error10Fold <- c()
accuracy10Fold <- c()
iteration <-?c(1:10)
for (i in 1:10) {
  pred <- knn(train=trainData[ -idx[[i]] , ], test=trainData[ idx[[i]], ], cl=trainLabel$cardio[ -idx[[i]] , ], k=26)
  print(paste0(i,") error rate: ", round(mean(trainLabel$cardio[ idx[[i]], ] != pred),3)))
  error10Fold[i] <- r?und(mean(trainLabel$cardio[ idx[[i]], ] != pred),3)
  print(accuracy(table(pred,trainLabel$cardio[ idx[[i]], ])))
  accuracy10Fold[i] <- accuracy(table(pred,trainLabel$cardio[ idx[[i]], ]))
}
error10Fold
accuracy10Fold
Fold10 <- data.frame(iteration,error1?Fold,accuracy10Fold)
Fold10
ggplot(Fold10, aes(x = iteration, y = accuracy10Fold)) +
  geom_line()
ggplot(Fold10, aes(x = iteration, y = error10Fold)) +
  geom_line()


##----------------------------------------------------------------

lr.mod <- glm(cardi? ~ age + height +
                weight + ap_hi + ap_lo, 
              data = data ,family = "binomial")
summary(lr.mod)

# Setting up the non-parametric bootstrap
logit.bootstrap <- function(data, indices) {
  d <- data[indices, ]
  fit <- glm(cardio ~ ?ge + height +
               weight + ap_hi + ap_lo, 
             data = d ,family = "binomial")
  
  return(coef(fit))
}
nrow(data)
logit.bootstrap(data=data, 1:68586)

set.seed(956)
# using sample to randomly select 100 observations from the range 1 to ?00, with replacement.
logit.bootstrap(data=data, sample(100, 68586, replace = TRUE))

logit.boot <- boot(data=data, logit.bootstrap, R= 1000) # 1000 samples
logit.boot

# Calculate confidence intervals for each coefficient

boot.ci(logit.boot, type="basic"? index=1)  # intercept
boot.ci(logit.boot, type="basic", index=2)  # age
boot.ci(logit.boot, type="basic", index=3)  # height
boot.ci(logit.boot, type="basic", index=4)  # weight
boot.ci(logit.boot, type="basic", index=5)  # ap_hi
boot.ci(logit.boot, type=?basic", index=6)  # ap_lo

plot(logit.boot)
#----------------------------------------bmi --------------------------------------------------------

# additional work before D5

newdata <- data

newdata$bmi <- (newdata$weight)/(newdata$height/100)**2
newdata?height = NULL
newdata$weight = NULL
ndata <- newdata[,c(1:9,11,10)]



glm.fits2=glm(cardio~ ., data = ndata ,family =binomial )
summary (glm.fits2)

p <- predict(glm.fits2, newdata=ndata[,c(1:10)],type='response')
p <- ifelse(p > 0.5,1,0)

misClasificErro? <- mean(p != ndata$cardio)
print(paste('Accuracy',1-misClasificError))


library(leaps)
# ----------------------------------Step wise selection ----------------------------------------

full.model <-glm(cardio~ ., data = data ,family =binomial )

sub.mode?s <- regsubsets(cardio~ ., data = data , nvmax = 13)
summary(sub.models)

# ----------------------------forward selection--------------------------------------------------

set.seed(123)

# full model
# Our liner regression model
lr.mod <-glm(cardio~ ., da?a = data ,family =binomial )

#forward model using step function 
forward.model <- step(lr.mod, direction = "forward")
summary(forward.model)

#best forward.model based on AIC value 
step.model <- stepAIC(lr.mod, direction = "forward", 
                   ?  trace = FALSE)
summary(step.model)

# applying K fold on stepmodel with K = 10
cv.err =cv.glm(data ,step.model , K = 10)
print(cv.err$delta[1])
print(cv.err$delta[2])

#finding accuracy of step.model 
p <- predict(step.model, data ,type='response')
p <- ?felse(p > 0.5,1,0)
accuracy <- mean(p == data$cardio)
print(paste('Accuracy',accuracy ))

# ----------------------------backward selection--------------------------------------------------
# orignal liner regression model
lr.mod <-glm(cardio~ ., data = dat? ,family =binomial )
summary(lr.mod)

# bakward selection using step function
backward <- step(lr.mod)
summary(backward)

# backward selection using baest AIC value using StepAIC function
step.model <- stepAIC(lr.mod, direction = "backward", trace = FALSE)?summary(step.model)


# applying K fold on stepmodel with K = 10
cv.err =cv.glm(data ,step.model , K = 10)
print(cv.err$delta[1])
print(cv.err$delta[2])

# finding accuracy of the forward model
p <- predict(step.model, data ,type='response')
p <- ifelse(p ? 0.5,1,0)
accuracy <- mean(p == data$cardio)
print(paste('Accuracy',accuracy ))

#--------------------------- rigged ------------------------------------------------------------------------

lr.mod <-glm(cardio~ ., data = data ,family =binomial )
summary(l?.mod)

df <- data

#model.matrix creates a  model matrix, by expanding factors to a set of dummy variables.

df.x = model.matrix(cardio~ .,df )[,-1]
df.y = df$cardio


lambdas <- 10^seq(-5, 2, by = .1)

ridge_reg = glmnet(df.x, df.y, nlambda = 100, alpha =?0 , family = binomial, lambda = lambdas)
#assess.glmnet(ridge_reg, newx = df.test.x , newy = df.test.y )
plot(ridge_reg, xvar = "lambda")

#to find out optimal lambda
#Cross-Validation For Glmnet Does k-fold cross-validation for glmnet, produces a plot, an? returns a value for lambda, I have not defined nfold as default is 10

cv_ridge <- cv.glmnet(df.x, df.y, nlambda = 100, alpha = 0 , family = binomial, lambda = lambdas)
optimal_lambda <- cv_ridge$lambda.min
print(optimal_lambda)
plot(cv_ridge)

#-creating?new model with optimal lambda value


ridge_reg2 = glmnet(df.x, df.y, nlambda = 1, alpha = 0 , family = binomial, lambda = optimal_lambda)
#summary (ridge_reg2)
#coef(ridge_reg2)


fitted.results <- predict(ridge_reg2, newx = df.x)
#final <- cbind(df.y, fi?ted.results)
#head(final)
fitted.results <- ifelse(fitted.results > 0,1,0)
misClasificError <- mean(fitted.results != df.y)
print(paste('Accuracy',1-misClasificError))

#------------------------------lasso --------------------------------------------------?----------------

df <- data

#model.matrix creates a  model matrix, by expanding factors to a set of dummy variables.
df.x = model.matrix(cardio~ .,df )[,-1]
df.y = df$cardio

#range of lambda values
lambdas <- 10^seq(-5, 2, by = .1)

#creating lasso mode? with range of lambda values to see change in daviance 
lasso.mod = glmnet(df.x,df.y, alpha=1, family = "binomial", lambda= lambdas) 
plot(lasso.mod)

# founding optimal lambda value With K fold cross validation 
set.seed (123)
cv.lasso =cv.glmnet(df.x,df.?, alpha=1, family = binomial, lambda = lambdas)
plot(cv.lasso)

# finding optimal lambda value with $lambda.min
optimal_lambda = cv.lasso$lambda.min
print(optimal_lambda)


# now with optimal lambda value creating best lasso model
lasso.best=glmnet(df.x,df?y,alpha=1,family = "binomial",lambda= optimal_lambda) 
coef(lasso.best)

lasso.pred = predict(lasso.best,s= optimal_lambda , newx = df.x )
fitted.results <- ifelse(lasso.pred > 0,1,0)
error <- mean(fitted.results != df.y)
print(paste('Accuracy',1- error))
?#------------------------------------ non liner regression model --------------------------

gamdata <- ndata
print(gamdata)
library(gam)  # To search on the gam function 
logitgam.m1<-gam(cardio ~ s(age,df=5) + s(bmi,df=4) + cholesterol + gluc ,data=gamda?a,family=binomial)
#in the above function s() is the shorthand for fitting smoothing splines 
#in gam() function

summary(logitgam1)
par(mfrow=c(1,4))
plot(logitgam.m1, se=TRUE,col="blue") #se stands for standard error Bands

#model which is Linear in vari?ble 'bmi'.
logitgam.m2<-gam(cardio ~ s(age,df=5) + bmi + cholesterol + gluc,data=gamdata,family=binomial)
plot(logitgam.m2,se=TRUE)

#anova() function is to test the goodness of fit and choose the best Model

anova(logitgam.m1,logitgam.m2, test = "Chisq") ?#Using Chi-squared Non parametric Test for Binary Classification Problem and categorical Target

logitgam.m3<-gam(cardio ~ s(age,df=5) + s(ap_hi, df= 6) + cholesterol + gluc,data=gamdata,family=binomial)
plot(logitgam.m3,se=TRUE)

#model which is Linear in?variable 'ap_hi'
logitgam.m4<-gam(cardio ~ s(age,df=5) + ap_hi + cholesterol + gluc,data=gamdata,family=binomial)
plot(logitgam.m4,se=TRUE)

anova(logitgam.m3,logitgam.m4, test = "Chisq") 

# ------- checking with all numeric variables --------------------?---

logitgam.mod1 <-gam(cardio ~ s(age,df=4) + s(bmi,df=4) 
                    + s(ap_hi ,df = 4) + s(ap_lo ,df = 4)+cholesterol + gluc ,data=gamdata,family=binomial)

logitgam.mod2 <-gam(cardio ~ s(age,df=4) + s(bmi,df=4) 
                    + s(ap_hi ?df = 4) + ap_lo +cholesterol + gluc ,data=gamdata,family=binomial)

logitgam.mod3 <-gam(cardio ~ s(age,df=4) + s(bmi,df=4) 
                    + ap_hi + ap_lo +cholesterol + gluc ,data=gamdata,family=binomial)

logitgam.mod4 <-gam(cardio ~ s(age,df=4) + b?i 
                    + ap_hi + ap_lo +cholesterol + gluc ,data=gamdata,family=binomial)

summary(logitgam.mod1)
summary(logitgam.mod2)
summary(logitgam.mod3)
summary(logitgam.mod4)

par(mfrow=c(1,4))
plot(logitgam.mod1, se=TRUE,col="blue")
par(mfrow=c(1,?))
plot(logitgam.mod2, se=TRUE,col="blue")
par(mfrow=c(1,4))
plot(logitgam.mod3, se=TRUE,col="blue")
par(mfrow=c(1,4))
plot(logitgam.mod4, se=TRUE,col="blue")

anova(logitgam.mod1,logitgam.mod2,logitgam.mod3,logitgam.mod4 ,test = "Chisq") 
