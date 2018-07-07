# Setting working directory
setwd("E:/IMS BA/Capstone Project 8Jun2018/project 23jun2018")

# Getting working directory
getwd()

# Reading the data file and storing the same to object data
data = read.csv("market campaign.csv")

# Few commands to veiw and analyze data
View(data)
str(data)
summary(data)
head(data)
tail(data)
library(dplyr)
glimpse(data)
# So prima-facie with the help of above commands, we could see that there are few NAs (i.e. missing values) and blanks in the data. So next step is to treat these missing values, for that we need to see the distribution of these variables and also need to see presence of outliers if any.

#Missing values visvualization
library(mice)
md.pattern(data)
?md.pattern
library(VIM)
?aggr
aggr(data,numbers=TRUE, prop=FALSE)
matrixplot(data)
mice_plot <- aggr(data, col=c('navyblue','yellow'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(data), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"))

# Checking distribution of variables
hist(data$age)
#so age is nearly normally distributed
summary(data$age)
#Replacing NAs with mean as age is normally distributed
data$age[is.na(data$age)]=41
summary(data$age)
#Now missing values of variable age is replaced with mean
#Checking for outliers
boxplot(data$age)
#SO there are some outliers, but keep them as it is as we do not want to alter our original data much

summary(data$days_since._signed_in)

hist(data$days_since._signed_in)

data$days_since._signed_in[is.na(data$days_since._signed_in)]=15.91

summary(data$days_since._signed_in)

summary(data$Time.spend.on.website)

hist(data$Time.spend.on.website)

data$Time.spend.on.website[is.na(data$Time.spend.on.website)]=185

summary(data$Time.spend.on.website)

summary(data$Number_of_campaigns)

hist(data$Number_of_campaigns)

data$Number_of_campaigns[is.na(data$Number_of_campaigns)]=2

summary(data$Number_of_campaigns)

summary(data$previous_purchases)

hist(data$previous_purchases)

data$previous_purchases[is.na(data$previous_purchases)]=0

summary(data$previous_purchases)

summary(data)

View(data)
#deleting missing value records
data1=data[-c(740,40,116,3176,170,211,56,131,251,547,491,3177,157,119,306,142,93),]
write.csv(data1, file = "my_data1.csv")
View(data1)

summary(data1$job)

str(data1$job)
table(data1$job)
#Dummy variable creation:
data1$admin. = ifelse(data1$job=="admin.",1,0)
data1$bluecollar = ifelse(data1$job == "blue-collar",1,0)
data1$entrepreneur = ifelse(data1$job == "entrepreneur",1,0)
data1$housemaid = ifelse(data1$job == "housemaid",1,0)
data1$management = ifelse(data1$job == "management",1,0)
data1$retired = ifelse(data1$job == "retired",1,0)
data1$selfemployed = ifelse(data1$job == "self-employed",1,0)
data1$services = ifelse(data1$job == "services",1,0)
data1$student = ifelse(data1$job == "student",1,0)
data1$technician = ifelse(data1$job == "technician",1,0)

View(data1)

summary(data1$marital)

data1$married = ifelse(data1$marital == "married",1,0)
data1$single = ifelse(data1$marital == "single",1,0)

summary(data1$education)

data1$primary = ifelse(data1$education == "primary",1,0)
data1$secondary = ifelse(data1$education == "secondary",1,0)
data1$tertiary = ifelse(data1$education == "tertiary",1,0)

summary(data1$contact)

data1$cellular = ifelse(data1$contact == "cellular",1,0)
data1$telephone = ifelse(data1$contact == "telephone",1,0)

View(data1)

data1$Purchase_Made = ifelse(data1$Purchase_Made == "no",0,1)

table(data$Purchase_Made)

View(data1)

finaldata=data1[-c(2:5)]

write.csv(finaldata, file = "finaldata.csv")

View(finaldata)

summary(finaldata)

boxplot(finaldata$married)
library(car)

scatterplot(finaldata$age,finaldata$Purchase_Made)
scatterplot(finaldata$age,finaldata$Time.spend.on.website)
#Dividing dataset into train and test data sets
set.seed(9)
t=sample(1:nrow(finaldata),0.7*nrow(finaldata))
t_train=finaldata[t,]
t_test=finaldata[-t,]

#Biulding model on train dataset
mod= lm(Purchase_Made ~ ., data = t_train)
summary(t_train)
#Checking multicollinearity
t = vif(mod)
sort(t, decreasing = T)
#Removing variables with vif>5 to reduce multicollinearity
finaldata1=finaldata[-c(11,20,21,8,16)]
View(finaldata1)
#post removal of variables causing multicollinearity, dividing the dataset in train & test
set.seed(9)
t=sample(1:nrow(finaldata1),0.7*nrow(finaldata1))
t_train=finaldata1[t,]
View(t_train)
t_test=finaldata1[-t,]
View(t_test)
mod= lm(Purchase_Made ~ ., data = t_train)
t = vif(mod)
sort(t, decreasing = T)
#now vif is <5, so dataset do not have much multicollinearity and now we can move ahead with model building
mod1 <- glm(as.factor(Purchase_Made) ~ ., family="binomial", data=t_train)
summary(mod1)
stpmod = step(mod1, direction = "both")
formula(stpmod)
summary(stpmod)
mod2 <- glm(as.factor(Purchase_Made) ~ age + Time.spend.on.website + Number_of_campaigns + previous_purchases + admin. + retired + student + married + primary + cellular + telephone , family="binomial", data=t_train)
summary(mod2)
t_train$score=predict(mod2,newdata=t_train,type = "response")
head(t_train$score)
tail(t_train$score)
library(lattice)
library(ggplot2)
library(caret)
library(e1071)
prediction = ifelse(t_train$score>=0.52,1,0)
confusionMatrix(factor(prediction),factor(t_train$Purchase_Made), positive = "1")
library(pscl)
pR2(mod2)
#library(InformationValue)
library(caret)
concor <- Concordance(t_train$Purchase_Made,t_train$score)
concor
plotROC(actuals = t_train$Purchase_Made,predictedScores = as.numeric(fitted(mod2)))
ks_plot(actuals = t_train$Purchase_Made,predictedScores = as.numeric(fitted(mod2)))
ks_stat(actuals = t_train$Purchase_Made,predictedScores = as.numeric(fitted(mod2)))
t_test$score2= predict(mod2, t_test, type="response")
View(t_test)

prediction_test = ifelse(t_test$score2>=0.52,1,0)
View(prediction_test)
confusionMatrix(factor(prediction_test),factor(t_test$Purchase_Made),positive = "1")
tt = cbind(t_test,prediction_test)
View(tt)
library(pscl)

#library(InformationValue)
library(caret)
concor <- Concordance(t_test$Purchase_Made,t_test$score2)
concor
plotROC(actuals = t_test$Purchase_Made,predictedScores = as.numeric(t_test$score2))
ks_plot(actuals = t_test$Purchase_Made,predictedScores = as.numeric(t_test$score2))
ks_stat(actuals = t_test$Purchase_Made,predictedScores = as.numeric(t_test$score2))
#end
View(t_train)
#Random forest model:
library(randomForest)
modelrf <- randomForest(as.factor(Purchase_Made) ~ . , data = rf_train, do.trace=T)
modelrf
importance(modelrf)
varImpPlot(modelrf)
View(t_train)

View(t_test)
rf_train=t_train[-19]
View(rf_train)
rf_test=t_test[-19]
View(rf_test)
predrf_tr <- predict(modelrf, rf_train)
predrf_test <- predict(modelrf, rf_test)
confusionMatrix(factor(predrf_tr),factor(rf_train$Purchase_Made))
confusionMatrix(factor(predrf_test),factor(rf_test$Purchase_Made))
#end of RF

#Decison tree:
library(data.table)
library(reshape2)
library(randomForest)
library(party)
library(rpart)
library(rpart.plot)
library(lattice)
require(caret)
library(pROC)
library(corrplot)
library(e1071)
library(RColorBrewer)
View(finaldata)

corrplot(cor(finaldata[,1:23]), method="circle")
fit = rpart(Purchase_Made ~ ., data = rf_train,method = "class", control = rpart.control(minsplit = 30,cp = 0.01))
rpart.plot(fit)
summary(fit)
prp(fit)
plotcp(fit)
predtr <- predict(fit,rf_train,type = "class" )
predtr
confusionMatrix(factor(predtr),factor(rf_train$Purchase_Made))
predtest <- predict(fit,rf_test, type = "class")
confusionMatrix(factor(predtest),factor(rf_test$Purchase_Made))
auctrain <- roc(as.numeric(rf_train$Purchase_Made), as.numeric(predtr))
auctest <- roc(as.numeric(rf_test$Purchase_Made), as.numeric(predtest))
plot(auctrain, ylim=c(0,1), print.thres=TRUE, main=paste('AUC:',round(auctrain$auc[[1]],3)),col = 'blue')
plot(auctest, ylim=c(0,1), print.thres=TRUE, main=paste('AUC:',round(auctest$auc[[1]],3)),col = 'blue')
#end