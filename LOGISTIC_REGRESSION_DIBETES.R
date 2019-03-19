data <- read.csv("C:/Users/LENOVO/Desktop/R programs/Logistic_Regressioin_Diabetes_dataset.csv")
head(data)
str(data)
names(data)


data$Pregnancies <- as.numeric(data$Pregnancies)
data$Glucose <- as.numeric(data$Glucose)
data$BloodPressure <- as.numeric(data$BloodPressure)
data$SkinThickness <-as.numeric(data$SkinThickness)
data$Insulin <- as.numeric(data$Insulin)
data$Age <- as.numeric(data$Age)
data$Outcome <- as.factor(data$Outcome)
str(data)

summary(data)
boxplot(data)
sapply(data, function(x) sum(is.na(x)))

############################ Pregnancies ####################
boxplot(data$Pregnancies)
summary(data$Pregnancies)
upper <- 6.0 + 1.5 * IQR(data$Pregnancies)
upper

data$Pregnancies[data$Pregnancies > upper] <- upper
boxplot(data$Pregnancies)

############################ Glucose #########################
boxplot(data$Glucose)
summary(data$Glucose)


lower <- 99 - 1.5 * IQR(data$Glucose)
lower
data$Glucose[data$Glucose < lower] <- lower
boxplot(data$Glucose)

############################ BloodPressure ####################
boxplot(data$BloodPressure)
summary(data$BloodPressure)
upper <- 80 + 1.5 * IQR(data$BloodPressure)
upper


data$BloodPressure[data$BloodPressure > upper] <- upper
boxplot(data$BloodPressure)

lower <- 62 - 1.5 * IQR(data$BloodPressure)
lower
data$BloodPressure[data$BloodPressure < lower] <- lower
boxplot(data$BloodPressure)
############################ SkinThickness ####################
boxplot(data$SkinThickness)
summary(data$SkinThickness)
upper <- 32 + 1.5 * IQR(data$SkinThickness)
upper


data$SkinThickness[data$SkinThickness > upper] <- upper
boxplot(data$SkinThickness)

############################ Insulin ####################
boxplot(data$Insulin)
summary(data$Insulin)
upper <- 127 + 1.5 * IQR(data$Insulin)
upper


data$Insulin[data$Insulin > upper] <- upper
boxplot(data$Insulin)


############################ BMI ####################
boxplot(data$BMI)
summary(data$BMI)
upper <- 36.60 + 1.5 * IQR(data$BMI)
upper


data$BMI[data$BMI > upper] <- upper
boxplot(data$BMI)

lower <- 27.30 - 1.5 * IQR(data$BMI)
lower
data$BMI[data$BMI < lower] <- lower
boxplot(data$BMI)
############################ DiabetesPedigreeFunction ####################
boxplot(data$DiabetesPedigreeFunction)
summary(data$DiabetesPedigreeFunction)
upper <- 0.6262 + 1.5 * IQR(data$DiabetesPedigreeFunction)
upper


data$DiabetesPedigreeFunction[data$DiabetesPedigreeFunction > upper] <- upper
boxplot(data$DiabetesPedigreeFunction)

############################ Age ####################

boxplot(data$Age)
summary(data$Age)
upper <- 41 + 1.5 * IQR(data$Age)
upper


data$Age[data$Age > upper] <- upper
boxplot(data$Age)

boxplot(data)
##############################################################
#                   DATA PARTITION                           #
##############################################################

library(caret)
Train <- createDataPartition(data$Outcome , p= 0.7, list = FALSE)
training <- data[ Train, ]
testing <- data [-Train, ]

###############################################################
#                    MODEL BUILDING                           #
###############################################################

logit1 <- glm(Outcome ~ . , data = training , family = "binomial")
summary(logit1)

logit <- step(glm(Outcome ~ . - Age, data = training , family = "binomial"), direction = "both")
summary(logit)
Acc(logit)
library(car)
vif(logit)

###############################################################
#                       ODDS RATIO                            #
###############################################################

coef(logit)

exp(coef(logit))

cbind( odds_ratio = exp(coef(logit)) ,exp(confint(logit)) )

###############################################################
#                 PREDICTION ON TEST DATA                     # 
###############################################################

testing$Probs <- predict(logit, testing, type ="response")
testing$Predict <- as.factor(ifelse(testing$Probs>0.70,1,0))
table(testing$Predict,testing$Outcome)
confusionMatrix( testing$Outcome ,  testing$Predict)


 ###############################################################
#                          ROC CURVE                          # 
###############################################################



res<- predict(logit , testing , family="binomial")

library(ROCR)
ROCRPred = prediction(res,testing$Outcome)
ROCRPref <- performance( ROCRPred,"tpr","fpr")
plot(ROCRPref,colorize=TRUE)
pred = prediction(testing$Probs , testing$Outcome)
as.numeric(performance(pred,"auc")@y.values)

###############################################################
#                  K-FOLD CROSS VALIDATION                    # 
###############################################################
library(caret)
crossValSettings <- trainControl(method = "repeatedcv" ,
                                 number = 10 ,
                                 savePredictions = TRUE)

crossVal <- train(as.factor(Outcome) ~ Pregnancies + Glucose + 
                              BloodPressure + BMI + DiabetesPedigreeFunction , 
                              data = data , 
                              family = "binomial" ,
                              method= "glm" ,
                              trControl = crossValSettings)
crossVal
summary(crossVal)
