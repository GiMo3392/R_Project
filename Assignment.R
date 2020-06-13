data=data.frame(read.csv("Attrition.csv") )# read CSv

str(data)
summary(data)
View(data)


data$EmployeeCount <- NULL
data$Over18 <- NULL
data$StandardHours <- NULL
data$EmployeeNumber<- NULL

data$Education <- as.factor(data$Education)
data$EnvironmentSatisfaction <- as.factor(data$EnvironmentSatisfaction)
data$JobInvolvement <- as.factor(data$JobInvolvement)
data$JobSatisfaction <- as.factor(data$JobSatisfaction)
data$PerformanceRating <- as.factor(data$PerformanceRating)
data$RelationshipSatisfaction <- as.factor(data$RelationshipSatisfaction)
data$WorkLifeBalance <- as.factor(data$WorkLifeBalance)
data$StockOptionLevel <- as.factor(data$StockOptionLevel)
data$JobLevel <- as.factor(data$JobLevel)


str(data)
#-----------------------------------------------------------------------------------------------------------------------
skewness(data$DistanceFromHome)

skewness(data$MonthlyIncome)

#ourliers for monthly income

out_qua_check = function(x){
  Q1 = quantile(s5,0.25,na.rm = TRUE)
  Q3= quantile(s5,0.75,na.rm = TRUE)
  IQR = Q3-Q1
  lc = Q1 - 1.5*IQR
  uc = Q3 + 1.5*IQR
  n =which(s5>uc | s5 < lc)
  val=list(num=n,lower_cutoff=lc,upper_cutoff=uc)
  return(val)
}

outlier_function(data$MonthlyIncome) # calling outlier function
outlier_val=outlier_function(data$MonthlyIncome) # substituting the outlier in one varible
data[outlier_val$num,]$MonthlyIncome # check values at above location
mean(data$MonthlyIncome) # check mean of target varible 
data[outlier_val$num,]$MonthlyIncome <- mean(data$MonthlyIncome)

skewness(data$MonthlyIncome)
hist(data$MonthlyIncome)

View(data)
str(data)

skewness(data$YearsAtCompany)
#-------------------------------------------------------------------------------------------------------------------
#ourliers for YearsAtCompany

outlier_function(data$YearsAtCompany) # calling outlier function
outlier_val=outlier_function(data$YearsAtCompany) # substituting the outlier in one varible
data[outlier_val$num,]$YearsAtCompany # check values at above location
mean(data$YearsAtCompany) # check mean of target varible 
data[outlier_val$num,]$YearsAtCompany <- mean(data$YearsAtCompany)

skewness(data$YearsAtCompany)
hist(data$YearsAtCompany)

#-------------------------------------------------------------------------------------------------------------------
  str(data)
  skewness(data$MonthlyRate)
skewness(data$NumCompaniesWorked)

outlier_function(data$NumCompaniesWorked) # calling outlier function
outlier_val=outlier_function(data$NumCompaniesWorked) # substituting the outlier in one varible
data[outlier_val$num,]$NumCompaniesWorked # check values at above location
mean(data$NumCompaniesWorked) # check mean of target varible 
data[outlier_val$num,]$NumCompaniesWorked <- mean(data$NumCompaniesWorked)

skewness(data$NumCompaniesWorked)
hist(data$NumCompaniesWorked)

#-------------------------------------------------------------------------------------------------------------------
str(data)
skewness(data$TotalWorkingYears)

outlier_function(data$TotalWorkingYears) # calling outlier function
outlier_val=outlier_function(data$TotalWorkingYears) # substituting the outlier in one varible
data[outlier_val$num,]$TotalWorkingYears # check values at above location
mean(data$TotalWorkingYears) # check mean of target varible 
data[outlier_val$num,]$TotalWorkingYears <- mean(data$TotalWorkingYears)

skewness(data$TotalWorkingYears)
hist(data$TotalWorkingYears)

#-------------------------------------------------------------------------------------------------------------------
str(data)
skewness(data$PercentSalaryHike)
skewness(data$YearsInCurrentRole)
skewness(data$YearsWithCurrManager)

#----------------------------------------------------------------------------------------------------------------------
skewness(data$YearsSinceLastPromotion)

outlier_function(data$YearsSinceLastPromotion) # calling outlier function
outlier_val=outlier_function(data$YearsSinceLastPromotion) # substituting the outlier in one varible
data[outlier_val$num,]$YearsSinceLastPromotion # check values at above location
mean(data$YearsSinceLastPromotion) # check mean of target varible 
data[outlier_val$num,]$YearsSinceLastPromotion <- mean(data$YearsSinceLastPromotion)

skewness(data$TotalWorkingYears)
hist(data$TotalWorkingYears)

#----------------------------------------------------------------------------------------------------------------------

#To segrigate the data in numeric and categorical level


data.factor= data[sapply(data,is.factor)] 
data.numeric= data[sapply(data,is.numeric)] 

str(data.numeric)
str(data.factor)

corrplot(cor(data.numeric))


#Significance with target variable.
View(data.factor)


df <- data.factor    #this supposed to be a data frame
x<-list()       # return var as a list
for (i in 1:length(df))
{
  if(i < length(df)){ k= i+1 }
  if((class(df[,i])== class(df[,k])) & class(df[,i])=='factor' )
  {
    y<-  chisq.test(table(df[,i], df[,k]))
    vec<-c(Var=paste(colnames(df[i]),"-",colnames(df[k])), y$statistic, pval=y$p.value, significant = (if(y$p.value<0.05 & y$p.value!='NaN') "yes" else "no"))
    x<-list.append(x, vec)
  }
}
print(x)
#
#
#Checking co-relation matrix.
descrCor <- cor(data.numeric)
print(descrCor)

corrplot(descrCor, type = "upper", method = "number")
corrplot.mixed(descrCor, lower.col = "black", number.cex = .7)


#----------------------------------------------------------------------------------------------------------------------

y <- data$Attrition

View(y)

data$Attrition <- NULL

data <- cbind(data,y)
View(data)

y=data$Attrition

#----------------------------------------------------------------------------------------------------------------------

# Manova testing 

testing= manova(cbind(data$age, data$DailyRate, data$DistanceFromHome, data$HourlyRate, data$MonthlyIncome, data$NumCompaniesWorked) ~data$y)
summary(testing)


#----------------------------------------------------------------------------------------------------------------------

#By checking the coploting and qsuqare resting below are the columns which are not highly significant hence removing all by checking the co-relation matrix 
data$Department <- NULL
data$DailyRate <- NULL
data$EnvironmentSatisfaction <- NULL
data$Gender <- NULL
data$HourlyRate <-NULL
data$JobInvolvement <- NULL
data$JobRole <- NULL
data$JobSatisfaction <- NULL
data$MaritalStatus <- NULL
data$MonthlyRate <- NULL
data$NumCompaniesWorked <- NULL
data$OverTime <- NULL
data$PercentSalaryHike <- NULL
data$PerformanceRating <- NULL
data$RelationshipSatisfaction <- NULL
data$StockOptionLevel <- NULL
data$TrainingTimesLastYear <- NULL

#------------------------------------------------------------------------------------------------------------------------
# Test data set 

set.seed(99)
s=sample(1:nrow(data),0.70*nrow(data))
train=data[s,]
test=data[-s,]
test1 <- test[, -14]
View(test1)
View(train)
View(test)

#--------------------------------------------------------------------------------------------------------------------------
#applying logistic regression

model=glm(y~.,data=train,family=binomial("logit"))

summary(model)

#stepwise sleection:

nullModel<- glm(y ~ 1, data=train,family=binomial("logit"))

summary(nullModel)

fullmodel <- glm(y ~., data=train,family=binomial("logit"))
summary(fullmodel)

fit <- step(nullModel, scope=list(lower=nullModel, upper=fullmodel), direction="both")

#revel the model
fit


#Final model
model <- glm(formula = y ~ JobLevel + BusinessTravel + Age + YearsAtCompany + DistanceFromHome + 
               YearsSinceLastPromotion + WorkLifeBalance + EducationField, family = binomial("logit"), data = train)


summary(model)

dim(test)
View(test1)
fitted.results = predict(model,newdata=test1, type='response')

View(fitted.results)

fitted.results1 = ifelse(fitted.results >=0.5  ,1,0)

#confusion matrix
table(fitted.results1)
table(test$y)
cf1 = table(test$y, fitted.results1)
cf1

TP=358
FP=3
FN=71
TN=9



accuracy = ((TN+TP)/(TP+FP+FN+TN))*100
accuracy
Sensitivity=(TP/(TP+FN))*100  
Sensitivity
specificity=(TN/(TN+FP))*100  
specificity
error=((FP+FN)/(TP+TN))*100
error


roccurve=roc(test$y, fitted.results1)
plot(roccurve)
auc(roccurve)


