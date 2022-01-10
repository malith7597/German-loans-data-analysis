
library(RMySQL)
#setup mysql connection
dbcon <- dbConnect(MySQL(),user="root",dbname="loans",password="",host="localhost")

#load the csv into R
credit_info <- read.csv("Loans.csv",sep=",", header = TRUE)
dbWriteTable(dbcon,name="loans",value=credit_info)
# List out table names
dbListTables(dbcon)
# view columns in the "loans" table
dbListFields(dbcon,"loans")

#View the content in the db
dbReadTable(dbcon,"loans")

#data on credit info
head(credit_info)
tail(credit_info)
str(credit_info)
summary(credit_info)

#convert all non-numeric variables into factors using as.factor

credit_info$checking_balance <- as.factor(credit_info$checking_balance)
credit_info$credit_history <- as.factor(credit_info$credit_history)
credit_info$purpose <- as.factor(credit_info$purpose)
credit_info$savings_balance <- as.factor(credit_info$savings_balance)
credit_info$employment_duration <- as.factor(credit_info$employment_duration)
credit_info$other_credit <- as.factor(credit_info$other_credit)
credit_info$housing <- as.factor(credit_info$housing)
credit_info$job <- as.factor(credit_info$job)
credit_info$default <- as.factor(credit_info$default)
str(credit_info)

#calculate the frequency and proportion 
# using  dplyr library.
library(dplyr)
install.packages("plyr")
install.packages("dplyr")
library(plyr)
library(dbplyr)
fre_checking_balance <- data.frame(count(credit_info$checking_balance))
fre_checking_balance$proportion <- prop.table(table(credit_info$checking_balance))
fre_checking_balance
 
#credit history frequency and proportion

fre_credit_history <-data.frame(count(credit_info$credit_history))
fre_credit_history$proportion <- prop.table(table(credit_info$credit_history))
fre_credit_history

#purpose frequency and proportion
fre_purpose <-data.frame(count(credit_info$purpose))
fre_purpose$proportion <- prop.table(table(credit_info$purpose))
fre_purpose

#savings frequency and proportion
fre_savings_balance <- data.frame(count(credit_info$savings_balance))
fre_savings_balance$proportion <- prop.table(table(credit_info$savings_balance))
fre_savings_balance

#employment duration frequency and proportion
fre_employment_duration<- data.frame(count(credit_info$employment_duration))
fre_employment_duration$proportion <- prop.table(table(credit_info$employment_duration))
fre_employment_duration

#percentage of income frequency and proportion
fre_precent_of_income <- data.frame(count(credit_info$percent_of_income))
fre_precent_of_income$proportion <- prop.table(table(credit_info$percent_of_income))
fre_precent_of_income

#frequency of years of residence and proportion
fre_years_of_residence <- data.frame(count(credit_info$years_at_residence))
fre_years_of_residence$proportion <- prop.table(table(credit_info$years_at_residence))
fre_years_of_residence

#frequency of other credit and proportion
fre_other_credit <- data.frame(count(credit_info$other_credit))
fre_other_credit$proportion<- prop.table(table(credit_info$other_credit))
fre_other_credit

#frequency of housing
fre_housing <- data.frame(count(credit_info$housing))
fre_housing$proportion <- prop.table(table(credit_info$housing))
fre_housing

#frequency of existing loans count and proportion
fre_existing_loans_count<- data.frame(count(credit_info$existing_loans_count))
fre_existing_loans_count$proportion <- prop.table(table(credit_info$existing_loans_count))
fre_existing_loans_count

#frequency of job frequency and proportion
fre_job_frequency<- data.frame(count(credit_info$job))
fre_job_frequency$proportion <- prop.table(table(credit_info$job))
fre_job_frequency

#frequency of dependents and proportion
fre_dependents<- data.frame(count(credit_info$dependents))
fre_dependents$proportion <- prop.table(table(credit_info$dependents))
fre_dependents

#frequency of default and proportion
fre_default<- data.frame(count(credit_info$default))
fre_default$proportion <- prop.table(table(credit_info$default))
fre_default

#average calculation
# avg: loan duration
library(dplyr)
library(dbplyr)
library(plyr)
avg_ml<-credit_info %>%summarize(avg_loan_duration =mean(months_loan_duration,na.rm = TRUE))
# avg: loan amount
avg_amount <- credit_info %>% summarize(avg_loan_amount=mean(amount,na.rm=TRUE))
# avg: precent of income
avg_precent_of_income <- credit_info %>% summarize(avg_precent_income= mean(percent_of_income,na.rm = TRUE))
# avg: residence of years
avg_residence_years <- credit_info %>% summarize(avg_residence= mean(years_at_residence,na.rm = TRUE))
#avg:age
avg_age <- credit_info %>% summarize(avg_age= mean(age,na.rm = TRUE))
#avg :existing loan count
avg_existing_loan_count <-credit_info %>% summarize(avg_existing_loan_count= mean(existing_loans_count,na.rm = TRUE))
# avg:dependents
avg_dependents <- credit_info %>% summarize(avg_dependents= mean(dependents,na.rm = TRUE))
avg_amount
avg_precent_of_income
avg_residence_years
avg_age
avg_existing_loan_count
avg_dependents
avg_ml

#3.4 Data visualization
#histogram ,when the default value is "YES"
library(dbplyr)
library(plyr)
library(dplyr)
default_yes <- credit_info %>% filter(default=="yes")
#creating the histogram
hist(default_yes$months_loan_duration,main="Default value yes  histogram",xlab="Months loan duration",border="blue",col="purple", las=1)

#histogram ,when the default value is "YES"
library(dbplyr)
library(plyr)
library(dplyr)
default_no <- credit_info %>% filter(default=="no")
hist(default_no$months_loan_duration,main="Default value No  histogram",xlab="Months loan duration",border="green",col="yellow", las=1)

#3D plot creation
credit_info$default[which(credit_info$default=="yes")] <-'yes'
credit_info$default[which(credit_info$default=="no")] <-'no'
install.packages("plotly")
library(plotly)
credit_plot <- plot_ly(credit_info, x= ~amount, y= ~age, z= ~months_loan_duration, color=~default,colors=c('navyblue', 'red')) %>% 
  add_markers() %>% plotly::layout(scene=list(xaxis=list(title='Amount'),yaxis=list(title='Age'),zaxis=list(title='Month loan Duration')))
credit_plot

# scatter plot of amount and age with colour separation of two default classes (yes/no).
plot(amount~age ,data=credit_info,main ="Amount and age", xlab= "Age", ylab="Amount", col=c("red","navyblue")[default])
legend("topright",c("yes","no"), cex=1, fill=c("indianred3","lightsteelblue3"))

#creating training and test data
set.seed(77)
rows <-sample( nrow(credit_info))
credit_info <- credit_info[rows,]

#inspect credit_info
head(credit_info)
# 800 records as training data
credit_info.train <-credit_info[1:800,]
# 200 records as testing data
credit_info.test <- credit_info[801:1000,]

#view the structure of both train and test data set
str(credit_info.train)
str(credit_info.test)

defaultColNo <- grep('default', names(credit_info.train))
defaultColNo


# to remove the default column index of 'default'
credit.train <- credit_info.train[, -defaultColNo]
credit.test <- credit_info.test[, -defaultColNo]

str(credit.train)
str(credit.test)

#c 5.0 model 
#install c5.0 package
install.packages("C50")
library("C50")
#create the model

c5.0_credit.fit <- C5.0(credit.train, credit_info.train$default)
print(c5.0_credit.fit)
# summary of the model
summary(c5.0_credit.fit)

# Predict the result using c5.0_credit.fit model
c5.0_prediction <- predict(c5.0_credit.fit,newdata = credit.test,type = "class")
c5.0_prediction


#Evaluation of C5.0 Matrix
# confusion matrix
#installing library caret
#install.packages("caret")
ConfusionMatrix(c5.0_prediction,credit_info.test$default)



#Area under the curve

install.packages('ROCR')
library(ROCR)
install.packages("pROC")
library(pROC)
c5.0_rcurve <- roc(credit_info.test$default, factor(c5.0_prediction,ordered=TRUE))
c5.0_rcurve
plot(c5.0_rcurve,col="blue")

#f1 score calculation over c5.0
library(caret)
library(MLmetrics)
c5.0_f1 <- F1_Score(credit_info.test$default,factor(c5.0_prediction,ordered = TRUE),positive=NULL)
c5.0_f1


# Cart algorithm
library(rpart)
install.packages("rpart.plot")

cart_credit.fit <- rpart(default~., data=credit_info.test,method='class')
print(cart_credit.fit)
#plot the tree
library(rpart.plot)
rpart.plot(cart_credit.fit,box.col=c("lightcoral","springgreen"),cex=0.7)

#cart model prediction

cart_prediction <- predict(cart_credit.fit,newdata = credit.test,type = 'class')
cart_prediction

#3.4.3 Evaluation on Cart algorithm
ConfusionMatrix(cart_prediction,credit_info.test$default)

#area under curve
cart_rcurve <- roc(credit_info.test$default,factor(cart_prediction,ordered = TRUE))
plot(cart_rcurve,col="blue")
auc_cart <- auc(cart_rcurve)
auc_cart

#f1 score on cart model
cart_f1score <- F1_Score(credit_info.test$default,factor(cart_prediction,ordered = TRUE),positive = NULL)
cart_f1score


# model improvement 
library(C50)
Adaboost_c5.0_credit.fit <- C5.0(x=credit_info.train[-16],y=credit_info.train$default,trials=10)
Adaboost_c5.0_credit.fit
summary(Adaboost_c5.0_credit.fit)

#model prediction

Adaboost_c5.0_prediction <- predict(Adaboost_c5.0_credit.fit,newdata = credit.test,type='class')
Adaboost_c5.0_prediction

#confusion matrix
ConfusionMatrix(Adaboost_c5.0_prediction,credit_info.test$default)

#Area under curve
auc_5.0_rcurve <- roc(credit_info.test$default, factor(Adaboost_c5.0_prediction,ordered=TRUE))
plot(auc_5.0_rcurve,col="blue")
plot(c5.0_rcurve, add=TRUE, col='red')
legend("topright",c("Adaptive Boosting area_under_curve","C5.0 area_under_curve"), cex=0.5, fill=c("blue","red"))
auc_5.0_rcurve

#F1 score
Adaboost_c5.0_f1_score = F1_Score(credit_info.test$default,factor(Adaboost_c5.0_prediction,ordered=TRUE))
Adaboost_c5.0_f1_score

#Pruning to improve Cart 
printcp(cart_credit.fit)
#inspect cross validation error results
plotcp(cart_credit.fit)

#to prune
pruned_cart_credit.fit <- prune(cart_credit.fit, cp= cart_credit.fit$cptable[which.min(cart_credit.fit$cptable[,"xerror"]),"CP"])
plot(pruned_cart_credit.fit,uniform=TRUE,main="Pruned classification Tree ")
text(pruned_cart_credit.fit,use.n = TRUE, all=TRUE,cex=.9)

# model prediction
pruned_cart_prediction <- predict(pruned_cart_credit.fit,newdata = credit.test,type ='class')
pruned_cart_prediction

#confusion matrix 
ConfusionMatrix(pruned_cart_prediction,credit_info.test$default)

#Area under curve
auc_cart_rcurve <- roc(credit_info.test$default, factor(pruned_cart_prediction,ordered=TRUE))
plot(auc_cart_rcurve,col="blue")
plot(cart_rcurve, add=TRUE, col='red')
legend("topright",c("Pruned decision tree","Cart algorithm"), cex=0.7, fill=c("blue","red"))
cart_rcurve

#f1 score
pruned_cart_f1score <- F1_Score(credit_info.test$default,factor(pruned_cart_prediction,ordered=TRUE),positive = NULL)
pruned_cart_f1score

#display average monthly loan duration
avg_ml

