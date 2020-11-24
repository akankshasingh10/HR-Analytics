# HR-Analytics


HR ANALYTICS : ANALYSIS


We shall start by loading all the required libraries for visualizing and establishing relationships amongst  the variables provided in the dataset.
library(MASS)
library(tidyverse)
library(car)
library(perturb)
library(corpcor)
library(tidyr)
library(dplyr)

getwd()
setwd("C:/Users/Akanksha Singh ISB/Desktop")

# Load the dataset
hrdata<-read.csv("HR_comma_sep.csv", header = TRUE, stringsAsFactors = F, na.strings = c("NA", "N/A", ""))

#Names of the columns in the dataset
colnames(hrdata)
"satisfaction_level""last_evaluation" "number_project" "average_montly_hours" 
"time_spend_company" "Work_accident" "left" "promotion_last_5years"
"sales" "salary"      



#Count of number of rows present in the dataset
nrow(hrdata)
14999

#Check if there are any NA values in the dataset
sum(is.na(hrdata))
# Alternatively, we may want to see if there are any missing values in any of the columns
apply(hrdata,2,function(x)sum(is.na(x)))
There are no missing values in the entire dataset(in any of the columns)

#Study the structure of the dataset
str(hrdata)

#Summary of the dataset
summary(hrdata)

The goal is to analyze the reason/s for employees leaving the organization and to predict the employee who would leave next based on the information available in the data. Hence, LEFT will be our respondent variable and the other remaining variables can be seen as potential regressors.


#Partition the data into training and test to start the process of analysis
sample_size <- floor(0.80 * nrow(hrdata))
set.seed(123)
train_ind <- sample(seq_len(nrow(hrdata)), size = sample_size)
train <- hrdata[train_ind, ]
test <- hrdata[-train_ind, ]
dim(test) # Dimensions of the test data
dim(train) # Dimensions of the training data

#Look at the histograms :
train %>% gather(satisfaction_level:promotion_last_5years, key = "variable", value = "value") %>%
  ggplot(aes(x = value)) + 
  geom_histogram(bins = 30) + facet_wrap(~ variable, scales = 'free_x')

#The boxplots of the columns give similar details.
train %>% gather(satisfaction_level:promotion_last_5years, key = "variable", value = "value") %>%
  ggplot(aes(x=variable,y = value)) + 
  geom_boxplot() + facet_wrap(~ variable, scales = 'free_y')

OBSERVATIONS:
1.	About 10.5% employees have been evaluated below 0.5.
2.	More than 30% employees fall below the satisfaction level of 0.5
3.	Maximum time spent in the company is around 3 to 4 years after which we observe a decreasing trend in the number of employees with the increase in time spent in the company.
4.	Less than 30% employees have been promoted roughly.
5.	Less than 10% employees spend less than 120 or more than 280 average monthly hours in the company with the actual average hovering around 201 hours.

# Bar-chart of the remaining variables 
train %>% gather(sales:salary, key = "variable", value = "value") %>% ggplot(aes(x = value)) +
  geom_bar()+ facet_wrap(~variable, scales = 'free_x')

#Now we will try to visualize attributes and draw insights. Let's gauge the satisfaction levels of the employees.
satisfaction_level <- ggplot(train, aes(train$satisfaction_level)) + geom_density(fill = 'orange')
satisfaction_level

#Satisfaction level of employees who left
satlevel_left <- train[train$left == 1, c("satisfaction_level", "left")]
satlevel_1 <- ggplot(satlevel_left, aes(satisfaction_level)) + geom_density(fill = 'green')
satlevel_1

# Satisfaction levels of those who did not quit
satlevel_exist <- train[train$left == 0, c("satisfaction_level", "left")]
satlevel_0 <- ggplot(satlevel_exist, aes(satisfaction_level)) + geom_density(fill = 'yellow')
satlevel_0

It shows that employees who left are to be found located more towards the left of the whole visualization. Levels of satisfaction are very low for the category (<0.5). The satisfaction level visualization for the employees who did not leave is denser towards the right i.e. greater satisfaction level. The means of the two categories look very different.

cor(train$satisfaction_level, as.numeric(train$left))
t.test(satisfaction_level~left, train)
# The above two test show a negative relationship between variables satisfaction level and left. This confirms that there are lot of unsatisfied employees who have left. Correlation test turns out to be -0.3847492.

On similar lines we will try to discover relationship between left and the other attributes.

last_evaluation <- ggplot(train, aes(train$last_evaluation)) + geom_density(fill = 'orange')
last_evaluation

last_evaluation_left <- train[train$left == 1, c("last_evaluation", "left")]
last_evaluation_1 <- ggplot(last_evaluation_left, aes(last_evaluation)) + geom_density(fill = 'green')
last_evaluation_1

last_evaluation_exist <- train[train$left == 0, c("last_evaluation", "left")]
last_evaluation_0 <- ggplot(satlevel_exist, aes(satisfaction_level)) + geom_density(fill = 'yellow')
last_evaluation_0

This viz shows that people with poor last evaluation scores as well as higher scores have left the organisation though the majority who stayed back are with higher scores of evaluation.

t.test(last_evaluation~left, train)
#t-test states that last_evaluation does not impact the attribute left.

number_project_left <- train[train$left == 1, c("number_project", "left")]
number_project_1 <- ggplot(number_project_left, aes(number_project, fill = factor(number_project))) + geom_bar()
number_project_1

number_project_exist <- train[train$left == 0, c("number_project", "left")]
number_project_0 <- ggplot(number_project_exist, aes(number_project, fill = factor(number_project))) + geom_bar()
number_project_0

Majority of the people who left have done 2 projects.
hist(train$number_project).

mean(train$average_montly_hours) - 201.2069
hist(train$average_montly_hours)
cor(train$average_montly_hours,train$left) -  0.07286505
#Correlation gives an interesting insight that as this is a strong positive correlation, it means that people who have left the organisation have given more no. of average_montly_hours (more than the mean).

table <- table(train$time_spend_company, train$left)
table
barplot(table, beside = TRUE, legend = TRUE)

        0    1
  2  2559   47
  3  3897 1255
  4  1303  715
  5   530  669
  6   402  168
  7   153    0
  8   130    0
  10  171    0
#Another interesting insight we draw here is that maximum employees have spent around 3 to 4 years in the company. Employees tend to be quitting after they have spent 4 years or more.
We can see this as below also:-

time_spend_company_left <- train[train$left == 1, c("time_spend_company", "left")]
time_spend_company_1 <- ggplot(time_spend_company_left, aes(time_spend_company)) + geom_bar()
time_spend_company_1

time_spend_company_exist <- train[train$left == 0, c("time_spend_company", "left")]
time_spend_company_0 <- ggplot(time_spend_company_exist, aes(time_spend_company)) + geom_bar()
time_spend_company_0

hist(train$time_spend_company)

count<- table(train$sales)
count
barplot(count,col="yellow")

count<- table(train$salary)
count
barplot(count,col="blue")

The above two frequency tables show that the maximum staff is employed in the sales department followed by technical and then the support team. The least no. of people are present in the management vertical. Also, the maximum employee strength is on Low salary, followed by Medium salary and very few employees have High salary. (almost 1/5th of the Medium salary). 

count<- table(train$sales)
count
barplot(count,col="yellow")
Only 266 people have received promotion in the last 5 years, while 11733 employees have not been promoted.

Quant_colnames <- c ("satisfaction_level", "last_evaluation", "number_project", "average_montly_hours", "time_spend_company", "Work_accident", "left","promotion_last_5years")
Qual_colnames <- c("sales", "salary")

CORRELATION MATRIX AND SCATTERPLOT MATRIX
#Make scatterplots
plot(train[,Quant_colnames]) # Plot all the Quantitative variables from the training set.
cor(train[,Quant_colnames])  

library(corrplot)
corrplot(round(cor(train[,Quant_colnames]),2), method = "number") # Let's look at the correlation numbers.
#Let's run partial correlation
cor2pcor(cor(train[,Quant_colnames]))

Interestingly, none of the variables show multicollinearity, however there exists weak positive correlation between attributes like number of projects and average monthly hours; number of projects and last evaluation etc. Also, satisfaction level and left ; number of projects and satisfaction level portray a negative correlation.

# To further the process of understanding the relationship of the business problem that revolves around employees quitting the organization with the rest of the attributes, we can plot boxplots. 
So, let us study the DISTRIBUTION OF “LEFT”.

library(ggplot2)
data <- data.frame(x = train$left)
ggplot(data, aes(x=train$left)) + geom_density(fill="orange")

#Log Transformation
data <- data.frame(x = log(train$left))
ggplot(data, aes(x=log(train$left))) + geom_density(fill="orange")


Model 1 :-
Our first linear model may be represented as below:-

model1 <-lm (log(left+1e-15) ~ ., data= train)
summary(model1)
anova(model1)
par(mfrow=c(2,2))
plot(model1)

It shows that there are quite a few insignificants we can see – This is from HR, Marketing, Sales, Support, Technical departments. Adjusted R-squared = 0.2127 which is very low

#QQ-plot of the residuals
qqnorm(rstandard(model1))
qqline(rstandard(model1))

#Studentized Residuals:
stu.resid <- studres(model1)
hist(stu.resid, freq=FALSE,main="Distribution of Studentized Residuals") 
xfit<-seq(min(stu.resid),max(stu.resid),length=40) 
yfit<-dnorm(xfit)
lines(xfit, yfit)

It shows that the distribution is heavier towards the left side.

residualPlot(model1, id.n=5)
residualPlots(model1,id.n=4)

The Q-Q plot shows its bimodal.
Residual - 1349,1001,1610,844 – outlier observations
Residual plots and Tukey test state that satisfaction_level,last_evaluation,number_project,average_monthly_hours,time_spend_company, need to be transformed.

boxplot(train$satisfaction_level)
boxplot((train$satisfaction_level)^2)
train$sqr_satisfaction_level <- train$satisfaction_level^2

boxplot(train$last_evaluation)
boxplot((train$last_evaluation)^2)
train$sqr_last_evaluation <- train$last_evaluation^2

boxplot(train$number_project)
boxplot((train$number_project)^2)
train$sqr_number_project <- train$number_project^2

boxplot(train$average_montly_hours)
boxplot((train$average_montly_hours)^2)
train$sqr_average_montly_hours <- train$average_montly_hours^2

boxplot(train$time_spend_company)
boxplot(log(train$time_spend_company))
train$time_spend_companyLOG <- log(train$time_spend_company)

colnames(train)

model2 <- lm(log(left+1e-15)~.,data=train[,-c(5)])
summary(model2)
anova(model2)
We see that the R-squared is now 0.4029.

residualPlot(model2, id.n=5)
residualPlots(model2,id.n=4)

Model 1- HR, IT,Management,Marketing,Sales,Support,Technical - not significant
Model 2 -HR,Marketing,Sales, Support, Technical are not significant

#QQ-plot of the residuals
qqnorm(rstandard(model2))
qqline(rstandard(model2),col="red")

residualPlot(model2, id.n=5)
residualPlots(model2,id.n=4)


#Checking for Influential Observations/ Deletion Diagnostics
cutoff <- 4/((nrow(train)-length(model1$coefficients)-2))
plot(model1, which=4, cook.levels=cutoff)

14924,83,1001 are still the influential points

step <- stepAIC(model2, direction="both")
	
#comparing model1 and model2
par(mfrow = c(2,2))
qqnorm(rstandard(model1))
qqline(rstandard(model1))
qqnorm(rstandard(model2))
qqline(rstandard(model2))
plot(fitted(model1),residuals(model1),type="p",col="red");abline(h=0)
plot(fitted(model2),residuals(model2),type="p",col="blue");abline(h=0)

confint(model1,level=0.95)
confint(model2,level=0.95)

#qq-plot with confidence band
qqPlot(model1,distribution = "norm")
qqPlot(model2,distribution = "norm")


#There is still a lot of research that needs to be done in order to strengthen the analysis and hence achieve better accuracy levels. We will work further on model 2 to create couple of models to arrive at an accuracy level of atleast greater than 70%. This would involve achieving higher value for R-square but not overfitting the data at the same time.







