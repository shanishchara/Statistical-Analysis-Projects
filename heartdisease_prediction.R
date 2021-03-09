#Cleaveland data for heart disease.

library(tidyverse)
library(ggplot2)
library(broom)
library(Metrics)

#read in the data:

hd_data <- read.csv("processed.cleveland.csv", header = T)
head(hd_data)
str(hd_data)

#class data is binary for heart disease. we convert class and sex variables:
hd_data %>% mutate(hd=ifelse(class > 0, 1, 0)) -> hd_data
hd_data %>% mutate(sex= factor(sex,levels = 0:1, labels = c("Female", "Male"))) ->hd_data

#now we need to identify important clinical variables among sex, age and 
#maximum heart rate during exercise:
hd_sex <- chisq.test(hd_data$sex, hd_data$hd) #sex and heart disease
hd_age <- t.test(hd_data$age ~ hd_data$hd)
hd_heartrate <- t.test(hd_data$thalach ~ hd_data$hd)

print(hd_sex)
print(hd_age)
print(hd_heartrate)

#visually assessing the association among aforementioned variables with outcome hd:
#first we properly label the hd variable:
hd_data %>% mutate(hd_labelled = ifelse(hd==0,"No Disease", "Disease")) -> hd_data
ggplot(data = hd_data, aes(x = hd_labelled, y = age, fill=hd_labelled)) + geom_boxplot()

ggplot(data = hd_data, aes(x = hd_labelled, y = thalach)) + geom_boxplot(col="darkgreen")
             

#now we figure out the effects of three variable on our response variable hd:
#we can use glm function from base R and since hd is binary response variable 
#we can build logistic regression:

logistic.hd <- glm(data = hd_data, hd ~ age + sex + thalach,
                   family = "binomial")
summary(logistic.hd)

# we want the odds ratio and corresponding 95% CI:

tidy_logisticm <- tidy(logistic.hd)
tidy_logisticm

#calculate odds ratio:
tidy_logisticm$OR <- exp(tidy_logisticm$estimate)

#calculate 95% CI and store the lower and upper values:
tidy_logisticm$lowerCI <- exp(tidy_logisticm$estimate - 1.96 * tidy_logisticm$std.error)
tidy_logisticm$upperCI <- exp(tidy_logisticm$estimate + 1.96 * tidy_logisticm$std.error)
tidy_logisticm

# now we want to predict the person's likelihood of having a heart disease based on
#a person's age, sex and maximum heart rate:

pred.prob <- predict(logistic.hd, hd_data, type = "response")

#create cutoff rule for yes or no:
hd_data$prediction.hd <- ifelse(pred.prob >= 0.5,1,0)

#predict the probability for a new case:

newcase <- data.frame(age = 45, sex = "Female", thalach = 150)

pred.newcase <- predict(logistic.hd, newcase, type = "response")
pred.newcase

# Now we assess the model performance:
# first we calculate AUC, accuracy and classification error

model.auc <- auc(hd_data$hd, hd_data$prediction.hd)
model.accuracy <- accuracy(hd_data$hd, hd_data$prediction.hd)
class.error <- ce(hd_data$hd, hd_data$prediction.hd)

print(paste("AUC =", model.auc))
print(paste("Accuracy =", model.accuracy))
print(paste("Classification Error =", class.error))

#confusion matrix:
table(hd_data$hd, hd_data$prediction.hd, dnn = c("True Status", "Predicted Status"))










