#This project aims to determine if there is a correlation in final weight by
#measures associated with vigor. The data consists of sample from 3308 animals.

#let's start by reading the file and taking a look at the data:

library(lessR)
library(psych)
library(Hmisc)

livestock.df <- read.table(file = "Livestock_Vigor.csv", header = T, sep = ",")
str(livestock.df) #subject indicates animal identity.should be a factor column.
head(livestock.df)

# we make the row names more relevant:
rownames(livestock.df) <- paste('Animal', 1:3308)
#now we properly label the variables for the ease of readability.
library(epicalc)
label.var(Subject,"Animal ID", dataFrame = livestock.df)
label.var(WeightInitial,"Weight When Purchased", dataFrame = livestock.df)
label.var(WeightFinish,"Weight when sold", dataFrame = livestock.df)
label.var(VigorInitial,"Vigor When purchased", dataFrame = livestock.df)
label.var(Vigor100Lbs,"Vigor at 100 pounds", dataFrame = livestock.df)
label.var(Vigor200Lbs,"Vigor at 200 pounds", dataFrame = livestock.df)
label.var(VigorFinish,"Vigor at sale", dataFrame = livestock.df)
des(livestock.df)

#alright, it is time to make variables of appropriate class:

livestock.df$Subject <- as.factor(livestock.df$Subject)
livestock.df$WeightInitial <- as.numeric(livestock.df$WeightInitial)
livestock.df$WeightFinish <- as.numeric(livestock.df$WeightFinish)

des(livestock.df) #everything seems fine here.

#for the purpose of quality assurance, we do a brief data check:

summary(livestock.df)

#visualize the distribution of data for vigor variables:

boxplot(livestock.df[,4:7],
  main="Comparative Box Plots For Vigor",
  col="darkgreen", lwd=2, cex.axis=1.25,
  ylab="Vigor", cex.lab=1.25) 


# Now we look for correlation:
#we don't need subject column for correlation plot. 

livestock.corr <- livestock.df[,-1]
head(livestock.corr)

cor.plot(cor(livestock.corr,
         use = "complete.obs", method = "pearson"),
         main = "Correlation Plot for Weight and Vigor", 
         font.lab=2, font.axis=2)

# we can definitely observe some strong correlations among vigor and weight. 

rcorr(as.matrix(livestock.corr, type=pearson))

#WeightFinish has significant correlations with Initial vigor, vigor at 100lbs,
#vigor at 200lbs and with vigor at finish.

# we would like to know what would be relationship for Weight at finish and Vigor
# We build multiple linear regression model where we do stepwise subset selection.
# we can remove missing value data as they are significantly low in numbers:

livestock.corr <- na.omit(livestock.corr)

livestock.multireg <- lm(WeightFinish ~ 
                           VigorInitial 
                         + Vigor100Lbs 
                         + Vigor200Lbs
                         + VigorFinish, 
                         data = livestock.corr)

summary(livestock.multireg) # Definitely not a good model - R-squared adj value

step(livestock.multireg, direction = "backward")

stepmodel<-lm(formula = WeightFinish ~ VigorInitial + Vigor100Lbs, 
              +               data = livestock.corr)
summary(stepmodel)

#model diagnostics:
par(mfrow=c(2,2))
plot(stepmodel)
#We can see that residual plots reveal that residuals are not of 
#a constant variance. Also, standardized residuals don't exhibit a normal 
#distribution.
#In the second part, we will look for possible outliers, influencers or
#polynomial regression to address this issue.

# have look at the scatter plot for possible curve:
plot(livestock.corr$WeightFinish, livestock.corr$Vigor100Lbs)
plot(livestock.corr$WeightFinish, livestock.corr$Vigor200Lbs)
#yes, linear regression is not an efficient ay to address this problem.

