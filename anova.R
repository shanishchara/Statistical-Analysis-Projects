#this project on ANOVA aims to determine if there are differences in 
#mwasurements of a biological specimen by a factor variable with five 
#break through groups

setwd("~/R data science/biostats data")

download.file(url = "http://cran.r-project.org/src/contrib/Archive/epicalc/epicalc_2.15.1.0.tar.gz",
              destfile = "epicalc_2.15.1.0.tar.gz")
install.packages(pkgs="epicalc_2.15.1.0.tar.gz", 
                 type="source", repos=NULL)
unlink("epicalc_2.15.1.0.tar.gz")
install.packages("ggplot2")
library(ggplot2)
install.packages("sciplot")
library(sciplot)

#importing the data and conducting a basic operations:

biospeci <- read.csv("Biological_Specimen.csv", header = TRUE)
str(biospeci)
dim(biospeci)
head(biospeci)
#F1, F2a and F2b need to be as factors

variables <- c("ID", "F1", "F2a", "F2b")
biospeci[variables] <- lapply(biospeci[variables], factor)
str(biospeci)

summary(biospeci)
#we certainly have many missing values spread all over. But summary function
#doesn't give broad idea of how missing values are distributed. 
#Also, we need to change the label for factor variables to more appropriate labels.

biospeci$F1.recode <- factor(biospeci$F1, 
                             labels = c("group F1-1", "group F1-2"))
biospeci$F2a.recode <- factor(biospeci$F2a,
                              labels = c("group F2a-1","group F2a-2"))
biospeci$F2b.recode <- factor(biospeci$F2b,
                              labels = c("group F2b-1","group F2b-2","group F2b-3",
                                         "group F2b-4","group F2b-5"))

summary(biospeci) #data check

#exploratory visual data check:

par(mfrow=c(2,3))
plot(density(biospeci$M1, na.rm = TRUE),
     main = "Density plot for M1",
     lwd=6, col="red", font.axis=2, font.lab=2)

plot(density(biospeci$M2, na.rm = TRUE),
     main = "Density plot for M2",
     lwd=6, col="red", font.axis=2, font.lab=2)

plot(density(biospeci$M3a, na.rm = TRUE),
     main = "Density plot for M3a",
     lwd=6, col="red", font.axis=2, font.lab=2)

plot(density(biospeci$M3b, na.rm = TRUE),
     main = "Density plot for M3b",
     lwd=6, col="red", font.axis=2, font.lab=2)

plot(density(biospeci$M3c, na.rm = TRUE),
     main = "Density plot for M3c",
     lwd=6, col="red", font.axis=2, font.lab=2)

par(mfrow=c(1,3))
plot(biospeci$F1.recode,
     main="F1 recode visual check")

plot(biospeci$F2a.recode,
     main="F2a recode visual check")

plot(biospeci$F2b.recode,
     main="F2b recode visual check")

#we can observe that for F2b variable, groupF2b-1 are in significantly larger 
#proportion compare to other four factors:

#ANOVA: now, first we need to check if mean differences by breakout groups of F2b 
#for M1 are true differences in measurement or they are merely by chance.
#then we will check the same for M2 as well.

m1_f2b.anova <- aov(M1 ~ F2b.recode, data = biospeci)
summary(m1_f2b.anova)
m1_f2b.anova
m1_anova<- TukeyHSD(m1_f2b.anova) 

#The TukeyHSD function provided very specific results. We can observed the detail outcomes
#on where there are statistically significant differences for M1 measurements
#by F2b categories.

m2_f2b.anova <- aov(M2~F2b.recode, data= biospeci)
m2_anova <- TukeyHSD(m2_f2b.anova)
m2_anova

par(mfrow=c(2,1))
qplot(F2b.recode, M1,
      data=biospeci,
      main = "M1 values against F2b categories",
      geom = "auto",
      position = "dodge") + theme_bw()

qplot(F2b.recode, M2,
      data=biospeci,
      main = "M2 values against F2b categories",
      geom = "auto",
      position = "dodge") + theme_bw()

par(mfrow=c(2,1))

stripchart(biospeci$M1 ~ biospeci$F2b.recode,
           method ="jitter", jitter=0.1, vertical=T,
           main="Stripchart of M1 values by F2b subgroups",
           xlab="F2b subgroups", ylab="M1 measurements", cex.lab=1.25,
           cex.axis=1.25, pch=19, col="darkred")

stripchart(biospeci$M2 ~ biospeci$F2b.recode,
           method ="jitter", jitter=0.1, vertical=T,
           main="Stripchart of M2 values by F2b subgroups",
           xlab="F2b subgroups", ylab="M1 measurements", cex.lab=1.25,
           cex.axis=1.25, pch=19, col="darkblue")







