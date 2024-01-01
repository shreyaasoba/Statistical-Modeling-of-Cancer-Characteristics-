#load necessary libraries
library(dplyr)
library(ggplot2)
library(car)

#load dataset and summarize
data <- read.csv("breastcancer.csv")
summary <- summary(data)
summaryStatistics <- aggregate(data$BMI, 
                               by = list(data$Age, data$Resistin, data$Adiponectin), 
                               summary)
print(summaryStatistics)

#SIMPLE LINEAR REGRESSION

###############
# SECTION 1
#simple linear regression model - Resistin
##############

lmodel <- lm(BMI ~ Resistin, data = data)
summaryl <- summary(lmodel)
anoval <- anova(lmodel)
fitted(lmodel)
resid(lmodel)

#Correleation coefficient
rR <- cor(data$BMI, data$Resistin)

#R squared value  
r_squaredl <- summaryl$r.squared

# Critical value of F test
qf(0.95, df1=1, df2=114)

# Critical value of t test
# for each individual predictor
# CV approach
qt(0.975, df=114)
summary(lmodel)$coefficients[, "Pr(>|t|)"]
#CI for coefficients
confint(lmodel, level=0.95)

#equation of linear model -> y = 26.41 + 0.07R

#ggplot2 - scatterplot
ggplot(data, aes(x = Resistin, y = BMI)) +
  geom_point(color="maroon") +
  geom_smooth(method="lm",    #Add linear regression line
              se=FALSE,       #Don't add shaded confidence region
              fullrange=TRUE, #Extend regression lines
              color="blue",   #Line color
              linewidth=1) +        #Line size
  ggtitle("Scatterplot BMI vs Resistin") +
  xlab("Resistin") +
  ylab("BMI")

#residual plots
par(mfrow = c(2,2))
plot(lmodel)
plot(lmodel, ask=F)
par(mfrow = c(1,1))
plot(lmodel, which=1)

#influential test
cd <- cooks.distance(lmodel)
plot(lmodel, which = 4)
plot(lmodel, which = 5)

par(mar = c(5, 5, 2, 2)) # Split graphing window
plot(fitted(lmodel), resid(lmodel), axes=TRUE, 
     frame.plot=TRUE, xlab='fitted values', ylab='residue')
abline(a=0, b=0)
plot(data$Resistin, resid(lmodel), axes=TRUE, 
     frame.plot=TRUE, xlab='Resistin', ylab='residue')
hist(resid(lmodel)) # check for normality


#outliers
outlierTest(lmodel)

#hypothesis f-test at alpha = 0.05
#H0 : betaR = 0 (No linear association)
#H1 : betaR != 0 (There is linear association)
#alpha = 0.05
#df1 = 1
#df2 = 116-1-1 = 114 

# Decision Rule: Reject H0 if p>=alpha
# Otherwise, do not reject H0

# F=MS Reg SS/ MS Res SS 
# We have significant evidence at the alpha=0.05  level 
# that betaR != 0
# There is evidence of a significant linear 
# association between BMI and Glucose.

###############
# SECTION 2
#simple linear regression model - Adiponectin
###############
lmodelm <- lm(BMI ~ Adiponectin, data = data)
summarym <- summary(lmodelm)
anovalm <- anova(lmodelm)
fitted(lmodelm)
resid(lmodelm)

#R squared value  
r_squaredm <- summarym$r.squared

#Correleation coefficient
rAd <- cor(data$BMI, data$Adiponectin)

# Critical value of F test
qf(0.95, df1=1, df2=114)

# Critical value of t test
# for each individual predictor
# CV approach
qt(0.975, df=114)
summary(lmodelm)$coefficients[, "Pr(>|t|)"]
#CI for coefficients
confint(lmodelm, level=0.95)

#equation of linear model -> y = 29.8 - 0.2Ad

#ggplot2 - scatterplot
ggplot(data, aes(x = Adiponectin, y = BMI)) + 
  geom_point(color="darkgreen") +
  geom_smooth(method="lm",    #Add linear regression line
              se=FALSE,       #Don't add shaded confidence region
              fullrange=TRUE, #Extend regression lines
              color="blue",   #Line color
              linewidth=1) +        #Line size
  ggtitle("Scatterplot of BMI vs Adiponectin") +
  xlab("Adiponectin") +
  ylab("BMI")

#residual plots ------------

#residual plots
par(mfrow = c(2,2))
plot(lmodelm)
plot(lmodelm, ask=F)
par(mfrow = c(1,1))
plot(lmodelm, which=1)

#influential test
cd <- cooks.distance(lmodel)
plot(lmodelm, which = 4)
plot(lmodelm, which = 5)

par(mar = c(5, 5, 2, 2)) # Split graphing window
plot(fitted(lmodelm), resid(lmodelm), axes=TRUE, 
     frame.plot=TRUE, xlab='fitted values', ylab='residue')
abline(a=0, b=0)
plot(data$Adiponectin, resid(lmodelm), axes=TRUE, 
     frame.plot=TRUE, xlab='Adiponectin', ylab='residue')
hist(resid(lmodelm)) # check for normality

#outliers
outlierTest(lmodelm)

#hypothesis f-test at alpha = 0.05
#H0 : betaAd = 0 (No linear association)
#H1 : betaAd != 0 (There is linear association)
#alpha = 0.05
#df1 = 1
#df2 = 116-1-1 = 114

# Decision Rule: Reject H0 if F>=cv
# Otherwise, do not reject H0

# F=MS Reg SS/ MS Res SS 
# We have significant evidence at the alpha=0.05  level 
# that betaAd != 0
# There is evidence of a significant linear 
# association between BMI and Adiponectin.

#MULTI LINEAR REGRESSION 

###############
#SECTION 3
#multi-linear regression model
###############

multiModel <- lm(BMI ~ Resistin + Adiponectin + Age, data = data)
abline(multiModel , col = "blue")
anovaMulti <- anova(multiModel)
summaryModel <- summary(multiModel)
fitted(multiModel)
resid(multiModel)

#R squared value  
r_squaredMulti <- summaryModel$r.squared

# ANOVA table by hand
(totalss <- sum((data$BMI - mean(data$BMI))^2))
(regss <- sum((fitted(multiModel) - mean(data$BMI))^2))
(resiss <- sum((data$BMI-fitted(multiModel))^2))
(fstatistic <- (regss/3)/(resiss/112))
(pvalue <- 1-pf(fstatistic, df1=3, df2=112))
(R2 <- regss/totalss)

# Critical value of F test
qf(0.95, df1=3, df2=112)

# Critical value of t test
# for each individual predictor
## CV approach
qt(0.975, df=112)
summary(multiModel)$coefficients[, "Pr(>|t|)"]
##CI for coefficients
confint(multiModel, level=0.95)

#equation of the model -> y = 29.92 + 0.05R - 0.02Ad - 0.01Ag

# Create a data frame for the plot
plot_data <- data.frame(Fitted_Values = fitted(multiModel), Residuals = resid(multiModel))

# Create the residual plot way1
ggplot(plot_data, aes(x = Fitted_Values, y = Residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(x = "Fitted Values MLR", y = "Residuals MLR") +
  ggtitle("Residual Plot for Regression Model")

resid(multiModel)
par()              #Query Graphical Parameters
current <- par()   #save current settings
par(col.lab="red") #red x and y labels

#residual plots
par(mfrow = c(2,2))
plot(multiModel)
plot(multiModel, ask=F)
par(mfrow = c(1,1))
plot(multiModel, which=1)

#influential test
cd <- cooks.distance(multiModel)
plot(multiModel, which = 4)
plot(multiModel, which = 5)

par(mar = c(5, 5, 2, 2)) # Split graphing window
plot(data$Resistin, resid(multiModel), axes=TRUE, 
     frame.plot=TRUE, xlab='Resistin', ylab='residueR')
plot(data$Adiponectin, resid(multiModel), axes=TRUE, 
     frame.plot=TRUE, xlab='Adiponectin', ylab='residueAd')
plot(data$Age, resid(multiModel), axes=TRUE, 
     frame.plot=TRUE, xlab='Age', ylab='residueAg')
hist(resid(multiModel))

#outlier
outlierTest(multiModel)

#hypothesis f-test at alpha = 0.05
#H0 : betaR = 0; betaAd = 0; betaAg = 0 (No linear association)
#H1 : betaR != 0; betaAd != 0; betaAg != 0 (There is linear association)
#alpha = 0.05
#df1 = 3
#df2 = 112

# Decision Rule: Reject H0 if F>=cv
# Otherwise, do not reject H0

# F=MS Reg SS/ MS Res SS 
# We have significant evidence at the alpha=0.05  level 
# that betaR != 0; betaAd != 0; betaAg != 0 
# There is evidence of a significant linear 
# association between Resistin, Adiponectin and Age related to BMI.


