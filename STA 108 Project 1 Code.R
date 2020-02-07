#STA 108 Project 1 Code

##Part I(Problems 1.43 and 1.44 in the book)

###For 1.43 Part A: Regress the Number of active physicians in turn on each of the 3 predictor variables(total population, number of hospital beds, and total income):
total_population_fit = lm(V8~V5, data = CDI)
summary(total_population_fit)
total_population_fit$coefficients

number_of_hospital_beds_fit = lm = lm(V8~V9, data = CDI)
summary(number_of_hospital_beds_fit)
number_of_hospital_beds_fit$coefficients

total_income_fit = lm(V8~V16, data = CDI)
summary(total_income_fit)
total_income_fit$coefficients

###For 1.43 Part B: Plot the 3 estimated regression functions and data on seperate graphs

#Total Population Graph
plot(CDI$V5, CDI$V8, xlab = "Total Population", ylab = "Number of Active Physicians", main = "Number of Active Physicians vs Total Population")
abline(total_population_fit, col = "red")

#Number of Hospital Beds Graph
plot(CDI$V9, CDI$V8, xlab = "Number of Hospital Beds", ylab = "Number of Active Physicians", main = "Number of Active Physicians vs Number of Active Paients")
abline(number_of_hospital_beds_fit, col = "red")

#Total Income Graph
plot(CDI$V16, CDI$V8, xlab = "Total Income", ylab = "Number of Active Physicians", main = "Number of Active Physicians vs Total Income")
abline(total_income_fit, col = "red")

###For 1.43 Part C: Calculate the MSE of each Predictor Variable:

#Total Population MSE
anova(total_population_fit)
y1 = total_population_fit$model$V5
y1_hat = total_population_fit$fitted.values
n1 = length(y1)
SSE1 = sum((y1-y1_hat)^2)
MSE1 = SSE1/(n1-2)

#Number of Hospital Beds MSE
anova(number_of_hospital_beds_fit)
y2 = number_of_hospital_beds_fit$model$V9
y2_hat = number_of_hospital_beds_fit$fitted.values
n2 = length(y2)
SSE2 = sum((y2-y2_hat)^2)
MSE2 = SSE2/(n2-2)

#Total Income MSE
anova(total_income_fit)
y3 = total_income_fit$model$V16
y3_hat = total_income_fit$fitted.values
n3 = length(y3)
SSE3 = sum((y3-y3_hat)^2)
MSE3 = SSE3/(n3-2)

###For 1.44 Part A: Regress per capita income in CDI (Y) against the percentage of individuals in a country having at least a bachelors degree (X)
CDIR1 = read.csv("C:\\Users\\ugoch\\Downloads\\CDI.Region1.csv")
CDIR2 = read.csv("C:\\Users\\ugoch\\Downloads\\CDI.Region2.csv")
CDIR3 = read.csv("C:\\Users\\ugoch\\Downloads\\CDI.Region3.csv")
CDIR4 = read.csv("C:\\Users\\ugoch\\Downloads\\CDI.Region4.csv")

Region_1_Fit = lm(CDIR1$Per.capita.income~CDIR1$Percent.bachelor.degree, data = CDIR1)
summary(Region_1_Fit)
Region_1_Fit$coefficients

Region_2_Fit = lm(CDIR2$Per.capita.income~CDIR2$Percent.bachelor.degree, data = CDIR2)
summary(Region_2_Fit)
Region_2_Fit$coefficients

Region_3_Fit = lm(CDIR3$Per.capita.income~CDIR3$Percent.bachelor.degree, data = CDIR3)
summary(Region_3_Fit)
Region_3_Fit$coefficients

Region_4_Fit = lm(CDIR4$Per.capita.income~CDIR4$Percent.bachelor.degree, data = CDIR4)
summary(Region_4_Fit)
Region_4_Fit$coefficients

##For 1.44 Part B: Are the estimated regression functions similar for the 4 regions? Discuss
###Answer: The estimated regression functions for the 4 regions aren't similar to
###each other since they have different slopes in their functions and have
###different intercepts.

###For 1.44 Part C:
anova(Region_1_Fit)

anova(Region_2_Fit)

anova(Region_3_Fit)

anova(Region_4_Fit)

##Part III (Problem 2.63 in the Book)

### For 2.63: Obtain a seperate interval of B1 for each region. Use a 90% Confidence Coefficient
confint(Region_1_Fit, level=0.9)
anova(Region_1_Fit)

confint(Region_2_Fit, level=0.9)
anova(Region_2_Fit)

confint(Region_3_Fit, level=0.9)
anova(Region_3_Fit)

confint(Region_4_Fit, level=0.9)
anova(Region_4_Fit)