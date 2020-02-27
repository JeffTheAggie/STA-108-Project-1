fit=lm(V8~V5, data=CDI)
summary(fit)
fit2=lm(V8~V16, data=CDI)
summary(fit2)
fit3=lm(V8~V9, data=CDI)
summary(fit3)

anova(fit)
anova(fit2)
anova(fit3)

plot(CDI$V5, CDI$V8, xlab = 'Total population', ylab = 'Number of active physicians', main = 'Number of active physicians vs total population')
abline(fit, col = 'red')
plot(CDI$V16, CDI$V8, xlab = 'Total personal income', ylab = 'Number of active physicians', main = 'Number of active physicians vs total personal income')
abline(fit2, col = 'red')
plot(CDI$V9, CDI$V8, xlab = 'Number of hospital beds', ylab = 'Number of active physicians', main = 'Number of active physicians vs Number of hospital beds')
abline(fit3, col = 'red')

summary(fit)$r.squared
summary(fit2)$r.squared
summary(fit3)$r.squared

confint(Region_1_Fit, level=0.9)
anova(Region_1_Fit)
confint(Region_2_Fit, level=0.9)
anova(Region_2_Fit)
confint(Region_3_Fit, level=0.9)
anova(Region_3_Fit)
confint(Region_4_Fit, level=0.9)
anova(Region_4_Fit)

#Ian’s code data#
CDI = read.csv("/Users/Ian/Documents/STA 108/CDI.csv")
fit = lm(Number.of.active.physicians~Total.population, data = CDI)
summary(fit)
fit2 = lm(Number.of.active.physicians~Number.of.hospital.beds, data = CDI)
summary(fit2)
fit3 = lm(Number.of.active.physicians~Total.personal.income, data=CDI)
summary(fit3)

plot(CDI$Total.population, CDI$Number.of.active.physician, xlab = 'Total population', ylab = 'Number of active physicians', main = 'Plot of Total Population vs. Number of Active Physicians')
plot(CDI$Number.of.hospital.beds, CDI$Number.of.active.physician, xlab = 'Number of hospital beds', ylab = 'Number of active physicians', main = 'Plot of total population vs number of hospital beds')
plot(CDI$Total.personal.income, CDI$Number.of.active.physician, xlab = 'Total personal income', ylab = 'Number of active physicians', main = 'Plot of total population vs Total personal income')

residualsfit1 = fit$residuals
residualsfit2 = fit2$residuals
residualsfit3 = fit3$residuals
plot(residualsfit1~CDI$Total.population, xlab = "Total population", ylab = "Residuals", main = 'Plot of Total Population Model Residuals against Total Population')
abline(h=0, col = 'red')
plot(residualsfit2~CDI$Number.of.hospital.beds, xlab = "Number of hospital beds", ylab = 'Residuals', main = 'Plot of Number of Hospital Beds Model Residuals against Number of Hospital Beds')
abline(h=0, col = 'red')
plot(residualsfit3~CDI$Total.personal.income, xlab = "Total personal income", ylab = 'Residuals', main = 'Plot of Total Personal Income Model Residuals against Total Personal Income')
abline(h=0, col = 'red')

qqnorm(residualsfit1, main = "Q-Q Normal Plot of Total Population Model Residuals")
qqline(residualsfit1, col='red')
qqnorm(residualsfit2, main = "Q-Q Normal Plot of Number of Hospital Beds Model Residuals")
qqline(residualsfit2, col='red')
qqnorm(residualsfit3, main = "Q-Q Normal Plot of Total Personal Income Model Residuals")
qqline(residualsfit3, col='red')
