# STAT 6020: End Term Project


#Importing Data

Patient_Satisfaction <- read.table(file="PatientSatisfaction.txt", header=TRUE, sep="", na.strings="*", stringsAsFactors = FALSE)


write.table(Patient_Satisfaction, file = "Minitab_Patient_Satisfaction.csv" , row.names = F, sep = ",")

#Corelation Coefficients
# 1.

ggscatter(Patient_Satisfaction, x = "satisf", y = "age", conf.int = T, cor.coef = T, cor.method = "pearson", 
          title = "Scatter Plot of Satisfaction vs Age", xlab = "Satisfaction Rating", ylab = "Age")

summary1 <- cor.test(Patient_Satisfaction$satisf, Patient_Satisfaction$age, 
                method = "pearson")
summary1

# 2.

ggscatter(Patient_Satisfaction, x = "satisf", y = "severity", conf.int = T, cor.coef = T, cor.method = "pearson", 
          title = "Scatter Plot of Satisfaction vs Severity",xlab = "Satisfaction Rating", ylab = "Severity")

summary2 <- cor.test(Patient_Satisfaction$satisf, Patient_Satisfaction$severity, 
                     method = "pearson")
summary2

#3.
ggscatter(Patient_Satisfaction, x = "satisf", y = "anxiety", conf.int = T, cor.coef = T, cor.method = "pearson", 
          title = "Scatter Plot of Satisfaction vs Anxiety",xlab = "Satisfaction Rating", ylab = "Anxiety")

summary3 <- cor.test(Patient_Satisfaction$satisf, Patient_Satisfaction$anxiety, 
                     method = "pearson")
summary3


#Multiplt Linear Regression

model1 <- lm(satisf~age+severity+anxiety, data = Patient_Satisfaction)
summary(model1)
plot(model1)

#Multicollinearity
library(jtools)
summ(model1, scale = T, transform.response = T, part.corr = T, vifs = T, digits = 4)

library(car)
vif(model1)

#Re-fitting model

model2 <- lm(satisf~age+anxiety, data = Patient_Satisfaction)
summary(model2)
vif(model2)
summ(model2, scale = T, transform.response = T, part.corr = T, vifs = T, digits = 4)

#Residual Analysis

library(olsrr)

ols_plot_resid_qq(model2)
ols_test_normality(model2)

mean(resid(model2))

ols_plot_resid_fit(model2)

library(qicharts)
qic(resid(model2), chart = 'i')
qic(resid(model2), chart = 'mr')

#Prediction and Confindence Intervals

#If we wanted to see what the prediction was for age = 26, severity = 42, anxiety = 2.8

predict_model2 <- data.frame(age = 26, severity = 42, anxiety = 2.8)
predict(model2, newdata = predict_model2)

#Confidence Interval

predict(model2, newdata = predict_model2, interval = "confidence")

#Prediction Interval

predict(model2, newdata = predict_model2, interval = "prediction")

#Stepwise Regression

lm <- lm(satisf~., data = Patient_Satisfaction)

##Forward Elimination

model3<- step(lm,direction="forward",test = "F")
summary(model3)

##Backwaard Elimination

model4 <- step(lm,direction = "backward",test = "F")
summary(model4)

##Stepwise Regression

model5 <- step(lm,direction = "both",test = "F")
summary(model5)


##Best Subset

library(leaps)
best <- regsubsets(satisf~.,data = Patient_Satisfaction)

plot(best, scale = "adjr2", main = "Best-fit:Adjusted R square")
plot(best, scale = "Cp", main = "Best-fit:Cp")
plot(best, scale = "bic", main = "Best-fit:BIC")

best_model <- summary(best)
model_table <- data.frame(best_model$outmat,best_model$adjr2,best_model$rss,best_model$cp,best_model$bic)
model_table


#1 sample - Avg satisfaction should be 75 for a hospital else changes should be done to improve quality

one_sample <- t.test(Patient_Satisfaction$satisf , mu=75)
print(one_sample)


