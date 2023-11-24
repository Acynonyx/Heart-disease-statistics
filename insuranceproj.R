insurance = read.csv("insurance.csv", stringsAsFactors = F)
summary(insurance)

#Linear Regressions below here

agePredReg = lm(charges ~ age, data = insurance)
summary(agePredReg)

png(file="ChargesvsAgeRegression.png")
plot(insurance$age, insurance$charges, main="Linear regression with age as predictor", abline(lm(insurance$charges~insurance$age)))
dev.off()

ageBMIReg = lm(charges ~ age + bmi, data = insurance)
summary(ageBMIReg)

png(file="CharvsAge_Bmi.png")
plot(ageBMIReg)
dev.off()

ageBMISmokeReg = lm(charges ~ smoker + age + bmi, data = insurance)
summary(ageBMISmokeReg)

png(file="CharvsAge_Bmi_smoker.png")
plot(ageBMISmokeReg)
dev.off()

finalmodel = lm(charges ~ smoker + age + bmi + region + children, data = insurance)
summary(finalmodel)

finalmodel2 = lm(charges ~ smoker + sex +  age + bmi + region + children, data = insurance)
summary(finalmodel2)

png(file="finalmodel.png")
plot(finalmodel)
dev.off()


#Boxplots below here

library(ggplot2)
library(ggthemes)
png(file="BoxplotofChargeperRegion.png")
ggplot(data = insurance,aes(region,charges)) + geom_boxplot(fill = c(2:5)) +
  theme_classic() + ggtitle("Boxplot of Medical Charges per Region")
dev.off()

library(ggplot2)
library(ggthemes)
png(file="BoxplotofChargebySmokers.png")
ggplot(data = insurance,aes(smoker,charges)) + geom_boxplot(fill = c(2:3)) +
  theme_classic() + ggtitle("Boxplot of Medical Charges by Smoking Status")
dev.off()

library(ggplot2)
library(ggthemes)
png(file="BoxplotofChargebyGender.png")
ggplot(data = insurance,aes(sex,charges)) + geom_boxplot(fill = c(2:3)) +
  theme_classic() + ggtitle("Boxplot of Medical Charges by Gender")
dev.off()

library(ggplot2)
library(ggthemes)
png(file="BoxplotofChargebyChildrenr.png")
ggplot(data = insurance,aes(as.factor(children),charges)) + geom_boxplot(fill = c(2:7)) +
  theme_classic() +  xlab("children") +
  ggtitle("Boxplot of Medical Charges by Number of Children")
dev.off()


#Prediction vs real

library(ggplot2)
library(ggthemes)
finalmodel = lm(charges ~ smoker + age + bmi + region + children, data = insurance)
insurance$prediction = predict(finalmodel, newdata = insurance)
png(file="PredictionvsReal.png")
ggplot(insurance, aes(x = prediction, y = charges)) + 
  geom_point(color = "blue") + 
  geom_abline(color = "red") +
  ggtitle("Prediction vs. Real values")
dev.off()

#qq plots

qqnorm(insurance$)