library(ggplot2)
library(psych) #summary statistics by group
library(lattice)  # multivariate data visualization
library(vcd)  # data visualization for categorical variables
library(ROCR)  # evaluation of binary classifiers
library(tidyverse) # various packages taht assit with
#visualization and plotting - make take time to install

#set your working directory
setwd("/Users/alfonsoberumen/Desktop/Marketing Analytics/2U Course/Tutorials and Exercises/Logistic Regression")

#note that this is a semicolon-delimited file
camp <- read.csv("Campaign Data.csv")

#examine the bank data
dplyr::glimpse(camp)

#basic summary statistics
summary(camp)

#----------------------------------
#fit logistic regression model 
#----------------------------------
camp_fit <- glm(Supporter~Age+Democrat+Income, 
                family=binomial,
                data=camp)
print(summary(camp_fit))

#print coefficients
camp_fit$coefficients
#print the exp()
exp(coef(camp_fit))

#library(car)
#vif(bank_fit)
#print(anova(bank_fit, test="Chisq"))
#fit: deviance
with(camp_fit, null.deviance - deviance)
#psuedo r-squared
names(camp_fit)
(camp_fit$null.deviance-camp_fit$deviance)/camp_fit$null.deviance
pR2 = 1 - camp_fit$deviance / camp_fit$null.deviance
pR2

#compute predicted probability
camp$Predict_Prob_Response <- predict.glm(camp_fit, 
                                              type = "response") 

#plot the predicted probability
plotting_object <- densityplot( ~ Predict_Prob_Response | Supporter, 
                                data = camp, 
                                layout = c(1,2), aspect=1, col = "darkblue", 
                                plot.points = "rug",
                                strip=function(...) strip.default(..., style=1),
                                xlab="Predicted Probability of Being a Supporter") 
print(plotting_object) 

#plot the probabilities
ggplot(data=camp, aes(x=Predict_Prob_Response)) +
  geom_histogram(binwidth=.10) + 
  xlab(expression(hat(p))) +
  ylab("Frequency") +
  #xlim(c(0.00,1.00)) +
  geom_vline(xintercept=0.5, col="red", size=1.2)

# predicted response to offer using using 0.5 cut-off
camp$Predict_Response <- 
  ifelse((camp$Predict_Prob_Response > 0.5), 1, 0)
camp$Predict_Response <- factor(camp$Predict_Response,
                                    levels = c(0, 1), labels = c("Non-Supporter", "Supporter"))  
confusion_matrix <- table(camp$Predict_Response, camp$Supporter)
cat("\nConfusion Matrix (rows=Predicted Response, columns=Actual Choice\n")
print(confusion_matrix)
predictive_accuracy <- (confusion_matrix[1,1] + confusion_matrix[2,2])/
  sum(confusion_matrix)                                              
cat("\nPercent Accuracy: ", round(predictive_accuracy * 100, digits = 1))

#prediction object
camp_prediction <- 
  prediction(camp$Predict_Prob_Response, camp$Supporter)

#we could also take a look at what's called
#an ROC curve
#TPR = true positive rate
#FPR = false postive rate
ROC <- performance(camp_prediction, 
                   measure="tpr", 
                   x.measure="fpr")
plot(ROC)
abline(a=0, b= 1)

#AUC
auc_ROCR <- performance(camp_prediction, measure = "auc")
auc_ROCR <- auc_ROCR@y.values[[1]]
auc_ROCR