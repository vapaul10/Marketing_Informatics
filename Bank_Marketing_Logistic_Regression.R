#Bank Marketing
#Marketing Informatics
#Adpated from:
#Miller, T. W. (2015). 
#Marketing data science: modeling techniques 
#in predictive analytics with R and Python. 
#Old Tappan, NJ: Pearson Education.

#R packages - you need to use this function to install
#these packages if you do not have them installed
#install.packages("insert package name here")
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
bank <- read.csv("bank.csv", sep = ";", stringsAsFactors = FALSE)

#examine the bank data
dplyr::glimpse(bank)

#basic summary statistics
summary(bank)

#plot a histogram for this variable
with(bank,hist(age,col="orange"))

#examine the frequency tables for categorical/factor variables  
#showing the number of observations with missing data (if any)
#by using option useNA

#table gives us frequencies
table(bank$job , useNA = c("always"))
table(bank$marital , useNA = c("always"))
table(bank$education , useNA = c("always"))
table(bank$default , useNA = c("always"))
table(bank$housing , useNA = c("always"))
table(bank$loan , useNA = c("always"))

#Type of job (admin., unknown, unemployed, management,
#housemaid, entrepreneur, student, blue-collar, self-employed,
#retired, technician, services)
#put job into three major categories defining the factor variable jobtype
#the "unknown" category is how missing data were coded for job 
#include these in "Other/Unknown" category/level
white_collar_list <- c("admin.",
                       "entrepreneur",
                       "management",
                       "self-employed")  
blue_collar_list <- c("blue-collar",
                      "services",
                      "technician")
bank$jobtype <- rep(3, length = nrow(bank))
bank$jobtype <- ifelse((bank$job %in% white_collar_list), 1, bank$jobtype) 
bank$jobtype <- ifelse((bank$job %in% blue_collar_list), 2, bank$jobtype) 
bank$jobtype <- factor(bank$jobtype, levels = c(1, 2, 3), 
                       labels = c("White Collar", "Blue Collar", "Other/Unknown"))
#check the new definition    
with(bank, table(job, jobtype, useNA = c("always")))  

#define factor variables with labels for plotting
table(bank$marital)
bank$marital <- factor(bank$marital, 
                       labels = c("Divorced", 
                                  "Married", 
                                  "Single"))
table(bank$education)
bank$education <- factor(bank$education, 
                         labels = c("Primary", 
                                    "Secondary", 
                                    "Tertiary", 
                                    "Unknown"))
bank$default <- factor(bank$default, labels = c("No", "Yes"))
bank$housing <- factor(bank$housing, labels = c("No", "Yes"))
bank$loan <- factor(bank$loan, labels = c("No", "Yes"))
bank$response <- factor(bank$response, labels = c("No", "Yes"))

###########################################################
#select subset of cases never previously contacted by sales
###########################################################
#previous: number of contacts performed 
#before this campaign and for this client
table(bank$previous)

#keeping variables needed for modeling
bankwork <- subset(bank, subset = (previous == 0),
                   select = c("response", 
                              "age", 
                              "jobtype", 
                              "marital", 
                              "education", 
                              "default", 
                              "balance", 
                              "housing", 
                              "loan"))

#examine the structure of the bank data frame
glimpse(bankwork)
#something more manageable

# -----------------
# age  Age in years
# -----------------
#examine relationship between age and response to promotion
lattice_plot_object <- histogram(~age | response, data = bankwork,
                                 type = "density", 
                                 xlab = "Age of Bank Client", layout = c(1,2))
print(lattice_plot_object)  #responders tend to be older

#summary statistics by group
describeBy(bankwork$age,bankwork$response)
#t.test(bankwork$age~bankwork$response)
#box plot
boxplot(age~response,data=bankwork, 
        main="Age by Term Deposit",
        xlab="Deposit", ylab="Age",
        col=c("blue","orange")) 

# -----------------------------------------------------------
# education
# Level of education (unknown, secondary, primary, tertiary)
# -----------------------------------------------------------
#examine the frequency table for education
# reminder that the "unknown" category is how missing data were coded 
with(bankwork, print(table(education, response, useNA = c("always"))))

#create a mosaic plot in using vcd package
mosaic( ~ response + education, data = bankwork,
        labeling_args = list(set_varnames = c(response = "Response to Offer", 
                                              education = "Education Level")),
        highlighting = "education",
        highlighting_fill = c("cornsilk","violet","purple","white",
                              "cornsilk","violet","purple","white"),
        rot_labels = c(left = 0, top = 0),
        pos_labels = c("center","center"),
        offset_labels = c(0.0,0.6))
#those that subscribe are highly educated

# ---------------------------------------------------------------
# job status using jobtype
# White Collar: admin., entrepreneur, management, self-employed  
# Blue Collar: blue-collar, services, technician
# Other/Unknown
# ---------------------------------------------------------------
# review the frequency table for job types
with(bankwork, print(table(jobtype, response, useNA = c("always"))))

mosaic( ~ response + jobtype, data = bankwork,
        labeling_args = list(set_varnames = c(response = "Response to Offer", 
                                              jobtype = "Type of Job")),
        highlighting = "jobtype",
        highlighting_fill = c("cornsilk","violet","purple",
                              "cornsilk","violet","purple"),
        rot_labels = c(left = 0, top = 0),
        pos_labels = c("center","center"),
        offset_labels = c(0.0,0.6))
#more likely white collar than blue collar

# ----------------------------------------------
# marital status
# Marital status (married, divorced, single)
# [Note: ``divorced'' means divorced or widowed]
# ----------------------------------------------
# examine the frequency table for marital status
# anyone not single or married was classified as "divorced"
with(bankwork, print(table(marital, response, useNA = c("always"))))

mosaic( ~ response + marital, data = bankwork,
        labeling_args = list(set_varnames = c(response = "Response to Offer", 
                                              marital = "Marital Status")),
        highlighting = "marital",
        highlighting_fill = c("cornsilk","violet","purple",
                              "cornsilk","violet","purple"),
        rot_labels = c(left = 0, top = 0),
        pos_labels = c("center","center"),
        offset_labels = c(0.0,0.6))
#more likely to be single, divorced/widowed than married

# -----------------------------------------
# default  Has credit in default? (yes, no)
# -----------------------------------------
with(bankwork, print(table(default, response, useNA = c("always"))))

mosaic( ~ response + default, data = bankwork,
        labeling_args = list(set_varnames = c(response = "Response to Offer", 
                                              default = "Has credit in default?")),
        highlighting = "default",
        highlighting_fill = c("cornsilk","violet"),
        rot_labels = c(left = 0, top = 0),
        pos_labels = c("center","center"),
        offset_labels = c(0.0,0.6))
#not much here

# ------------------------------------------
# balance  Average yearly balance (in Euros)
# ------------------------------------------
# examine relationship between age and response to promotion
lattice_plot_object <- histogram(~balance | response, data = bankwork,
                                 type = "density", 
                                 xlab = "Bank Client Average Yearly Balance (in dollars)", 
                                 layout = c(1,2))
print(lattice_plot_object)
#summary statistics by group
describeBy(bankwork$balance,bankwork$response)
#t.test(bankwork$balance~bankwork$response)
#not much here either
#box plot
boxplot(balance~response,data=bankwork, 
        main="Balance by Term Deposit",
        xlab="Deposit", ylab="Balance",
        col=c("blue","orange")) 

# ------------------------------------
# housing  Has housing loan? (yes, no)
# ------------------------------------
with(bankwork, print(table(housing, response, useNA = c("always"))))

mosaic( ~ response + housing, data = bankwork,
        labeling_args = list(set_varnames = c(response = "Response to Offer", 
                                              housing = "Has housing loan?")),
        highlighting = "housing",
        highlighting_fill = c("cornsilk","violet"),
        rot_labels = c(left = 0, top = 0),
        pos_labels = c("center","center"),
        offset_labels = c(0.0,0.6))
#less likely to have a housing loan with the bank

# ----------------------------------
# loan  Has personal loan? (yes, no)
# ----------------------------------
with(bankwork, print(table(loan, response, useNA = c("always"))))

mosaic( ~ response + loan, data = bankwork,
        labeling_args = list(set_varnames = c(response = "Response to Offer", 
                                              loan = "Has personal loan?")),
        highlighting = "loan",
        highlighting_fill = c("cornsilk","violet"),
        rot_labels = c(left = 0, top = 0),
        pos_labels = c("center","center"),
        offset_labels = c(0.0,0.6))
#somewhat less likely to have a personal loan

#collectively, 
#those who subscribe
#are very slightly older,
#more highly educated,
#more likely to be white collar workers vs. blue,
#more likely to not be married
#less likely to have a housing loan with the bank
#somewhat less likely to have a personal loan

# ----------------------------------
# specify predictive model
# ----------------------------------
bank_spec <- {response ~ age + jobtype + education + marital +
    default + balance + housing + loan}

#----------------------------------
#fit logistic regression model 
#----------------------------------
#R will select dummy levels automatically
#or you can specify


bank_fit <- glm(bank_spec, family=binomial, 
                contrasts = list(jobtype=contr.treatment(c("White Collar", 
                                                           "Blue Collar", 
                                                            "Other/Unknown"),base = 3)),
                data=bankwork)
print(summary(bank_fit))
#library(car)
#vif(bank_fit)
#print(anova(bank_fit, test="Chisq"))
#fit: deviance
with(bank_fit, null.deviance - deviance)
#psuedo r-squared
names(bank_fit)
(bank_fit$null.deviance-bank_fit$deviance)/bank_fit$null.deviance
pR2 = 1 - bank_fit$deviance / bank_fit$null.deviance
pR2
#our model
#Our logistic regression model is said 
#to explain roughly 3.6% 
#of the null deviance

#compute predicted probability of responding to the offer 
bankwork$Predict_Prob_Response <- predict.glm(bank_fit, 
                                              type = "response") 

#plot the predicted probability
plotting_object <- densityplot( ~ Predict_Prob_Response | response, 
                                data = bankwork, 
                                layout = c(1,2), aspect=1, col = "darkblue", 
                                plot.points = "rug",
                                strip=function(...) strip.default(..., style=1),
                                xlab="Predicted Probability of Responding to Offer") 
print(plotting_object) 

#plot the probabilities
ggplot(data=bankwork, aes(x=Predict_Prob_Response)) +
  geom_histogram(binwidth=.10) + 
  xlab(expression(hat(p))) +
  ylab("Frequency") +
  #xlim(c(0.00,1.00)) +
  geom_vline(xintercept=0.5, col="red", size=1.2)

# predicted response to offer using using 0.5 cut-off
# notice that this does not work due to low base rate
# we get more than 90 percent correct with no model 
# (predicting all NO responses)
# the 0.50 cutoff yields all NO predictions 
bankwork$Predict_Response <- 
  ifelse((bankwork$Predict_Prob_Response > 0.5), 2, 1)
bankwork$Predict_Response <- factor(bankwork$Predict_Response,
                                    levels = c(1, 2), labels = c("NO", "YES"))  
confusion_matrix <- table(bankwork$Predict_Response, bankwork$response)
cat("\nConfusion Matrix (rows=Predicted Response, columns=Actual Choice\n")
print(confusion_matrix)
predictive_accuracy <- (confusion_matrix[1,1] + confusion_matrix[2,2])/
  sum(confusion_matrix)                                              
cat("\nPercent Accuracy: ", round(predictive_accuracy * 100, digits = 1))

#this problem requires either a much lower cut-off
#or other criteria for evaluation... let's 
#try 0.10 (10 percent cut-off)
bankwork$Predict_Response <- 
  ifelse((bankwork$Predict_Prob_Response > 0.1), 2, 1)
bankwork$Predict_Response <- factor(bankwork$Predict_Response,
                                    levels = c(1, 2), labels = c("NO", "YES"))  
confusion_matrix <- table(bankwork$Predict_Response, bankwork$response)
cat("\nConfusion Matrix (rows=Predicted Response, columns=Actual Choice\n")
print(confusion_matrix)
predictive_accuracy <- (confusion_matrix[1,1] + confusion_matrix[2,2])/
  sum(confusion_matrix)                                              
cat("\nPercent Accuracy: ", round(predictive_accuracy * 100, 
                                  digits = 1))

#compute lift using prediction() from ROCR and plot lift chart
bankwork_prediction <- 
  prediction(bankwork$Predict_Prob_Response, bankwork$response)
bankwork_lift <- performance(bankwork_prediction , "lift", "rpp")

plot(bankwork_lift, 
     col = "blue", lty = "solid", main = "", lwd = 2,
     xlab = paste("Proportion of Clients Ordered by Probability",
                  " to Subscribe\n(from highest to lowest)", sep = ""), 
     ylab = "Lift over Baseline Subscription Rate")

##########################################################
# --------------------------------------------------------
# --------------------------------------------------------
# direct calculation of lift
# --------------------------------------------------------
# --------------------------------------------------------
##########################################################
baseline_response_rate <- 
  as.numeric(table(bankwork$response)[2])/nrow(bankwork)
#this is the same as saying, what percent deposited overall
#for example
table(bankwork$response)
#337/(337+3368) gives you the percent deposited overall

lift <- function(x, baseline_response_rate) {
  mean(x) / baseline_response_rate
}

decile_break_points <- c(as.numeric(quantile(bankwork$Predict_Prob_Response,
                                             probs=seq(0, 1, 0.10))))   

bankwork$decile <- cut(bankwork$Predict_Prob_Response,      
                       breaks = decile_break_points,
                       include.lowest=TRUE,
                       labels=c("Decile_10","Decile_9","Decile_8","Decile_7","Decile_6",
                                "Decile_5","Decile_4","Decile_3","Decile_2","Decile_1"))    

# define response as 0/1 binary 
bankwork$response_binary <- as.numeric(bankwork$response) - 1

cat("\nLift Chart Values by Decile:\n")    
print(by(bankwork$response_binary, bankwork$decile, 
         function(x) lift(x, baseline_response_rate)))

#Berumen: alternative with detail
summary_by_decile<-bankwork %>%
  group_by(decile) %>%
  summarise(Total_Actual_Subscribers=sum(response_binary),
            n=n()) %>%
  mutate(Proportion_Actually_Subscribed=Total_Actual_Subscribers/n) %>%
  mutate(Overall_Proportion_Actually_Subscribed=baseline_response_rate) %>%
  mutate(Lift_Rate=Proportion_Actually_Subscribed/Overall_Proportion_Actually_Subscribed)
summary_by_decile
#plot
ggplot(data=summary_by_decile, 
       aes(x=reorder(decile), y=Lift_Rate, group = 1)) +
  geom_line(color="blue") +
  geom_point()

#we could also take a look at what's called
#an ROC curve
#TPR = true positive rate
#FPR = false postive rate
ROC <- performance(bankwork_prediction, 
                   measure="tpr", 
                   x.measure="fpr")
plot(ROC)
abline(a=0, b= 1)

#AUC
auc_ROCR <- performance(bankwork_prediction, measure = "auc")
auc_ROCR <- auc_ROCR@y.values[[1]]
auc_ROCR