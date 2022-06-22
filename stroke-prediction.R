library(dplyr)
library(ggplot2)
library(corrplot)
library(car)
library(jtools)
library(interactions)
library(tidyverse)
library(ggpubr)
library(summarytools) 
library(cowplot)
library(quantable)
library(dlookr)
library (ggpubr)
library(ggExtra)
library(caret)
library(ROCR)

options(scipen = 999)
# importing stroke data set
stroke <- read.csv("C:/Users/hp/Downloads/healthcare-dataset-stroke-data.csv" , na.strings=c("N/A"))

summary(stroke$stroke)
summary(stroke)
summary(stroke$gender)
str(stroke)


# removing observation where gender = Other as it is only 1
stroke <- subset(stroke, gender!="Other")

# removing ID column
stroke <- subset(stroke, select = -c(id) )

# removing duplicates if there are any
dim(stroke[duplicated(stroke),])[1] #to check count of duplicate rows
dim(stroke)

stroke <- as.data.frame(stroke %>% distinct())

# turning categorical columns to factor
col_factor <- c("gender", "hypertension", "heart_disease", "ever_married", 
                "work_type", "Residence_type", "smoking_status", "stroke")
for (i in col_factor) {
  stroke[,i] <- as.factor(stroke[,i])
}

str(stroke)
summary(stroke$stroke)

summary(stroke)

summary(stroke$gender)

# visualizing stroke

ggplot(stroke, aes(x = stroke,)) +  geom_bar(fill = "light blue",alpha=0.8) +
  labs(title = 'Count of Stroke', 
       x = 'Stroke', y = 'Count') + theme(plot.title = element_text(hjust = 0.5))

# imputing bmi missing values with gender based mean

stroke <- stroke %>% 
  group_by(gender) %>% 
  mutate(bmi = ifelse(is.na(bmi),
                      mean(bmi, na.rm=TRUE),
                      bmi))
summary(stroke)
str(stroke$bmi)
diagnose(stroke)


# Does age affect stroke? YES
group_by(stroke, stroke) %>%
  summarise(
    count = n(),
    median = median(age, na.rm = TRUE),
    IQR = IQR(age, na.rm = TRUE)
  )
ggplot(stroke, aes(x=stroke, y=age, fill=stroke)) +
  geom_boxplot(alpha=0.3) +
  theme(legend.position="none") +
  labs(title = "Boxplot of Age Grouped by Stroke", 
       x = "Stroke",
       y = "Age")+ theme(plot.title = element_text(hjust = 0.5))

# t test age vs stroke
# assumption 1: independent samples

# assumption 2: normal distribution
with(stroke, shapiro.test(age[stroke == "1"])) # not normal
with(stroke, shapiro.test(age[stroke == "0"])) # not normal

# assumption 3: equal variances
var_age <- var.test(age ~ stroke, data = stroke)
var_age # not equal variances

# two-samples Wilcoxon test because the data does not satisfy the assumptions

wilcox_test_age <- wilcox.test(age ~ stroke, data = stroke, exact = FALSE)
wilcox_test_age # p-value less than 0.05 means median age is significantly different between groups

# Does bmi affect stroke? Not so much, do t test
group_by(stroke, stroke) %>%
  summarise(
    count = n(),
    mean = mean(bmi, na.rm = TRUE),
    IQR = IQR(bmi, na.rm = TRUE)
  )

ggplot(stroke, aes(x=stroke, y=bmi, fill=stroke)) +
  geom_boxplot(alpha=0.3) +
  theme(legend.position="none") +
  labs(title = "Boxplot of BMI Grouped by Stroke", 
       x = "Stroke",
       y = "BMI")+ theme(plot.title = element_text(hjust = 0.5))


# assumption 1: independent samples

# assumption 2: normal distribution
with(stroke, shapiro.test(bmi[stroke == "1"])) # not normal
with(stroke, shapiro.test(bmi[stroke == "0"])) # not normal

# assumption 3: equal variances
var_bmi <- var.test(bmi ~ stroke, data = stroke)
var_bmi # not equal variances

# two-samples Wilcoxon test because the data does not satisfy the assumptions

wilcox_test_bmi <- wilcox.test(bmi ~ stroke, data = stroke, exact = FALSE)
wilcox_test_bmi # p-value less than 0.05 means mean bmi is significantly different between groups

# Does avg_glucose_level affect stroke? Maybe, do t test
group_by(stroke, stroke) %>%
  summarise(
    count = n(),
    mean = mean(avg_glucose_level, na.rm = TRUE),
    IQR = IQR(avg_glucose_level, na.rm = TRUE)
  )

ggplot(stroke, aes(x=stroke, y=avg_glucose_level, fill=stroke)) +
  geom_boxplot(alpha=0.3) +
  theme(legend.position="none") +
  labs(title = "Boxplot of Glucose Grouped by Stroke", 
       x = "Stroke",
       y = "Average Glucose Level")+ theme(plot.title = element_text(hjust = 0.5))

# assumption 1: independent samples

# assumption 2: normal distribution
with(stroke, shapiro.test(avg_glucose_level[stroke == "1"])) # not normal
with(stroke, shapiro.test(avg_glucose_level[stroke == "0"])) # not normal

# assumption 3: equal variances
var_bmi <- var.test(avg_glucose_level ~ stroke, data = stroke)
var_bmi # not equal variances

# two-samples Wilcoxon test because the data does not satisfy the assumptions

wilcox_test_glucose <- wilcox.test(avg_glucose_level ~ stroke, data = stroke, exact = FALSE)
wilcox_test_glucose # p-value less than 0.05 means mean glucose is significantly different between groups

# Perform chi-square for categorical variables
# Stroke and Gender
stroke_gender <- table(stroke$gender,stroke$stroke, dnn = c("Gender","Stroke"))
addmargins(stroke_gender)

chisq.test(stroke_gender) # independent 

# Stroke and heart_disease
stroke_heart <- table(stroke$heart_disease,stroke$stroke, dnn = c("Heart Disease","Stroke"))
addmargins(stroke_heart)

chisq.test(stroke_heart) # not independent

# Stroke and hypertension
stroke_hypertension <- table(stroke$hypertension,stroke$stroke, dnn = c("Hypertension","Stroke"))
addmargins(stroke_hypertension)

chisq.test(stroke_hypertension) # not independent

# Stroke and ever_married
stroke_married <- table(stroke$ever_married,stroke$stroke, dnn = c("Ever Married","Stroke"))
addmargins(stroke_married)

chisq.test(stroke_married) # not independent

# Stroke and work_type
stroke_work <- table(stroke$work_type,stroke$stroke, dnn = c("Work Type","Stroke"))
addmargins(stroke_work)

# chisq.test(stroke_work) does not work because cells with < 5 frequency
fisher.test(stroke_work, workspace=2e8) # not independent

# Stroke and Residence_type
stroke_residence <- table(stroke$Residence_type,stroke$stroke, dnn = c("Residence Type","Stroke"))
addmargins(stroke_residence)

chisq.test(stroke_residence) # independent

# Stroke and smoking_status
stroke_smoking <- table(stroke$smoking_status,stroke$stroke, dnn = c("Smoking Status","Stroke"))
addmargins(stroke_smoking)

chisq.test(stroke_smoking) # not independent

# Model with Initial variables

# including variables found associated to stroke
model <- glm(stroke ~ age + hypertension + heart_disease + avg_glucose_level +
                bmi + ever_married + work_type + smoking_status, 
              family = "binomial", data = stroke)
summary(model)


str(stroke)
# Multicollinearity

### Using correlation analysis
stroke_num <- stroke[, c(2,8,9)]
corr <- cor(stroke_num)
corr
corrplot(corr)

# Calculating GVIF
car::vif(model)

# Detecting presence of interactions

str(stroke)
stroke_interaction <- stroke[, -c(1,2,7,8,9,11)]
str(stroke_interaction)

idx <- 1
for (i in 1: (ncol(stroke_interaction)-1)) {
  for(j in (i+1): ncol(stroke_interaction)){
    
    selection_condition <- !(is.numeric(stroke_interaction[,i]) | 
                               is.numeric(stroke_interaction[,j]) | i==j)
    if(selection_condition){
      
      col_i <- as.factor(pull(stroke_interaction,i))
      col_j <- as.factor(pull(stroke_interaction,j))
      contigency_table <- table(col_i,col_j)
      chi_sq_pval <- suppressWarnings(chisq.test(contigency_table)$p.value)
      if(chi_sq_pval<0.05)
      {
        ith_col <- colnames(stroke_interaction)[i]
        jth_col <- colnames(stroke_interaction)[j]
        print(paste(idx,".",ith_col, 'and',jth_col,'are','not independent' ))
        idx <- idx +1
        
      }
      
    }
    
    
    
  }
}

summary(stroke)

# Continuous vs Continuous interactions

age_bucket <- cut(stroke$age, breaks = c(0, 25, 61, 82), labels = c("0-25", "26-61", "61-82"))
stroke <- data.frame(stroke, age_bucket)

bmi_bucket <- cut(stroke$bmi, breaks = c(0, 24, 33, 100), labels = c("0-24 ", "25-33", "34-100"))
stroke <- data.frame(stroke, bmi_bucket)

avg_glucose_level_bucket <- cut(stroke$avg_glucose_level, breaks = c(0, 77.24, 114.09, 272), labels = c("0-77.24 ", "77.25-114.09", "114.10-272"))
stroke <- data.frame(stroke, avg_glucose_level_bucket)

View(stroke)

# Interaction plots (with function cat_plot) for categorical vs categorical interactions

Model_initial<- glm(stroke ~ .-Residence_type -gender, family=binomial, data = stroke)
summary(Model_initial)
vif(Model_initial)

cat_plot(Model_initial, pred = hypertension, modx = heart_disease, geom = 'line')
cat_plot(Model_initial, pred = hypertension, modx = ever_married, geom = 'line')
cat_plot(Model_initial, pred = work_type, modx = hypertension, geom = 'line')
cat_plot(Model_initial, pred = smoking_status, modx = hypertension, geom = 'line')
cat_plot(Model_initial, pred = ever_married, modx = heart_disease, geom = 'line')
cat_plot(Model_initial, pred = work_type, modx = heart_disease, geom = 'line')
cat_plot(Model_initial, pred = smoking_status, modx = heart_disease, geom = 'line')
cat_plot(Model_initial, pred = work_type, modx = ever_married, geom = 'line')
cat_plot(Model_initial, pred = smoking_status, modx = ever_married, geom = 'line')
cat_plot(Model_initial, pred = smoking_status, modx = work_type, geom = 'line')

# Interaction plots for numerical vs categorical interactions

interaction.plot(x.factor = stroke$stroke, 
                 trace.factor = stroke$hypertension, 
                 response = stroke$age, 
                 fun = median, 
                 ylab = "age",
                 xlab = "stroke",
                 col = c("red", "blue"),
                 lty = 1, 
                 lwd = 2, 
                 trace.label = "hypertension")

interaction.plot(x.factor = stroke$stroke, 
                 trace.factor = stroke$hypertension, 
                 response = stroke$avg_glucose_level, 
                 fun = median, 
                 ylab = "avg_glucose_level",
                 xlab = "stroke",
                 col = c("red", "blue"),
                 lty = 1, 
                 lwd = 2, 
                 trace.label = "hypertension")

interaction.plot(x.factor = stroke$stroke,  
                 trace.factor = stroke$hypertension,  
                 response = stroke$bmi,  
                 fun = median,  
                 ylab = "bmi",
                 xlab = "stroke",
                 col = c("red", "blue"),
                 lty = 1, 
                 lwd = 2,  
                 trace.label = "hypertension")


# Heart Disease and Age
interaction.plot(x.factor = stroke$stroke,  
                 trace.factor = stroke$heart_disease,  
                 response = stroke$age,  
                 fun = median,  
                 ylab = "Age",
                 xlab = "Stroke",
                 col = c("red", "blue"),
                 lty = 1,  
                 lwd = 2,  
                 trace.label = "Heart Disease")

#Heart Disease and Avg Glucose Level

interaction.plot(x.factor = stroke$stroke,  
                 trace.factor = stroke$heart_disease,  
                 response = stroke$avg_glucose_level,  
                 fun = median,  
                 ylab = "Avg Glucose Level",
                 xlab = "Stroke",
                 col = c("red", "blue"),
                 lty = 1,  
                 lwd = 2,  
                 trace.label = "Heart Disease")



#Heart Disease and BMI

interaction.plot(x.factor = stroke$stroke,  
                 trace.factor = stroke$heart_disease,  
                 response = stroke$bmi,  
                 fun = median,  
                 ylab = "BMI",
                 xlab = "Stroke",
                 col = c("red", "blue"),
                 lty = 1,  
                 lwd = 2,  
                 trace.label = "Heart Disease")




#ever_married and age

interaction.plot(x.factor = stroke$stroke,  
                 trace.factor = stroke$ever_married,  
                 response = stroke$age,  
                 fun = median, 
                 ylab = "Age",
                 xlab = "Stroke",
                 col = c("red", "blue"),
                 lty = 1, 
                 lwd = 2,  
                 trace.label = "Ever Married")

#ever_married and avg_glucose_level
interaction.plot(x.factor = stroke$stroke,  
                 trace.factor = stroke$ever_married,  
                 response = stroke$avg_glucose_level,  
                 fun = median,  
                 ylab = "Average Glucose Level",
                 xlab = "Stroke",
                 col = c("red", "blue"),
                 lty = 1, 
                 lwd = 2,  
                 trace.label = "Ever Married")


#ever_married and bmi
interaction.plot(x.factor = stroke$stroke,  
                 trace.factor = stroke$ever_married,  
                 response = stroke$bmi,  
                 fun = median,  
                 ylab = "BMI",
                 xlab = "Stroke",
                 col = c("red", "blue"),
                 lty = 1,  
                 lwd = 2,  
                 trace.label = "Ever Married")


#work_type and age
interaction.plot(x.factor = stroke$stroke,  
                 trace.factor = stroke$work_type,  
                 response = stroke$age,  
                 fun = median,  
                 ylab = "Age",
                 xlab = "Stroke",
                 col = c("red", "blue","green","orange","violet"),
                 lty = 1,  
                 lwd = 2,  
                 trace.label = "Work Type")


#work_type and bmi
interaction.plot(x.factor = stroke$stroke,  
                 trace.factor = stroke$work_type,  
                 response = stroke$bmi, 
                 fun = median,  
                 ylab = "BMI",
                 xlab = "Stroke",
                 col = c("red", "blue","green","orange","violet"),
                 lty = 1,  
                 lwd = 2,  
                 trace.label = "Work Type")



#work_type and avg_glucose_level
interaction.plot(x.factor = stroke$stroke,  
                 trace.factor = stroke$work_type,  
                 response = stroke$avg_glucose_level,  
                 fun = median,  
                 ylab = "Avg Glucose Level",
                 xlab = "Stroke",
                 col = c("red", "blue","green","orange","violet"),
                 lty = 1,  
                 lwd = 2,  
                 trace.label = "Work Type")


#smoking_status and age
interaction.plot(x.factor = stroke$stroke,  
                 trace.factor = stroke$smoking_status,  
                 response = stroke$age, 
                 fun = median,  
                 ylab = "Age",
                 xlab = "Stroke",
                 col = c("red", "blue","green","orange"),
                 lty = 1,  
                 lwd = 2,  
                 trace.label = "Smoking Status")


#smoking_status and bmi
interaction.plot(x.factor = stroke$stroke,  
                 trace.factor = stroke$smoking_status,  
                 response = stroke$bmi,  
                 fun = median,  
                 ylab = "BMI",
                 xlab = "Stroke",
                 col = c("red", "blue","green","orange","violet"),
                 lty = 1,  
                 lwd = 2,  
                 trace.label = "Smoking Status")



#smoking_status and avg_glucose_level
interaction.plot(x.factor = stroke$stroke,  
                 trace.factor = stroke$smoking_status,  
                 response = stroke$avg_glucose_level,  
                 fun = median,  
                 ylab = "Avg Glucose Level",
                 xlab = "Stroke",
                 col = c("red", "blue","green","orange","violet"),
                 lty = 1,  
                 lwd = 2,  
                 trace.label = "Smoking Status")



#####################
## Numerical and Numerical Variables

# BMi and age_bucket
interaction.plot(x.factor = stroke$stroke,  
                 trace.factor = stroke$age_bucket,  
                 response = stroke$bmi,  
                 fun = median,  
                 ylab = "BMI",
                 xlab = "Stroke",
                 col = c("red", "blue","green"),
                 lty = 1,  
                 lwd = 2,  
                 trace.label = "Age")


### age and avg_glucose_level
interaction.plot(x.factor = stroke$stroke,  
                 trace.factor = stroke$avg_glucose_level_bucket,  
                 response = stroke$age,  
                 fun = median, 
                 ylab = "Age",
                 xlab = "Stroke",
                 col = c("red", "blue", "green"),
                 lty = 1, 
                 lwd = 2,  
                 trace.label = "Avg Glucose Level")


# Average glucose level and bmi_bucket
interaction.plot(x.factor = stroke$stroke, 
                 trace.factor = stroke$bmi_bucket,  
                 response = stroke$avg_glucose_level,  
                 fun = median, 
                 ylab = "Avg Glucose Level",
                 xlab = "Stroke",
                 col = c("red", "blue","green"),
                 lty = 1,  
                 lwd = 2,  
                 trace.label = "BMI")


#Splitting the data into Training set and Test set
set.seed(414)
trainIndex <- createDataPartition(stroke$stroke, p = .7,
                                  list = FALSE,
                                  times = 1)
training_set <- stroke[ trainIndex,]
testing_set <- stroke[-trainIndex,]

#backward selection to select variables for the two models
fmodel <- glm(stroke ~ heart_disease+hypertension+ever_married+work_type+smoking_status+age+bmi+avg_glucose_level, family = "binomial", data = stroke)
step_model <- step(fmodel, direction = "backward")

#summary of the variable selection
summary(step_model)

#model1 with interaction
model1 <- glm(stroke ~  age + hypertension + heart_disease + avg_glucose_level + age*heart_disease, data = training_set, family = "binomial")
summary(model1) 

#model2 without interaction
model2 <- glm(stroke ~  age + hypertension + heart_disease + avg_glucose_level, family = "binomial", data = training_set)
summary(model2) 

#comparing model1 and model2 - LRT
pchisq(0.7,1,lower.tail = FALSE)

#Subsetting target column and independent columns separately from test dataset
x_test <- subset(testing_set, select = c(age,hypertension,heart_disease,avg_glucose_level))
y_test <- subset(testing_set, select = c(stroke))

#Making prediction using the model1 (with interactions) and test data
predictions1 <- predict(model1, x_test,type="response")

#Making prediction using the model2 (without interactions) on test data
predictions2 <- predict(model2, x_test,type="response")

#factoring predictions1 at 0.5 threshold
predictions1<-ifelse(predictions1 > 0.2,1,0)

#factoring predictions2 at 0.5 threshold
predictions2<-ifelse(predictions2> 0.2,1,0)

#factoring y_test$RainTomorrow and predictions
a1<-as.factor(predictions1)
b1<-as.factor(y_test$stroke)

#Generating confusion matrix
library(caret)
confusionMatrix(a1, b1, positive = '1')

#factoring y_test$RainTomorrow and predictions
a2<-as.factor(predictions2)
b2<-as.factor(y_test$stroke)

#Generating confusion matrix
confusionMatrix(a2, b2, positive = '1')

#ROC Curve
model <- glm(stroke ~ age + hypertension + heart_disease + avg_glucose_level, 
             family = "binomial", data = training_set)
pred <- predict(model, x_test,type="response" )


model_int <- glm(stroke ~ age + hypertension + heart_disease + hypertension*heart_disease + avg_glucose_level, 
                 family = "binomial", data = training_set)
pred_int <-predict(model_int, x_test,type="response" )

pred <- prediction(pred,y_test$stroke)
pred_int <- prediction(pred_int,y_test$stroke)
perf <- performance(pred,"acc")
perf_int <- performance(pred_int, "acc")
plot(perf,  main = "Threshold for Predictions")
plot(perf_int,  main = "Threshold for Predictions")
max_ind <- which.max(slot(perf,"y.values")[[1]])
acc <- slot(perf, "y.values")[[1]][max_ind]
cutoff = slot(perf,"x.values")[[1]][max_ind]
print(c(accuracy=acc,cutoff=cutoff))


roc = performance(pred,"tpr","fpr")
roc_int = performance(pred_int, "tpr", "fpr")
plot(roc, lwd = 2, main = "ROC Curves")
abline(a=0,b=1)
plot(roc_int, add =TRUE, col = 4, lwd = 2)

legend(x="bottomright", legend=c("Without Interaction", "With Interaction"),
       col=c("black", 4), lwd = c(2,2))

#hoslem-lemshow goodness of fit test for model1 (with interaction)
library(ResourceSelection)
hl <- hoslem.test(training_set$stroke, fitted(model1), g=10)
hl

#hoslem-lemshow goodness of fit test for model2 (without interaction)
h2 <- hoslem.test(training_set$stroke, fitted(model2), g=10)
h2

