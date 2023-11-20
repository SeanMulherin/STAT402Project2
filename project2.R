##### What are the most influential predictors for Stroke #####
# Biological Factors: gender, age, hypertension, heart_disease, avg_glucose_level, bmi
# Lifestyle Habits: ever_married, work_type, Residence_type, smoking_status 

library(ggplot2)
library(dplyr)
library(tidyverse)
library(car)
library(GGally) #scatterplot matrix
library(corrplot) #correlation heat map

stroke_df <- read.csv("healthcare-dataset-stroke-data.csv", header = T, na.strings = "N/A") |> na.omit()
stroke_df_numeric <- stroke_df |> select(c("age", "hypertension", "heart_disease", "avg_glucose_level", "bmi", "stroke"))

############################ EDA ############################ 
# histograms for numeric vars
ggplot(stroke_df) +
  geom_bar(mapping = aes(x = age), width = 2) + theme_classic() + ggtitle("Age")
ggplot(stroke_df) +
  geom_bar(mapping = aes(x = avg_glucose_level), width = .001) + theme_classic() + ggtitle("Glucose Level")
ggplot(stroke_df) +
  geom_bar(mapping = aes(x = bmi), width = 10) + theme_classic() + ggtitle("BMI")

# transform avg_glucose_level because it's right skewed
symbox(~avg_glucose_level, data = stroke_df)
stroke_df$avg_glucose_level <- avg_glucose_level^(-1)

# correlation heat map
corrplot.mixed(cor(stroke_df_numeric),
               lower = "number", upper = "circle", tl.pos = "l", diag = "l",
               main = "Correlation Matrix")


####### Variable Selection ####### 
m1 <- glm(stroke ~ ., data = stroke, family = binomial) # logistic regression
summary(m1) # significant vars include: age, hypertension, avg_glucose_level
plot(m1)

ggpairs(stroke, columns = c(12, 2, 3, 8)) # scatterplot matrix for sig vars

####### Check for colinearity with VIF




