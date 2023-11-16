##### Analyzing The Influence of Biological Factors vs. Lifestyle Habits on Stroke Victims #####
# Biological Factors: gender, age, hypertension, heart_disease, avg_glucose_level, bmi
# Lifestyle Habits: ever_married, work_type, Residence_type, smoking_status 

library(ggplot2)
library(dplyr)

stroke <- read.csv("healthcare-dataset-stroke-data.csv", header = T, na.strings = "N/A") |> na.omit()

####### Biological Variable Selection ####### 
b1 <- lm(stroke ~ gender + age + hypertension + heart_disease + avg_glucose_level + bmi, data = stroke)
summary(m1)

####### Lifestyle Variable Selection ####### 
l1 <- lm(stroke ~ ever_married + work_type + Residence_type + smoking_status, data = stroke)
summary(l1)
