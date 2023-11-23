##### Analyzing The Influence of Biological Factors vs. Lifestyle Habits on Stroke Victims #####
# Biological Factors: gender, age, hypertension, heart_disease, avg_glucose_level, bmi
# Lifestyle Habits: ever_married, work_type, Residence_type, smoking_status 

set.seed(10);
library(ggplot2);
library(dplyr);


load_stroke_data <- function(path) {
    read.csv(path, header = T, na.strings = "N/A");
}

prep_stroke_data <- function(stroke_data, training_proportion) {
    stroke_data <- na.omit(stroke_data);
    
    n <- nrow(stroke_data);
    num_training <- n * training_proportion;
    training_i <- sample.int(n, num_training);
    
    list(
        training = stroke_data[training_i, ],
        test = stroke_data[-training_i, ]
    );
}

stroke <- prep_stroke_data(
    load_stroke_data("healthcare-dataset-stroke-data.csv"),
    training_proportion = 0.8
);

stroke

####### Biological Variable Selection ####### 
b1 <- lm(stroke ~ gender + age + hypertension + heart_disease + avg_glucose_level + bmi, data = stroke)
summary(m1)

####### Lifestyle Variable Selection ####### 
l1 <- lm(stroke ~ ever_married + work_type + Residence_type + smoking_status, data = stroke)
summary(l1)
