# Analyzing and dealing with the data related to the "HEART"
# for visualizing and predicting the functioning & working 
# of the patients heart and implementing the model for the chances 
# of "HEART-ATTACK" with the variables present.

# Importing & Installing the required packages & libraries
# wherever needed for analysing and visualizing

# Importing the dataset into a DF
heart_data <- read.csv("heart.csv", na="")

# Display the first six entries from the DF
head(heart_data)

# Structure of DF
str(heart_data)

# Verifying that it is a DF
class(heart_data)

# Displaying the number of rows and columns
nrow(heart_data)
ncol(heart_data)

# Display the summary of the data
summary(heart_data)

# Rename the column names
colnames(heart_data)
name <- c("Age","Sex", "Chest_pain", "Resting_BP", "Cholestoral", "Fasting_BS", 
          "Resting_ECG", "Max_heartrate", "Excercise_angina", "Oldpeak", "Slope", "Num_major_vessel", 
          "Thall", "Target")
names(heart_data) <- name

# Display the column-names of DF
colnames(heart_data)

# Displaying the summary
summary(heart_data)

# To check if any NA data present
incomplete_data <- heart_data[!complete.cases(heart_data),]
incomplete_data

# Display the missing data in rows
nrow(incomplete_data)

# visualize the missing data
# install.packages("VIM")
library(VIM)
missing_values <- aggr(heart_data, prop = FALSE, numbers = TRUE)

# Display the summary of missing data
summary(missing_values)
# No missing data present in the DF

# Checking if any NA is present in the DF
# FALSE represents no NA's in the DF
# TRUE represent there are NA's in the DF
any(is.na(heart_data))

# Fetching the required columns/variables for analysing & predicting
# the best model & storing the required variables into a new DF
new_heart_data <- heart_data[, c(1,2,3,4,5,6,7,8,9,12,14)]

# Checking if any NA is present in the DF
any(is.na(new_heart_data))

# Display the structure of DF
str(new_heart_data)

# Display the column-names of DF
colnames(new_heart_data)

# Installing the library 'psych'
# install.packages("psych")
library(psych)

# To visualize the distribution and correlation
pairs(new_heart_data)

pairs.panels(new_heart_data, 
             smooth = FALSE, # If TRUE, draws less smooths
             scale = FALSE, # If TRUE, scales the correlation text font
             density = TRUE, # If TRUE, adds density plots and histograms
             ellipses = FALSE, # If TRUE, draws ellipses
             method = "spearman",# Correlation method (also "pearson" or "kendall")
             pch = 21, # pch symbol
             lm = FALSE, # If TRUE, plots linear fit rather than the LOESS (smoothed) fit
             cor = TRUE, # If TRUE, reports correlations
             jiggle = FALSE, # If TRUE, data points are jitered
             factor = 2, # Jitering factor
             hist.col = 4, # Histograms color
             stars = TRUE, # If TRUE, adds significance level with stars
             ci = TRUE) # If TRUE, adds confidence intervals


######################## HEART DATA ################################
# Heart Dataset - Predicting the chances of getting a "Heart-Attack"
# Using "Multiple Linear Regression" Technique
####################################################################

# For further implementation we need to keep in mind about the
# dependent & independent variables for predicting the model
# INDEPENDENT variable = "TARGET"
# DEPENDENT variables = All others except 'TARGET'


# Including the variables we need to use for predicting a model


# Independent var = X-axis
# Dependent var = Y-axis

# Low correlation = value equal to zero
# High Correlation = straight line in plot (value > 0.60) 
# (positive & negative)
# No correlation = No correlation no line

attach(new_heart_data)
str(new_heart_data)

# For AGE
scatter.smooth(x = Target, y = Age, 
               main = "Target ~ Age", 
               xlab = "Chance of Heart Attack",
               ylab = "Age (Years)")
# The plot shows there is very NO Correlation

# Checking Correlation
# Values of -0.2 < x < 0.2 - Low Correlation
cor(Target, Age)
# It is giving a negavtive correlation value = -0.22
# The correlation tests shows that the correlation between the Target & 
# Age variable = -0.22 indicating a negative correlation.
# The AGE variable is of no use in predicting the model.

############# For SEX
scatter.smooth(x = Target, y = Sex, 
               main = "Target ~ Sex", 
               xlab = "Chance of Heart Attack",
               ylab = "Sex (Gender)")
# The plot shows there is No Correlation

# Checking Correlation
# Values of -0.2 < x < 0.2 - Low Correlation
cor(Target, Sex)
# It is giving a negavtive correlation value = -0.28
# The correlation tests shows that the correlation between the Target & 
# Sex variable = -0.28 indicating a negavtive correlation.
# The SEX field can also be excluded for predicting an model.

# For CHEST_PAIN
scatter.smooth(x = Target, y = Chest_pain, 
               main = "Target ~ Cheat_Pain", 
               xlab = "Chance of Heart Attack",
               ylab = "Chest Pain")
# The plot shows there is very less correlation

# Checking Correlation
# Values of -0.2 < x < 0.2 - Low Correlation
cor(Target, Chest_pain)
# It is giving a medium correlation value = 0.43
# The correlation tests shows that the correlation between the Target & 
# Chest_Pain variable = 0.43 indicating a medium correlation.
# We can keep this variable for Prediction

# For BLOOD_PRESSURE
scatter.smooth(x = Target, y = Resting_BP, 
               main = "Target ~ Blood_Pressure", 
               xlab = "Chance of Heart Attack",
               ylab = "Blood Pessure (mm/Hg)")
# The plot shows there is very less correlation

# Checking Correlation
# Values of -0.2 < x < 0.2 - Low Correlation
cor(Target, Resting_BP)
# It is giving a less correlation value = -0.14
# The correlation tests shows that the correlation between the Target & 
# Blood Pressure variable = -0.14 indicating a low negative correlation.
# We can decide to keep this variable for prediction or not.

# For CHOLESTEROL
scatter.smooth(x = Target, y = Cholestoral, 
               main = "Target ~ Cholestoral", 
               xlab = "Chance of Heart Attack",
               ylab = "Cholestoral (mg/dl)")
# The plot shows there is very less correlation

# Checking Correlation
# Values of -0.2 < x < 0.2 - Low Correlation
cor(Target, Cholestoral)
# It is giving a less correlation value = -0.08
# The correlation tests shows that the correlation between the Target & 
# Cholesterol variable = -0.08 indicating a low negative correlation.
# We can decide to keep this variable for prediction or not

# FOR SUGAR
scatter.smooth(x = Target, y = Fasting_BS, 
               main = "Target ~ Sugar_Level", 
               xlab = "Chance of Heart Attack",
               ylab = "Sugar_Level (mg/dl)")
# The plot shows there is very low correlation

# Checking Correlation
# Values of -0.2 < x < 0.2 - Low Correlation
cor(Target, Fasting_BS)
# It is giving a less correlation value = -0.02
# The correlation tests shows that the correlation between the Target & 
# Sugar variable = -0.02 indicating a low negative correlation.
# We can decide to keep this variable for prediction or not

# For ECG
scatter.smooth(x = Target, y = Resting_ECG, 
               main = "Target ~ ECG", 
               xlab = "Chance of Heart Attack",
               ylab = "ECG")
# The plot shows there is very less correlation

# Checking Correlation
# Values of -0.2 < x < 0.2 - Low Correlation
cor(Target, Resting_ECG)
# It is giving a low correlation value = 0.13
# The correlation tests shows that the correlation between the Target & 
# ECG variable = 0.13 indicating a less correlation.
# We can decide to keep this variable for prediction or not.


# For HEART_RATE
scatter.smooth(x = Target, y = Max_heartrate, 
               main = "Target ~ Heart_Rate", 
               xlab = "Chance of Heart Attack",
               ylab = "Heart_Rate")
# The plot shows there is medium correlation

# Checking Correlation
# Values of -0.2 < x < 0.2 - Low Correlation
cor(Target, Max_heartrate)
# It is giving a medium correlation value = 0.42
# The correlation tests shows that the correlation between the Target & 
# Heart_Rate variable = 0.42 indicating a medium correlation.
# We can keep this variable for Prediction


# For EXCERCISE
scatter.smooth(x = Target, y = Excercise_angina, 
               main = "Target ~ Excercise", 
               xlab = "Chance of Heart Attack",
               ylab = "Excercise")
# The plot shows there is negative correlation

# Checking Correlation
# Values of -0.2 < x < 0.2 - Low Correlation
cor(Target, Excercise_angina)
# It is giving a negative correlation value = -0.43
# The correlation tests shows that the correlation between the Target & 
# Exercise variable = -0.43 indicating a strong negative correlation.
# The Excercise variable is of no use in predicting the model.

# For BLOOD_VESSELS
scatter.smooth(x = Target, y = Num_major_vessel, 
               main = "Target ~ Blood_Vessels", 
               xlab = "Chance of Heart Attack",
               ylab = "Blood Vessels")
# The plot shows there is very less correlation

# Checking Correlation
# Values of -0.2 < x < 0.2 - Low Correlation
cor(Target, Num_major_vessel)
# It is giving a negative correlation value = -0.39
# The correlation tests shows that the correlation between the Target & 
# Blood Vessel variable = -0.39 indicating a strong negative correlation.


# To check the correlation for all the variables from the DF
paste("Correlation for the Target & Age:", cor(Target, Age))
paste("Correlation for the Target & Sex:", cor(Target, Sex))
paste("Correlation for the Target & Chest_Pain:", cor(Target, Chest_pain))
paste("Correlation for the Target & Blood_Pressure:", cor(Target, Resting_BP))
paste("Correlation for the Target & Cholestorol:", cor(Target, Cholestoral))
paste("Correlation for the Target & Sugar_Level:", cor(Target, Fasting_BS))
paste("Correlation for the Target & ECG:", cor(Target, Resting_ECG))
paste("Correlation for the Target & Heart_Rate:", cor(Target, Max_heartrate))
paste("Correlation for the Target & Excercise:", cor(Target, Excercise_angina))
paste("Correlation for the Target & Blood_Vessel:", cor(Target, Num_major_vessel))


# Here we decided to remove the variables (AGE & SEX) as it is 
# not much useful in predicting a model
# new_heart_data <- subset(new_heart_data, select = c(-Age, -Sex))
str(new_heart_data)
head(new_heart_data)

# Check the outliers
opar <- par(no.readonly = TRUE)

boxplot(Age, 
        main = "Age", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(Age)$out))
# There is no outlier

boxplot(Sex, 
        main = "Sex", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(Sex)$out))
# There is no outlier

boxplot(Target, 
        main = "Target", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(Target)$out))
# There is no outlier

boxplot(Chest_pain, 
        main = "Chest_Pain", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(Chest_pain)$out))
# There is no outlier

boxplot(Resting_BP, 
        main = "Blood_Pressure", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(Resting_BP)$out))
# There is an outlier

boxplot(Cholestoral, 
        main = "Cholestorol", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(Cholestoral)$out))
# There is an outlier

boxplot(Fasting_BS, 
        main = "Sugar_Level", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(Fasting_BS)$out))
# There is an outlier

boxplot(Resting_ECG, 
        main = "ECG", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(Resting_ECG)$out))
# There is no outlier

boxplot(Max_heartrate, 
        main = "Heart_Rate", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(Max_heartrate)$out))
# There is an outlier

boxplot(Excercise_angina, 
        main = "Excercise", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(Excercise_angina)$out))
# There is no outlier

boxplot(Num_major_vessel, 
        main = "Blood_Vessel", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(Num_major_vessel)$out))
# There is an outlier

par(opar)

# use the boxplot.stats() function to extract 
# the outliers for population
outlier_values <- boxplot.stats(Resting_BP)$out
paste("Blood_Pressure Outliers: ", 
      paste(outlier_values, 
            collapse = ", "))

outlier_values <- boxplot.stats(Cholestoral)$out
paste("Cholestorl Outliers: ", 
      paste(outlier_values, 
            collapse = ", "))

outlier_values <- boxplot.stats(Fasting_BS)$out
paste("Sugar Outliers: ", 
      paste(outlier_values, 
            collapse = ", "))

outlier_values <- boxplot.stats(Max_heartrate)$out
paste("Heart_Rate Outliers: ", 
      paste(outlier_values, 
            collapse = ", "))

outlier_values <- boxplot.stats(Num_major_vessel)$out
paste("Blood_Vessel Outliers: ", 
      paste(outlier_values, 
            collapse = ", "))

# We need to remove the outliers from the 
# (Blood_Pressure, Cholestorol, & Heart_Rate).
# As (Sugar & Blood_Vessels) relates to the category of pre-defined values

new_heart_data <- subset(new_heart_data, 
                         Resting_BP != 172 & Resting_BP != 178 & 
                           Resting_BP != 180 & Resting_BP != 200 & 
                           Resting_BP != 174 & Resting_BP != 192)

new_heart_data <- subset(new_heart_data, 
                         Cholestoral != 417 & Cholestoral != 564 & 
                           Cholestoral != 394 & Cholestoral != 407 & 
                           Cholestoral != 409)

new_heart_data <- subset(new_heart_data, 
                         Max_heartrate != 71)

attach(new_heart_data)
# To check if the oulier is removed
boxplot(Resting_BP, 
        main = "Blood_Pressure", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(Resting_BP)$out))
# There is no outlier present now

boxplot(Cholestoral, 
        main = "Cholestorol", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(Cholestoral)$out))
# There is no outlier present now

boxplot(Max_heartrate, 
        main = "Heart_Rate", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(Max_heartrate)$out))
# There is no outlier present now

###### Check the normality
library(e1071)
opar <- par(no.readonly = TRUE)
par(mfrow =c(3,4)) # 2rows * 4cols

plot(density(Age), 
     main = "Density plot for Age", 
     ylab = "Frequency", xlab = "Age",
     sub = paste("Skewness: ", 
                 round(e1071::skewness(Age), 2)))

# Fill in the area under the plot with red
polygon(density(Age), col = "red")

plot(density(Sex), 
     main = "Density plot for Sex", 
     ylab = "Frequency", xlab = "Sex",
     sub = paste("Skewness: ", 
                 round(e1071::skewness(Sex), 2)))

# Fill in the area under the plot with red
polygon(density(Sex), col = "red")

plot(density(Target), 
     main = "Density plot for Target", 
     ylab = "Frequency", xlab = "Target",
     sub = paste("Skewness: ", 
                 round(e1071::skewness(Target), 2)))

# Fill in the area under the plot with red
polygon(density(Target), col = "red")

plot(density(Chest_pain), 
     main = "Density plot for Chest_Pain", 
     ylab = "Frequency", xlab = "Chest_Pain",
     sub = paste("Skewness: ", 
                 round(e1071::skewness(Chest_pain), 2)))

# Fill in the area under the plot with red
polygon(density(Chest_pain), col = "red")

plot(density(Resting_BP), 
     main = "Density plot for Blood_Pressure", 
     ylab = "Frequency", xlab = "Blood_Pressure",
     sub = paste("Skewness: ", 
                 round(e1071::skewness(Resting_BP), 2)))

# Fill in the area under the plot with red
polygon(density(Resting_BP), col = "red")

plot(density(Cholestoral), 
     main = "Density plot for Cholestorol", 
     ylab = "Frequency", xlab = "Cholestorol",
     sub = paste("Skewness: ", 
                 round(e1071::skewness(Cholestoral), 2)))

# Fill in the area under the plot with red
polygon(density(Cholestoral), col = "red")

plot(density(Fasting_BS), 
     main = "Density plot for Sugar", 
     ylab = "Frequency", xlab = "Sugar",
     sub = paste("Skewness: ", 
                 round(e1071::skewness(Fasting_BS), 2)))

# Fill in the area under the plot with red
polygon(density(Fasting_BS), col = "red")

plot(density(Resting_ECG), 
     main = "Density plot for ECG", 
     ylab = "Frequency", xlab = "ECG",
     sub = paste("Skewness: ", 
                 round(e1071::skewness(Resting_ECG), 2)))

# Fill in the area under the plot with red
polygon(density(Resting_ECG), col = "red")

plot(density(Max_heartrate), 
     main = "Density plot for Heart_Rate", 
     ylab = "Frequency", xlab = "Heart_Rate",
     sub = paste("Skewness: ", 
                 round(e1071::skewness(Max_heartrate), 2)))

# Fill in the area under the plot with red
polygon(density(Max_heartrate), col = "red")

plot(density(Excercise_angina), 
     main = "Density plot for Exercise", 
     ylab = "Frequency", xlab = "Exercise",
     sub = paste("Skewness: ", 
                 round(e1071::skewness(Excercise_angina), 2)))

# Fill in the area under the plot with red
polygon(density(Excercise_angina), col = "red")

plot(density(Num_major_vessel), 
     main = "Density plot for Blood_Vessels", 
     ylab = "Frequency", xlab = "Blood_Vessels",
     sub = paste("Skewness: ", 
                 round(e1071::skewness(Num_major_vessel), 2)))

# Fill in the area under the plot with red
polygon(density(Num_major_vessel), col = "red")

paste("Skewness for Age: ", round(e1071::skewness(Age), 2))
paste("Skewness for Sex: ", round(e1071::skewness(Sex), 2))
paste("Skewness for Target: ", round(e1071::skewness(Target), 2))
paste("Skewness for Chest_Pain: ", round(e1071::skewness(Chest_pain), 2))
paste("Skewness for Blood_Pressure: ", round(e1071::skewness(Resting_BP), 2))
paste("Skewness for Cholestorol: ", round(e1071::skewness(Cholestoral), 2))
paste("Skewness for Sugar: ", round(e1071::skewness(Fasting_BS), 2))
paste("Skewness for ECG: ", round(e1071::skewness(Resting_ECG), 2))
paste("Skewness for Heart_Rate: ", round(e1071::skewness(Max_heartrate), 2))
paste("Skewness for Excercise: ", round(e1071::skewness(Excercise_angina), 2))
paste("Skewness for Blood_Vessel: ", round(e1071::skewness(Num_major_vessel), 2))

# Minimal skewness = -0.11 - slightly skewed 
# NB a skewness value <-1 or >1 = highly skewed 
# Skewness -1 to -0.5 and 0.5 to 1 = moderately skewed 
# And skewness -0.5 to 0.5 = approx symmetric

# Age = -0.15 - approx symmetric
# Sex = -0.88 - moderately skewed
# Target = -0.21 - approx symmetric
# Chest_Pain = 0.47 - moderately skewed
# Blood_Pressure = 0.25 - approx symmetric
# Cholestorol = 0.19 - approx symmetric
# Sugar = 2.08 - highly skewed
# ECG = 0.1 - approx symmetric
# Heart_Rate = -0.46 - approx symmetric
# Excercise = 0.77 - moderately skewed
# Blood_Vessel = 1.31 - highly skewed

# Histogram Visualization
opar <- par(no.readonly = TRUE)
par(mfrow = c(1,2)) # divide the graph area in 2 cols

hist(Target, 
     main = "Normalility proportion of Target", 
     xlab = "Target")

qqnorm(Target)
qqline(Target, col = "red")


hist(Age, 
     main = "Normalility proportion of Age", 
     xlab = "Age")

qqnorm(Age)
qqline(Age, col = "red")


hist(Sex, 
     main = "Normalility proportion of Sex", 
     xlab = "Sex")

qqnorm(Sex)
qqline(Sex, col = "red")

hist(Chest_pain, 
     main = "Normalility proportion of Chest_Pain", 
     xlab = "Chest_Pain")

qqnorm(Chest_pain)
qqline(Chest_pain, col = "red")


hist(Resting_BP, 
     main = "Normalility proportion of Blood_Pressur", 
     xlab = "Blood_Pressure (mm/Hg)")

qqnorm(Resting_BP)
qqline(Resting_BP, col = "red")

hist(Cholestoral, 
     main = "Normalility proportion of Cholestorol", 
     xlab = "Cholestorol (mg/dl)")

qqnorm(Cholestoral)
qqline(Cholestoral, col = "red")

hist(Fasting_BS, 
     main = "Normalility proportion of Sugar", 
     xlab = "Sugar (mg/dl)")

qqnorm(Fasting_BS)
qqline(Fasting_BS, col = "red")

hist(Resting_ECG, 
     main = "Normalility proportion of ECG", 
     xlab = "ECG")

qqnorm(Resting_ECG)
qqline(Resting_ECG, col = "red")

hist(Max_heartrate, 
     main = "Normalility proportion of Heart_Rate", 
     xlab = "Heart_Rate")

qqnorm(Max_heartrate)
qqline(Max_heartrate, col = "red")

hist(Excercise_angina, 
     main = "Normalility proportion of Excercise", 
     xlab = "Excercise")

qqnorm(Excercise_angina)
qqline(Excercise_angina, col = "red")

hist(Num_major_vessel, 
     main = "Normalility proportion of Blood_Vessels", 
     xlab = "Blood_Vessels")

qqnorm(Num_major_vessel)
qqline(Num_major_vessel, col = "red")

par <- opar

attach(new_heart_data)
mlr_model <- lm(Target ~ 
                  Age + Resting_BP + Cholestoral + Max_heartrate)
detach(new_heart_data)

summary(mlr_model)

attach(new_heart_data)
mlr_model_1 <- lm(Target ~ 
                    Resting_BP + Chest_pain + Cholestoral + Max_heartrate + 
                    Resting_ECG + Fasting_BS + Num_major_vessel)
summary(mlr_model_1)

mlr_model_2 <- lm(Target ~ 
                    Resting_BP + Cholestoral + Max_heartrate + Chest_pain)
summary(mlr_model_2)













