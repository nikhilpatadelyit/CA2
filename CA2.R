# Analyzing and dealing with the data related to the "HEART"
# for visualizing and predicting the functioning & working 
# of the patients heart and implementing the predictive model for the chance 
# of "HEART-ATTACK" with the variables present.

# Importing & Installing the required packages & libraries
# wherever needed for analyzing and visualizing

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

# Investigation for the initial variables
pairs(new_heart_data)

# To visualize the distribution and correlation
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
# Heart Data-set - Predicting the chances of getting a "Heart-Attack"
# Using "Multiple Linear Regression" Technique
####################################################################

# For further implementation we need to keep in mind about the
# dependent & independent variables for predicting the model
# DEPENDENT variable = "TARGET"
# INDEPENDENT variables = All others except 'TARGET'


# Including the variables we need to use for predicting a model
# Dependent var = X-axis
# Independent var = Y-axis

# Low correlation = value equal to zero
# High Correlation = straight line in plot (value > 0.60) 
# (positive & negative)
# No correlation = No correlation no line

# We will check the assumptions stricked for Linearity
# Checking for variable having any Outliers, Normality, Collinearity

# Step-1: We will check the correlation
attach(new_heart_data)
str(new_heart_data)
opar <- par(no.readonly = TRUE)
par(mfrow = c(2, 3)) # divide graph area in 2 rows by 3 columns
# We can examine whether there is a linear correlation between both variables
# For Target v/s Age
# Plot the graph to analyze the specified attributes 
# with Target & Age
scatter.smooth(x = Target, y = Age, 
               main = "Target ~ Age", 
               xlab = "Chance of Heart Attack",
               ylab = "Age (Years)")
# The plot shows there is very NO Correlation

# Checking Correlation
# Values of -0.2 < x < 0.2 - Low Correlation
cor(Target, Age)
# It is giving a negative correlation value = -0.22
# The correlation tests shows that the correlation between the Target & 
# Age variable = -0.22 indicating a negative correlation.
# The AGE variable is of no use in predicting the model.

# For Target v/s Sex
# Plot the graph to analyze the specified attributes 
# with Target & Sex
scatter.smooth(x = Target, y = Sex, 
               main = "Target ~ Sex", 
               xlab = "Chance of Heart Attack",
               ylab = "Sex (Gender)")
# The plot shows there is No Correlation

# Checking Correlation
# Values of -0.2 < x < 0.2 - Low Correlation
cor(Target, Sex)
# It is giving a negative correlation value = -0.28
# The correlation tests shows that the correlation between the Target & 
# Sex variable = -0.28 indicating a negavtive correlation.
# The SEX field can also be excluded for predicting an model.

# For Target v/s Chest Pain
# Plot the graph to analyze the specified attributes 
# with Target & Chest_Pain
scatter.smooth(x = Target, y = Chest_pain, 
               main = "Target ~ Chest_Pain", 
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

# For Target v/s Blood_Pressure
# Plot the graph to analyze the specified attributes 
# with Target & Blood_Pressure
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

# For Target v/s Cholestoral
# Plot the graph to analyze the specified attributes 
# with Target & Cholestoral
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

# For Target v/s Sugar
# Plot the graph to analyze the specified attributes 
# with Target & Sugar
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

# For Target v/s ECG
# Plot the graph to analyze the specified attributes 
# with Target & ECG
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

# For Target v/s Heart_Rate
# Plot the graph to analyze the specified attributes 
# with Target & Heart_Rate
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

# For Target v/s Excercise
# Plot the graph to analyze the specified attributes 
# with Target & Excercise
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

# For Target v/s Blood_Vessels
# Plot the graph to analyze the specified attributes 
# with Target & Blood_Vessels
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

par(opar)

# To check the results of correlation for all the variables from the DF
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


# Structure of the DF
str(new_heart_data)
head(new_heart_data)

# Initially making the DF to use for just READ-ONLY option
opar <- par(no.readonly = TRUE)

par(mfrow = c(2, 3)) # divide graph area in 3 rows by 2 columns

# We will check here for any Outliers
# Step 2: we will check the Outliers here
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

# We will remove the Outliers if present in any variable
# using the boxplot.stats() function to extract 
# the outliers values where they are identified in the DF
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
opar <- par(no.readonly = TRUE)
par(mfrow = c(1, 3)) # divide graph area in 1 rows by 3 columns
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
par(opar)
# Here we will check the Normality and skewness
# Step-3: We are going to check the normality and skewness
library(e1071)
opar <- par(no.readonly = TRUE)
par(mfrow =c(2,3)) # 2rows * 3cols

# We can also get to the skewness for all the variables into the DF
# using the following function
# Plot the density graph for the variable specified
plot(density(Age), 
     main = "Density plot for Age", 
     ylab = "Frequency", xlab = "Age",
     sub = paste("Skewness: ", 
                 round(e1071::skewness(Age), 2)))

# Fill in the area under the plot with red
polygon(density(Age), col = "red")

# Plot the density graph for the variable specified
plot(density(Sex), 
     main = "Density plot for Sex", 
     ylab = "Frequency", xlab = "Sex",
     sub = paste("Skewness: ", 
                 round(e1071::skewness(Sex), 2)))

# Fill in the area under the plot with red
polygon(density(Sex), col = "red")

# Plot the density graph for the variable specified
plot(density(Target), 
     main = "Density plot for Target", 
     ylab = "Frequency", xlab = "Target",
     sub = paste("Skewness: ", 
                 round(e1071::skewness(Target), 2)))

# Fill in the area under the plot with red
polygon(density(Target), col = "red")

# Plot the density graph for the variable specified
plot(density(Chest_pain), 
     main = "Density plot for Chest_Pain", 
     ylab = "Frequency", xlab = "Chest_Pain",
     sub = paste("Skewness: ", 
                 round(e1071::skewness(Chest_pain), 2)))

# Fill in the area under the plot with red
polygon(density(Chest_pain), col = "red")

# Plot the density graph for the variable specified
plot(density(Resting_BP), 
     main = "Density plot for Blood_Pressure", 
     ylab = "Frequency", xlab = "Blood_Pressure",
     sub = paste("Skewness: ", 
                 round(e1071::skewness(Resting_BP), 2)))

# Fill in the area under the plot with red
polygon(density(Resting_BP), col = "red")

# Plot the density graph for the variable specified
plot(density(Cholestoral), 
     main = "Density plot for Cholestorol", 
     ylab = "Frequency", xlab = "Cholestorol",
     sub = paste("Skewness: ", 
                 round(e1071::skewness(Cholestoral), 2)))

# Fill in the area under the plot with red
polygon(density(Cholestoral), col = "red")

# Plot the density graph for the variable specified
plot(density(Fasting_BS), 
     main = "Density plot for Sugar", 
     ylab = "Frequency", xlab = "Sugar",
     sub = paste("Skewness: ", 
                 round(e1071::skewness(Fasting_BS), 2)))

# Fill in the area under the plot with red
polygon(density(Fasting_BS), col = "red")

# Plot the density graph for the variable specified
plot(density(Resting_ECG), 
     main = "Density plot for ECG", 
     ylab = "Frequency", xlab = "ECG",
     sub = paste("Skewness: ", 
                 round(e1071::skewness(Resting_ECG), 2)))

# Fill in the area under the plot with red
polygon(density(Resting_ECG), col = "red")

# Plot the density graph for the variable specified
plot(density(Max_heartrate), 
     main = "Density plot for Heart_Rate", 
     ylab = "Frequency", xlab = "Heart_Rate",
     sub = paste("Skewness: ", 
                 round(e1071::skewness(Max_heartrate), 2)))

# Fill in the area under the plot with red
polygon(density(Max_heartrate), col = "red")

# Plot the density graph for the variable specified
plot(density(Excercise_angina), 
     main = "Density plot for Exercise", 
     ylab = "Frequency", xlab = "Exercise",
     sub = paste("Skewness: ", 
                 round(e1071::skewness(Excercise_angina), 2)))

# Fill in the area under the plot with red
polygon(density(Excercise_angina), col = "red")

# Plot the density graph for the variable specified
plot(density(Num_major_vessel), 
     main = "Density plot for Blood_Vessels", 
     ylab = "Frequency", xlab = "Blood_Vessels",
     sub = paste("Skewness: ", 
                 round(e1071::skewness(Num_major_vessel), 2)))

# Fill in the area under the plot with red
polygon(density(Num_major_vessel), col = "red")

par(opar)

# Displaying the result obtained for skewness through the density graph
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

# Histogram Visualization and distribution
# Here we will be checking the normality using Q-Q-Norm-Plot
opar <- par(no.readonly = TRUE)

# Defining the length of the graph to get them plot
par(mfrow = c(1,2)) # divide the graph area in 2 cols

# Visual analysis of the data using histogram
# To check the normality of the data present in different variable
# also showing a line representing if the data is normally distributed or not
# using qqnorm() & qqline() function

# Visual representation for analyzing the 
# normal distribution of the variables

# For Target
hist(Target, 
     main = "Normalility proportion of Target", 
     xlab = "Target")

qqnorm(Target)
qqline(Target, col = "red")

# For Age
hist(Age, 
     main = "Normalility proportion of Age", 
     xlab = "Age")

qqnorm(Age)
qqline(Age, col = "red")

# For Sex
hist(Sex, 
     main = "Normalility proportion of Sex", 
     xlab = "Sex")

qqnorm(Sex)
qqline(Sex, col = "red")

# For Chest_Pain
hist(Chest_pain, 
     main = "Normalility proportion of Chest_Pain", 
     xlab = "Chest_Pain")

qqnorm(Chest_pain)
qqline(Chest_pain, col = "red")

# For Blood_Pressure
hist(Resting_BP, 
     main = "Normalility proportion of Blood_Pressure", 
     xlab = "Blood_Pressure (mm/Hg)")

qqnorm(Resting_BP)
qqline(Resting_BP, col = "red")

# For Cholestoral
hist(Cholestoral, 
     main = "Normalility proportion of Cholestorol", 
     xlab = "Cholestorol (mg/dl)")

qqnorm(Cholestoral)
qqline(Cholestoral, col = "red")

# For Sugar
hist(Fasting_BS, 
     main = "Normalility proportion of Sugar", 
     xlab = "Sugar (mg/dl)")

qqnorm(Fasting_BS)
qqline(Fasting_BS, col = "red")

# For ECG
hist(Resting_ECG, 
     main = "Normalility proportion of ECG", 
     xlab = "ECG")

qqnorm(Resting_ECG)
qqline(Resting_ECG, col = "red")

# For Heart_Rate
hist(Max_heartrate, 
     main = "Normalility proportion of Heart_Rate", 
     xlab = "Heart_Rate")

qqnorm(Max_heartrate)
qqline(Max_heartrate, col = "red")

# For Excercise
hist(Excercise_angina, 
     main = "Normalility proportion of Excercise", 
     xlab = "Excercise")

qqnorm(Excercise_angina)
qqline(Excercise_angina, col = "red")

# For Blood_vessels
hist(Num_major_vessel, 
     main = "Normalility proportion of Blood_Vessels", 
     xlab = "Blood_Vessels")

qqnorm(Num_major_vessel)
qqline(Num_major_vessel, col = "red")

par <- opar

##################################
# IMPLEMENTING A LINEAR REGRESSION
##################################
attach(new_heart_data)
fit_test_lr <- lm(Target ~ Age + Sex + Resting_BP + Resting_ECG + 
                 Fasting_BS + Max_heartrate + Num_major_vessel + Cholestoral + 
                 Excercise_angina + Chest_pain)

# observe the statistic result for linear regression
summary(fit_test_lr)

###############################################
# Model Validation (Train & Test)
###############################################
# Keeping in mind we have dropped the null values & 
# also the categorical variables are converted to factor as required

# The training set will evaluate the model using all the variables we defined.
# Comparing the model with the ratio of 80% training & 
# 20% for testing the instances
# Observing that the distribution of the Dependent var (Target) need to be same

attach(new_heart_data)
set.seed(1)
no_rows_heart_data <- nrow(new_heart_data)
sample <- sample(1:no_rows_heart_data, 
                 size = round(0.8 * no_rows_heart_data), 
                 replace = FALSE)

training_data <- new_heart_data[sample, ] # it has 230 records
testing_data <- new_heart_data[-sample, ] # it has 58 records


##################################################################
# Normal  analysis model = (fit_model) with all the variables
#################################################################

# The trained data is stored into the fit_model with a defined formula for lm()
fit_model <- lm(Target ~ Age + Sex + Chest_pain + 
                  Resting_BP + Resting_ECG + Fasting_BS + 
                  Max_heartrate + Cholestoral + Excercise_angina + 
                  Num_major_vessel, data = training_data)

# Summary of the fit_model to view the statistic results
summary(fit_model)
# After viewing the results,
# the *** signifies that they have a high correlation between the variables.
# The coefficient is significantly different from zero at the p-value < 0.0001.

# The coefficient values for Age, Resting_BP, Resting_ECG, Fasting_BS signifies 
# they are not linearly correlated comparing the other predictor variables.
# While the Sex, Chest_Pain, Max_heartrate, Excercise_angina & Num_major_vessels 
# values shows that they are having a linear relationship.

# The Multiple R-squared value explains that there is 45% of the variation 
# of getting the Heart-Attack for the patients data provided.

# Analyzing the confidence interval result of the model build
confint(fit_model)

attach(new_heart_data)
# Importing the library
library(car)
par(mfrow = c(1,1))

# Plots empirical quantiles of studentized residuals from a linear model, 
# against theoretical quantiles of a comparison distribution
qqPlot(fit_model, 
       labels=row(new_heart_data), 
       id.method="identify", 
       simulate=TRUE, 
       main = "Q-Q Plot for fit_model")
# We identified that there are two values in the plot which have outliers
# We will analyze them and make the decision

# Training the outlier data and analyzing whether it affect or not
training_data["140",]
training_data["260",]

# Fitting the outlier data to the new model
fitted(fit_model)["140"]
fitted(fit_model)["260"]

# We will use the standardize residuals for better statistical analysis 
# as they are independent and the randomly generated samples are not zero.
library(car)
# Histogram Visualization for the distribution error
# Defining the length of the graph to get them plot
student_fit_model <- rstudent(fit_model)
hist(student_fit_model,
     breaks=10,
     freq=FALSE,
     xlab="Studentized Residual",
     main="Distribution of Errors")

rug(jitter(student_fit_model), col="brown")

curve(dnorm(x, mean=mean(student_fit_model), sd=sd(student_fit_model)), 
      add=TRUE, col="blue", lwd=2)

lines(density(student_fit_model)$x, density(student_fit_model)$y, col="red", lwd=2, lty=2)

legend("topright", legend = c( "Normal Curve", "Kernel Density Curve"), 
       lty=1:2, col=c("blue","red"), cex=.7)

# We can use the below function to check whether a model 
# contains any outliers
outlierTest(fit_model)
par <- opar
# Here we removed the entire row as we came across that there were 
# some outliers present when we performed (outlierTest) on the model to fit
new_heart_data[-c(140), ]
new_heart_data <- new_heart_data[-c(140), ]

# REBUILDING A MODEL
# Training the data available by dropping the outlier row and building 
# the set of new train & test data.
attach(new_heart_data)
set.seed(1)
no_rows_heart_data <- nrow(new_heart_data)
sample <- sample(1:no_rows_heart_data, 
                 size = round(0.8 * no_rows_heart_data), 
                 replace = FALSE)

training_data <- new_heart_data[sample, ] # it has 230 records
testing_data <- new_heart_data[-sample, ] # it has 57 records

# The trained data is stored into the fit_model with a defined formula for lm()
fit_model <- lm(Target ~ Age + Sex + Chest_pain + 
                  Resting_BP + Resting_ECG + Fasting_BS + 
                  Max_heartrate + Cholestoral + Excercise_angina + 
                  Num_major_vessel, data = training_data)


# Summary of the fit_model to view the statistic results
summary(fit_model)
# The Multiple R-squared value explains that there is 43% of the variation 
# of getting the Heart-Attack for the patients data provided.

# AIC() for the normal model build
AIC(fit_model)
# AIC value = 227.85

# After using the outlierTest function and getting reed of the outliers 
# there is not much difference in the R-squared values of model build.
# Only a slight difference of 1% is observed even after removing the outliers 
# by best fitting the model again with the set of train & test data.

opar <- par(no.readonly = TRUE)

# Histogram Visualization for the distribution error after deleting the outliers
# Defining the length of the graph to get them plot
par(mfrow = c(1,1)) # divide the graph area in 1 cols
library(car)
attach(new_heart_data)
student_fit_model <- rstudent(fit_model)
hist(student_fit_model,
     breaks=10,
     freq=FALSE,
     xlab="Studentized Residual",
     main="Distribution of Errors")

rug(jitter(student_fit_model), col="brown")

curve(dnorm(x, mean=mean(student_fit_model), sd=sd(student_fit_model)), 
      add=TRUE, col="blue", lwd=2)

lines(density(student_fit_model)$x, density(student_fit_model)$y, col="red", lwd=2, lty=2)

legend("topright", legend = c( "Normal Curve", "Kernel Density Curve"), 
       lty=1:2, col=c("blue","red"), cex=.7)
# The plot shows that the outliers are been removed

# We can use the below function to check whether a model 
# contains any outliers
outlierTest(fit_model)
# As we decided we can keep the outliers as processed further to build
# and evaluate a model
par <- opar

# Checking the linearity of the data using different plot techniques
# To visualize the linear relationship between the 
# dependent & independent through a linear line.
crPlots(fit_model)

# Influential observations using the cooks distance formula on trained data
cutoff <- 4/(nrow(training_data) - length(fit_model$coefficients) - 2)


# Plotting the graphical analysis of the values using Cook-D method
plot(fit_model, which = 4, cook.levels = cutoff)

# It draws a cutoff line where any data above it can be ignored
abline(h = cutoff, lty = 2, col = "red")

# We will not plot any influence data as we are not observing any influence 
# below the cutoff value line, we can proceed further

# We can create a graphical analysis of the predictor variables with all 
# independent variables which yeild only a single response on the dependent var
# Av plots are known as added-variable plots used to show the regression 
# coefficient of the predictor variables
avPlots(fit_model, ask = FALSE)

# Influence Plot
library(car)
influencePlot(fit_model, main = "Influence Plot", 
              sub = "Circle size is proportional to Cooks distance")

# The influence plot shows that the row (300, 260, 140) are very close to the 
# boundary level which is selected by default and may be they are the outliers. 

# We can now check the Homoscedasticity Test using (ncvTest) which generates the 
# result for the hypothesis of constant error variance with a fitted model data
# If p-value < 0.05, then the error variance value may change (Homoscedasticity)
# If p-value > 0.05, then the error variance value may not change (Heteroscedasticity)
ncvTest(fit_model)
# A result shows that p-value = 0.8, which is greater than cut-off,
# then the error variance value does not change

# The following visualization will show the scatter plot of the 
# standardized residuals versus the fitted model values & draws 
# the line which best fits the data
par(mfrow = c(1,1))
spreadLevelPlot(fit_model)
par <- opar
# Suggested power transformation:  0.816969
# we observed a best fit line with the above function but it is not actual to the
# default line, but it best fits with the data used for building a model

# install.packages("gvlma")
# It determines the global validation of the linear model assumptions & also 
# evaluates separately the different test performed while building a model
library(gvlma)
gvmodel <- gvlma(fit_model)
summary(gvmodel)
# We observed that the model build accepts all the statistical assumptions we 
# made with the regression model.
# Also the p-values > 0.05, so the decisions are acceptable.

# Checking the multi-collinearity
# It can be detected with the Variance Inflation Factor(VIF)
library(car)
vif(fit_model)

# We can check whether any of the variables indicate a 
# multi-collinearity problem if the value > 2
sqrt(vif(fit_model)) > 2
# The individual coefficient of the variables are < 2, so there is no 
# problem of multi-collinearity.

# Model Forecasting
# Log transformation of the variables
# As we have already generated the power transformation value = 0.81, there was
# no need to do the power transformation for the variables

# Just to acknowledge that the value 0.8 lies between (0 - 1) I will perform 
# a sqrt() on the new model with the dependent variable
# Transforming the Target variable with the sqrt() funtion
sqrt_transform_Target <- sqrt(training_data$Target)
training_data$Target_sqrt <- sqrt_transform_Target

# Building a model with the log transformation 
fit_test_model <- lm(Target_sqrt ~ Age + Sex + Chest_pain + 
                       Resting_BP + Resting_ECG + Fasting_BS + 
                       Max_heartrate + Cholestoral + Excercise_angina + 
                       Num_major_vessel, data = training_data)

# Summary of the fit_test_model transformation model to view the statistic results
summary(fit_test_model)
# The Multiple R-squared value explains that there is 43% of the variation 
# of getting the Heart-Attack for the patients data provided.

# AIC() for the transform model build
AIC(fit_test_model)
# AIC value = 227.85

# Comparing the model build with different variables using the AIC() function
AIC(fit_model, fit_test_model)
# When AIC scored is compared it does not have any difference on the 
# model (fit_model) which was normally build using the examined variables 
# & model (fit_test_model) which was build after performing transformation.

# So for further evaluation we can use any of the model for further prediction
# as both model does not have any difference between the AIC score.

# Evaluating the analysis of the model using a Step-wise Regression
library(MASS)
stepAIC(fit_test_model, direction = "backward")
# Performing the 'backward' stepwise regression on this model it showed 
# that the predictor variables are removed one by one by the stepAIC() function  
# for calculating the AIC score with the better combination of the variables.
# At some point when the combinations of the variables does not change its value
# the stepAIC() stops calculating further and gives a result which better fits.
# Here it suggests that (Sex + Chest_pain + Resting_ECG + Max_heartrate + 
# Cholestoral + Excercise_angina + Num_major_vessel) is the best combination 
# for the model with AIC score = -431.84
# We cannot conclude that this is a best fit for the predictive model.

# install.packages("leaps")
library(leaps)
leaps <- regsubsets(Target_sqrt ~ Age + Sex + Chest_pain + 
                      Resting_BP + Resting_ECG + Fasting_BS + 
                      Max_heartrate + Cholestoral + Excercise_angina + 
                      Num_major_vessel, data = training_data, nbest = 4)
plot(leaps,scale = "adjr2")
# The leap plot shows the best correlation of the variables with the score of 
# R-squared and Adjusted R-squared values on the y-axis.
# A model (bottom row) with the intercept and Excercise_angina has an 
# adjusted R-squared = 0.15.
# At the top with the variables(intercept, Sex, Chest_pain, Resting_ECG, Max_heartrate, 
# Cholestoral, Excercise_angina, Num_major_vessel) shows the adjusted R-squared = 0.46.
# Thus the top row with the value 0.41 shows that the variables used are the best fit
# model build. A stepwise regression check also correlates with this varaibles.

# After performing the stepwise regression we examined and came across some 
# different predictor variables so here we will build a new model (fit_model_new) 
# and observe the statistics result for it and then compare the model with each 
# other and can evaluate ahead for prediction.
fit_model_new <- lm(Target_sqrt ~ Sex + Chest_pain + Resting_ECG + 
                        Max_heartrate + 
                        Cholestoral + Excercise_angina + Num_major_vessel, 
                      data = training_data)

# Summary observation for the new model
summary(fit_model_new)

################################################################
# Comparing the both models with the AIC test, and make decision
AIC(fit_model, fit_model_new)

# For fit_model = Model-1
# Now examine the accuracy of the model build earlier
predicted_heart_attack <- predict(fit_model, testing_data)

actual_prediction <- data.frame(cbind(actuals = testing_data$Target, 
                                      predicted = predicted_heart_attack))

head(actual_prediction)

correlation_accuracy <- cor(actual_prediction)
correlation_accuracy
# This build model shows 73% of the correlation accuracy.

# For fit_model_new = Model-2
# Now examine the accuracy of the model build earlier
predicted_heart_attack_new <- predict(fit_model_new, testing_data)

actual_prediction_new <- data.frame(cbind(actuals = testing_data$Target, 
                                      predicted = predicted_heart_attack_new))

head(actual_prediction_new)

correlation_accuracy <- cor(actual_prediction_new)
correlation_accuracy
# This build model shows 71% of the correlation accuracy.

############################################################################
# COMPARING THE TWO PREDICTIVE MODELs:
############################################################################
# Model1 = fit_model [ACCURACY = 73%]
# Model1: It has all the independent variables as predictor variables
# (Age, Sex, Chest_pain, Resting_BP, Resting_ECG, Fasting_BS, Max_heartrate) 
# (Cholestoral, Excercise_angina, Num_major_vessel)
############################################################################

############################################################################
# Model2 = fit_model_new [ACCURACY = 71%]
# Model2: It has the predictor variables examined by step-wise regression
# (Sex, Chest_pain,  Resting_ECG, Max_heartrate, Cholestoral) 
# (Excercise_angina, Num_major_vessel)
############################################################################

# When compared with each other it signifies that the Model-1(fit_model) is the 
# best fit with all the predictor variables, as they rely on each other 
# for predicting a good correlation accuracy result of 73%.

#############################################################################
# Thus we can conclude that high are the chances that patient will get a 
# heart attack, based on the  predictive model build and also along with the 
# analysis of the result statistic observed.
##############################################################################


#######################################################
# MODEL FORECASTING with the predictive built Model-1:
#######################################################
# We will forecast the model built providing the information from the data 
# to analyze and check for the prediction accuracy in results obtained.
summary(new_heart_data)
# First Forecasting Record:
# We will build a new df based on the model validated and predict the chance 
# of getting a heart attack through this built model.
df_heart_attack <- data.frame(Age = c(41), Sex = c(0), Chest_pain = c(1), 
                              Resting_BP = c(130), Resting_ECG= c(0), 
                              Fasting_BS = c(0), Cholestoral = c(204), 
                              Excercise_angina = c(0), Num_major_vessel = c(0), 
                              Max_heartrate = c(172))

predicted_heart_attack <- predict(fit_model, df_heart_attack)
predicted_heart_attack
# The results suggests that when there is an increase in the different part of 
# the human body such as the pressure, sugar, cholesterol, heart-rate level 
# and sudden chest pain and arteries are blocked then the model predicts that 
# there is 93% of the chances that the patient will get a heart attack.

# Second Forecasting Record:
# We will build a new df based on the model validated and predict the chance 
# of getting a heart attack through this built model.
df_heart_attack <- data.frame(Age = c(44), Sex = c(1), Chest_pain = c(0), 
                              Resting_BP = c(110), Resting_ECG= c(0), 
                              Fasting_BS = c(0), Cholestoral = c(197), 
                              Excercise_angina = c(0), Num_major_vessel = c(1), 
                              Max_heartrate = c(177))

predicted_heart_attack <- predict(fit_model, df_heart_attack)
predicted_heart_attack
# The results suggests that while observing the above fitted data to the model 
# when there is a increase in the cholesterol and heart-rate, even if rest of 
# the variables are operating the heart in normal position than there is a 50% 
# chance that patient may get a heart attack.

# Third Forecasting Record:
# We will build a new df based on the model validated and predict the chance 
# of getting a heart attack through this built model.
df_heart_attack <- data.frame(Age = c(71), Sex = c(0), Chest_pain = c(0), 
                              Resting_BP = c(112), Resting_ECG= c(1), 
                              Fasting_BS = c(0), Cholestoral = c(149), 
                              Excercise_angina = c(0), Num_major_vessel = c(0), 
                              Max_heartrate = c(125))

predicted_heart_attack <- predict(fit_model, df_heart_attack)
predicted_heart_attack
# Results stated that even when the heart is functioning properly but the 
# patient is old depending on the age variable then there is 77% possibility 
# that he may get a heart-attack.

# Fourth Forecasting Record:
# We will build a new df based on the model validated and predict the chance 
# of getting a heart attack through this built model.
df_heart_attack <- data.frame(Age = c(68), Sex = c(1), Chest_pain = c(0), 
                              Resting_BP = c(144), Resting_ECG= c(1), 
                              Fasting_BS = c(1), Cholestoral = c(193), 
                              Excercise_angina = c(0), Num_major_vessel = c(2), 
                              Max_heartrate = c(141))

predicted_heart_attack <- predict(fit_model, df_heart_attack)
predicted_heart_attack
# It states that the if patient is old age and every operating functions of 
# heart are working properly then they have a 25% possibility to get a heart 
# attack.

# Fifth Forecasting Record:
# We will build a new df based on the model validated and predict the chance 
# of getting a heart attack through this built model.
df_heart_attack <- data.frame(Age = c(35), Sex = c(0), Chest_pain = c(0), 
                              Resting_BP = c(138), Resting_ECG= c(1), 
                              Fasting_BS = c(0), Cholestoral = c(183), 
                              Excercise_angina = c(0), Num_major_vessel = c(0), 
                              Max_heartrate = c(182))

predicted_heart_attack <- predict(fit_model, df_heart_attack)
predicted_heart_attack
# It states that even if the patient is young age and has an increase in the 
# levels of pressure, cholesterol and heart_rate there are 95% chances that 
# he will get a heart attack.

# After forecasting the predictor variables with different information and 
# evaluating that particular data into the validated training and testing model,
# we analyzed the output of the static result that the built model shows a 
# good prediction accuracy for the information provided to train and test data 
# to further predict the assumptions we brought from the start, as it provides a 
# better accuracy in predicting the chances that patient will get a heart attack.

# We analyzed and evaluated through our model that if there is an increase in 
# the Cholestoral, Heart-rate, Blood-Pressure our model provides a best fit 
# with the prediction accuracy results of more than 90%.
# Comparatively males have a more chance of getting a heart attack.
# Also we observed that, If the variables have a normal level operation with the 
# heart, and there is an increase in the Age of the patient, here also our model 
# states that it the best fit with the observation made of more than 75%.

#############################################################################
# Thus we can conclude that high are the chances that patient will get a 
# heart attack, based on the  predictive model build and also along with the 
# analysis of the result statistic observed.
##############################################################################
