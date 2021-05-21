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
# using (pairs) & (pairs.panel)
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
# Using Different Technique
####################################################################

# Tranforming the categorical variable to the required factor format 
# using (as.factor) function
new_heart_data$Sex <- as.factor(new_heart_data$Sex)
new_heart_data$Chest_pain <- as.factor(new_heart_data$Chest_pain)
new_heart_data$Fasting_BS <- as.factor(new_heart_data$Fasting_BS)
new_heart_data$Resting_ECG <- as.factor(new_heart_data$Resting_ECG)
new_heart_data$Excercise_angina <- as.factor(new_heart_data$Excercise_angina)
new_heart_data$Target <- as.factor(new_heart_data$Target)

# Providing levels and labels for the factored variables
# for Sex category 
# Defining the levels of the Sex as:
# Value 0 = Female
# Value 1= Male
levels(new_heart_data$Sex) <- c("Female", 
                                "Male")

# for Chest_Pain category
# Defining the levels of the Chest Pain as:
# Value 0 = Typical Angina
# Value 1 = Atypical Angina
# Value 2 = Non-Anginal Pain
# Value 3 = Asymptomatic
levels(new_heart_data$Chest_pain) <- c("Typical angina", 
                                       "Atypical angina", 
                                       "Non angina", 
                                       "Asymptomatic")

# for Fasting_Sugar category
# Defining the levels of the Blood SUgar as:
# Value 0 = False
# Value 1= True
levels(new_heart_data$Fasting_BS) <- c("False", 
                                       "True")

# for Resting_ECG category
# Defining the levels of the ECG as:
# Value 0 = Normal
# Value 1 = Abnormal
# Value 2 = Hypertrophy
levels(new_heart_data$Resting_ECG) <- c("Normal", 
                                        "Abnormal", 
                                        "Hypertrophy")

# for Excercise_Angina category 
# Defining the levels of the Excercise as:
# Value 0 = No
# Value 1= Yes
levels(new_heart_data$Excercise_angina) <- c("No", 
                                             "Yes")

# for Target category 
# Defining the levels of the Target as:
# Value 0 = Less chance of HA
# Value 1= More chance of HA
levels(new_heart_data$Target) <- c("Less chance of HA", 
                                   "More chance of HA")

# Observe the structure of DF
str(new_heart_data)

# Display the enties from the DF
head(new_heart_data)

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

# Importing & Installing the required packages & libraries
install.packages("ggplot2")
install.packages("caret")
install.packages("rpart.plot")
library(ggplot2)
library(caret)
library(rpart.plot)

# Plot the graph to analyze the specified attributes with the Target
ggplot(new_heart_data, aes(Target, fill = Target)) + 
  geom_bar() + labs(x = "Heart-Attack status", y = "Patien Count") + 
  guides(fill = FALSE)








