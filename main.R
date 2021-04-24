#------------------------------------------------------------------------------
# Clustering - PMIM402
# Michael Johns - 853369
#
# # NOTE: Run line by line - sometimes a plot will appear blank
#         - if run too quickly in succession
#
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
# Install required packages (Uncomment to install missing packages)
#------------------------------------------------------------------------------
#install.packages("tidyverse")
#install.packages("e1071")

#------------------------------------------------------------------------------
# Load required Libraries
#------------------------------------------------------------------------------
library(tidyverse) # Data manipulation
library(e1071) # Required for Machine Learning Functions

#------------------------------------------------------------------------------
# Load required data set
#------------------------------------------------------------------------------
heart <- read.csv("heart_disease_modified.csv")

# View data set
view(heart)

#------------------------------------------------------------------------------
# Check data and remove rows containing NA (incase there are some present)
#------------------------------------------------------------------------------
heart %>% summary
heart <- heart %>% drop_na()

#------------------------------------------------------------------------------
# Remove columns that contain no useful information for sueprvised learning
# 'X' and 'Patient_ID' identifiers
#------------------------------------------------------------------------------
heart %>% head(1)
heart <- heart %>% select(-X, -Patient_ID)
heart %>% head(1)

#------------------------------------------------------------------------------
# Split Data into training and test sets
#------------------------------------------------------------------------------

# 75% training, 25% testing split
bound <- floor((nrow(heart) / 4) * 3)

# Shuffle the data
heart <- heart[sample(nrow(heart)), ]
# Training set
heartTrain <- heart[1:bound, ]
# Test set
heartTest <- heart[(bound + 1):nrow(heart), ]

#------------------------------------------------------------------------------
# Naive Bayes Model
#------------------------------------------------------------------------------
nbModel <- naiveBayes(class ~ ., data = heartTrain)

# Print nbModel to console
nbModel

# Classify unseen(test set) data
nbPred <- predict(nbModel, heartTest)

# Confusion Matrix
table(predicted = nbPred, observed = heartTest$class)



