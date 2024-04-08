# An Vu Nguyen Dieu - Pledged
# BAT 3305 - Kaggle Project
# Dr. Zhu


##### Table of Contents #####
# A. Load the necessary packages and read data into data frames. 
# B. General summary of all variables: data type and percentage of missing values.
# C. Clean the original data. 
# D. Create new variables from existing variables. 
# E. Split the cleaned data into the Training and the Test data sets. 
# F. Develop models using Machine Learning methods. 


##### Data Cleaning Process #####

# A. Loading the necessary packages and reading data into data frames. 
# 1. Loading packages:
library(reshape2)
library(tidyverse)
library(fastDummies)
library(car)
library(e1071) 
library(MASS)
library(leaps)
library(glmnet)
library(randomForest)
library(gbm)

# 2. Load the original train and test data sets into R - Combining to create 
# a joint data set.
options(scipen = 99)
original_train <- read.csv("train.csv")
SalePrice <- original_train$SalePrice
original_train <- subset(original_train, select = -c(SalePrice))

original_test <- read.csv("test.csv")
original <- data.frame(rbind(original_train, original_test))

# 3. Keep only distinct observations:
cleaned <- distinct(original)
# The number of observations for cleaned is the same as that of train, 
# suggesting that the data set does not have any duplicated rows. 



# B. General summary of all variables: each variable's data type and their percentage
# of NA values.
Data <- original[1,]
Gen_Sum <- data.frame(Names = names(Data))
Gen_Sum$Class <- sapply(original, class)
Gen_Sum$Total_obs <- sapply(original, function(x) length(x))
Gen_Sum$NA_count <- sapply(original, function(x) length(which(is.na(x))))
Gen_Sum$NA_perc <- round(sapply(original, function(x) length(which(is.na(x)))/2919*100), 2)



# C. Clean the original data. 
# 1. Id
Gen_Sum[1,] 
cleaned$Id <- as.numeric(cleaned$Id)
# No missing values.
# Id is only an index for observations, so further cleaning is not necessary. 

# 2. MSSubClass
Gen_Sum[2,]
# No missing values.
# Replace level 150 with level 50 to avoid perfect multi-collinearity:
cleaned$MSSubClass <- ifelse(cleaned$MSSubClass == "150", "50", cleaned$MSSubClass)
# Because MSSubClass identifies the type of dwelling in each sale and only use
# numeric values to represent each type, it is a categorical variable.
cleaned$MSSubClass <- as.factor(cleaned$MSSubClass)

# Distribution of data
ggplot(cleaned, aes(MSSubClass)) + geom_bar()
round(prop.table(table(cleaned$MSSubClass))*100, 2)
# No level makes up >95%.


# 3. MSZoning
Gen_Sum[3, ]
# 4 missing values. 
# Replace the missing value with the most frequent value of MSZoning, RL.
cleaned$MSZoning[is.na(cleaned$MSZoning)] <- "RL"

# Because MSZoning classifies the general zoning of a sale, it is a categorical 
# variable.
cleaned$MSZoning <- as.factor(cleaned$MSZoning)
# Change the MSZoning "C (all)" level to "C" to match the data description.
levels(cleaned$MSZoning)[levels(cleaned$MSZoning) == "C (all)"] <- "C"

# Distribution of data:
ggplot(cleaned, aes(MSZoning)) + geom_bar()
round(prop.table(table(cleaned$MSZoning))*100, 2)
# No level makes up >95% data.


# 4. LotFrontage
Gen_Sum[4, ]
cleaned$LotFrontage <- as.numeric(cleaned$LotFrontage)
# There are 486 missing values (16.65%). 
# Replace the NAs with the mean LotFrontage of other observations with the same 
# MSZoning:
est_LotFrontage <- cleaned %>%
                      group_by(MSZoning) %>%
                        summarize(est_LF = mean(LotFrontage, na.rm = T))

for (i in 1:nrow(est_LotFrontage)) {
  cleaned$LotFrontage <- ifelse(is.na(cleaned$LotFrontage) == T & 
           cleaned$MSZoning == est_LotFrontage$MSZoning[i], 
         est_LotFrontage$est_LF[i],
         cleaned$LotFrontage)
  i <- i + 1
}
summary(is.na(cleaned$LotFrontage))
# No more missing values. 
rm(est_LotFrontage, i)

# Identify outliers.
ggplot(cleaned, aes(LotFrontage)) + geom_boxplot()
# Outliers of LotFrontage occur outside of 300.
# Replace outliers with the mean LotFrontage value.
cleaned$LotFrontage <- ifelse(cleaned$LotFrontage > 300, 
                                         mean(cleaned$LotFrontage), 
                                         cleaned$LotFrontage)

# Data distribution + Skewness:
ggplot(cleaned, aes(LotFrontage)) + geom_density()
skewness(cleaned$LotFrontage)
# The value is 0.65, slightly right skewed.
# Transformation:
ggplot(cleaned, aes(sqrt(LotFrontage))) + geom_density()
skewness(sqrt(cleaned$LotFrontage))
# After transformation, the skewness value reduced to -0.28, slightly skewed.
cleaned$LotFrontage <- sqrt(cleaned$LotFrontage)


# 5. LotArea
Gen_Sum[5,]
# No missing values. 

# Identify outliers.
ggplot(cleaned, aes(LotArea)) + geom_boxplot()
# Outliers of LotArea occur outside of 100,000.
# Replace outliers with the mean LotArea value.
cleaned$LotArea <- ifelse(cleaned$LotArea > 100000, 
                                    mean(cleaned$LotArea), 
                                    cleaned$LotArea)
#Skewness 
ggplot(cleaned, aes(LotArea)) + geom_density()
skewness(cleaned$LotArea)
# From the plot and the skewness value (3.75), it can be seen that the distribution
# is highly right-skewed.
# Transformation: 
ggplot(cleaned, aes(log(LotArea))) + geom_density()
skewness(log(cleaned$LotArea))
# Skewness is only -0.79, moderately skewed. 
cleaned$LotArea <- log(cleaned$LotArea)

# 6. Street
Gen_Sum[6,]
# No missivng value.
cleaned$Street <- as.factor(cleaned$Street)

# Distribution of data:
ggplot(cleaned, aes(Street)) + geom_bar()
round(prop.table(table(cleaned$Street))*100, 2)
# Because more than 99% of the observations are Pave, Street won't provide any 
# useful insights in modeling and should be removed from the data. 
cleaned <- subset(cleaned, select = -c(Street))


# 7. Alley
Gen_Sum[7,]
# There are 2721 NA values but they are not missing values and represent "No 
# alley access". NAs should be reclassified as "No alley access".
cleaned$Alley[is.na(cleaned$Alley)] <- "No alley access"
cleaned$Alley <- as.factor(cleaned$Alley)

# Data distribution:
ggplot(cleaned, aes(Alley)) + geom_bar()
round(prop.table(table(cleaned$Alley))*100, 2)
# No level makes up >95% of the data.


# 8. LotShape
Gen_Sum[8, ]
# No missing value. 
cleaned$LotShape<- factor(cleaned$LotShape, ordered = T,
                          levels = c("IR3", "IR2", "IR1", "Reg"))

# Data distribution:
ggplot(cleaned, aes(LotShape)) + geom_bar()
round(prop.table(table(cleaned$LotShape))*100, 2)
# No level makes up >95% of the data.


# 9. LandContour
Gen_Sum[9,]
# No missing value. 
cleaned$LandContour <- as.factor(cleaned$LandContour)

# Data distribution:
ggplot(cleaned, aes(LandContour)) + geom_bar()
round(prop.table(table(cleaned$LandContour))*100, 2)
# No level makes up >95% of the data.


# 10. Utilities
Gen_Sum[10,]
# 2 missing values. 
cleaned$Utilities <- as.factor(cleaned$Utilities)

# Data distribution:
ggplot(cleaned, aes(Utilities)) + geom_bar()
round(prop.table(table(cleaned$Utilities))*100, 2)
# AllPub makes up over 99% of the data for this variable and therefore Utilities
# should be removed because useful insights are unlikely to be extracted from it. 
cleaned <- subset(cleaned, select = -c(Utilities))


# 11. LotConfig
Gen_Sum[11,]
# No missing value. 
cleaned$LotConfig <- as.factor(cleaned$LotConfig)

# Data distribution:
ggplot(cleaned, aes(LotConfig)) + geom_bar()
round(prop.table(table(cleaned$LotConfig))*100, 2)
# No level makes up >95% of the data.


# 12. LandSlope
Gen_Sum[12, ]
# No Missing value.
cleaned$LotConfig <- as.factor(cleaned$LotConfig)

# Data distribution:
ggplot(cleaned, aes(LandSlope)) + geom_bar()
round(prop.table(table(cleaned$LandSlope))*100, 2)
# Gtl makes up 95% of the data for this variable and therefore LandSlope
# should be removed because useful insights are unlikely to be extracted from it. 
cleaned <- subset(cleaned, select = -c(LandSlope))


# 13. Neighborhood
Gen_Sum[13,]
# No missing value. 
cleaned$Neighborhood <- as.factor(cleaned$Neighborhood)

# Data distribution:
ggplot(cleaned, aes(Neighborhood)) + geom_bar()
round(prop.table(table(cleaned$Neighborhood))*100, 2)
# No level makes up >95% of the data.


# 14. Condition1
Gen_Sum[14,]
# No missing value
cleaned$Condition1 <- as.factor(cleaned$Condition1)

# Data distribution:
ggplot(cleaned, aes(Condition1)) + geom_bar()
round(prop.table(table(cleaned$Condition1))*100, 2)
# No level makes up >95% of the data.


# 15. Condition2
Gen_Sum[15,]
# No missing value. 
cleaned$Condition2 <- as.factor(cleaned$Condition2)

# Data distribution:
ggplot(cleaned, aes(Condition2)) + geom_bar()
round(prop.table(table(cleaned$Condition2))*100, 2)
# Because more than 98% of the observations are Norm, Condition2 won't provide any 
# useful insights in modeling and should be removed from the data. 
cleaned <- subset(cleaned, select = -c(Condition2))


# 16. BldgType
Gen_Sum[16,]
# No missing value.

summary(factor(cleaned$BldgType))
# There is an error/typo in "Twnhs" because it doesn't match with TwnhsI in the 
# data set description. "Twnhs" will be replaced with "TWnhsI" because there 
# are no values for "TWnhsI".
cleaned$BldgType <- ifelse(cleaned$BldgType == "Twnhs", "TwnhsI",
                                 cleaned$BldgType)
# Replace level 150 with level 50 to avoid perfect multi-collinearity:
cleaned$BldgType <- ifelse(cleaned$BldgType == "Duplex", "2fmCon",
                           cleaned$BldgType)
cleaned$BldgType <- as.factor(cleaned$BldgType)

# Data distribution:
ggplot(cleaned, aes(BldgType)) + geom_bar()
round(prop.table(table(cleaned$BldgType))*100, 2)
# No level makes up >95% of the data.


# 17. HouseStyle
Gen_Sum[17,]
# No missing value
cleaned$HouseStyle <- as.factor(cleaned$HouseStyle)

# Data distribution:
ggplot(cleaned, aes(HouseStyle)) + geom_bar()
round(prop.table(table(cleaned$HouseStyle))*100, 2)
# No level makes up >95% of the data.


# 18. OverallQual
Gen_Sum[18,]
# No missing value. 

# Because OverallQual uses numeric values to assess the overall material and finish
# of the house, it is a categorical variable. 
cleaned$OverallQual <- factor(cleaned$OverallQual, ordered = T,
                                        levels = c("1", "2", "3","4", "5", "6", "7", "8","9","10"))

# Data distribution:
ggplot(cleaned, aes(OverallQual)) + geom_bar()
round(prop.table(table(cleaned$OverallQual))*100, 2)
# No level makes up >95% of the data.


# 19. OverallCond
Gen_Sum[19,]
# No missing value.

# Because OverallCond uses numeric values to assess the overall condition 
# of the house, it is a categorical variable. 
cleaned$OverallCond <- factor(cleaned$OverallCond, ordered = T,
                                    levels = c("1", "2", "3","4", "5",
                                               "6", "7", "8", "9", "10"))

# Data distribution:
ggplot(cleaned, aes(OverallCond)) + geom_bar()
round(prop.table(table(cleaned$OverallCond))*100, 2)
# No level makes up >95% of the data.


# 20. YearBuilt
Gen_Sum[20,]
# No missing value

# Data distribution
ggplot(cleaned, aes(YearBuilt)) + geom_histogram(binwidth = 1)

# Create new variable: Age of each house since it was first built:
cleaned$HomeAge <- NA
for (i in 1:nrow(cleaned)) {
  cleaned$HomeAge[i] <- 2022 - cleaned$YearBuilt[i] + 1
  i <- i + 1
}
cleaned <- subset(cleaned, select = -c(YearBuilt))


# 21. YearRemodAdd
Gen_Sum[21,]
# No missing value

# Data distribution
ggplot(cleaned, aes(YearRemodAdd)) + geom_histogram(binwidth = 1)

# Create new variable: Age of each house since it was first built:
cleaned$AgeSinceRemod <- NA
for (i in 1:nrow(cleaned)) {
  cleaned$AgeSinceRemod[i] <- 2022 - cleaned$YearRemodAdd[i] + 1
  i <- i + 1
}
cleaned <- subset(cleaned, select = -c(YearRemodAdd))


# 22. RoofStyle
Gen_Sum[22,]
# No missing value. 
cleaned$RoofStyle <- as.factor(cleaned$RoofStyle)

# Data distribution:
ggplot(cleaned, aes(RoofStyle)) + geom_bar()
round(prop.table(table(cleaned$RoofStyle))*100, 2)
# No level makes up >95% of the data.


# 23. RoofMatl
Gen_Sum[23,]
# No missing value. 
cleaned$RoofMatl <- as.factor(cleaned$RoofMatl)

# Data distribution:
ggplot(cleaned, aes(RoofMatl)) + geom_bar()
round(prop.table(table(cleaned$RoofMatl))*100, 2)
# Because more than 98% of the observations are CompShg, RoofMatl won't provide any 
# useful insights in modeling and should be removed from the data. 
cleaned <- subset(cleaned, select = -c(RoofMatl))


# 24. Exterior1st
Gen_Sum[24,]
# 1 missing value. 
# Replace the missing value with the most frequent value of Exterior1st, VinylSd:
cleaned$Exterior1st[is.na(cleaned$Exterior1st)] <- "VinylSd"
cleaned$Exterior1st <- as.factor(cleaned$Exterior1st)

# Data distribution:
ggplot(cleaned, aes(Exterior1st)) + geom_bar()
round(prop.table(table(cleaned$Exterior1st))*100, 2)
# No level makes up >95% of the data.


# 25. Exterior2nd
Gen_Sum[25,]

# 1 missing value. 
# Replace the missing value with the most frequent value of Exterior1st, VinylSd:
cleaned$Exterior2nd[is.na(cleaned$Exterior2nd)] <- "VinylSd"
# Replace level CBlock with level CmentBd to avoid perfect multi-collinearity:
cleaned$Exterior2nd <- ifelse(cleaned$Exterior2nd == "CBlock", "CmentBd", cleaned$Exterior2nd)
cleaned$Exterior2nd <- as.factor(cleaned$Exterior2nd)
# Data distribution:
ggplot(cleaned, aes(Exterior2nd)) + geom_bar()
round(prop.table(table(cleaned$Exterior2nd))*100, 2)
# No level makes up >95% of the data.


# 26. MasVnrType
Gen_Sum[26,]
# 24 missing values  
# Replace missing values with the most frequent value, which is "None" in this case.
cleaned$MasVnrType[is.na(cleaned$MasVnrType)] <- "None"
cleaned$MasVnrType <- as.factor(cleaned$MasVnrType)

# Data distribution:
ggplot(cleaned, aes(MasVnrType)) + geom_bar()
round(prop.table(table(cleaned$MasVnrType))*100, 2)
# No level makes up >95% of the data.


# 27. MasVnrArea
Gen_Sum[27,]
# 23 missing values. 

# MasVnrArea and MasVnrType are related according to the description so we need 
# to view the fields with missing values and decide what to do.
cleaned %>% dplyr::select(MasVnrType, MasVnrArea) %>% filter(is.na(MasVnrArea))
# Missing values of MasVnrArea correspond to MasVnrType = None (i.e., no Masonry 
# veneer) so we need to eplace missing values with 0.
cleaned$MasVnrArea[is.na(cleaned$MasVnrArea)] <- 0

# Detect outliers
ggplot(cleaned, aes(MasVnrArea)) + geom_boxplot()
# Outliers seem to occur around 1250.
# Replace outliers with mean.
cleaned$MasVnrArea <- ifelse(cleaned$MasVnrArea > 1250, 
                                        mean(cleaned$MasVnrArea), 
                                        cleaned$MasVnrArea)

# Distribution plot + skewness:
ggplot(cleaned, aes(MasVnrArea)) + geom_density()
skewness(cleaned$MasVnrArea)
# From the plot and the skewness value (2.4), the plot is highly right skewed. 
# Transformation to reduce skewness:
ggplot(cleaned, aes(log(MasVnrArea +1)))+ geom_density()
skewness(log(cleaned$MasVnrArea+1))
# Skewness has been greatly improved to only 0.54, making the plot moderately skewed.
cleaned$MasVnrArea <- log(cleaned$MasVnrArea +1)


# 28. ExterQual
Gen_Sum[28,]
# No missing value. 
cleaned$ExterQual <- factor(cleaned$ExterQual, ordered = T,
                                      levels = c("Po", "Fa", "TA", "Gd", "Ex"))

# Data distribution:
ggplot(cleaned, aes(ExterQual)) + geom_bar()
round(prop.table(table(cleaned$ExterQual))*100, 2)
# No level makes up >95% of the data.


# 29. ExterCond
Gen_Sum[29,]
# No missing value. 
cleaned$ExterCond <- factor(cleaned$ExterCond, ordered = T,
                                  levels = c("Po", "Fa", "TA", "Gd", "Ex"))

# Data distribution:
ggplot(cleaned, aes(ExterCond)) + geom_bar()
round(prop.table(table(cleaned$ExterCond))*100, 2)
# No level makes up >95% of the data.


# 30. Foundation
Gen_Sum[30,]
# No missing value 
cleaned$Foundation <- as.factor(cleaned$Foundation)

# Data distribution:
ggplot(cleaned, aes(Foundation)) + geom_bar()
round(prop.table(table(cleaned$Foundation))*100, 2)
# No level makes up >95% of the data.


# 31. BsmtQual
Gen_Sum[31,]
# 81 NAs but these NAs are not missing values but they actually represent "No 
# Basement" according to data description.
# Replace NA with "No Basement":
cleaned$BsmtQual[is.na(cleaned$BsmtQual)] <- "No Basement"
cleaned$BsmtQual <- factor(cleaned$BsmtQual, ordered = T,
                                      levels = c("No Basement","Po", "Fa", "TA", "Gd", "Ex"))

# Data distribution:
ggplot(cleaned, aes(BsmtQual)) + geom_bar()
round(prop.table(table(cleaned$BsmtQual))*100, 2)
# No level makes up >95% of the data.


# 32. BsmtCond
Gen_Sum[32,]
# 81 NAs, but similar to variable #31, these NAs are not missing values but 
# they represent "No Basement" according to data description file.
# Replace NA with "No Basement":
cleaned$BsmtCond[is.na(cleaned$BsmtCond)] <- "No Basement"
# Replace level No Basement with level Po to avoid perfect multi-collinearity:
cleaned$BsmtCond <- ifelse(cleaned$BsmtCond == "No Basement", "Po", cleaned$BsmtCond)
cleaned$BsmtCond <- factor(cleaned$BsmtCond)

# Data distribution:
ggplot(cleaned, aes(BsmtCond)) + geom_bar()
round(prop.table(table(cleaned$BsmtCond))*100, 2)
# No level makes up >95% of the data.

# 33. BsmtExposure 
Gen_Sum[33,]
# There are 82 NAs but they represent "No Basement" and not actual missing values.
# Replace NA with "No Basement": 
cleaned$BsmtExposure[is.na(cleaned$BsmtExposure)] <- "No Basement"
cleaned$BsmtExposure <- factor(cleaned$BsmtExposure, ordered = T,
                                          levels = c("No Basement","No", "Mn", "Av", "Gd"))

# Data distribution:
ggplot(cleaned, aes(BsmtExposure)) + geom_bar()
round(prop.table(table(cleaned$BsmtExposure))*100, 2)
# No level makes up >95% of the data.


# 34. BsmtFinType1
Gen_Sum[34,]
# There are 79 NAs but they mean "No Basement" rather than missing values.
# Replace NA with "No Basement".
cleaned$BsmtFinType1[is.na(cleaned$BsmtFinType1)] <- "No Basement"
# Replace level Unf with level No Basement to avoid perfect multi-collinearty:
cleaned$BsmtFinType1 <- ifelse(cleaned$BsmtFinType1 == "Unf", "No Basement", cleaned$BsmtFinType1)
cleaned$BsmtFinType1 <- factor(cleaned$BsmtFinType1, ordered = T,
                                          levels = c("No Basement", "LwQ", "Rec", "BLQ", "GLQ", "ALQ"))

# Data distribution:
ggplot(cleaned, aes(BsmtFinType1)) + geom_bar()
round(prop.table(table(cleaned$BsmtFinType1))*100, 2)
# No level makes up >95% of the data.

# 35. BsmtFinSF1
Gen_Sum[35,]
cleaned[is.na(cleaned$BsmtFinSF1) == T, "BsmtFinType1"]
# 1 missing value
# Because the missing value corresponds with level No Basement for BsmtFinType1, 
# the finished Square Feet of the Basement is 0. Replace missing value with 0:
cleaned$BsmtFinSF1[is.na(cleaned$BsmtFinSF1)] <- 0

# Detect outliers:
ggplot(cleaned, aes(BsmtFinSF1)) + geom_boxplot()
# Outliers occur outside of 2000.
# Replace outliers and the missing value with mean.
cleaned$BsmtFinSF1 <- ifelse(cleaned$BsmtFinSF1 > 2000, 
                                        mean(cleaned$BsmtFinSF1),
                                        cleaned$BsmtFinSF1)

# Data dsitribution + skewness:
ggplot(data = cleaned, aes(BsmtFinSF1)) + geom_density()
skewness(cleaned$BsmtFinSF1)
# From the plot and the skewness value (0.76), the plot is moderately right skewed. 
# No transformation is needed.


# 36. BsmtFinType2
Gen_Sum[36,]
# 80 NAs but they mean "No Basement" not missing values.
# Replace NAs with "No Basement":
cleaned$BsmtFinType2[is.na(cleaned$BsmtFinType2)] <- "No Basement"
cleaned$BsmtFinType2 <- factor(cleaned$BsmtFinType2, ordered = T,
                                          levels = c("No Basement","Unf","LwQ","Rec","BLQ","ALQ","GLQ"))

# Data distribution:
ggplot(cleaned, aes(BsmtFinType2)) + geom_bar()
round(prop.table(table(cleaned$BsmtFinType2))*100, 2)
# No level makes up >95% of the data.


# 37. BsmtFinSF2
Gen_Sum[37,]
cleaned[is.na(cleaned$BsmtFinSF2) == T, "BsmtFinType2"]
# 1 missing value
# Because the missing value corresponds with level No Basement for BsmtFinType2, 
# the finished Square Feet of the Basement is 0. Replace missing value with 0:
cleaned$BsmtFinSF2[is.na(cleaned$BsmtFinSF2)] <- 0

# Detect outliers:
ggplot(cleaned, aes(BsmtFinSF2)) + geom_boxplot()
# Outliers seem to occur outside of 1250.
# Replace outliers with mean.
cleaned$BsmtFinSF2 <- ifelse(cleaned$BsmtFinSF2 > 1250, 
                                        mean(cleaned$BsmtFinSF2),
                                        cleaned$BsmtFinSF2)

# Data distribution + skewness:
ggplot(data = cleaned, aes(BsmtFinSF2)) + geom_density()
skewness(cleaned$BsmtFinSF2)
# Based on the plot and the skewness value (3.99), the distribution of data points 
# is highly right skewed. 
nrow(cleaned[cleaned$BsmtFinSF2 == 0,])
# Transformation is not meaningful for this variable because 2572 observations out
# of 2919 observations take on a value of 0. This variable should be removed.
cleaned <- subset(cleaned, select = -c(BsmtFinSF2))


# 38. BsmtUnfSF
Gen_Sum[38,]
cleaned[is.na(cleaned$BsmtUnfSF) == T, c("BsmtFinType1","BsmtFinType2")]
# 1 missing value
# Because the missing value corresponds with level No Basement for BsmtFinType 1&2, 
# the unfinished Square Feet of the Basement is 0. Replace missing value with 0:
cleaned$BsmtUnfSF[is.na(cleaned$BsmtUnfSF)] <- 0

# Detect outliers:
ggplot(cleaned, aes(BsmtUnfSF)) + geom_boxplot()
# Outliers seem to occur outside of 2250.
# Replace outliers with mean.
cleaned$BsmtUnfSF <- ifelse(cleaned$BsmtUnfSF > 2250, 
                             mean(cleaned$BsmtUnfSF),
                             cleaned$BsmtUnfSF)

# Data distribution + skewness: 
ggplot(data = cleaned, aes(BsmtUnfSF)) + geom_density()
skewness(cleaned$BsmtUnfSF)
# From the plot and the skewness value (0.91), the data is moderately right skewed. 
# Transformation: 
ggplot(data = cleaned, aes(sqrt(BsmtUnfSF))) + geom_density()
skewness(sqrt(cleaned$BsmtUnfSF))
# After transformation, the skewness reduced to -0.24
cleaned$BsmtUnfSF <- sqrt(cleaned$BsmtUnfSF)


# 39. TotalBsmtSF
Gen_Sum[39,]
cleaned[is.na(cleaned$TotalBsmtSF) == T, c("BsmtFinType1","BsmtFinType2")]
# 1 missing value
# Because the missing value corresponds with level No Basement for BsmtFinType2, 
# the total Square Feet of the Basement is 0. Replace missing value with 0:
cleaned$TotalBsmtSF[is.na(cleaned$TotalBsmtSF)] <- 0

# Detect outliers:
ggplot(cleaned, aes(TotalBsmtSF)) + geom_boxplot()
# Outliers seem to be outside of 4000.
# Replace outliers with mean.
cleaned$TotalBsmtSF <- ifelse(cleaned$TotalBsmtSF > 4000,
                                         mean(cleaned$TotalBsmtSF),
                                         cleaned$TotalBsmtSF)

# Data distribution + Skewness:
ggplot(data = cleaned, aes(TotalBsmtSF)) + geom_density()
skewness(cleaned$TotalBsmtSF)
#The value is 0.44, moderately skewed, so no transformation is needed. 


# 40. Heating:
Gen_Sum[40,]
# No missing value.
cleaned$Heating <- as.factor(cleaned$Heating)

# Data distribution:
ggplot(cleaned, aes(Heating)) + geom_bar()
round(prop.table(table(cleaned$Heating))*100, 2)
# Because over 98% of the data is made up of GasA heating, this variable will not
# provide much value in the model. It needs to be removed. 
cleaned <- subset(cleaned, select = -c(Heating))


# 41. HeatingQC
Gen_Sum[41,]
# No missing value.
cleaned$HeatingQC <- factor(cleaned$HeatingQC, ordered = T,
                                       levels = c("Po", "Fa", "TA", "Gd", "Ex"))
# Data distribution:
ggplot(cleaned, aes(HeatingQC)) + geom_bar()
round(prop.table(table(cleaned$HeatingQC))*100, 2)
# No level makes up >95% of the data.


# 42. CentralAir
Gen_Sum[42,]
# No missing value
cleaned$CentralAir <- as.factor(cleaned$CentralAir)

# Data distribution:
ggplot(cleaned, aes(CentralAir)) + geom_bar()
round(prop.table(table(cleaned$CentralAir))*100, 2)
# No level makes up >95% of the data.


# 43. Electrical
Gen_Sum[43,]
# 1 missing value
# Replace the NA with the most frequent value - SBrkr.
cleaned$Electrical[is.na(cleaned$Electrical)] <- "SBrkr"
cleaned$Electrical <- ifelse(cleaned$Electrical == "Mix", "FuseP", cleaned$Electrical)
cleaned$Electrical <- as.factor(cleaned$Electrical)

# Data distribution:
ggplot(cleaned, aes(Electrical)) + geom_bar()
round(prop.table(table(cleaned$Electrical))*100, 2)
# No level makes up >95% of the data.


# 44. X1stFlrSF
Gen_Sum[44,]
# No missing value

# Detect outliers:
ggplot(data = cleaned, aes(X1stFlrSF)) + geom_boxplot()
# Outliers seem to be around 2500.
# Replace outliers with mean.
cleaned$X1stFlrSF <- ifelse(cleaned$X1stFlrSF > 2500,
                                       mean(cleaned$X1stFlrSF),
                                  cleaned$X1stFlrSF)

# Data distribution + Skewness:
ggplot(data = cleaned, aes(x = X1stFlrSF)) + geom_density()
skewness(cleaned$X1stFlrSF)
#The value is 0.71, moderately skewed, so no transformation needed.


# 45. X2ndFlrSF
Gen_Sum[45,]
# No missing value

# Detect outliers:
ggplot(data = cleaned, aes(X2ndFlrSF)) + geom_boxplot()
# Outliers seem to occur outside of 1750.
# Replace outliers with mean.
cleaned$X2ndFlrSF <- ifelse(cleaned$X2ndFlrSF > 1750,
                                       mean(cleaned$X2ndFlrSF),
                                  cleaned$X2ndFlrSF)

# Data distribution + Skewness: 
ggplot(data = cleaned, aes(X2ndFlrSF)) + geom_density()
skewness(cleaned$X2ndFlrSF)
# The value is 0.80, moderately skewed, so transformation is not needed.


# 46. LowQualFinSF
Gen_Sum[46,]
# No missing value

# Detect outliers:
ggplot(data = cleaned, aes(LowQualFinSF)) + geom_boxplot()
# Outliers seem to occur outside of 600.
# Replace outliers with mean.
cleaned$LowQualFinSF <- ifelse(cleaned$LowQualFinSF > 600,
                            mean(cleaned$LowQualFinSF),
                            cleaned$LowQualFinSF)

# Data distribution + Skewness: 
ggplot(data = cleaned, aes(x = LowQualFinSF)) + geom_density()
skewness(cleaned$LowQualFinSF)
nrow(cleaned[cleaned$LowQualFinSF == 0,])
# The value is 10.73, highly right skewed.
# This is due to the fact that this variable contains too many cases with the 
# same value, which won't provide much useful information during data modeling.
# 2879 out of 2919 cases are 0 value.This variable should be removed:
cleaned <- subset(cleaned, select = -c(LowQualFinSF))


# 47. GrLivArea
Gen_Sum[47,]
# No missing value

# Detect outliers:
ggplot(data = cleaned, aes(GrLivArea)) + geom_boxplot()
# Outliers seem to occur outside of 4000.
# Replace outliers with mean.
cleaned$GrLivArea <- ifelse(cleaned$GrLivArea > 4000,
                                       mean(cleaned$GrLivArea),
                                  cleaned$GrLivArea)

# Data distribution + skewness:
ggplot(data = cleaned, aes(GrLivArea)) + geom_density()
skewness(cleaned$GrLivArea)
# The value is 0.87, moderately skewed.
# Transformation:
ggplot(data = cleaned, aes(log(GrLivArea))) + geom_density()
skewness(log(cleaned$GrLivArea))
# Skewness after transformation reduced to -0.06, almost symmetrical.
cleaned$GrLivArea <- log(cleaned$GrLivArea)


# 48. BsmtFullBath
Gen_Sum[48,]
cleaned[is.na(cleaned$BsmtFullBath) == T, c("BsmtFinType1","BsmtFinType2")]
# 2 missing value
# Because the missing value corresponds with level No Basement for BsmtFinType 1&2, 
# the number of Full Baths in Basement is 0. Replace missing value with 0:
cleaned$BsmtFullBath[is.na(cleaned$BsmtFullBath)] <- 0

# Detect outliers
ggplot(data = cleaned, aes(BsmtFullBath)) + geom_boxplot()
# No obvious outliers.
skewness(cleaned$BsmtFullBath)
#The value is 0.62, moderately skewed, so no transformation is needed.


# 49. BsmtHalfBath
Gen_Sum[49,]
cleaned[is.na(cleaned$BsmtHalfBath) == T, c("BsmtFinType1","BsmtFinType2")]
# 2 missing value
# Because the missing value corresponds with level No Basement for BsmtFinType 1&2, 
# the number of Half Baths in Basement is 0. Replace missing value with 0:
cleaned$BsmtHalfBath[is.na(cleaned$BsmtHalfBath)] <- 0

# Detect outliers:
ggplot(data = cleaned, aes(BsmtHalfBath)) + geom_boxplot()
# No obvious outliers.
skewness(cleaned$BsmtHalfBath)
# The value is 3.93, highly skewed. However, no tranformation will be performed
# because BsmtHalfBath will be combined with HalfBath to create a new variable.


# 50. FullBath
Gen_Sum[50,]
# No missing value

# Detect outliers:
ggplot(data = cleaned, aes(FullBath)) + geom_boxplot()
# No obvious outliers.
skewness(cleaned$FullBath)
#The value is 0.17, approximately symmetrical.


# 51. HalfBath
Gen_Sum[51,]
# No missing value

# Detect outliers
ggplot(data = cleaned, aes(HalfBath)) + geom_density()
# No obvious outliers.
skewness(cleaned$HalfBath)
# The value is 0.694, moderately skewed, so no transformation is needed.


# 52. BedroomAbvGr
Gen_Sum[52,]
# No missing value
cleaned$BedroomAbvGr <- as.numeric(cleaned$BedroomAbvGr)

# Detect outliers:
ggplot(cleaned, aes(BedroomAbvGr)) + geom_boxplot()
# No obvious outliers.
skewness(cleaned$BedroomAbvGr)
# The value is 0.33, slightly skewed. 


# 53. KitchenAbvGr
Gen_Sum[53,]
# No missing value

# Data distribution + skewness:
ggplot(data = cleaned, aes(KitchenAbvGr)) + geom_boxplot()
skewness(cleaned$KitchenAbvGr)
#The value is 4.3, highly skewed.
nrow(cleaned[cleaned$KitchenAbvGr == 1,])
# 2785 out of 2919 cases share the same value (i.e., 1), which won't provid much 
# useful insights in the modeling stage.
# KitchenAbvGr should be removed:
cleaned$KitchenAbvGr <- original$KitchenAbvGr
cleaned <- subset(cleaned, select = -c(KitchenAbvGr))


# 54. KitchenQual
Gen_Sum[54,]
# 1 missing value. This value is unknown despite the house has 1 Kitchen Above Grade.
# Replace missing value with the most frequent value, TA:
cleaned$KitchenQual[is.na(cleaned$KitchenQual)] <- "TA"
cleaned$KitchenQual <- factor(cleaned$KitchenQual, ordered = T,
                                        levels = c("Po", "Fa", "TA", "Gd", "Ex"))

# Data distribution:
ggplot(cleaned, aes(KitchenQual)) + geom_bar()
round(prop.table(table(cleaned$KitchenQual))*100, 2)
# No level makes up >95% of the data.


# 55. TotRmsAbvGrd
Gen_Sum[55,]
# No missing value
cleaned$TotRmsAbvGrd <- as.numeric(cleaned$TotRmsAbvGrd)

# Detect outliers:
ggplot(data = cleaned, aes(TotRmsAbvGrd)) + geom_boxplot()
# No obvious outliers.  
skewness(cleaned$TotRmsAbvGrd)
#The value is 0.76, moderately skewed, so no further transformation is needed.


# 56. Functional
Gen_Sum[56,]
# 2 missing values.
# Replace the missing values with Typical Functionality:
cleaned$Functional[is.na(cleaned$Functional)] <- "Typ"
cleaned$Functional <- factor(cleaned$Functional, ordered = T,
                                       levels = c("Sal", "Sev", "Maj2", "Maj1", "Mod","Min2","Min1","Typ"))

# Data distribution:
ggplot(cleaned, aes(Functional)) + geom_bar()
round(prop.table(table(cleaned$Functional))*100, 2)
# No level makes up >95% of the data.


# 57. Fireplaces
Gen_Sum[57,]
# No missing value
cleaned$Fireplaces <- as.numeric(cleaned$Fireplaces)

# Detect outliers:
ggplot(data = cleaned, aes(Fireplaces)) + geom_boxplot()
# No obvious outliers.
skewness(cleaned$Fireplaces)
#The value is 0.733, moderately skewed, no transformation is needed.


# 58. FireplaceQu
Gen_Sum[58,]
# There are 1420 NAs but they represent "No fireplace" and not actual missing values.
# Replace NAs with "No fireplace":
cleaned$FireplaceQu[is.na(cleaned$FireplaceQu)] <- "No fireplace"
cleaned$FireplaceQu <- factor(cleaned$FireplaceQu, ordered = T,
                                         levels = c("No fireplace", "Po", "Fa", "TA", "Gd", "Ex"))

# Data distribution:
ggplot(cleaned, aes(FireplaceQu)) + geom_bar()
round(prop.table(table(cleaned$FireplaceQu))*100, 2)
# No level makes up >95% of the data.


# 59. GarageType
Gen_Sum[59,]
# 157 NAs but they represent "No Garage" and not actual missing values.
# Replace NAs with "No Garage":
cleaned$GarageType[is.na(cleaned$GarageType)] <- "No Garage"
cleaned$GarageType <- as.factor(cleaned$GarageType)

# Data distribution:
ggplot(cleaned, aes(GarageType)) + geom_bar()
round(prop.table(table(cleaned$GarageType))*100, 2)
# No level makes up >95% of the data.


# 60. GarageYrBlt
Gen_Sum[60,]
# 159 missing values
# Replace missing values with mean:
cleaned$GarageYrBlt <- ifelse(is.na(cleaned$GarageYrBlt) == T,
                                  round(mean(cleaned$GarageYrBlt, na.rm = T),0),
                                  cleaned$GarageYrBlt)

# Create new variable: Age of the garage in each house since it was first built:
cleaned$GarageAge <- NA
for (i in 1:nrow(cleaned)) {
  cleaned$GarageAge[i] <- 2022 - cleaned$GarageYrBlt[i] + 1
  i <- i + 1
}
cleaned <- subset(cleaned, select = -c(GarageYrBlt))


# 61. GarageFinish
Gen_Sum[61,]
# 159 NAs but 157 of them represent "No Garage" and the other 2 are actual unknown values.
# Replace NAs with "No Garage" GarageType:
cleaned[is.na(cleaned$GarageFinish) & cleaned$GarageType == "No Garage", "GarageFinish"] <- "No Garage"
# Replace NAs with "Detchd" GarageType:
cleaned[is.na(cleaned$GarageFinish) & cleaned$GarageType == "Detchd", "GarageFinish"] <- "Unf"
cleaned$GarageFinish <- ifelse(cleaned$GarageFinish == "Unf","No Garage", cleaned$GarageFinish)
cleaned$GarageFinish <- factor(cleaned$GarageFinish, ordered = T,
                                          levels = c("No Garage", "RFn", "Fin"))

# Data distribution:
ggplot(cleaned, aes(GarageFinish)) + geom_bar()
round(prop.table(table(cleaned$GarageFinish))*100, 2)
# No level makes up >95% of the data.


# 62. GarageCars
Gen_Sum[62,]
cleaned[is.na(cleaned$GarageCars) == T, "GarageType"]
# 1 missing value with Garage Type as Detchd. 
# Replace missing value with the most frequent value, 2:
cleaned$GarageCars[is.na(cleaned$GarageCars)] <- 2
cleaned$GarageCars <- as.numeric(cleaned$GarageCars)

# Detect outliers:
ggplot(data = cleaned, aes(GarageCars)) + geom_bar()
# No obvious outliers
skewness(cleaned$GarageCars)
#The value is -0.22, approximately symmetrical.


# 63: GarageArea
Gen_Sum[63,]
cleaned[is.na(cleaned$GarageArea) == T, "GarageType"]
# 1 missing value with Garage Type as Detchd. 
# Replace missing value with the mean:
cleaned$GarageArea[is.na(cleaned$GarageArea)] <- mean(cleaned$GarageArea, na.rm = T)

# Detect outliers:
ggplot(data = cleaned, aes(GarageArea)) + geom_boxplot()
# Outliers seem to be around 1250.
# Replace outliers with mean.
cleaned$GarageArea <- ifelse(cleaned$GarageArea > 1250,
                                        mean(cleaned$GarageArea),
                                   cleaned$GarageArea)

# Data distribution + skewness:
ggplot(cleaned, aes(GarageArea)) + geom_density()
skewness(cleaned$GarageArea)
#The value is 0.114, approximately symmetrical.


# 64. GarageQual
Gen_Sum[64,]
# 159 NAs but 157 of them represent "No Garage" and the other 2 are actual unknown values.
# Replace NA with "No Garage" GarageType:
cleaned[is.na(cleaned$GarageQual) & cleaned$GarageType == "No Garage", "GarageQual"] <- "No Garage"
# Replace NA with "Detchd" GarageType:
cleaned[is.na(cleaned$GarageQual) & cleaned$GarageType == "Detchd", "GarageQual"] <- "TA"
cleaned$GarageQual <- factor(cleaned$GarageQual, ordered = T,
                                        levels = c("No Garage", "Po", "Fa", "TA", "Gd", "Ex"))

# Data distribution:
ggplot(cleaned, aes(GarageQual)) + geom_bar()
round(prop.table(table(cleaned$GarageQual))*100, 2)
# Level TA makes up almost 90% of the data for this variable so it should be removed.
cleaned <- subset(cleaned, select = -c(GarageQual))


# 65. GarageCond
Gen_Sum[65,]
# 159 NAs but 157 of them represent "No Garage" and the other 2 are actual unknown values.
# Replace NA with "No Garage" GarageType:
cleaned[is.na(cleaned$GarageCond) & cleaned$GarageType == "No Garage", "GarageCond"] <- "No Garage"
# Replace NA with "Detchd" GarageType:
cleaned[is.na(cleaned$GarageCond) & cleaned$GarageType == "Detchd", "GarageCond"] <- "TA"
# Replace level No Garage with TA to avoid perfect multi-collinearity:
cleaned$GarageCond <- ifelse(cleaned$GarageCond == "No Garage", "TA", cleaned$GarageCond)
cleaned$GarageCond <- factor(cleaned$GarageCond, ordered = T,
                                        levels = c("Po", "Fa", "TA", "Gd", "Ex"))

# Data distribution:
ggplot(cleaned, aes(GarageCond)) + geom_bar()
round(prop.table(table(cleaned$GarageCond))*100, 2)
# No level makes up >95% of the data.


# 66. PavedDrive
Gen_Sum[66,]
# No missing values.
cleaned$PavedDrive <- as.factor(cleaned$PavedDrive)

# Data distribution:
ggplot(cleaned, aes(PavedDrive)) + geom_bar()
round(prop.table(table(cleaned$PavedDrive))*100, 2)
# No level makes up >95% of the data.


# 67. WoodDeckSF
Gen_Sum[67,]
# No missing value.

# Detect outliers:
ggplot(cleaned, aes(WoodDeckSF)) + geom_boxplot()
# Outliers seem to occur outside of 750.
# Replace outliers with mean.
cleaned$WoodDeckSF <- ifelse(cleaned$WoodDeckSF > 750,
                                        mean(cleaned$WoodDeckSF),
                                   cleaned$WoodDeckSF)

# Data distribution + Skewness:
ggplot(data = cleaned, aes(WoodDeckSF)) + geom_density()
skewness(cleaned$WoodDeckSF)
# The value is 1.45, highly skewed.
# Transformation.
ggplot(data = cleaned, aes(log(WoodDeckSF+1))) + geom_density()
skewness(log(cleaned$WoodDeckSF +1))
# Skewness has been greatly improved (0.16).
cleaned$WoodDeckSF <- log(cleaned$WoodDeckSF + 1)


# 68. OpenPorchSF
Gen_Sum[68,]
# No missing value.

# Detect outliers:
ggplot(cleaned, aes(OpenPorchSF)) + geom_boxplot()
# Outliers seem to occur outside of 400.
# Replace outliers with mean.
cleaned$OpenPorchSF <- ifelse(cleaned$OpenPorchSF >= 400,
                                         mean(cleaned$OpenPorchSF),
                                    cleaned$OpenPorchSF)

# Data distribution + Skewness:
ggplot(data = cleaned, aes(OpenPorchSF)) + geom_density()
skewness(cleaned$OpenPorchSF)
# From the plot and the  skewness value (1.83), the data is highly skewed.
# As OpenPorchSF will be combined  with other variables to create a new variable,
# for now no transformation is needed.


# 69. EnclosedPorch
Gen_Sum[69,]
# No missing value 

# Detect outliers:
ggplot(data = cleaned, aes(EnclosedPorch)) + geom_boxplot()
# Outliers seem to be around 500.
# Replace outliers with mean.
cleaned$EnclosedPorch <- ifelse(cleaned$EnclosedPorch > 500,
                                           mean(cleaned$EnclosedPorch),
                                      cleaned$EnclosedPorch)

# Data distribution + Skewness:
ggplot(data = cleaned, aes(EnclosedPorch)) + geom_density()
skewness(cleaned$EnclosedPorch)
# From the plot and the  skewness value (2.91), the data is highly skewed.
# As EnclosedPorch will be combined  with other variables to create a new variable,
# for now no transformation is needed.


# 70. X3SsnPorch
Gen_Sum[70,]
# No missing value

# Detect outliers:
ggplot(cleaned, aes(X3SsnPorch)) + geom_boxplot()
# Outliers seem to be outside of 350.
# Replace outliers with mean.
cleaned$X3SsnPorch <- ifelse(cleaned$X3SsnPorch > 350,
                                        mean(cleaned$X3SsnPorch),
                                   cleaned$X3SsnPorch)

# Data distribution + Skewness:
ggplot(data = cleaned, aes(X3SsnPorch)) + geom_density()
skewness(cleaned$X3SsnPorch)
# From the plot and the  skewness value (9.27), the data is highly skewed.
# As X3SsnPorch will be combined  with other variables to create a new variable,
# for now no transformation is needed.

# 71. ScreenPorch
Gen_Sum[71,]
# No missing value.
cleaned$ScreenPorch <- as.numeric(cleaned$ScreenPorch)

# Detect outliers:
ggplot(cleaned, aes(ScreenPorch)) + geom_boxplot()
# No obvious outliers.

# Data distribution + Skewness:
ggplot(data = cleaned, aes(ScreenPorch)) + geom_density()
skewness(cleaned$ScreenPorch)
# From the plot and the  skewness value (3.94), the data is highly skewed.
# As ScreenPorch will be combined  with other variables to create a new variable,
# for now no transformation is needed.


# 72. PoolArea
Gen_Sum[72,]
# No missing value.

# Detect outliers:
ggplot(cleaned, aes(PoolArea)) + geom_boxplot()
# No obvious outliers.
nrow(cleaned[cleaned$PoolArea == 0,])
# However, 2903 out of 2919 case have the same value (i.e., 0), which
# won't provide much useful information in the modeling stage.PoolArea should be 
# removed. 
cleaned <- subset(cleaned, select = -c(PoolArea))


# 73. PoolQC
Gen_Sum[73,]
# 1453 NAs but they represent "No Pool" and not actual missing values.
# Replace NAs with "No Pool".
cleaned$PoolQC[is.na(cleaned$PoolQC)] <- "No Pool"
cleaned$PoolQC <- factor(cleaned$PoolQC, ordered = T,
                                    levels = c("No Pool", "Fa", "TA", "Gd", "Ex"))

# Data distribution:
ggplot(cleaned, aes(PoolQC)) + geom_bar()
round(prop.table(table(cleaned$PoolQC))*100, 2)
# Level No Pool makes up over 99% of the variable so it should be removed.
cleaned <- subset(cleaned, select = -c(PoolQC))


# 74: Fence
Gen_Sum[74,]
# 1179 NAs but they represent "No Fence" and not actual missing value.
# Replace NAs with "No Fence".
cleaned$Fence[is.na(cleaned$Fence)] <- "No Fence"
cleaned$Fence <- factor(cleaned$Fence, ordered = T,
                                   levels = c("No Fence", "MnWw", "GdWo", "MnPrv", "GdPrv"))

# Data distribution:
ggplot(cleaned, aes(Fence)) + geom_bar()
round(prop.table(table(cleaned$Fence))*100, 2)
# No level makes up >95% of the data.


# 75. MiscFeature
Gen_Sum[75,]
# 2814 NAs but they mean "None" and not actual missing values.
# Replace NAs with "None" to avoid confusion.
cleaned$MiscFeature[is.na(cleaned$MiscFeature)] <- "None"
cleaned$MiscFeature <- as.factor(cleaned$MiscFeature)
nrow(cleaned[cleaned$MiscFeature == "None",])
# MiscFeature should be removed because 2814 out of 2919 cases share the same value,
# which won't generate much useful information in data modeling.
cleaned <- subset(cleaned, select = -c(MiscFeature))


# 76. MiscVal
Gen_Sum[76,]
# No missing value

# Detect outliers
ggplot(cleaned, aes(MiscVal)) + geom_boxplot()
# Outliers occur outside of 5000.
# Replace outliers with mean.
cleaned$MiscVal <- ifelse(cleaned$MiscVal > 5000,
                                     mean(cleaned$MiscVal),
                                cleaned$MiscVal)

# Data distribution + Skewness:
ggplot(data = cleaned, aes(MiscVal)) + geom_density()
skewness(cleaned$MiscVal)
# The value is 10.487, highly skewed.
nrow(cleaned[cleaned$MiscVal == 0,])
# MiscVal has 2816 out of 2919 cases with the same value (i.e., 0), 
# which won't provide much useful information during the modeling stage.
# MiscVal should be removed:
cleaned <- subset(cleaned, select = -c(MiscVal))


# 77. MoSold
Gen_Sum[77,]
# No missing value 

# It would be more reasonable to explain the coefficient of MoSold
# when MoSold is a categorical variable.
cleaned$MoSold <- as.factor(cleaned$MoSold)

# Data distribution:
ggplot(cleaned, aes(MoSold)) + geom_bar()
round(prop.table(table(cleaned$MoSold))*100, 2)
# No level makes up >95% of the data.


# 78. YrSold
Gen_Sum[78,]
# No missing value 
# It would be more reasonable to explain the coefficient of YrSold
# when YrSold is a categorical variable.
cleaned$YrSold <- as.factor(cleaned$YrSold)

# Data distribution:
ggplot(cleaned, aes(YrSold)) + geom_bar()
round(prop.table(table(cleaned$YrSold))*100, 2)
# No level makes up >95% of the data.


# 79. SaleType
Gen_Sum[79,]
# 1 missing value 
# Replace missing value with most frequent value, WD:
cleaned$SaleType[is.na(cleaned$SaleType) == T] <- "WD"
cleaned$SaleType <- as.factor(cleaned$SaleType)

# Data distribution:
ggplot(cleaned, aes(SaleType)) + geom_bar()
round(prop.table(table(cleaned$SaleType))*100, 2)
# No level makes up >95% of the data.

# 80. SaleCondition
Gen_Sum[80,]
# No missing value.
cleaned$SaleCondition <- as.factor(cleaned$SaleCondition)

# Data distribution:
ggplot(cleaned, aes(SaleCondition)) + geom_bar()
round(prop.table(table(cleaned$SaleCondition))*100, 2)
# No level makes up >95% of the data.

# 81. SalePrice
sum(is.na(SalePrice))
# No missing value.
original_train$SalePrice <- as.numeric(SalePrice)

# Data distribution + skewness:
par(mfrow = c(1,1))
ggplot(original_train, aes(SalePrice)) + geom_density()
skewness(original_train$SalePrice)
# From the plot and the skewness value (1.88), the data is highly right skewed. 
# Transformation is necessary:
par(mfrow = c(1,1))
ggplot(original_train, aes(log10(SalePrice))) + geom_density()
skewness(log10(original_train$SalePrice))
# The skewness value after transformation is 0.121,approximately symmetrical.
original_train$SalePrice <- log10(original_train$SalePrice)


# D. Create new variables from existing variables. 
# 1. TotalSF_1st2nd
# This variable is the total Square Feet of a house and is the sum of 
# X1stFlrSF, and X2ndFlrSF. 
cleaned$TotalSF_1st2nd <- cleaned$X1stFlrSF + cleaned$X2ndFlrSF
ggplot(cleaned, aes(TotalSF_1st2nd)) + geom_density()
skewness(cleaned$TotalSF_1st2nd)
# From the plot and the skewness value (0.55), the distribution of data points
# is moderately right skewed. 
# Transformation:
ggplot(cleaned, aes(sqrt(TotalSF_1st2nd))) + geom_density()
skewness(sqrt(cleaned$TotalSF_1st2nd))
# After transformation, skewness reduced to 0.071, approximately symmetrical.
cleaned$TotalSF_1st2nd <- sqrt(cleaned$TotalSF_1st2nd)
cleaned <- subset(cleaned, select = -c(X1stFlrSF, X2ndFlrSF))


#2. TotalPorchSF
# This variable is the total Square Feet of porches in a house and is the sum of
# OpenPorchSF, EnclosedPorch, X3SsnPorch, and ScreenPorch. 
cleaned$TotalPorchSF <- cleaned$OpenPorchSF + cleaned$EnclosedPorch + cleaned$X3SsnPorch + cleaned$ScreenPorch
ggplot(cleaned, aes(TotalPorchSF)) + geom_density()
skewness(cleaned$TotalPorchSF)
# From the plot and the skewness value (1.53), the distribution of data points
# is moderately right skewed. 
# Transformation:
ggplot(cleaned, aes(sqrt(TotalPorchSF))) + geom_density()
skewness(sqrt(cleaned$TotalPorchSF))
# After transformation, skewness reduced to 0.24.
cleaned$TotalPorchSF <- sqrt(cleaned$TotalPorchSF)
cleaned <- subset(cleaned, select = -c(OpenPorchSF, EnclosedPorch, X3SsnPorch, ScreenPorch))


# 3. TotalFullBath
# This variable is the total number of full bathrooms in a house and is the sum of
# BsmtFullBath and FullBath. 
cleaned$TotalFullBath <- cleaned$BsmtFullBath + cleaned$FullBath 
ggplot(cleaned, aes(TotalFullBath)) + geom_bar()
skewness(cleaned$TotalFullBath)
# From the plot and the skewness value (0.45), the distribution of data points
# is slightly skewed. 
cleaned$TotalFullBath <- as.numeric(cleaned$TotalFullBath)
cleaned <- subset(cleaned, select = -c(BsmtFullBath, FullBath))


# 4. TotalHalfBath
# This variable is the total number of half bathrooms in a house and is the sum of
# BsmtHalfBath and HalfBath. 
cleaned$TotalHalfBath <- cleaned$BsmtHalfBath + cleaned$HalfBath 
ggplot(cleaned, aes(TotalHalfBath)) + geom_bar()
skewness(cleaned$TotalHalfBath)
# From the plot and the skewness value (0.80), the distribution of data points
# is moderately skewed. 
cleaned$TotalHalfBath <- as.numeric(cleaned$TotalHalfBath)
cleaned <- subset(cleaned, select = -c(BsmtHalfBath, HalfBath))


# E. Split data into the Training data set and the Test data set. 
cleaned <- subset(cleaned, select = -c(Id))
cleaned_train <- cleaned[1:1460,]
cleaned_train$SalePrice <- original_train$SalePrice

cleaned_test <- cleaned[1461:2919,]



# F. Develop models using Machine Learning methods. 

##### PLAIN VANILLA MODEL #####
# 1. Implement a linear model using Least-square regression:
model_plainvanilla <- lm(formula = SalePrice ~., data = cleaned_train)
summary(model_plainvanilla)

# 2. Predict the home prices for the Test set:
Test_plainvanilla_predict <- predict(model_plainvanilla, cleaned_test)
Test_plainvanilla_predict <- 10^Test_plainvanilla_predict
# Kaggle score for this prediction: 0.13205

# 3. Perform diagnostic tests on the plainvanilla model:
par(mfrow = c(2,3))
suppressWarnings(plot(model_plainvanilla, which = c(1:6)))
# (Warnings are suppressed for points with leverage = 1)

# Residual vs Fitted:
# The plot displays a linear relationship between the residuals and the fitted 
# values with the distribution of the points forming a straight line as the fitted
# value increases. The red line almost coincides with the dashed line, suggesting
# strong linearity.

# Normal Q-Q plot:
# The plot shows that the residuals are quite normally distributed with the 
# majority of the points lying on the dashed line. However, the beginning and the 
# end of the distribution shape formed by the points deviate from the dashed line,
# suggesting that the residuals are not entirely normally distributed. 

# Scale-Location Plot:
# This plot suggests some issues of multi-collinearity. Whilst the distribution 
# of the points are somewhat constant, the points appear to slightly narrow closer 
# to the red line as the fitted value increases. The points are sparse on the 
# left (where fitted value < 4.9) and the right (where 5.5 < fitted value) parts 
# of the plot, but heavily gather in the middle.

# Residuals vs Leverage Plot:
# From all 4 plots in general, it can be seen that there are obvious outliers 
# such as observations 633, 1325, 524, 1299, 326, 333. Points with high leverage 
# can be spotted in other plots such as the Cook's distance plot and the Cook's 
# dist vs Leverage plot, which highlight observations 524, 333, and 326 as 
# high leverage. However, there are no influential points. 


# 4. Apply prescriptions to further clean the data:
# Multi-collinearity issue:
vif(model_plainvanilla)
# Identify highly correlated pairs of numerical variables:
cleaned_num <- select_if(cleaned, is.numeric)
num_cor_table <- round(cor(cleaned_num[,], method = "pearson")*100, 2)

num_cor_melted <- melt(num_cor_table, na.rm = T)
num_cor_pairs <- as.data.frame(filter(num_cor_melted, 
                                        (value >= 60 & value < 100)  | (value <= -60 & value > -100)) %>%
                                   arrange(desc(value)))
num_cor_pairs <- num_cor_pairs[-c(seq(2,nrow(num_cor_pairs), 2)),]
# Correlation between all variables in num_cor_pairs and SalePrice:
cleaned_highcor <- cleaned_train[, c("TotalSF_1st2nd", "GrLivArea", "GarageArea",
                                       "GarageCars", "TotRmsAbvGrd", "GarageAge", 
                                       "HomeAge", "LotArea", "LotFrontage", 
                                       "BedroomAbvGr", "AgeSinceRemod", "SalePrice")]
num_cor_table2 <- round(cor(cleaned_highcor[,], method = "pearson")*100, 2)
num_cor_melted2 <- melt(num_cor_table2, na.rm = T)
filter(num_cor_melted2, Var1 == "SalePrice") %>% arrange(desc(value))

# With each highly correlated pairs, eliminate the variable with lower correlation
# to SalePrice:
cleaned <- subset(cleaned, select = -c(GrLivArea, GarageArea, LotFrontage, GarageAge,
                                       BedroomAbvGr, MasVnrArea))


##### PLAIN VANILLA 2 MODEL #####
# 1. Split the data into the Train and the Test data sets again:
cleaned_train <- cleaned[1:1460,]
cleaned_train$SalePrice <- original_train$SalePrice
cleaned_test <- cleaned[1461:2919,]

# 2. Run model plainvanilla2:
model_plainvanilla2 <- lm(formula = SalePrice ~., data = cleaned_train)
summary(model_plainvanilla2)
vif(model_plainvanilla2)

# 3. Predict the home prices for the Test set:
Test_plainvanilla2_predict <- predict(model_plainvanilla2, cleaned_test)
Test_plainvanilla2_predict <- 10^Test_plainvanilla2_predict
# Kaggle score for this prediction: 0.13757
# Because the score is higher, the model accuracy did not improve.



##### AIC FEATURE SELECTION MODEL #####
# 1. AIC Forward Selection
AIC_forward <- stepAIC(model_plainvanilla2, direction = "forward", k = 2, trace = 1)
summary(AIC_forward)

# Predict the home prices for the Test set:
Test_forAIC_predict <- predict(AIC_forward, cleaned_test)
Test_forAIC_predict <- 10^Test_forAIC_predict
# Kaggle score for this prediction: 0.13757

# 2. AIC Backward Selection
AIC_backward <- stepAIC(model_plainvanilla2, direction = "backward")
summary(AIC_backward)

# Predict the home prices for the Test set:
Test_backAIC_predict <- predict(AIC_backward, cleaned_test)
Test_backAIC_predict <- 10^Test_backAIC_predict
# Kaggle score for this prediction: 0.13231

# 2. AIC Hybrid Selection
AIC_hybrid <- stepAIC(model_plainvanilla2, direction = "both")
summary(AIC_hybrid)

# Predict the home prices for the Test set:
Test_hybAIC_predict <- predict(AIC_hybrid, cleaned_test)
Test_hybAIC_predict <- 10^Test_hybAIC_predict
# Kaggle score for this prediction: 0.13231


##### RIDGE & LASSO REGRESSION MODELS #####
# 1. Ridge Regression
# Create an x matrix and a y vector for both training and test data sets:
y_train_ridge <- cleaned_train$SalePrice
cleaned_train <- subset(cleaned_train, select = -c(SalePrice))
x_train_ridge <- model.matrix(~., data = cleaned_train)

x_test_ridge <- model.matrix(~., data = cleaned_test)
# Determine the best tuning parameter lambda through k-fold cross-validation:
set.seed(1)
cv_out_ridge <- cv.glmnet(x_train_ridge, y_train_ridge, alpha = 0, 
                          type.measure = "mse", nfolds = 50)
plot(cv_out_ridge)
bestlam_ridge <- cv_out_ridge$lambda.min
# Use the best lambda to run the ridge regression model on the training data set.
model_ridge <- glmnet(x_train_ridge, y_train_ridge, alpha = 0, 
                      standardize = TRUE, lambda = bestlam_ridge)
# Predict the home prices for the Test set:
Test_ridge_predict <- predict(model_ridge, newx = x_test_ridge)
Test_ridge_predict <- 10^Test_ridge_predict
# Kaggle score for this prediction: 0.12872


# 2. The Lasso
# Create an x matrix and a y vector for both training and test data sets:
x_train_lasso <- model.matrix(~., data = cleaned_train)
y_train_lasso <- original_train$SalePrice

x_test_lasso <- model.matrix(~., data = cleaned_test)
# Determine the best tuning parameter lambda through k-fold cross-validation:
set.seed(1)
cv_out_lasso <- cv.glmnet(x_train_lasso, y_train_lasso, alpha = 1, 
                          type.measure = "mse", nfolds = 50)
par(mfrow = c(1,1))
plot(cv_out_lasso)
bestlam_lasso <- cv_out_lasso$lambda.min
# Use the best lambda to run the LASSO model on the training data set:
model_lasso <- glmnet(x_train_lasso, y_train_lasso, alpha = 1, 
                      standardize = TRUE, lambda = bestlam_lasso)
model_lasso$beta

# Predict the home prices for the Test set:
Test_lasso_predict <- predict(model_lasso, newx = x_test_lasso)
Test_lasso_predict <- 10^Test_lasso_predict
# Kaggle score for this: 0.12406


##### REGRESSION TREES MODELS #####
# 1. Regression Trees with Bagging:
cleaned_train$SalePrice <- original_train$SalePrice
set.seed(1)
model_Tree_Bagging <- randomForest(SalePrice ~ ., data = cleaned_train,
                             ntrees = 500, mtry = 53, replace = TRUE,
                             importance = TRUE)
model_Tree_Bagging
plot(model_Tree_Bagging)

# Important predictors:
importance(model_Tree_Bagging)
varImpPlot(model_Tree_Bagging)

# Predict the home prices for the Test set:
Test_bagging_predict <- predict(model_Tree_Bagging, cleaned_test)
Test_bagging_predict <- 10^Test_bagging_predict
# Kaggle score for this prediction: 0.14920

# 2. Regression Trees with Random Forest:
set.seed(1)
model_Tree_RF <- randomForest(SalePrice ~ ., data = cleaned_train, 
                        ntrees = 500, mtry = 18, replace = TRUE,
                        importance = TRUE)
model_Tree_RF$cv.error
plot(model_Tree_RF)

# Important predictors:
importance(model_Tree_RF)
varImpPlot(model_Tree_RF)

# Predict the home prices for the Test set:
Test_RF_predict <- predict(model_Tree_RF, cleaned_test)
Test_RF_predict <- 10^Test_RF_predict
# Kaggle score for this prediction: 0.14528


# 3. Regression Trees with Boosting:
# Find the best number of trees and interaction.depth:
# (Note: Hi Dr. Zhu, the loop takes like years to run so running the best model 
# only takes less time.)
n_trees <- rep(0, 6)
min_cv_error <- rep(0, 6)

for(i in 1:6){
  set.seed(1)
  model_Tree_Boosting <- gbm(SalePrice ~., data = cleaned_train, distribution = "gaussian",
                       n.trees = 5000, interaction.depth = i, cv.folds = 10,
                       shrinkage = 0.01)
  n_trees[i] <- which.min(model_Tree_Boosting$cv.error)
  min_cv_error[i] <- model_Tree_Boosting$cv.error[which.min(model_Tree_Boosting$cv.error)]
}

which.min(min_cv_error)
n_trees[6]

# The best model with the best number of trees and interaction.depth:
model_Tree_Boosting <- gbm(SalePrice ~., data = cleaned_train, distribution = "gaussian",
                           n.trees = 1359, interaction.depth = 6, cv.folds = 10,
                           shrinkage = 0.01)
summary(model_Tree_Boosting)

# Predict the home prices for the Test set:
Test_boost_predict <- predict(model_Tree_Boosting, cleaned_test)
Test_boost_predict <- 10^Test_boost_predict
# Kaggle score for this prediction: 0.12647


