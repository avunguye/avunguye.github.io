# Goodwill Project
# Spring 2023

# Load relevant packages from user library

library(dplyr) 
library(tidyr)
library(ggplot2) 
library(onehot)  
library(MASS)
library(e1071) 
library(fable)
library(forcats)
library(caret)
library(car)
library(writexl)
library(glmnet)
library(gbm)
library(stringr)
library(readr)
library(ridgeline)
library(lubridate)

####################### Read the relevant data into R ##########################

# Read relevant data into R with individual tables
ProdSKU <- read.csv("ProductionSKUAgent.csv", header = F)

colnames(ProdSKU) <- c("PPSKUAgnt", "OrigProdDate", "ProdDate", "Month", "Year",
                       "ProdID", "LineNum", "EmpID", "StoreID", "GLAcct", "DeptID",
                       "Cate1", "Cate2", "SKU", "ProdAmt", "ProdValue", "PullAmt",
                       "ReconAmt", "SoldAmt", "DateLoaded", "DateUpdated")

ProdBin <- read.csv("ProductionBin.csv", header = F)
colnames(ProdBin) <- c("ProdID", "LineNum", "StoreID", "Bin", "Price")

Stores <- read.csv("Stores.csv", header = F)
colnames(Stores) <- c("StoreID", "StoreName", "Address")

Employee <- read.csv("Employee.csv", header = F)
colnames(Employee) <- c("EmpID", "FirstName", "LastName", "Add1", "Add2", "City",
                        "State", "Zip", "Phone", "Email", "UserID", "SecLevel",
                        "DisabledYN", "StoreID", "TS", "ModifiedTS", "LastModified")

ColorSchd <- read.csv("ColorSched.csv")
colnames(ColorSchd) <- c("SeqNum", "StartDate", "EndDate", "BinProd", "BinPull",
                         "BinDisc", "StoreID")


merge1 <- ProdSKU %>% left_join(ProdBin, by = c("ProdID" = "ProdID", "LineNum" = "LineNum",
                                                "StoreID" = "StoreID"))

merge2 <- merge1 %>% left_join(Stores, by = c("StoreID" = "StoreID"))
merge3 <- merge2 %>% left_join(Employee, by = c("EmpID" = "EmpID", "StoreID" = "StoreID"))

AllExceptColor_Data <- merge3
rm(merge1, merge2)


AllExceptColor_Data <- subset(AllExceptColor_Data, select = -c(GLAcct, DeptID, Cate1, 
                                                         Cate2, DateLoaded, DateUpdated, Add1, Add2, City, 
                                                         State, Zip, Phone, Email, TS, ModifiedTS, 
                                                         LastModified))
AllExceptColor_Data[AllExceptColor_Data == ""] <- NA
AllExceptColor_Data[AllExceptColor_Data == " "] <- NA
AllExceptColor_Data[is.null(AllExceptColor_Data)] <- NA


##### Rename PPSKUAgnt if necessary ############################################
# Code for Renaming the PPSKUAgnt Column in case there is an error exporting
# data from Microsoft SQL Server
# AllExceptColor_Data <- subset(AllExceptColor_Data, select = -PPSKUAgnt)

# AllExceptColor_Data <- AllExceptColor_Data %>%
#  rename(PPSKUAgnt = '...1')



##### Identify cut-off dates & Remove observations with incomplete 5-week period ######

# We need to remove observations that did not complete the 5-week color schedule
# as they will not have accurate end designation results (Pulled, Reconciled, &
# Sold). Keeping these observations would skew our calculation of Production 
# Percentage Error.

AllExceptColor_Data %>%
  filter(Year == "2023") %>%
  dplyr::select(OrigProdDate, ProdDate, ProdAmt, PullAmt, ReconAmt, SoldAmt) %>%
  arrange(desc(OrigProdDate))

max(AllExceptColor_Data$OrigProdDate)
# The latest date we have is 2023-01-22.

difftime((max(AllExceptColor_Data$OrigProdDate)), as.Date("2022-12-25"), units = "weeks")
# 4 weeks - Both days used in calculation occur on a Sunday.

difftime((max(AllExceptColor_Data$OrigProdDate)), as.Date("2022-12-18"), units = "weeks")
# 5 weeks - Both days used in calculation occur on a Sunday.

max(AllExceptColor_Data$OrigProdDate) - weeks(5)
# 12-18-2022 - I'll use 5 rather than 6 weeks since 1-22-2023 is the first day
# of a week (Sunday).

print(AllExceptColor_Data %>%
  filter(OrigProdDate < (max(AllExceptColor_Data$OrigProdDate) - weeks(5))) %>%
  dplyr::select(OrigProdDate, StoreName, ProdAmt, PullAmt, ReconAmt, SoldAmt) %>%
  group_by(OrigProdDate, StoreName) %>%
  summarise(sum(ProdAmt), sum(PullAmt), sum(ReconAmt), sum(SoldAmt), sum(PullAmt, ReconAmt, SoldAmt)) %>%
  arrange(desc(OrigProdDate)), n = 100)

AllExceptColor_Data <- AllExceptColor_Data %>%
  filter(OrigProdDate < (max(AllExceptColor_Data$OrigProdDate) - weeks(5)))

min(AllExceptColor_Data$OrigProdDate)

# difftime(max(AllExceptColor_Data$OrigProdDate), min(AllExceptColor_Data$OrigProdDate), units = #"weeks") # 2021-01-01

# ColorWeeks <- AllExceptColor_Data %>%
#   filter(OrigProdDate < (max(AllExceptColor_Data$OrigProdDate) - weeks(5))) %>%
#   dplyr::select(ProdDate, Bin) %>%
#   arrange(ProdDate) %>%
#   unique()

sample_n(AllExceptColor_Data, 15)

# write_xlsx(data.frame(sample_n(AllExceptColor_Data, 100000)),"C:\\Users\\adria\\OneDrive\\Documents\\Goodwill Consulting Project\\Full Goodwill2023 Data\\Sample for Percentage Error.xlsx")

## Begin Data Cleaning #########################################################

# Examine each individual column and perform general data cleaning steps

# For Categorical variables,
# 1. Identify and decide what to do with missing values.
# 2. Identify typos.
# 3. Check Whether a large percent (95%) of cases of a particular variable have the
#    same value.
#       If yes, should consider removing the variable as it won't provide much useful
#       information in the modeling stage.
# 4. Convert char into factors or ordered factors, according to the data description.

# For Numeric variables,
# 1. Is it more reasonable to treat certain int variables as factors?
#       If yes, do the conversion and perform cleaning by following the steps for 
#       categorical variables.
#       If no, move to the next step.
# 2. Convert int into numeric to avoid calculation errors.
# 3. Identify and decide what to do with missing values.
# 4. Check Whether a large percent (95%) of cases of a particular variable have the 
#    same value.
#       If yes, should consider removing the variable as it won't provide much
#       useful information in the modeling stage.
# 5. Check outliers. 
#       If obvious outliers exist, I replace outliers with mean.
# 6. Check skewness and decide what to do. 
#       I only perform log transformation for highly skewed variables.
#       No transformation performed on moderately or slightly skewed variables.

# Other things I also think about when examining and understanding each individual 
# variable.
# 1. Is it possible to combine several variables regarding the same aspect of homes
#    into a new variable? Need to reduce the number of variables because we have too 
#    many!
# 2. For a particular variable, can I intuitively tell that it may be highly related
#    to another variable or other variables? This will help us reduce the number of 
#    variables as much as we can before heading into the modeling stage.


# COLUMN 1: PPSKUAgnt #########################################################
class(AllExceptColor_Data$PPSKUAgnt)
AllExceptColor_Data$PPSKUAgnt <- as.factor(AllExceptColor_Data$PPSKUAgnt)
class(AllExceptColor_Data$PPSKUAgnt)

sum(is.na(AllExceptColor_Data$PPSKUAgnt))
# There are 0 missing values.

summary(factor(AllExceptColor_Data$PPSKUAgnt))
# Since PPSKUAgnt is an index, no further action is needed.

# COLUMN 2: OrigProdDate #######################################################

# DEFINITION: The date the item was physically produced.

class(AllExceptColor_Data$OrigProdDate)
AllExceptColor_Data$OrigProdDate <- as.Date(AllExceptColor_Data$OrigProdDate)
class(AllExceptColor_Data$OrigProdDate)

sum(is.na(AllExceptColor_Data$OrigProdDate))
# There are 0 missing values.

options(scipen = 999)
ggplot(data = AllExceptColor_Data, aes(OrigProdDate)) +
  geom_histogram() +
  scale_y_continuous(labels = scales::comma) 
# We see that prior to October 2022, the frequency of a date (tied to the number
# tag batches created on a particular date) never exceeded 50,000. However,
# after October 2022, date frequencies quickly shoot up to 250,000+. I believe
# this drastic change is associated with the implementation of the new 
# production model accross the GoodwillSA stores.

options(scipen = 999)
ggplot(data = AllExceptColor_Data, aes(OrigProdDate)) +
  geom_histogram() +
  scale_y_continuous(labels = scales::comma) +
  facet_wrap(~StoreName)

# COLUMN 3: ProdDate ###########################################################

# DEFINITION: The date the item was scheduled to be produced.

class(AllExceptColor_Data$ProdDate)
AllExceptColor_Data$ProdDate <- as.Date(AllExceptColor_Data$ProdDate)
class(AllExceptColor_Data$ProdDate)

sum(is.na(AllExceptColor_Data$ProdDate))
# There are 0 missing values.

options(scipen = 999)
ggplot(data = AllExceptColor_Data, aes(ProdDate)) +
  geom_histogram() +
  scale_y_continuous(labels = scales::comma)
# We see that prior to October 2022, the frequency of a date (tied to the number
# tag batches created on a particular date) never exceeded 50,000. However,
# after October 2022, date frequencies quickly shoot up to 250,000+. I believe
# this drastic change is associated with the implementation of the new 
# production model accross the GoodwillSA stores.

# Find the Duplicated Columns
duplicated_columns <- duplicated(as.list(AllExceptColor_Data))

# Show the Names of the Duplicated Columns
colnames(AllExceptColor_Data[duplicated_columns])
duplicated_columns
rm(duplicated_columns)

# I want to check if the values in OrigProdDate and ProdDate are identical. 
# According to the results, there are no duplicate columns; however, 
# OrigProdDate and ProdDate likely follow a very similar trend.

# # Remove the Duplicated Columns
# AllExceptColor_Data[!duplicated_columns]

# COLUMN 4: Month ##############################################################

# DEFINITION: The Month of Proddate

class(AllExceptColor_Data$Month)
AllExceptColor_Data$Month <- as.factor(AllExceptColor_Data$Month)
class(AllExceptColor_Data$Month)

sum(is.na(AllExceptColor_Data$Month))
# There are 0 missing values.

summary(factor(AllExceptColor_Data$Month))
# There are 0 typos. 

# There are 215,126 cases (18.27%) of 112 (December), 186,337 (15.83%) 11 
# (November) cases, and 91,061 (7.73%) 10 (October) cases.

# AllExceptColor_Data <- subset(AllExceptColor_Data, select = -Month)

# Month will likely be useful in observing trends related to seasonality, so I 
# will not remove this column.

# COLUMN 5: Year ###############################################################

# DEFINITION: The Year of Proddate

class(AllExceptColor_Data$Year)
AllExceptColor_Data$Year <- as.factor(AllExceptColor_Data$Year)
class(AllExceptColor_Data$Year)

sum(is.na(AllExceptColor_Data$Year))
# There are 0 missing values.

summary(factor(AllExceptColor_Data$Year))
# There are 0 typos. 

# There are 407,436 cases (34.61%) of 2021, 769,508 (65.382%) 2022
# cases, and 6 (0.00%) 2023 cases.

# Did GoodwillSA have a higher rate of production in 2022 in comparison to 2021
# or do we simply not have data for all of 2021? Let's check.
min(AllExceptColor_Data$OrigProdDate)
# Our earliest date is 2021-01-01, so it appears GoodwillSA truly did produce 
# more in 2022.

AllExceptColor_Data %>%
  filter(Year == "2023")
# While scheduled to be produced in 2023, these items were actually produced in 
# 2022.

# AllExceptColor_Data <- subset(AllExceptColor_Data, select = -Year)

# Year will likely be useful in observing trends related to seasonality, so I 
# will not remove this column.

# COLUMN 6: ProdID #############################################################

# DEFINITION: The system production ID for the batch - unique to StoreID

class(AllExceptColor_Data$ProdID)
AllExceptColor_Data$ProdID <- as.factor(AllExceptColor_Data$ProdID)
class(AllExceptColor_Data$ProdID)

sum(is.na(AllExceptColor_Data$ProdID))
# There are 0 missing values.

summary(factor(AllExceptColor_Data$ProdID))
# There are 0 typos. 

# I wonder why several batches tend to share the same number of items produced
# whereas a few batches have a seemingly unique number of items produced.
#     Example: 58650, 62064, and 105309 all have 216 observations.

# COLUMN 7: LineNum ############################################################

# DEFINITION: The line number of the [ProdID]

class(AllExceptColor_Data$LineNum)
AllExceptColor_Data$LineNum <- as.factor(AllExceptColor_Data$LineNum)
class(AllExceptColor_Data$LineNum)

sum(is.na(AllExceptColor_Data$LineNum))
# There are 0 missing values.

summary(factor(AllExceptColor_Data$LineNum))
# There are 0 typos. 

# COLUMN 8: EmpID ##############################################################

# DEFINITION: The unique identifier of the person who produced the item

class(AllExceptColor_Data$EmpID)
AllExceptColor_Data$EmpID <- as.factor(AllExceptColor_Data$EmpID)
class(AllExceptColor_Data$EmpID)

sum(is.na(AllExceptColor_Data$EmpID))
# There are 0 missing values.

summary(factor(AllExceptColor_Data$EmpID))
# There are 0 typos. Seeing how many observations are associated with a 
# particular EmpID highlights employees' productivity levels.

# COLUMN 9: StoreID ############################################################

# DEFINITION: The original Store ID where tag was produced

class(AllExceptColor_Data$StoreID)
AllExceptColor_Data$StoreID <- as.factor(AllExceptColor_Data$StoreID)
class(AllExceptColor_Data$StoreID)

sum(is.na(AllExceptColor_Data$StoreID))
# There are 0 missing values.

summary(factor(AllExceptColor_Data$StoreID))
# There are 0 typos. 

# This data seems to only have information for 24 of the 29 donation locations; 
# however, GoodwillSA only has 23 retail and 1 e-commerce store. The retail and 
# e-commerce locations adds up to 24 stores. Maybe the donation locations do not
# produce product?

# COLUMN 10: SKU ###############################################################

# DEFINITION: The Item

class(AllExceptColor_Data$SKU)
AllExceptColor_Data$SKU <- as.factor(AllExceptColor_Data$SKU)
class(AllExceptColor_Data$SKU)

sum(is.na(AllExceptColor_Data$SKU))
# There are 0 missing values.

summary(factor(AllExceptColor_Data$SKU))
# There are 0 typos. 

# We expected to see most tags simply categorizes as Wares. Most likely, only 
# tags produced within the last year from the Fredericksberg location will have
# more narrow SKUs (ex. Papers, Plastics, Metals, etc.). However, we are not
# seeing those types of SKU categories. Instead, beyond Wares, the only SKU 
# categories we have are bike, EASTER499, and EASTER699.

OutlierSKUs <- AllExceptColor_Data %>%
  filter(SKU != "Wares" & Year == "2021")
rm(OutlierSKUs)
  
PostModelSKUs <- AllExceptColor_Data %>%
    filter(SKU != "Wares" & OrigProdDate > "2022-06-01") %>%
    group_by(SKU)%>%
    summarise(length(SKU))
rm(PostModelSKUs)

AllExceptColor_Data$SKU <- ifelse(AllExceptColor_Data$SKU == "bike", "Wares",
                              ifelse(AllExceptColor_Data$SKU == "Bike Accessories", "Wares",
                              ifelse(AllExceptColor_Data$SKU == "Bikes", "Wares",
                              ifelse(AllExceptColor_Data$SKU == "EASTER499", "Wares",
                              ifelse(AllExceptColor_Data$SKU == "EASTER699", "Wares",
                              ifelse(AllExceptColor_Data$SKU == "EASTER999", "Wares",
                              ifelse(AllExceptColor_Data$SKU == "EasterBasket", "Wares",
                              ifelse(AllExceptColor_Data$SKU == "EASTEROPEN", "Wares",
                              ifelse(AllExceptColor_Data$SKU == "Stuffed Animals tote", "Wares",
                              ifelse(AllExceptColor_Data$SKU == "Wares blue bin", "Wares",       
                              ifelse(AllExceptColor_Data$SKU == "Bed/Bath", "Bed/Bath", 
                              ifelse(AllExceptColor_Data$SKU == "Cups/Glass", "Cups/Glass",
                              ifelse(AllExceptColor_Data$SKU == "Dishes", "Dishes",
                              ifelse(AllExceptColor_Data$SKU == "Games", "Games",
                              ifelse(AllExceptColor_Data$SKU == "Metal", "Metal",
                              ifelse(AllExceptColor_Data$SKU == "Office", "Office",
                              ifelse(AllExceptColor_Data$SKU == "Pictures/Frames", "Pictures/Frames",
                              ifelse(AllExceptColor_Data$SKU == "Plastic", "Plastic",
                              ifelse(AllExceptColor_Data$SKU == "Pots/Pans", "Pots/Pans",
                              ifelse(AllExceptColor_Data$SKU == "Seasonal", "Seasonal",
                              ifelse(AllExceptColor_Data$SKU == "Sports", "Sports",
                              ifelse(AllExceptColor_Data$SKU == "Toys", "Toys",
                              ifelse(AllExceptColor_Data$SKU == "Vases/Figurines", "Vases/Figurines",
                              ifelse(AllExceptColor_Data$SKU == "Wares", "Wares",
                              ifelse(AllExceptColor_Data$SKU == "Wicker", "Wicker",
                              ifelse(AllExceptColor_Data$SKU == "Wood", "Wood", "Wares"
                                     ))))))))))))))))))))))))))

summary(factor(AllExceptColor_Data$SKU))
# We took the outlier-esque Ware SKUs that existed pre-Model Implementation and
# consolidated those SKUs to simply be recategorizes as "Wares."

class(AllExceptColor_Data$SKU)
AllExceptColor_Data$SKU <- as.factor(AllExceptColor_Data$SKU)
class(AllExceptColor_Data$SKU)

# COLUMN 11: ProdAmt ########################################################

# DEFINITION: How many items were produced of this [ProdId], of this storied, of
# this line number

class(AllExceptColor_Data$ProdAmt)
AllExceptColor_Data$ProdAmt <- as.numeric(AllExceptColor_Data$ProdAmt)
class(AllExceptColor_Data$ProdAmt)

sum(is.na(AllExceptColor_Data$ProdAmt))
# There are 0 missing values.

AllExceptColor_Data %>%
  mutate(IntProdAmt = round(ProdAmt)) %>%
  dplyr::select(ProdAmt, IntProdAmt) %>%
  filter(ProdAmt != round(ProdAmt))
# There are 116 observations where ProdAmt contains decimal values when the 
# column should only contain integer values. For example, we might see 50.5, 
# 1.49, and 6.99 instead of 50, 1, and 7, respectively.

print(AllExceptColor_Data %>%
        dplyr::select(PPSKUAgnt, ProdDate, EmpID, StoreID, SKU, ProdAmt, ProdValue, PullAmt, ReconAmt, SoldAmt, Price) %>%
        filter(ProdAmt != round(ProdAmt)), n = 116)
       
print(AllExceptColor_Data %>%
  filter(ProdAmt != round(ProdAmt)) %>%
  dplyr::select(StoreID) %>%
  summarize(table(StoreID)), n = 24)
# These observations mostly occur during 1/2021, 9/2021, 10/2021, 1/2022, 2/2022,
# 3/2022, 6/2022, 7/2022, 8/2022, 9/2022, 10/2022, and 11/2022. In terms of store
# locations, many observations occur at 127, 150, 154, 188, etc.

AllExceptColor_Data <- AllExceptColor_Data %>%
  mutate(ProdAmt = round(ProdAmt))
# Anyway, we need to update ProdAmt so that the column's values are only 
# whole/integer values.

# Identify outliers.
# ggplot(AllExceptColor_Data, aes(ProdAmt)) + geom_density()
# ggplot(AllExceptColor_Data, aes(ProdAmt)) + geom_boxplot()

# Since it is difficult to see where the different quartiles occur on the 
# boxplot, I will look at quartile values to better identify outliers.

quantile(AllExceptColor_Data$ProdAmt, c(0.25, 0.50, 0.75))
# The quartiles are identified as follows: 25% (Q1) is 1, 50% (Q2 or Median) is 
# 2, and (Q3) 75% is 15.

# Interquartile Range (IQR) = Q3 - Q1
# 15 - 1 = 14

# Lower Bound = Q1 - 1.5 * IQR
# 1 - 1.5 * 14 = -20

# Upper Bound = Q3 + 1.5 * IQR 
# 15 + 1.5 * 14 = 36

sum(AllExceptColor_Data$ProdAmt < 0)
sum(AllExceptColor_Data$ProdAmt > 36)
sum(AllExceptColor_Data$ProdAmt > 1500)

ExtremeOutliers <- AllExceptColor_Data %>%
  # dplyr::select(ProdID, EmpID, OrigStoreID, ProdAmt, ProdValue, PullAmt, ReconAmt, SoldAmt, FirstName, LastName) %>%
  filter(ProdAmt > 1500)
# The extreme outliers of ProdAmt seem to occur above 1,500. All points 
# above the cut-off value 23.5 are technically considered outliers. We only have
# 6 extreme outliers that surpass 1,500 and 175,270 outliers that surpass the 
# cut-off value. I feel the extreme outliers might be manual entry errors; 
# however, I don't want to replace the extreme outliers with the mean as this 
# action would affect our accuracy analysis.
rm(ExtremeOutliers)

floor(mean(AllExceptColor_Data$ProdAmt))

# AllExceptColor_Data$ProdAmt[which(AllExceptColor_Data$ProdAmt >= 1500)] <- floor(mean(AllExceptColor_Data$ProdAmt))

# Skewness 
ggplot(AllExceptColor_Data, aes(ProdAmt)) + geom_histogram()
skewness(AllExceptColor_Data$ProdAmt)
# The value is 39.95308, highly skewed to the right (positively skewed). Since 
# the variable is highly skewed, I will perform a transformation later on after
# the creation of our accuracy measure.

# Try a Logarithm transformation. 

sum(AllExceptColor_Data$ProdAmt <= 0)
# As this variable does not contain 0 or negative values, I do not need to use 
# (value + 1) to ensure that the values are greater than 0.
skewness(log(AllExceptColor_Data$ProdAmt, 10))
# Skewness of 0.528516.

# Try an Exponential/Squared transformation.
skewness((AllExceptColor_Data$ProdAmt)^2)
# Skewness of 628.3079.

# Try a Square-Root transformation.
skewness(sqrt(AllExceptColor_Data$ProdAmt))
# Skewness of 1.595223.

# # The Log Transformation has the most improved skewness value.
# ggplot(AllExceptColor_Data, aes(log(ProdAmt, 10)))+ geom_histogram()
# AllExceptColor_Data$ProdAmt <- log(AllExceptColor_Data$ProdAmt, 10)

# COLUMN 12: ProdValue #########################################################

# DEFINITION: The value of all items that were produced in the [ProdAmt] field

class(AllExceptColor_Data$ProdValue)
AllExceptColor_Data$ProdValue <- as.numeric(AllExceptColor_Data$ProdValue)
class(AllExceptColor_Data$ProdValue)

sum(is.na(AllExceptColor_Data$ProdValue))
# There are 0 missing values.

# Identify outliers.
# ggplot(AllExceptColor_Data, aes(ProdValue)) + geom_density()
# ggplot(AllExceptColor_Data, aes(ProdValue)) + geom_boxplot()

# Since it is difficult to see where the different quartiles occur on the 
# boxplot, I will look at quartile values to better identify outliers.

quantile(AllExceptColor_Data$ProdValue, c(0.25, 0.50, 0.75))
# The quartiles are identified as follows: 25% (Q1) is 3.99, 50% (Q2 or Median)
# is 17.94, and (Q3) 75% is 54.50.

# Interquartile Range (IQR) = Q3 - Q1
# 54.50 - 3.99 = 50.51

# Lower Bound = Q1 - 1.5 * IQR
# 3.99 - 1.5 * 50.51 = -71.775

# Upper Bound = Q3 + 1.5 * IQR 
# 54.50 + 1.5 * 50.51 = 130.265

sum(AllExceptColor_Data$ProdValue < 0)
sum(AllExceptColor_Data$ProdValue > 130.265)
sum(AllExceptColor_Data$ProdValue > 20000)

ExtremeOutliers <- AllExceptColor_Data %>%
  # dplyr::select(ProdID, EmpID, OrigStoreID, ProdAmt, ProdValue, PullAmt, ReconAmt, SoldAmt, FirstName, LastName) %>%
  filter(ProdValue > 20000)

# The extreme outliers of ProdValue occur around and above 20,000. All points 
# above the cut-off value 93.76 are technically considered outliers. We have 11 
# extreme outliers that surpass 20,000 and 151,752 outliers that surpass the 
# cut-off value. I feel the extreme outliers might be manual entry errors; 
# however, I don't want to replace the extreme outliers with the mean as this 
# action would affect our accuracy analysis.
mean(AllExceptColor_Data$ProdValue)

# AllExceptColor_Data$ProdValue[which(AllExceptColor_Data$ProdValue > 20000)] <- mean(AllExceptColor_Data$ProdValue)

# Skewness 
ggplot(AllExceptColor_Data, aes(ProdValue)) + geom_histogram()
skewness(AllExceptColor_Data$ProdValue)
# The value is 184.353, highly skewed to the right (positively skewed). Since 
# the variable is highly skewed, I will perform a transformation.

# Try a Logarithm transformation. 

sum(AllExceptColor_Data$ProdValue <= 0)
# As this variable does contain 0 or negative values, I will need to use 
# (value + 1) to ensure that the values are greater than 0.
skewness(log(AllExceptColor_Data$ProdValue + 1, 10))
# Skewness of 0.2073817.

# Try an Exponential/Squared transformation.
skewness((AllExceptColor_Data$ProdValue)^2)
# Skewness of 586.2833.

# Try a Square-Root transformation.
skewness(sqrt(AllExceptColor_Data$ProdValue))
# Skewness of 3.007231.

# # The Log Transformation has the most improved skewness value.
# ggplot(AllExceptColor_Data, aes(log(ProdValue, 10)))+ geom_histogram()
# AllExceptColor_Data$ProdValue <- log(AllExceptColor_Data$ProdValue, 10)

# COLUMN 13: PullAmt ########################################################

# DEFINITION: The number of [ProdAmt] pulled. Item was not sold. Barcode 
# scanned and item marked as pulled and not sold.

class(AllExceptColor_Data$PullAmt)
AllExceptColor_Data$PullAmt <- as.numeric(AllExceptColor_Data$PullAmt)
class(AllExceptColor_Data$PullAmt)

sum(is.na(AllExceptColor_Data$PullAmt))
# There are 0 missing values.

# Identify outliers.
# ggplot(AllExceptColor_Data, aes(PullAmt)) + geom_density()
# ggplot(AllExceptColor_Data, aes(PullAmt)) + geom_boxplot()

# Since it is difficult to see where the different quartiles occur on the 
# boxplot, I will look at quartile values to better identify outliers.

quantile(AllExceptColor_Data$PullAmt, c(0.25, 0.50, 0.75))
# The quartiles are identified as follows: 25% (Q1) is 0, 50% (Q2 or Median)
# is 0, and (Q3) 75% is 0.

# Interquartile Range (IQR) = Q3 - Q1
# 0 - 0 = 0

# Lower Bound = Q1 - 1.5 * IQR
# 0 - 1.5 * 0 = 0

# Upper Bound = Q3 + 1.5 * IQR 
# 0 + 1.5 * 0 = 0

sum(AllExceptColor_Data$PullAmt < 0)
sum(AllExceptColor_Data$PullAmt == 0)
# 1,113,300 observations or 95.59% of the data.

sum(AllExceptColor_Data$PullAmt > 0)
# 63,650 observations or 5.40% of the data.

sum(AllExceptColor_Data$PullAmt > 100)
# 5 observations of the data.

ExtremeOutliers <- AllExceptColor_Data %>%
  # dplyr::select(ProdID, EmpID, OrigStoreID, ProdAmt, ProdValue, PullAmt, ReconAmt, SoldAmt, FirstName, LastName) %>%
  filter(PullAmt > 100)

# The extreme outliers of PullAmt occur above 100. All points above the 
# cut-off value 0 are technically considered outliers. We have 5 extreme 
# outliers that surpass 100 and 69,253 outliers that surpass the cut-off value. 
# I feel the extreme outliers might be manual entry errors; however, I don't 
# want to replace the extreme outliers with the mean as this action would affect
# our accuracy analysis.
floor(mean(AllExceptColor_Data$PullAmt))

rm(ExtremeOutliers)

# AllExceptColor_Data$PullAmt[which(AllExceptColor_Data$PullAmt > 50)] <- mean(AllExceptColor_Data$PullAmt)

# Skewness 
ggplot(AllExceptColor_Data, aes(PullAmt)) + geom_histogram()
skewness(AllExceptColor_Data$PullAmt)
# The value is 33.82452, highly skewed to the right (positively skewed). Since 
# the variable is highly skewed, I will perform a transformation.

# Try a Logarithm transformation. 

sum(AllExceptColor_Data$PullAmt <= 0)
# As this variable contains many 0 values, I will use (value + 1) to ensure that
# the values are greater than 0.
skewness(log(AllExceptColor_Data$PullAmt + 1, 10))
# Skewness of 6.05183.

# Try an Exponential/Squared transformation.
skewness((AllExceptColor_Data$PullAmt)^2)
# Skewness of 581.9428.

# Try a Square-Root transformation.
skewness(sqrt(AllExceptColor_Data$PullAmt))
# Skewness of 6.406063.

# # The Log Transformation has the most improved skewness value.
# ggplot(AllExceptColor_Data, aes(log(PullAmt + 1, 10)))+ geom_histogram()
# AllExceptColor_Data$PullAmt <- log(AllExceptColor_Data$PullAmt + 1, 10)

# COLUMN 14: ReconAmt #######################################################

# DEFINITION: The number of [ProdAmt] reconciled.  Produced in Error.  Barcode 
# scanned and item marked as "unproduced"  or production reconciled.

class(AllExceptColor_Data$ReconAmt)
AllExceptColor_Data$ReconAmt <- as.numeric(AllExceptColor_Data$ReconAmt)
class(AllExceptColor_Data$ReconAmt)

sum(is.na(AllExceptColor_Data$ReconAmt))
# There are 0 missing values.

# Identify outliers.
# ggplot(AllExceptColor_Data, aes(ReconAmt)) + geom_density()
# ggplot(AllExceptColor_Data, aes(ReconAmt)) + geom_boxplot()

# Since it is difficult to see where the different quartiles occur on the 
# boxplot, I will look at quartile values to better identify outliers.

quantile(AllExceptColor_Data$ReconAmt, c(0.25, 0.50, 0.75))
# The quartiles are identified as follows: 25% (Q1) is 0, 50% (Q2 or Median)
# is 0, and (Q3) 75% is 0.

# Interquartile Range (IQR) = Q3 - Q1
# 0 - 0 = 0

# Lower Bound = Q1 - 1.5 * IQR
# 0 - 1.5 * 0 = 0

# Upper Bound = Q3 + 1.5 * IQR 
# 0 + 1.5 * 0 = 0

sum(AllExceptColor_Data$ReconAmt < 0)
sum(AllExceptColor_Data$ReconAmt == 0)
# 1,112,465 observations or 94.52% of the data.

sum(AllExceptColor_Data$ReconAmt > 0)
# 64,485 observations or 5.47% of the data.

sum(AllExceptColor_Data$ReconAmt > 150)
# 7 observations of the data.

ExtremeOutliers <- AllExceptColor_Data %>%
  # dplyr::select(ProdID, EmpID, OrigStoreID, ProdAmt, ProdValue, PullAmt, ReconAmt, SoldAmt, FirstName, LastName) %>%
  filter(ReconAmt > 150)

# The extreme outliers of ReconAmt occur above 150. All points above the 
# cut-off value 0 are technically considered outliers. We have 7 extreme 
# outliers that surpass 150 and 64,869 outliers that surpass the cut-off value.
# I feel the extreme outliers might be manual entry errors; however, I don't 
# want to replace the extreme outliers with the mean as this action would affect
# our accuracy analysis.
floor(mean(AllExceptColor_Data$ReconAmt))

rm(ExtremeOutliers)

# AllExceptColor_Data$ReconAmt[which(AllExceptColor_Data$ReconAmt > 40)] <- mean(AllExceptColor_Data$ReconAmt)

# Skewness 
ggplot(AllExceptColor_Data, aes(ReconAmt)) + geom_histogram()
skewness(AllExceptColor_Data$ReconAmt)
# The value is 15.72153, highly skewed to the right (positively skewed). Since 
# the variable is highly skewed, I will perform a transformation.

# Try a Logarithm transformation. 

sum(AllExceptColor_Data$ReconAmt <= 0)
# As this variable contains many 0 values, I will use (value + 1) to ensure that
# the values are greater than 0.
skewness(log(AllExceptColor_Data$ReconAmt + 1, 10))
# Skewness of 5.244065.

# Try an Exponential/Squared transformation.
skewness((AllExceptColor_Data$ReconAmt)^2)
# Skewness of 209.2105.

# Try a Square-Root transformation.
skewness(sqrt(AllExceptColor_Data$ReconAmt))
# Skewness of 5.977516.

# # The Log Transformation has the most improved skewness value.
# ggplot(AllExceptColor_Data, aes(log(ReconAmt + 1, 10)))+ geom_histogram()
# AllExceptColor_Data$ReconAmt <- log(AllExceptColor_Data$ReconAmt + 1, 10)

# COLUMN 15: FirstName #########################################################
class(AllExceptColor_Data$FirstName)
AllExceptColor_Data$FirstName <- as.factor(AllExceptColor_Data$FirstName)
class(AllExceptColor_Data$FirstName)

sum(is.na(AllExceptColor_Data$FirstName))
# There are 62 missing values.

NAs <- AllExceptColor_Data %>%
  filter(is.na(FirstName))
rm(NAs)

IdentifyingEmployees <- AllExceptColor_Data %>%
  dplyr::select(EmpID, UserID, FirstName, LastName, StoreID, StoreName) %>%
  filter(EmpID == "1580000046" | EmpID == "1580000045")
rm(IdentifyingEmployees)

# There are no observations with EmpID 1580000046 or 1580000045 that provide us
# with the employees' missing first and last names. I will check to see if I can
# find the employees' names in the original Employee table.

Employee_Data <- read_csv("Employee.csv", col_names = FALSE)

Employee_Data <- Employee_Data %>%
  rename(EmpID = X1, FirstName = X2, LastName = X3, Addr1 = X4, Addr2 = X5, City = X6,
         State = X7, Zip = X8, Phone = X9, Email = X10, UserID = X11, SecLevel = X12,
         DisabledYn = X13, StoreID = X14, TimeStamp = X15, ModifiedTimeStamp = X16,
         LastModified = X17)

IdentifyingEmployees <- Employee_Data %>%
  filter(EmpID == "1580000045" | EmpID == "1580000046")

IdentifyingEmployees <- Employee_Data %>%
  filter(UserID == "300432" | UserID == "300492")
# Using both EmpID and UserID from the original Employee table to try and 
# identify these two employees' first and last names; however, even in the 
# original table these names are missing. Dr. Young suggested I impute FirstName
# with "Unnamed" and LastName with "Employee." Since there are two employees 
# missing both first and last name, I will have "Employee One" and "Employee Two"
# under LastName. I will include a disclaimer regarding these imputations in the
# presentation.
rm(IdentifyingEmployees)

AllExceptColor_Data$FirstName <- as.character(AllExceptColor_Data$FirstName)

AllExceptColor_Data$FirstName[which((AllExceptColor_Data$EmpID == "1580000045"))] <- "Unnamed"
AllExceptColor_Data$LastName[which((AllExceptColor_Data$EmpID == "1580000045"))] <- "Employee One"

AllExceptColor_Data$FirstName[which((AllExceptColor_Data$EmpID == "1580000046"))] <- "Unnamed"
AllExceptColor_Data$LastName[which((AllExceptColor_Data$EmpID == "1580000046"))] <- "Employee Two"

summary(factor(AllExceptColor_Data$FirstName))

unique(AllExceptColor_Data$FirstName)
length(unique(AllExceptColor_Data$FirstName))
# There are some names in all CAPS, so these names are categorized as a separate
# individual versus the names with proper formatting. Currently, there are 633
# unique first names.

AllExceptColor_Data$FirstName <- str_to_title(AllExceptColor_Data$FirstName)

unique(AllExceptColor_Data$FirstName)
length(unique(AllExceptColor_Data$FirstName))
# After formatting the first names using str_to_title, there are now only 564
# unique first names.

AllExceptColor_Data$FirstName[which((AllExceptColor_Data$FirstName == "Wwwhite"))] <- "WW White"

AllExceptColor_Data$FirstName <- as.factor(AllExceptColor_Data$FirstName)

# COLUMN 16: LastName #########################################################
class(AllExceptColor_Data$LastName)
AllExceptColor_Data$LastName <- as.factor(AllExceptColor_Data$LastName)
class(AllExceptColor_Data$LastName)

sum(is.na(AllExceptColor_Data$LastName))
# There are 199 missing values.

NAs <- AllExceptColor_Data %>%
  filter(is.na(LastName))
# The 199 NAs appear to be associated with an employee whose first name is 
# Gracie.
rm(NAs)

IdentifyingEmployees <- AllExceptColor_Data %>%
  dplyr::select(EmpID, UserID, FirstName, LastName, StoreID, StoreName) %>%
  filter(FirstName == "Gracie")
# There is an employee with the name Gracie Reyna; however, if this employee and
# the employee without a last name were the same person, they should 
# theoretically have the same UserID. Gracie Reyna has the UserID 302333 and
# the Gracie without a last name has the User ID 125. It is interesting that 
# both employees work at the same location, Blanco (StoreID 125). Actually, the
# UserID of the Gracie without a last name might be a manual entry error, using
# the StoreID instead of the UserID. Furthermore, the only employees with a 
# three digit User ID (ex. 125) are Donation Locations, substantiating the 
# possibility her User ID mistakenly contains the Store ID.

rm(IdentifyingEmployees)

# I will check to see if I can find the employees' names in the original 
# Employee table.

IdentifyingEmployees <- Employee_Data %>%
  filter(FirstName == "Gracie")

IdentifyingEmployees <- Employee_Data %>%
  filter(UserID == "302333" | UserID == "125")
# Using both EmpID and UserID from the original Employee table to try and 
# identify the employee's missing last name; however, even in the 
# original table her last name is missing.

rm(IdentifyingEmployees)
rm(Employee_Data)

AllExceptColor_Data$LastName <- as.character(AllExceptColor_Data$LastName)

AllExceptColor_Data$LastName[which((AllExceptColor_Data$FirstName == "Gracie" & is.na(AllExceptColor_Data$LastName)))] <- "Unknown"

summary(factor(AllExceptColor_Data$LastName))
AllExceptColor_Data$LastName <- as.factor(AllExceptColor_Data$LastName)

unique(AllExceptColor_Data$LastName)
length(unique(AllExceptColor_Data$LastName))
# There are some names in all CAPS, so these names are categorized as a separate
# individual versus the names with proper formatting. Currently, there are 633
# unique first names.

AllExceptColor_Data$LastName <- str_to_title(AllExceptColor_Data$LastName)

summary(factor(AllExceptColor_Data$LastName))
unique(AllExceptColor_Data$LastName)
length(unique(AllExceptColor_Data$LastName))
# After formatting the last names using str_to_title, there are now only 579
# unique last names.

IdentifyingEmployees <- AllExceptColor_Data %>%
  dplyr::select(EmpID, UserID, FirstName, LastName, StoreID, StoreName) %>%
  filter(str_detect(LastName, '^D')) %>%
  unique()
rm(IdentifyingEmployees)

StoreNAs <- AllExceptColor_Data %>%
  dplyr::select(StoreID, StoreName, Address) %>%
  filter(StoreID == "112")
rm(StoreNAs)

AllExceptColor_Data %>%
  dplyr::select(FirstName, LastName, UserID, StoreID, StoreName) %>%
  filter(LastName == "Donations") %>%
  unique()

# COLUMN 17: UserID #########################################################
class(AllExceptColor_Data$UserID)
AllExceptColor_Data$UserID <- as.factor(AllExceptColor_Data$UserID)
class(AllExceptColor_Data$UserID)

sum(is.na(AllExceptColor_Data$UserID))
# There are 0 missing values.

summary(factor(AllExceptColor_Data$UserID))
# Since UserID is an index, no further action is needed.

# COLUMN 18: SecLevel #########################################################
class(AllExceptColor_Data$SecLevel)
AllExceptColor_Data$SecLevel <- as.factor(AllExceptColor_Data$SecLevel)
class(AllExceptColor_Data$SecLevel)

sum(is.na(AllExceptColor_Data$SecLevel))
# There are 705 missing values.

summary(factor(AllExceptColor_Data$SecLevel))
# There are no typos.

# We mainly expected to see security levels of 0 (No Security), 50 (Cashier), 
# and 70+ (Manager). We also see levels of 10, 20, 50, 71, 72, 73, 90, and 99.
# We will bin Security Level in groups of 0, 10 to 25, 50, and 70 & Above.

AllExceptColor_Data$SecLevel <- as.character(AllExceptColor_Data$SecLevel)

AllExceptColor_Data$SecLevel[is.na(AllExceptColor_Data$SecLevel)] <- "0"

AllExceptColor_Data$SecLevel[AllExceptColor_Data$SecLevel == "90" | 
                            AllExceptColor_Data$SecLevel == "99" | AllExceptColor_Data$SecLevel == "73" |
                            AllExceptColor_Data$SecLevel == "71" | AllExceptColor_Data$SecLevel == "72" |
                            AllExceptColor_Data$SecLevel == "70"] <- "70 and Above"

AllExceptColor_Data$SecLevel[AllExceptColor_Data$SecLevel == "10" | AllExceptColor_Data$SecLevel == "20" | 
                            AllExceptColor_Data$SecLevel == "25"] <- "10 to 25"

sum(is.na(AllExceptColor_Data$SecLevel))
# There are no longer any missing values.

summary(factor(AllExceptColor_Data$SecLevel))
# The categories are properly formatted.

AllExceptColor_Data$SecLevel <- as.factor(AllExceptColor_Data$SecLevel)
levels(AllExceptColor_Data$SecLevel)

# COLUMN 19: DisabledYN #########################################################
class(AllExceptColor_Data$DisabledYN)
AllExceptColor_Data$DisabledYN <- as.factor(AllExceptColor_Data$DisabledYN)
class(AllExceptColor_Data$DisabledYN)

sum(is.na(AllExceptColor_Data$DisabledYN))
# There are 705 missing values. The observations missing 705 values for SecLevel
# might be the same observations missing 705 DisabledYN values. We imputed the 
# SecLevel NAs with 0 (No Security). Marjorie informed us that when someone is
# is designated as 0 (No Security), the corresponding DisabledYN tends to be 1 
# as Disabled. So, we will replace the NAs of DisabledYN with 1.

summary(factor(AllExceptColor_Data$DisabledYN))
# There are no typos.

AllExceptColor_Data$DisabledYN[is.na(AllExceptColor_Data$DisabledYN)] <- "1"

sum(is.na(AllExceptColor_Data$DisabledYN))

levels(factor(AllExceptColor_Data$DisabledYN))
# No unusual categories.

# COLUMN 20: StoreID #########################################################
class(AllExceptColor_Data$StoreID)
AllExceptColor_Data$StoreID <- as.factor(AllExceptColor_Data$StoreID)
class(AllExceptColor_Data$StoreID)

sum(is.na(AllExceptColor_Data$StoreID))
# There are 0 missing values.

summary(factor(AllExceptColor_Data$StoreID))
# There are no typos. While StoreID 1154 seems odd, this is a proper StoreID 
# that represents GoodwillSA's Central Processing.

# Since StoreID is an ID, no further action is needed.

# COLUMN 21: StoreName #########################################################
class(AllExceptColor_Data$StoreName)
AllExceptColor_Data$StoreName <- as.factor(AllExceptColor_Data$StoreName)
class(AllExceptColor_Data$StoreName)

sum(is.na(AllExceptColor_Data$StoreName))
# There are 5,016 missing values.

length(AllExceptColor_Data$StoreName[which(AllExceptColor_Data$StoreID == "112")])
# There are 5,016 values associated with StoreID 112, Crosstowne.

print(AllExceptColor_Data %>%
  filter(is.na(StoreName)), n = 100)

print(AllExceptColor_Data %>%
  dplyr::select(StoreID, StoreName) %>%
  filter(is.na(StoreName)), n = 5016)
# Confirms that all the NAs of StoreName are associated with StoreID 112.

# Since Store 112 missing its StoreName, I decided to impute the proper
# corresponding StoreName: Crosstowne.

AllExceptColor_Data$StoreName <- as.character(AllExceptColor_Data$StoreName)
# We must convert StoreName to character to add the new level Crosstowne.

AllExceptColor_Data$StoreName[is.na(AllExceptColor_Data$StoreName) & AllExceptColor_Data$StoreID == "112"] <- "Crosstowne"

summary(factor(AllExceptColor_Data$StoreName))
# Issue is resolved with the new level Crosstowne.

AllExceptColor_Data$StoreName <- as.factor(AllExceptColor_Data$StoreName)

# COLUMN 22: Address #########################################################
class(AllExceptColor_Data$Address)
AllExceptColor_Data$Address <- as.factor(AllExceptColor_Data$Address)
class(AllExceptColor_Data$Address)

sum(is.na(AllExceptColor_Data$Address))
# There are 5,016 missing values. These missing values are likely also associated
# with StoreID 112 (Crosstowne) as there is the exact same number of missing 
# values.

print(AllExceptColor_Data %>%
        filter(is.na(Address)), n = 100)

print(AllExceptColor_Data %>%
        dplyr::select(StoreID, Address) %>%
        filter(is.na(Address)), n = 5016)
# Confirms that all the NAs of Address are associated with StoreID 112.

# Since Store 112 missing its Address, I decided to impute the proper
# corresponding Address: 4861 W Commerce St, San Antonio, TX 78237.

AllExceptColor_Data$Address <- as.character(AllExceptColor_Data$Address)
# We must convert Address to character to add the new level Crosstowne.

AllExceptColor_Data$Address[is.na(AllExceptColor_Data$Address) & AllExceptColor_Data$StoreID == "112"] <- "4861 W Commerce St, San Antonio, TX 78237"

summary(factor(AllExceptColor_Data$Address))
# Issue is resolved with the new level 4861 W Commerce St, San Antonio, TX 78237.

AllExceptColor_Data$Address <- as.factor(AllExceptColor_Data$Address)

# COLUMN 23: Bin #########################################################
class(AllExceptColor_Data$Bin)
AllExceptColor_Data$Bin <- as.factor(AllExceptColor_Data$Bin)
class(AllExceptColor_Data$Bin)

sum(is.na(AllExceptColor_Data$Bin))
# There are 37,283 missing values.

summary(factor(AllExceptColor_Data$Bin))
# There are no typos. 

print(AllExceptColor_Data %>%
  dplyr::select(OrigProdDate, ProdDate, StoreID, StoreName, Bin) %>%
  filter(Bin == "No Color"))

print(AllExceptColor_Data %>%
  filter(Bin == "No Color") %>%
  dplyr::select(OrigProdDate, ProdDate) %>%
  unique(), n = 175)
# We could use the ColorSchedule table to identify the Color Bin produced on
# the observations' production date. We would think that same process could be
# applied to the observations categorized as No Color, but as we can see those
# observations have a distinct No Color categorization rather than a Bin Color.
# Beyond production date, there must be another reason behind the category No
# Color. So, we will likely replace the NAs with No Color.

AllExceptColor_Data$Bin[is.na(AllExceptColor_Data$Bin)] <- "No Color"

print(AllExceptColor_Data %>%
  filter(Bin == "No Color") %>%
  group_by(StoreID, StoreName) %>%
  summarise(n = length(Bin)) %>%
  arrange(desc(n)), n = 23)
# Interestingly, the first store to undergo the new production model, 
# Fredericksburg, is the store with one of the most observations/batches (3,004)
# designated as No Color. Other stores with a high number of No Color batches 
# are DeZavala (3,578) and Potranco (2,516).

levels(factor(AllExceptColor_Data$Bin))
# No unusual categories.

# COLUMN 24: Price #########################################################
class(AllExceptColor_Data$Price)
AllExceptColor_Data$Price <- as.numeric(AllExceptColor_Data$Price)
class(AllExceptColor_Data$Price)

sum(is.na(AllExceptColor_Data$Price))
# Originally, here are 37,180 missing values -> now only 1 after removing last
# 5 weeks

# AllExceptColor_Data$Price <- ifelse(is.na(AllExceptColor_Data$Price),
#                                  round(AllExceptColor_Data$ProdValue/AllExceptColor_Data$ProdAmt, 2),
#                                  AllExceptColor_Data$Price)

# Originally, we were going to replace the Price NAs with quotient of 
# ProdValue/ProdAmt as ideally the two are supposed to be the same; however,
# since we are going to create an accuracy measure for Price, PricePercError, it
# would make more sense for us to impute the NAs with 0s. When we eventually 
# calculate PricePercError with the NAs replaced with 0s, we will be able to see
# that the two sources for viewing a item's price presented different results. 
# If we imputed PricePercError with the quotient, PricePercError would tell us
# that the two sources provided the same results/values and that is not true.

AllExceptColor_Data$Price[is.na(AllExceptColor_Data$Price)] <- 0

# Identify outliers.
# ggplot(AllExceptColor_Data, aes(Price)) + geom_density()
# ggplot(AllExceptColor_Data, aes(Price)) + geom_boxplot()

# Since it is difficult to see where the different quartiles occur on the 
# boxplot, I will look at quartile values to better identify outliers.

quantile(AllExceptColor_Data$Price, c(0.25, 0.50, 0.75))
# The quartiles are identified as follows: 25% (Q1) is 2.49, 50% (Q2 or Median)
# is 3.99, and (Q3) 75% is 6.99.

# Interquartile Range (IQR) = Q3 - Q1
# 6.99 - 2.49 = 4.5

# Lower Bound = Q1 - 1.5 * IQR
# 2.49 - 1.5 * 4.5 = -4.26

# Upper Bound = Q3 + 1.5 * IQR 
# 5.99 + 1.5 * 4.5 = 12.74

sum(AllExceptColor_Data$Price < 0)
sum(AllExceptColor_Data$Price == 0)
# 37,192 observations or 2.34% of the data. -> Dropped all the way to 13

sum(AllExceptColor_Data$Price > 11.24)
# 153458 observations or 10.61% of the data.

sum(AllExceptColor_Data$Price > 10000)
# 3 observations of the data.

ExtremeOutliers <- AllExceptColor_Data %>%
  # dplyr::select(ProdID, EmpID, FirstName, LastName, OrigStoreID, ProdAmt, ProdValue, PullAmt, ReconAmt, SoldAmt, Price) %>%
  filter(Price > 10000)
rm(ExtremeOutliers)

# Skewness 
ggplot(AllExceptColor_Data, aes(Price)) + geom_histogram()
skewness(AllExceptColor_Data$Price)
# The value is 356.6998, extremely skewed to the right (positively skewed). 
# Since the variable is extremely skewed, I will perform a transformation.

# Try a Logarithm transformation. 

sum(AllExceptColor_Data$Price <= 0)
# As this variable contains many 0 values, I will use (value + 1) to ensure that
# the values are greater than 0.
skewness(log(AllExceptColor_Data$Price + 1, 10))
# Skewness of 0.966153.

# Try an Exponential/Squared transformation.
skewness((AllExceptColor_Data$Price)^2)
# Skewness of 724.89.

# Try a Square-Root transformation.
skewness(sqrt(AllExceptColor_Data$Price))
# Skewness of 7.990732.

# # The Log Transformation has the most improved skewness value.
# ggplot(AllExceptColor_Data, aes(log(Price + 1, 10)))+ geom_histogram()
# AllExceptColor_Data$Price <- log(AllExceptColor_Data$Price + 1, 10)

# Creating Variables ###########################################################
### NEW VAR 1-2: OrigProdMonth & OrigProdYear Variables ########################

# Since our current Year and Month variables reference ProdDate (when an item 
# was scheduled to be produced) rather than OrigProdDate (when an item was 
# actually produced), our model results could be misleading using Year and 
# Month. Consequently, I will create two new variables that instead reference
# OrigProdDate to produce more accurate model results.

AllExceptColor_Data$OrigProdMonth <- month(ymd(AllExceptColor_Data$OrigProdDate))
AllExceptColor_Data$OrigProdYear <- year(ymd(AllExceptColor_Data$OrigProdDate))

AllExceptColor_Data$OrigProdMonth <- as.factor(AllExceptColor_Data$OrigProdMonth)
AllExceptColor_Data$OrigProdYear <- as.factor(AllExceptColor_Data$OrigProdYear)


print(AllExceptColor_Data %>%
        filter(Year == 2023 & ProdDate != OrigProdDate) %>%
        dplyr::select(OrigProdDate, ProdDate, Month, Year, OrigProdMonth, OrigProdYear), n = 172)
# 172 observations whose ProdDate's Year is 2023 and ProdDate is different from 
# OrigProdDate.

print(AllExceptColor_Data %>%
        filter(ProdDate != OrigProdDate & Month != OrigProdMonth) %>%
        dplyr::select(OrigProdDate, ProdDate, Month, Year, OrigProdMonth, OrigProdYear), n = 4902)
# 1,263 observations whose ProdDate is different from OrigProdDate and the 
# Months of ProdDate and OrigProdDate are different, implying ProdDate and 
# OrigProdDate are moderately off from one another.

print(AllExceptColor_Data %>%
        filter(ProdDate != OrigProdDate & Year != OrigProdYear) %>%
        dplyr::select(OrigProdDate, ProdDate, Month, Year, OrigProdMonth, OrigProdYear), n = 70)
# 70 observations whose ProdDate is different from OrigProdDate and the Years of 
# ProdDate and OrigProd are different, implying ProdDate and OrigProdDate are 
# extremely off from one another.

## NEW VAR 3: Production Percentage Error ######################################
AllExceptColor_Data <- AllExceptColor_Data %>%
  mutate(ProdPercError = round(abs(((PullAmt + ReconAmt + SoldAmt) - ProdAmt) / ProdAmt) * 100, digits = 2))

class(AllExceptColor_Data$ProdPercError)

sum(is.na(AllExceptColor_Data$ProdPercError))
# There are 0 missing values.

# Identify outliers.
# ggplot(AllExceptColor_Data, aes(ProdPercError)) + geom_density()
# ggplot(AllExceptColor_Data, aes(ProdPercError)) + geom_boxplot()

# Since it is difficult to see where the different quartiles occur on the 
# boxplot, I will look at quartile values to better identify outliers.

quantile(AllExceptColor_Data$ProdPercError, c(0.25, 0.50, 0.75))
# The quartiles are identified as follows: 25% (Q1) is 0, 50% (Q2 or Median)
# is 10, and (Q3) 75% is 40.

# Interquartile Range (IQR) = Q3 - Q1
# 40 - 0 = 40

# Lower Bound = Q1 - 1.5 * IQR
# 0 - 1.5 * 40 = -60

# Upper Bound = Q3 + 1.5 * IQR 
# 40 + 1.5 * 40 = 100

sum(AllExceptColor_Data$ProdPercError < 0)
sum(AllExceptColor_Data$ProdPercError == 0)
# 548,046 observations or 46.56% of the data.

sum(AllExceptColor_Data$ProdPercError > 0)
# 628,904 observations or 53.43% of the data.

sum(AllExceptColor_Data$ProdPercError > 100)
# 90 observations of the data.

# Skewness 
ggplot(AllExceptColor_Data, aes(ProdPercError)) + geom_histogram()
skewness(AllExceptColor_Data$ProdPercError)
# The value is 2.760486, skewed to the right (positively skewed). Since 
# the variable is skewed, I will perform a transformation.

# Try a Logarithm transformation. 

sum(AllExceptColor_Data$ProdPercError <= 0)
# As this variable contains many 0 values, I will use (value + 1) to ensure that
# the values are greater than 0.
skewness(log(AllExceptColor_Data$ProdPercError + 1, 10))
# Skewness of 0.1316224.

# Try an Exponential/Squared transformation.
skewness((AllExceptColor_Data$ProdPercError)^2)
# Skewness of 974.5515.

# Try a Square-Root transformation.
skewness(sqrt(AllExceptColor_Data$ProdPercError))
# Skewness of 0.539593.

# # The Log Transformation has the most improved skewness value.
# ggplot(AllExceptColor_Data, aes(log(ProdPercError + 1, 10)))+ geom_histogram()
# AllExceptColor_Data$ProdPercError <- log(AllExceptColor_Data$ProdPercError + 1, 10)

### NEW VAR 4: Calculate Price Percentage Error ################################
AllExceptColor_Data <- AllExceptColor_Data %>%
  mutate(PricePercError = round(abs((Price - (ProdValue / ProdAmt)) / (ProdValue / ProdAmt)) * 100, digits = 2))

class(AllExceptColor_Data$PricePercError)

sum(is.na(AllExceptColor_Data$PricePercError))
# There are 12 missing values.

AllExceptColor_Data %>%
  dplyr::select(ProdAmt, ProdValue, Price, ProdPercError, PricePercError) %>%
  filter(is.na(PricePercError))
# There are 12 observations with 0 for Price and ProdValue.

AllExceptColor_Data$PricePercError[AllExceptColor_Data$ProdValue == 0 & AllExceptColor_Data$Price == 0] <- 0

sum(is.na(AllExceptColor_Data$PricePercError)) 
# There are no longer any missing values.

AllExceptColor_Data %>%
  dplyr::select(ProdAmt, ProdValue, Price, ProdPercError, PricePercError) %>%
  filter(is.na(PricePercError))

# Our percentage error equation is prone to generating NAs with (ProdValue / ProdAmt)
# being in the overall denominator. If the result of (ProdValue / ProdAmt) is
# 0, the numerator will be divided by a 0, resulting in an undefined value.

# Identify outliers.
# ggplot(AllExceptColor_Data, aes(PricePercError)) + geom_density()
# ggplot(AllExceptColor_Data, aes(PricePercError)) + geom_boxplot()

print(AllExceptColor_Data %>%
  dplyr::select(ProdAmt, ProdValue, Price, ProdPercError, PricePercError) %>%
  unique() %>%
  arrange(desc(PricePercError)), n = 52)

AllExceptColor_Data %>%
  filter(PricePercError == 100)

# Since it is difficult to see where the different quartiles occur on the 
# boxplot, I will look at quartile values to better identify outliers.

quantile(AllExceptColor_Data$PricePercError, c(0.25, 0.50, 0.75))
# The quartiles are identified as follows: 25% (Q1) is 0, 50% (Q2 or Median)
# is 0, and (Q3) 75% is 0.

# Interquartile Range (IQR) = Q3 - Q1
# 0 - 0 = 0

# Lower Bound = Q1 - 1.5 * IQR
# 0 - 1.5 * 0 = -0

# Upper Bound = Q3 + 1.5 * IQR 
# 0 + 1.5 * 0 = 0

# Skewness 
ggplot(AllExceptColor_Data, aes(PricePercError)) + geom_histogram()
skewness(AllExceptColor_Data$PricePercError)
# The value is 527.2122, skewed to the right (positively skewed). Since 
# the variable is skewed, I will perform a transformation.

# Try a Logarithm transformation. 

sum(AllExceptColor_Data$PricePercError <= 0)
# As this variable contains many 0 values, I will use (value + 1) to ensure that
# the values are greater than 0.
skewness(log(AllExceptColor_Data$PricePercError + 1, 10))
# Skewness of 217.1476.

# Try an Exponential/Squared transformation.
skewness((AllExceptColor_Data$PricePercError)^2)
# Skewness of 998.9987.

# Try a Square-Root transformation.
skewness(sqrt(AllExceptColor_Data$PricePercError))
# Skewness of 253.4307.

# # The Log Transformation has the most improved skewness value.
# ggplot(AllExceptColor_Data, aes(log(ProdPercError + 1, 10)))+ geom_histogram()
# AllExceptColor_Data$ProdPercError <- log(AllExceptColor_Data$ProdPercError + 1, 10)

## NEW VAR 5: Pre/Post Implementation Variable ##################################

# firstDate OrgStoreid StoreName
# 
# 6/23/2022 154 Austin Hwy
# 6/19/2022 150 Bitters
# 7/12/2022 125 Blanco
# 6/26/2022 146 Blanco North
# 8/3/2022 159 Bulverde
# 7/1/2022 188 Cibolo
# 6/18/2022 128 Commerce
# 6/24/2022 157 Culebra
# 8/17/2022 152 DeZavala
# 7/14/2022 160 Evans
# 6/18/2022 120 Fredericksburg
# 9/19/2022 158 Gateway
# 9/25/2022 155 Goliad
# 9/7/2022 186 Kerrville
# 12/13/2022 156 Laredo
# 6/20/2022 127 Marbach
# 7/12/2022 151 New Braunfels
# 7/13/2022 183 Potranco
# 6/18/2022 126 Seguin
# 7/10/2022 153 South Park
# 6/18/2022 129 Summit
# 11/8/2022 184 WW White

AllExceptColor_Data <- AllExceptColor_Data %>%
  mutate(PrePostModel = ifelse(AllExceptColor_Data$StoreID == "154" & AllExceptColor_Data$OrigProdDate < as.Date("2022-6-23"), "Pre", 
  ifelse(AllExceptColor_Data$StoreID == "150" & AllExceptColor_Data$OrigProdDate < as.Date("2022-6-19"), "Pre", 
  ifelse(AllExceptColor_Data$StoreID == "125" & AllExceptColor_Data$OrigProdDate < as.Date("2022-7-12"), "Pre", 
  ifelse(AllExceptColor_Data$StoreID == "146" & AllExceptColor_Data$OrigProdDate  < as.Date("2022-6-26"), "Pre", 
  ifelse(AllExceptColor_Data$StoreID == "159" & AllExceptColor_Data$OrigProdDate < as.Date("2022-8-3"), "Pre", 
  ifelse(AllExceptColor_Data$StoreID == "188" & AllExceptColor_Data$OrigProdDate < as.Date("2022-7-1"), "Pre", 
  ifelse(AllExceptColor_Data$StoreID == "128" & AllExceptColor_Data$OrigProdDate < as.Date("2022-6-18"), "Pre", 
  ifelse(AllExceptColor_Data$StoreID == "157" & AllExceptColor_Data$OrigProdDate < as.Date("2022-6-24"), "Pre", 
  ifelse(AllExceptColor_Data$StoreID == "152" & AllExceptColor_Data$OrigProdDate < as.Date("2022-8-17"), "Pre", 
  ifelse(AllExceptColor_Data$StoreID == "160" & AllExceptColor_Data$OrigProdDate < as.Date("2022-7-14"), "Pre", 
  ifelse(AllExceptColor_Data$StoreID == "120" & AllExceptColor_Data$OrigProdDate < as.Date("2022-6-18"), "Pre", 
  ifelse(AllExceptColor_Data$StoreID == "158" & AllExceptColor_Data$OrigProdDate < as.Date("2022-9-19"), "Pre", 
  ifelse(AllExceptColor_Data$StoreID == "155" & AllExceptColor_Data$OrigProdDate < as.Date("2022-9-25"), "Pre", 
  ifelse(AllExceptColor_Data$StoreID == "186" & AllExceptColor_Data$OrigProdDate < as.Date("2022-9-7"), "Pre", 
  ifelse(AllExceptColor_Data$StoreID == "156" & AllExceptColor_Data$OrigProdDate < as.Date("2022-12-13"), "Pre", 
  ifelse(AllExceptColor_Data$StoreID == "127" & AllExceptColor_Data$OrigProdDate < as.Date("2022-6-20"), "Pre", 
  ifelse(AllExceptColor_Data$StoreID == "151" & AllExceptColor_Data$OrigProdDate < as.Date("2022-7-12"), "Pre", 
  ifelse(AllExceptColor_Data$StoreID == "183" & AllExceptColor_Data$OrigProdDate < as.Date("2022-7-13"), "Pre", 
  ifelse(AllExceptColor_Data$StoreID == "126" & AllExceptColor_Data$OrigProdDate < as.Date("2022-6-18"), "Pre", 
  ifelse(AllExceptColor_Data$StoreID == "153" & AllExceptColor_Data$OrigProdDate < as.Date("2022-7-10"), "Pre", 
  ifelse(AllExceptColor_Data$StoreID == "129" & AllExceptColor_Data$OrigProdDate < as.Date("2022-6-18"), "Pre", 
  ifelse(AllExceptColor_Data$StoreID == "184" & AllExceptColor_Data$OrigProdDate < as.Date("2022-11-8"), "Pre", 
         "Post")))))))))))))))))))))))

AllExceptColor_Data$PrePostModel <- as.factor(AllExceptColor_Data$PrePostModel)                   

sum(is.na(AllExceptColor_Data$PrePostModel))
# There are 0 missing values.

summary(factor(AllExceptColor_Data$PrePostModel))
# There are no typos. 


## NEW VAR 6: Perfect Batch Variable ##################################
AllExceptColor_Data$PerfectBatch <- NA
AllExceptColor_Data$PerfectBatch <- ifelse(AllExceptColor_Data$ProdPercError == 0,
                                           1, 0)

## NEW VAR 6: Percentage Sold Variable ##################################
AllExceptColor_Data$PercSold <- NA
AllExceptColor_Data$PercSold <- round(AllExceptColor_Data$SoldAmt/AllExceptColor_Data$ProdAmt*100, 2)



######## Checking for Multicollinearity before Model Building ##################
Numeric_AllExceptColor_Data <- AllExceptColor_Data %>%
  select_if(is.numeric)

library(ggcorrplot)
model.matrix(~ 0+., data = Numeric_AllExceptColor_Data) %>% 
  cor(use = "pairwise.complete.obs") %>% 
  ggcorrplot(show.diag = F, type="lower", lab=TRUE, lab_size=2)
# Instead of PricePercError, will pull out examples where price differed.

rm(Numeric_AllExceptColor_Data)

options(scipen = 999)
ggplot(data = AllExceptColor_Data, aes(OrigProdDate)) +
  geom_histogram() +
  scale_y_continuous(labels = scales::comma) +
  facet_wrap(~StoreName)



### Relevel Variables of Interest ################################################

# The main variables of interest are StoreID, SKU, SecLevel, and PrePostModel.

# StoreID 112 
summary(factor(AllExceptColor_Data$StoreID))
AllExceptColor_Data$StoreID  <- relevel(AllExceptColor_Data$StoreID, "120")

# SKU
summary(factor(AllExceptColor_Data$SKU))
AllExceptColor_Data$SKU  <- relevel(AllExceptColor_Data$SKU, "Wares")

# SecLevel
summary(factor(AllExceptColor_Data$SecLevel))
AllExceptColor_Data$SecLevel  <- relevel(AllExceptColor_Data$SecLevel, "0")

# PrePostModel
summary(factor(AllExceptColor_Data$PrePostModel))
AllExceptColor_Data$PrePostModel  <- relevel(AllExceptColor_Data$PrePostModel, "Pre")

# PerfectBatches
AllExceptColor_Data <- AllExceptColor_Data %>%
  mutate(PerfectBatch = ifelse(ProdPercError == 0, "Yes", "No"))

write.csv(AllExceptColor_Data, "C:\\Users\\adria\\OneDrive\\Documents\\Goodwill Consulting Project\\Full Goodwill2023 Data\\Goodwill Cleaned Data.csv", row.names = FALSE)

# Multiple Linear Regression Model ################################################

# Below include several things you need to do at the beginning of the modeling stage.

# 1. Remember to remove the columns used to create ProductionPercError, 
# PricePercError, and PrePostModel to avoid redundancy in the models. 

# 2. Remember to remove the columns that have been justified for removal 
# when examining each individual variable.

# 3. Remember to record which variables have been transformed to avoid 
# repeated transformation during diagnostic tests.

# Begin Diagnostic Tests #######################################################

# First, I will split the combined data back into a training set and a test set
# (50/50 split). (Might consider 70/30)


# Plain Vanilla Prod Model
Model_PlainVanillaProd <- lm(ProdPercError ~ ProdAmt + OrigProdMonth + OrigProdYear + StoreID + SKU + SecLevel + PrePostModel, data = Cleaned_Train)

Model_PlainVanillaProd <- lm(ProdPercError ~ ProdAmt + OrigProdMonth + OrigProdYear + StoreID + SKU + SecLevel + PrePostModel, data = AllExceptColor_Data)

summary(Model_PlainVanillaProd)

# Plain Vanilla Prod Predictions
PlainProd_Test_Pred <- predict(Model_PlainVanillaProd, Cleaned_Test)

# MSE = 1146.077
mean((PlainProd_Test_Pred - Cleaned_Test$ProdPercError)^2)

#RMSE = 33.85376
sqrt(mean((PlainProd_Test_Pred - Cleaned_Test$ProdPercError)^2))

# Plain Vanilla Price Model
Model_PlainVanillaPrice <- lm(PricePercError ~ ProdAmt + OrigProdMonth + OrigProdYear + StoreID + SKU + SecLevel + PrePostModel, data = Cleaned_Train)

Model_PlainVanillaPrice <- lm(PricePercError ~ ProdAmt + OrigProdMonth + OrigProdYear + StoreID + SKU + SecLevel + PrePostModel, data = AllExceptColor_Data)

summary(Model_PlainVanillaPrice)

# Plain Vanilla Price Predictions
PlainPrice_Test_Pred <- predict(Model_PlainVanillaPrice, Cleaned_Test)

# MSE = 0.009155955
mean((PlainPrice_Test_Pred - Cleaned_Test$PricePercError)^2)

#RMSE = 0.09568675
sqrt(mean((PlainPrice_Test_Pred - Cleaned_Test$PricePercError)^2))

# Multicollinearity ############################################################

# Assess multicollinearity issue and perform prescriptions if needed.

# Variables with vif scores between 4-5 are considered moderate to high will 
# require further investigation. Additionally, high vif scores beyond will also 
# require further investigation.

## ProdPercError
car::vif(Model_PlainVanillaProd)
 
# None of the variables have a vif score that exceeds 2.5, so no variables will 
# require further investigation in terms of multicollinearity. 
# (Using GVIF^91/(2*Df)) as the vif score.)

## PricePercError
car::vif(Model_PlainVanillaPrice)

# None of the variables have a vif score that exceeds 2.5, so no variables will 
# require further investigation in terms of multicollinearity. 
# (Using GVIF^91/(2*Df)) as the vif score.)

# Non-Linearity ################################################################

# Assess non-linearity issue and perform prescriptions if needed.

## ProdPercError
residualPlot(Model_PlainVanillaProd)

# par(mfrow = c(2, 2))
# plot(Model_PlainVanillaProd, which = c(1, 2, 3, 4), id.n = 5)
# outlierTest(Model_PlainVanillaProd)

# Non-linearity of the response-predictor relationships: Looking at the residual plot, 
# we see a fan-shape (quadratic-shape) displayed, so we will need to transform the 
# predictors.

# None of the predictors have already received transformations; however, I 
# tested various transformations on the predictors. So, I will use the 
# most-improved transformations on the predictors.



## PricePercError
residualPlot(Model_PlainVanillaPrice)
summary(Model_PlainVanillaPrice)

# Non-linearity of the response-predictor relationships: Looking at the residual
# plot, we see a fan-shape (quadratic-shape) displayed for one of the residual 
# groupings, so we will need to transform the predictors.

# None of the predictors have already received transformations; however, I 
# tested various transformations on the predictors. So, I will use the 
# most-improved transformations on the predictors.



# Heterscedasticity ############################################################

# Assess heterscedasticity issue (i.e., non-constant variance of error terms) 
# and perform prescriptions if needed.


###### Data Aggregation for Visualizations Building ############################

# Metric that would be beneficial... revenue yield comparing actual prodvalue to
# theoretical prodvalue... identifying the best price classification

# Dashboard Tableau - Percentage Error by Overalll (Pre/Post), Store (Pre/Post), 
# by Employee (Pre/Post), by SKU (Pre/Post)
# Rate/speed of productino (# of items produced per day, week, month)

DayXStore_Summary <- AllExceptColor_Data %>%
        dplyr::select(OrigProdDate, StoreID, StoreName, ProdAmt, PullAmt, ReconAmt, SoldAmt) %>%
        group_by(OrigProdDate, StoreID, StoreName) %>%
        summarise(ProdAmt = sum(ProdAmt), PullAmt = sum(PullAmt), ReconAmt = sum(ReconAmt), SoldAmt = sum(SoldAmt), ObservedAmount = sum(PullAmt, ReconAmt, SoldAmt), ProdPercError = round(abs((sum(PullAmt, ReconAmt, SoldAmt) - sum(ProdAmt)) / sum(ProdAmt))* 100, digits = 2))


DayXStoreXSKU_Summary <- AllExceptColor_Data %>%
  dplyr::select(OrigProdDate, StoreID, StoreName, SKU, ProdAmt, PullAmt, ReconAmt, SoldAmt) %>%
  group_by(OrigProdDate, StoreID, StoreName, SKU) %>%
  summarise(ProdAmt = sum(ProdAmt), PullAmt = sum(PullAmt), ReconAmt = sum(ReconAmt), SoldAmt = sum(SoldAmt), ObservedAmount = sum(PullAmt, ReconAmt, SoldAmt), ProdPercError = round(abs((sum(PullAmt, ReconAmt, SoldAmt) - sum(ProdAmt)) / sum(ProdAmt))* 100, digits = 2))


DayXStoreXEmployee_Summary <- AllExceptColor_Data %>%
  dplyr::select(OrigProdDate, StoreID, StoreName, UserID, FirstName, LastName, ProdAmt, PullAmt, ReconAmt, SoldAmt) %>%
  group_by(OrigProdDate, StoreID, StoreName, UserID, FirstName, LastName) %>%
  summarise(ProdAmt = sum(ProdAmt), PullAmt = sum(PullAmt), ReconAmt = sum(ReconAmt), SoldAmt = sum(SoldAmt), ObservedAmount = sum(PullAmt, ReconAmt, SoldAmt), ProdPercError = round(abs((sum(PullAmt, ReconAmt, SoldAmt) - sum(ProdAmt)) / sum(ProdAmt))* 100, digits = 2))


DayXStoreXSKUXEmployee_Summary <- AllExceptColor_Data %>%
  dplyr::select(PrePostModel, OrigProdDate, StoreID, StoreName, SKU, UserID, FirstName, LastName, ProdAmt, PullAmt, ReconAmt, SoldAmt) %>%
  group_by(PrePostModel, OrigProdDate, StoreID, StoreName, SKU, UserID, FirstName, LastName) %>%
  summarise(ProdAmt = sum(ProdAmt), PullAmt = sum(PullAmt), ReconAmt = sum(ReconAmt), SoldAmt = sum(SoldAmt), ObservedAmount = sum(PullAmt, ReconAmt, SoldAmt), ProdPercError = round(abs((sum(PullAmt, ReconAmt, SoldAmt) - sum(ProdAmt)) / sum(ProdAmt))* 100, digits = 2))


MonthXStore_Summary <- AllExceptColor_Data %>%
  dplyr::select(OrigProdYear, OrigProdMonth, StoreID, StoreName, ProdAmt, PullAmt, ReconAmt, SoldAmt) %>%
  group_by(OrigProdYear, OrigProdMonth, StoreID, StoreName) %>%
  summarise(ProdAmt = sum(ProdAmt), PullAmt = sum(PullAmt), ReconAmt = sum(ReconAmt), SoldAmt = sum(SoldAmt), ObservedAmount = sum(PullAmt, ReconAmt, SoldAmt), ProdPercError = round(abs((sum(PullAmt, ReconAmt, SoldAmt) - sum(ProdAmt)) / sum(ProdAmt))* 100, digits = 2))


MonthXStoreXSKU_Summary <- AllExceptColor_Data %>%
        dplyr::select(OrigProdYear, OrigProdMonth, StoreID, StoreName, SKU, ProdAmt, PullAmt, ReconAmt, SoldAmt) %>%
        group_by(OrigProdYear, OrigProdMonth, StoreID, StoreName, SKU) %>%
        summarise(ProdAmt = sum(ProdAmt), PullAmt = sum(PullAmt), ReconAmt = sum(ReconAmt), SoldAmt = sum(SoldAmt), ObservedAmount = sum(PullAmt, ReconAmt, SoldAmt), ProdPercError = round(abs((sum(PullAmt, ReconAmt, SoldAmt) - sum(ProdAmt)) / sum(ProdAmt))* 100, digits = 2)) 


MonthXStoreXEmployee_Summary <- AllExceptColor_Data %>%
  dplyr::select(OrigProdYear, OrigProdMonth, StoreID, StoreName, UserID, FirstName, LastName, ProdAmt, PullAmt, ReconAmt, SoldAmt) %>%
  group_by(OrigProdYear, OrigProdMonth, StoreID, StoreName, UserID, FirstName, LastName) %>%
  summarise(ProdAmt = sum(ProdAmt), PullAmt = sum(PullAmt), ReconAmt = sum(ReconAmt), SoldAmt = sum(SoldAmt), ObservedAmount = sum(PullAmt, ReconAmt, SoldAmt), ProdPercError = round(abs((sum(PullAmt, ReconAmt, SoldAmt) - sum(ProdAmt)) / sum(ProdAmt))* 100, digits = 2)) 


MonthXStoreXSKUXEmployee_Summary <- AllExceptColor_Data %>%
  dplyr::select(OrigProdYear, OrigProdMonth, StoreID, StoreName, SKU, UserID, FirstName, LastName, ProdAmt, PullAmt, ReconAmt, SoldAmt) %>%
  group_by(OrigProdYear, OrigProdMonth, StoreID, StoreName, SKU, UserID, FirstName, LastName) %>%
  summarise(ProdAmt = sum(ProdAmt), PullAmt = sum(PullAmt), ReconAmt = sum(ReconAmt), SoldAmt = sum(SoldAmt), ObservedAmount = sum(PullAmt, ReconAmt, SoldAmt), ProdPercError = round(abs((sum(PullAmt, ReconAmt, SoldAmt) - sum(ProdAmt)) / sum(ProdAmt))* 100, digits = 2))



DayXStoreXSKUXEmployee_Summary <- AllExceptColor_Data %>%
  dplyr::select(PrePostModel, OrigProdDate, StoreID, StoreName, SKU, UserID, FirstName, LastName, ProdAmt, PullAmt, ReconAmt, SoldAmt) %>%
  group_by(PrePostModel, OrigProdDate, StoreID, StoreName, SKU, UserID, FirstName, LastName) %>%
  summarise(ProdAmt = sum(ProdAmt), PullAmt = sum(PullAmt), ReconAmt = sum(ReconAmt), SoldAmt = sum(SoldAmt), ObservedAmount = sum(PullAmt, ReconAmt, SoldAmt), ProdPercError = round(abs((sum(PullAmt, ReconAmt, SoldAmt) - sum(ProdAmt)) / sum(ProdAmt))* 100, digits = 2)) 



DayxStorexSKUxEmp_PerfectBatch <- AllExceptColor_Data %>%
  dplyr::select(PrePostModel, OrigProdDate, StoreID, StoreName, SKU, UserID, FirstName, LastName, ProdAmt, PullAmt, ReconAmt, SoldAmt, ProdPercError, PerfectBatch, PercSold) %>%
    group_by(PrePostModel, OrigProdDate, StoreID, StoreName, SKU, UserID, FirstName, LastName) %>%
      summarise(ProdAmt = sum(ProdAmt), PullAmt = sum(PullAmt), ReconAmt = sum(ReconAmt), SoldAmt = sum(SoldAmt), 
                AvgPercSold = round(mean(PercSold),2), ObservedAmount = sum(PullAmt, ReconAmt, SoldAmt),
                ProdPercError = round(abs((sum(PullAmt, ReconAmt, SoldAmt) - sum(ProdAmt)) / sum(ProdAmt))* 100, digits = 2),
                TotalBatch = n(),PerfectBatch_Count = sum(PerfectBatch), PerfectBatch_Perc = round((sum(PerfectBatch)/n())*100, 2))

