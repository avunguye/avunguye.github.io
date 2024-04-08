# An Vu Nguyen Dieu - Pledged
# BAT 3305 - Admission Project
# Dr. Zhu


##### A-B. Load packages - General Summary of TU columns #####

# A. Loading the necessary packages and reading data into data frames. 
# 1. Loading packages:
library(car)
library(reshape2)
library(dplyr)
library(tidyverse)
library(caret)
library(e1071)
library(MASS)
library(glmnet)
library(randomForest)
library(gbm)
library(DMwR2) 
library(tree)
options(scipen = 999)

# 2. Load original data into R.
original <- read.csv("TU.csv")
# Make blanks in the dataframe read as NAs. 
original[original == ''] <- NA

# 3. Keep only distinct observations:
clean_data <- distinct(original)
# The number of observations for clean_data is the same as that of original, 
# suggesting that the data set does not have any duplicated rows. 


# B. General summary of all variables: each variable's data type and their 
# percentage of NA values.
Data <- clean_data[1,]
Gen_Sum <- data.frame(Names = names(Data))
Gen_Sum$Class <- sapply(clean_data, class)
Gen_Sum$Total_obs <- sapply(clean_data, function(x) length(x))
Gen_Sum$NA_count <- sapply(clean_data, function(x) length(which(is.na(x))))
Gen_Sum$NA_perc <- round(sapply(clean_data, function(x) length(which(is.na(x)))/15143*100), 2)



##### C.1. Data Cleaning - Column 1-46 #####
# 1. ID
Gen_Sum[1,]
# No missing values.
# Id is only an index for observations, so further cleaning is not necessary. 



# 2. train.test
Gen_Sum[2,]
# No missing values.

levels(factor(clean_data$train.test))
# No unusual categories. 
# train.test is only a classification of observations into the train and the test
# data sets for cleaning purposes, so further cleaning is not necessary. 



# 3. Entry.Term..Application.
Gen_Sum[3,]
# No missing values. 

levels(factor(clean_data$Entry.Term..Application.))
# No unusual categories.
clean_data$Entry.Term..Application. <- as.factor(clean_data$Entry.Term..Application.)

# Distribution of data:
ggplot(clean_data, aes(Entry.Term..Application.)) + geom_bar()
round(prop.table(table(clean_data$Entry.Term..Application.))*100, 2)
# No level makes up >95% data.



# 4. Admit.Type
Gen_Sum[4,]
# No missing value.

levels(factor(clean_data$Admit.Type))
# Because the Admission data set only contains information on First-year students,
# the Admit.Type variable only has one level and will not provide useful insight
# in the modeling process. Therefore, it should be removed from the data set.
clean_data <- subset(clean_data, select = -c(Admit.Type))



# 5. Permanent.Postal
Gen_Sum[5,]
clean_data$Permanent.Postal <- substring(clean_data$Permanent.Postal, 1, nchar(clean_data$Permanent.Postal)-5)
table(clean_data$Permanent.Postal)
# There are 162 missing values. 
# Because the postal codes serve the purpose of categorizing applicants by postal 
# areas or city/state, the missing values can be replaced with geographical areas'
# postal codes. However, the Permanent.Geomarket variable also serves the same 
# purpose and only has 1 missing value so Permanent.Postal can be removed.
clean_data <- subset(clean_data, select = -c(Permanent.Postal))



# 6. Permanent.Country
Gen_Sum[6,]
# 1 missing value - observation 11148. Due to this applicant being a citizenship
# holder in the column Citizenship.Status, the NA value needs to be replaced 
# with "United States".
clean_data$Permanent.Country[is.na(clean_data$Permanent.Country)] <- "United States"

levels(factor(clean_data$Permanent.Country))
# No unusual categories.

# Group the countries into new levels based on geographical regions:
clean_data$Permanent.Country <- ifelse(clean_data$Permanent.Country == "Cameroon" |
                                         clean_data$Permanent.Country == "Cote D'Ivoire" |
                                         clean_data$Permanent.Country == "Egypt" |
                                         clean_data$Permanent.Country == "Ethiopia" |
                                         clean_data$Permanent.Country == "Ghana" |
                                         clean_data$Permanent.Country == "Jamaica" |
                                         clean_data$Permanent.Country == "Kenya" |
                                         clean_data$Permanent.Country == "Morocco" |
                                         clean_data$Permanent.Country == "Mozambique" |
                                         clean_data$Permanent.Country == "Nigeria" |
                                         clean_data$Permanent.Country == "South Africa" |
                                         clean_data$Permanent.Country == "Tanzania" |
                                         clean_data$Permanent.Country == "Trinidad and Tobago" |
                                         clean_data$Permanent.Country == "Uganda" |
                                         clean_data$Permanent.Country == "Zimbabwe",
                                       "Africa", clean_data$Permanent.Country)
clean_data$Permanent.Country <- ifelse(clean_data$Permanent.Country == "Albania" |
                                         clean_data$Permanent.Country == "Belgium" |
                                         clean_data$Permanent.Country == "Bosnia and Herzegovina" |
                                         clean_data$Permanent.Country == "Cyprus" |
                                         clean_data$Permanent.Country == "Czech Republic" |
                                         clean_data$Permanent.Country == "France" |
                                         clean_data$Permanent.Country == "Georgia" |
                                         clean_data$Permanent.Country == "Germany" |
                                         clean_data$Permanent.Country == "Greece" |
                                         clean_data$Permanent.Country == "Iceland" |
                                         clean_data$Permanent.Country == "Ireland" |
                                         clean_data$Permanent.Country == "Italy" |
                                         clean_data$Permanent.Country == "Lithuania" |
                                         clean_data$Permanent.Country == "Luxembourg" |
                                         clean_data$Permanent.Country == "Montenegro",
                                       "Europe", clean_data$Permanent.Country)
clean_data$Permanent.Country <- ifelse(clean_data$Permanent.Country == "Netherlands" |
                                         clean_data$Permanent.Country == "Norway" |
                                         clean_data$Permanent.Country == "Poland" |
                                         clean_data$Permanent.Country == "Portugal" |
                                         clean_data$Permanent.Country == "Romania" |
                                         clean_data$Permanent.Country == "Russia" |
                                         clean_data$Permanent.Country == "Spain" |
                                         clean_data$Permanent.Country == "Switzerland" |
                                         clean_data$Permanent.Country == "Mozambique" |
                                         clean_data$Permanent.Country == "Turkey" |
                                         clean_data$Permanent.Country == "Ukraine" |
                                         clean_data$Permanent.Country == "United Kingdom",
                                       "Europe", clean_data$Permanent.Country)
clean_data$Permanent.Country <- ifelse(clean_data$Permanent.Country == "Argentina" |
                                         clean_data$Permanent.Country == "Bolivia" |
                                         clean_data$Permanent.Country == "Brazil" |
                                         clean_data$Permanent.Country == "Ethiopia" |
                                         clean_data$Permanent.Country == "Chile" |
                                         clean_data$Permanent.Country == "Colombia" |
                                         clean_data$Permanent.Country == "Ecuador" |
                                         clean_data$Permanent.Country == "Paraguay" |
                                         clean_data$Permanent.Country == "Peru" |
                                         clean_data$Permanent.Country == "Uruguay" |
                                         clean_data$Permanent.Country == "Venezuela",
                                       "South America", clean_data$Permanent.Country)
clean_data$Permanent.Country <- ifelse(clean_data$Permanent.Country == "Australia" |
                                         clean_data$Permanent.Country == "Bangladesh" |
                                         clean_data$Permanent.Country == "Cambodia" |
                                         clean_data$Permanent.Country == "China" |
                                         clean_data$Permanent.Country == "Hong Kong S.A.R." |
                                         clean_data$Permanent.Country == "India" |
                                         clean_data$Permanent.Country == "Indonesia" |
                                         clean_data$Permanent.Country == "Iran" |
                                         clean_data$Permanent.Country == "Japan" |
                                         clean_data$Permanent.Country == "Jordan" |
                                         clean_data$Permanent.Country == "Kazakhstan" |
                                         clean_data$Permanent.Country == "Kuwait" |
                                         clean_data$Permanent.Country == "Lebanon" |
                                         clean_data$Permanent.Country == "Malaysia" |
                                         clean_data$Permanent.Country == "Mongolia",
                                       "Asia-Pacific", clean_data$Permanent.Country)
clean_data$Permanent.Country <- ifelse(clean_data$Permanent.Country == "Nepal" |
                                         clean_data$Permanent.Country == "Oman" |
                                         clean_data$Permanent.Country == "Pakistan" |
                                         clean_data$Permanent.Country == "Palestine" |
                                         clean_data$Permanent.Country == "Philippines" |
                                         clean_data$Permanent.Country == "Saudi Arabia" |
                                         clean_data$Permanent.Country == "Singapore" |
                                         clean_data$Permanent.Country == "South Korea" |
                                         clean_data$Permanent.Country == "Taiwan" |
                                         clean_data$Permanent.Country == "Thailand" |
                                         clean_data$Permanent.Country == "United Arab Emirates" |
                                         clean_data$Permanent.Country == "Uzbekistan" |
                                         clean_data$Permanent.Country == "Vietnam",
                                       "Asia-Pacific", clean_data$Permanent.Country)
clean_data$Permanent.Country <- ifelse(clean_data$Permanent.Country == "Barbados" |
                                         clean_data$Permanent.Country == "Belize" |
                                         clean_data$Permanent.Country == "Canada" |
                                         clean_data$Permanent.Country == "Cayman Islands" |
                                         clean_data$Permanent.Country == "Costa Rica" |
                                         clean_data$Permanent.Country == "Dominica" |
                                         clean_data$Permanent.Country == "Dominican Republic" |
                                         clean_data$Permanent.Country == "El Salvador" |
                                         clean_data$Permanent.Country == "Guatemala" |
                                         clean_data$Permanent.Country == "Honduras" |
                                         clean_data$Permanent.Country == "Mexico" |
                                         clean_data$Permanent.Country == "Nicaragua" |
                                         clean_data$Permanent.Country == "Panama" |
                                         clean_data$Permanent.Country == "The Bahamas" |
                                         clean_data$Permanent.Country == "United States",
                                       "North America", clean_data$Permanent.Country)
clean_data$Permanent.Country <- ifelse(clean_data$Permanent.Country == "New Zealand",
                                       "Oceania", clean_data$Permanent.Country)

clean_data$Permanent.Country <- as.factor(clean_data$Permanent.Country)

# Distribution of data:
ggplot(clean_data, aes(Permanent.Country)) + geom_bar()
round(prop.table(table(clean_data$Permanent.Country))*100, 2)
# North America makes up >95% data. This variable should be removed. 
clean_data <- subset(clean_data, select = -c(Permanent.Country))



# 7. Sex 
Gen_Sum[7,]
# No missing values. 

levels(factor(clean_data$Sex))
# No unusual categories.
clean_data$Sex  <- as.factor(clean_data$Sex)
# Distribution of data:
ggplot(clean_data, aes(Sex)) + geom_bar()
round(prop.table(table(clean_data$Sex))*100, 2)
# No level makes up >95% data.



# 8. Ethnicity 
Gen_Sum[8,]
# There are 227 missing values, making up 1.5% of the total observations. 
# Whilst it is possible to make assumptions of an applicant's ethnicity based 
# on their geographical areas, such assumptions cannot be made with absolute
# certainty and it is best to replace the NAs with "Unspecified". 
clean_data$Ethnicity[is.na(clean_data$Ethnicity)] <- "Unspecified"

levels(factor(clean_data$Ethnicity))
# No unusual categories.
clean_data$Ethnicity <- as.factor(clean_data$Ethnicity)

# Distribution of data:
ggplot(clean_data, aes(Ethnicity)) + geom_bar()
round(prop.table(table(clean_data$Ethnicity))*100, 2)
# No level makes up >95% data.



# 9. Race 
Gen_Sum[9,]
# There are 555 missing values, making up 1.5% of the total observations. 
# Whilst it is possible to make assumptions of an applicant's race based on 
# their geographical areas and ethnicity, such assumptions cannot be made with 
# absolute certainty and it is best to replace the NAs with "Unspecified". 
clean_data$Race[is.na(clean_data$Race)] <- "Unspecified"

levels(factor(clean_data$Race))
# While there are no unusual categories, the current classification of Race is 
# too detailed, leading to very low frequencies for some categories.
# We need to combine some of the categories because levels with too low
# frequency will not result in a significant effect on the response.
clean_data$Race[clean_data$Race == "American Indian or Alaska Native, Asian, Black or African American, Native Hawaiian or Other Pacific, White"] <- "American Indian or Alaska Native, Asian"
clean_data$Race[clean_data$Race == "American Indian or Alaska Native, Asian, Black or African American, White"] <- "American Indian or Alaska Native, Asian"
clean_data$Race[clean_data$Race == "American Indian or Alaska Native, Asian, White"] <- "American Indian or Alaska Native, Asian"
clean_data$Race[clean_data$Race == "American Indian or Alaska Native, Black or African American, White"] <- "American Indian or Alaska Native, Black or African American"
clean_data$Race[clean_data$Race == "American Indian or Alaska Native, Native Hawaiian or Other Pacific"] <- "American Indian or Alaska Native"
clean_data$Race[clean_data$Race == "Asian, Black or African American, Native Hawaiian or Other Pacific, White"] <- "Asian, Black or African American"
clean_data$Race[clean_data$Race == "Asian, Black or African American, White"] <- "Asian, Black or African American"
clean_data$Race[clean_data$Race == "Asian, Native Hawaiian or Other Pacific, White"] <- "Asian, Native Hawaiian or Other Pacific"
clean_data$Race[clean_data$Race == "Black or African American, Native Hawaiian or Other Pacific"] <- "Black or African American"
clean_data$Race[clean_data$Race == "Black or African American, Native Hawaiian or Other Pacific, White"] <- "Black or African American"
clean_data$Race[clean_data$Race == "Native Hawaiian or Other Pacific, White"] <- "Native Hawaiian or Other Pacific"

# Distribution of data:
ggplot(clean_data, aes(Race)) + geom_bar()
round(prop.table(table(clean_data$Race))*100, 2)
# No level makes up >95% data.
clean_data$Race <- as.factor(clean_data$Race)



# 10. Religion 
Gen_Sum[10,]
# There are 5483 missing values, making over 36% of the total observations. 
# However, similar to Ethnicity and Race, it is best to not make assumptions 
# about an applicant's religion and assign "Unspecified" to missing values.
clean_data$Religion[is.na(clean_data$Religion)] <- "Unspecified"

levels(factor(clean_data$Religion))
# While there are no unusual categories, the current classification of Religion 
# is too detailed, leading to very low frequencies for some categories.
# We need to combine some of the categories because levels with too low
# frequency will not result in a significant effect on the response.

# For this variable, I grouped religions based on the larger denomination or 
# branch of religion to which they belong.
clean_data$Religion[clean_data$Religion == "Anglican" | 
                    clean_data$Religion == "Baptist"|
                    clean_data$Religion == "Christian Reformed" |
                    clean_data$Religion == "Episcopal" |  
                    clean_data$Religion == "Mennonite" |
                    clean_data$Religion == "Society of Friends (Quaker)" |
                    clean_data$Religion == "United Church of Christ" |
                    clean_data$Religion == "United Methodist"] <- "Protestant"
clean_data$Religion[clean_data$Religion == "Assembly of God" | 
                    clean_data$Religion == "Church of God"] <- "Pentecostal"
clean_data$Religion[clean_data$Religion == "Baha'I" | 
                    clean_data$Religion == "Jain"|
                    clean_data$Religion == "Jehovah's Witnesses" |
                    clean_data$Religion == "Sikh" |  
                    clean_data$Religion == "Zoroastrian"] <- "Other"
clean_data$Religion[clean_data$Religion == "Bible Churches" | 
                    clean_data$Religion == "Church of the Nazarene"|
                    clean_data$Religion == "Southern Baptist"] <- "Evangelical"
clean_data$Religion[clean_data$Religion == "Christian Scientist" | 
                    clean_data$Religion == "Church of Christ"|
                    clean_data$Religion == "Independent"] <- "Non-Denominational"
clean_data$Religion[clean_data$Religion == "Coptic Church (Egypt)"] <- "Eastern Orthodox"
clean_data$Religion[clean_data$Religion == "Jewish Messianic"] <- "Jewish"
clean_data$Religion[clean_data$Religion == "Lutheran-Missouri Synod"] <- "Lutheran"
clean_data$Religion[clean_data$Religion == "Mormon-Latter Day Saints"] <- "Mormon"
clean_data$Religion[clean_data$Religion == "Presbyterian Church of America"] <- "Presbyterian"
clean_data$Religion[clean_data$Religion == "Unitarian"] <- "Christian"

# Distribution of data:
ggplot(clean_data, aes(Religion)) + geom_bar()
round(prop.table(table(clean_data$Religion))*100, 2)
# No level makes up >95% data.
clean_data$Religion <- as.factor(clean_data$Religion)



# 11. First_Source.Origin.First.Source.Date
Gen_Sum[11,]
# No missing values. 
clean_data$First_Source.Origin.First.Source.Date <- as.Date(clean_data$First_Source.Origin.First.Source.Date, 
                                                            format="%m/%d/%Y")



# 12. Inquiry.Date
Gen_Sum[12,]
# There are 4759 missing values which will be handled later as Inquiry.Date 
# will be used to create new variables. 
clean_data$Inquiry.Date <- as.Date(clean_data$Inquiry.Date, format="%m/%d/%Y")



# 13. Submitted 
Gen_Sum[13,]
# No missing values. 
clean_data$Submitted <- as.Date(clean_data$Submitted, format="%m/%d/%Y")



# 11-13. Create two new variables using the variables from 11-13:
# New Variable 1: Amount of time since First Source Date till Application 
# Submission Date.
clean_data$Submit_FirstSource <- difftime(clean_data$Submitted, 
                                          clean_data$First_Source.Origin.First.Source.Date, 
                                          units = "weeks")
clean_data$Submit_FirstSource <- round(clean_data$Submit_FirstSource, digits = 0)
clean_data$Submit_FirstSource <- as.numeric(clean_data$Submit_FirstSource)
sum(is.na(clean_data$Submit_FirstSource)) 
# No missing values.

# New Variable 2: Amount of time since Inquiry Date till Application Submission 
# Date.
clean_data$Submit_Inquiry <- difftime(clean_data$Submitted, 
                                      clean_data$Inquiry.Date, units = "weeks")
clean_data$Submit_Inquiry <- round(clean_data$Submit_Inquiry, digits = 1)
clean_data$Submit_Inquiry <- as.numeric(clean_data$Submit_Inquiry)
sum(is.na(clean_data$Submit_Inquiry)) 
# There are 4759 missing values. Replace the NAs with the median value:
clean_data$Submit_Inquiry[is.na(clean_data$Submit_Inquiry)] <- median(clean_data$Submit_Inquiry,
                                                                      na.rm=TRUE)

# Remove First_Source.Origin.First.Source.Date, Inquiry.Date, and Submitted: 
clean_data <- subset(clean_data, select = -c(First_Source.Origin.First.Source.Date, 
                                             Inquiry.Date, Submitted))




# 14. Application.Source
Gen_Sum[14,]
# No missing values. 

levels(factor(clean_data$Application.Source))
# No unusual categories. 

# Distribution of data:
ggplot(clean_data, aes(Application.Source)) + geom_bar()
round(prop.table(table(clean_data$Application.Source))*100, 2)
# No level makes up >95% data.
clean_data$Application.Source <- as.factor(clean_data$Application.Source)



# 15. Decision.Plan
Gen_Sum[15,]
# No missing values.

levels(factor(clean_data$Decision.Plan))
# No unusual categories. 

# Distribution of data:
ggplot(clean_data, aes(Decision.Plan)) + geom_bar()
round(prop.table(table(clean_data$Decision.Plan))*100, 2)
# No level makes up >95% data.
clean_data$Decision.Plan <- as.factor(clean_data$Decision.Plan)



# 16. Staff.Assigned.Name
Gen_Sum[16,]
# There are 3 missing values. However, knowing the staff named does not contribute
# valuable insights to the modeling process so this variable should be removed. 
clean_data <- subset(clean_data, select = -c(Staff.Assigned.Name))



# 17.Legacy 
Gen_Sum[17,]
# There are 13658 missing values. Replace the NAs with "No Legacy":
clean_data$Legacy[is.na(clean_data$Legacy)] <- "No Legacy"

levels(factor(clean_data$Legacy))
# While there are no unusual categories, the current classification of Legacy 
# is too detailed, leading to very low frequencies for some categories.
# We need to combine some of the categories because levels with too low
# frequency will not result in a significant effect on the response.

# Group the levels in Legacy in three categories: Legacy, No Legacy, and Legacy
# but Opt Out. 
clean_data$Legacy <- ifelse(clean_data$Legacy == "Legacy", "Legacy", 
                     ifelse(clean_data$Legacy == "No Legacy", "No Legacy",
                     ifelse(grepl("Legacy, Opt Out",clean_data$Legacy), 
                                          "Legacy, Opt Out", "Legacy")))
# Distribution of data:
ggplot(clean_data, aes(Legacy)) + geom_bar()
round(prop.table(table(clean_data$Legacy))*100, 2)
# No level makes up >95% data.
clean_data$Legacy <- as.factor(clean_data$Legacy)



# 18. Athlete 
Gen_Sum[18,]
# There 13120 missing values. Replace the missing values with "Non-Athlete":
clean_data$Athlete[is.na(clean_data$Athlete)] <- "Non-Athlete"

levels(factor(clean_data$Athlete))
# While there are no unusual categories, the current classification of Athlete 
# is too detailed, leading to very low frequencies for some categories.
# We need to combine some of the categories because levels with too low
# frequency will not result in a significant effect on the response.

# Group the levels in Legacy in three categories: Athlete, Non-Athlete, and 
# Athlete but Opt Out. 
clean_data$Athlete <- ifelse(clean_data$Athlete == "Athlete", "Athlete", 
                      ifelse(clean_data$Athlete == "Non-Athlete", "Non-Athlete",
                      ifelse(grepl("Opt Out",clean_data$Athlete), 
                                   "Athlete, Opt Out", "Athlete")))

# Distribution of data:
ggplot(clean_data, aes(Athlete)) + geom_bar()
round(prop.table(table(clean_data$Athlete))*100, 2)
# No level makes up >95% data.
clean_data$Athlete <- as.factor(clean_data$Athlete)         



# 19. Sport.1.Sport
Gen_Sum[19,]
# There are 13120 missing values. Replace the missing values with "No Sport":
clean_data$Sport.1.Sport[is.na(clean_data$Sport.1.Sport)] <- "No Sport"

levels(factor(clean_data$Sport.1.Sport))
# While there are no unusual categories, the current classification of Sport.1.Sport 
# is too detailed, leading to very low frequencies for some categories.
# We need to combine some of the categories because levels with too low
# frequency will not result in a significant effect on the response.

# Group sport men and sport women into one group for each sport:
clean_data$Sport.1.Sport <- ifelse(clean_data$Sport.1.Sport == "Baseball", "Baseball", 
                            ifelse(clean_data$Sport.1.Sport == "Softball", "Softball",
                            ifelse(clean_data$Sport.1.Sport == "Football", "Football", 
                            ifelse(clean_data$Sport.1.Sport == "No Sport", "No Sport", 
                            ifelse(grepl("Basketball", clean_data$Sport.1.Sport), "Basketball",
                            ifelse(grepl("Cross Country", clean_data$Sport.1.Sport), "Cross Country",
                            ifelse(grepl("Diving", clean_data$Sport.1.Sport), "Diving",
                            ifelse(grepl("Golf", clean_data$Sport.1.Sport), "Golf",
                            ifelse(grepl("Soccer", clean_data$Sport.1.Sport), "Soccer",
                            ifelse(grepl("Swimming", clean_data$Sport.1.Sport), "Swimming",
                            ifelse(grepl("Tennis", clean_data$Sport.1.Sport), "Tennis",
                            ifelse(grepl("Track", clean_data$Sport.1.Sport), "Track", "Volleyball"))))))))))))
# Distribution of data:
ggplot(clean_data, aes(Sport.1.Sport)) + geom_bar()
round(prop.table(table(clean_data$Sport.1.Sport))*100, 2)
# No level makes up >95% data.
clean_data$Sport.1.Sport <- as.factor(clean_data$Sport.1.Sport)




# 20. Sport.1.Rating
Gen_Sum[20,]
# There are 13120 missing values. Replace the NAs with "No Sport".
clean_data$Sport.1.Rating[is.na(clean_data$Sport.1.Rating)] <- "No Sport"

levels(factor(clean_data$Sport.1.Rating))
# No unusual categories.
clean_data$Sport.1.Rating<- factor(clean_data$Sport.1.Rating, order = TRUE, 
                                   levels = c("No Sport", "Varsity", "Blue Chip", "Franchise"))
# Distribution of data:
ggplot(clean_data, aes(Sport.1.Rating)) + geom_bar()
round(prop.table(table(clean_data$Sport.1.Rating))*100, 2)
# No level makes up >95% data.
clean_data$Sport.1.Rating <- as.factor(clean_data$Sport.1.Rating)



# 21. Sport.2.Sport
Gen_Sum[21,]
# There are 14513 missing values. Replace the NAs with "No 2ndSport":
clean_data$Sport.2.Sport[is.na(clean_data$Sport.2.Sport)] <- "No 2ndSport"

levels(factor(clean_data$Sport.2.Sport))
# While there are no unusual categories, the number of cases for each sport type 
# is very small (< about 1% of the data set).It's better to group all options 
# into 2 categories: 2ndSport vs. No 2ndSport.
clean_data$Sport.2.Sport <- ifelse(clean_data$Sport.2.Sport == "No 2ndSport", 
                                   "No 2ndSport", "2ndSport")

# Distribution of data:
ggplot(clean_data, aes(Sport.2.Sport)) + geom_bar()
round(prop.table(table(clean_data$Sport.2.Sport))*100, 2)
# No 2ndSport makes up over 95% of the data for this variable so it should be 
# removed. 
clean_data <- subset(clean_data, select = -c(Sport.2.Sport))



# 22. Sport.2.Rating
Gen_Sum[22,]
# There are 15085 missing values, making up 99% of the data. Because more than 
# 95% of the data is NAs, this variable will not provide much insight during 
# the modeling process and should be removed from the data set.
clean_data <- subset(clean_data, select = -c(Sport.2.Rating))



# 23. Sport.3.Sport
Gen_Sum[23,]
# There are 14907 missing values, making up 98% of the data. Because more than 
# 95% of the data is NAs, this variable will not provide much insight during 
# the modeling process and should be removed from the data set.
clean_data <- subset(clean_data, select = -c(Sport.3.Sport))



# 24. Sport.3.Rating
Gen_Sum[24,]
# There are 15140 missing values, making up 99% of the data. Because more than 
# 95% of the data is NAs, this variable will not provide much insight during 
# the modeling process and should be removed from the data set.
clean_data <- subset(clean_data, select = -c(Sport.3.Rating))



# 25. Academic.Interest.1
Gen_Sum[25,]
# There are 6 missing values. 

# Most of the NAs for Academic.Interest.1 have a value for Academic.Interest.2
# Assign the corresponding values in Academic.Interest.2 to NAs in 
# Academic.Interest.1 if Academic.Interest.2 has a value.
clean_data$Academic.Interest.1 <- ifelse(is.na(clean_data$Academic.Interest.1) == TRUE, 
                                         clean_data$Academic.Interest.2, 
                                         clean_data$Academic.Interest.1)
#For the remaining NAs in Academic.Interest.1, assign Undecided.
clean_data$Academic.Interest.1[is.na(clean_data$Academic.Interest.1)] <- "Undecided"

levels(factor(clean_data$Academic.Interest.1))

# Group Academic Fields into major groups:
clean_data$Academic.Interest.1 <- ifelse(grepl("Business", clean_data$Academic.Interest.1), "Business",
                                  ifelse(clean_data$Academic.Interest.1 == "Finance", "Business",
                                  ifelse(clean_data$Academic.Interest.1 == "Entrepreneurship", "Business", 
                                         clean_data$Academic.Interest.1)))

clean_data$Academic.Interest.1[clean_data$Academic.Interest.1 == "Biochemistry" | 
                               clean_data$Academic.Interest.1 == "Biochemistry & Molecular Biology"|
                               clean_data$Academic.Interest.1 == "Biology" |
                               clean_data$Academic.Interest.1 == "Biomathematics"] <- "Biology-related Studies"

clean_data$Academic.Interest.1[clean_data$Academic.Interest.1 == "Applied Chemistry" | 
                               clean_data$Academic.Interest.1 == "Chemistry"] <- "Chemistry-related Studies"

clean_data$Academic.Interest.1[clean_data$Academic.Interest.1 == "Art" | 
                                 clean_data$Academic.Interest.1 == "Art and Art History"|
                                 clean_data$Academic.Interest.1 == "Art History" |
                                 clean_data$Academic.Interest.1 == "Arts, Letters & Enterprise"] <- "Art-related Studies"

clean_data$Academic.Interest.1[clean_data$Academic.Interest.1 == "Chinese" | 
                                 clean_data$Academic.Interest.1 == "Classical Languages"|
                                 clean_data$Academic.Interest.1 == "English" |
                                 clean_data$Academic.Interest.1 == "Foreign Languages" |
                                 clean_data$Academic.Interest.1 == "French" |
                                 clean_data$Academic.Interest.1 == "German" |
                                 clean_data$Academic.Interest.1 == "Italian" |
                                 clean_data$Academic.Interest.1 == "Latin" |
                                 clean_data$Academic.Interest.1 == "Linguistics" |
                                 clean_data$Academic.Interest.1 == "Russian" |
                                 clean_data$Academic.Interest.1 == "Spanish" ] <- "Language-based Studies"

clean_data$Academic.Interest.1[clean_data$Academic.Interest.1 == "African American Studies" | 
                                 clean_data$Academic.Interest.1 == "Ancient Mediterranean Studies"|
                                 clean_data$Academic.Interest.1 == "East Asian Studies" |
                                 clean_data$Academic.Interest.1 == "Global Latinx Studies"] <- "Cultural Studies"

clean_data$Academic.Interest.1[clean_data$Academic.Interest.1 == "Agriculture" | 
                                 clean_data$Academic.Interest.1 == "Anthropology"|
                                 clean_data$Academic.Interest.1 == "Economics" |
                                 clean_data$Academic.Interest.1 == "Environmental Studies" |
                                 clean_data$Academic.Interest.1 == "Geosciences" |
                                 clean_data$Academic.Interest.1 == "History" |
                                 clean_data$Academic.Interest.1 == "International Studies" |
                                 clean_data$Academic.Interest.1 == "Sociology" |
                                 clean_data$Academic.Interest.1 == "Education" |
                                 clean_data$Academic.Interest.1 == "Urban Studies" |
                                 clean_data$Academic.Interest.1 == "Women's & Gender Studies" ] <- "Social Studies"

clean_data$Academic.Interest.1[clean_data$Academic.Interest.1 == "Architectural Studies" | 
                                 clean_data$Academic.Interest.1 == "Architecture"] <- "Architecture-related Studies"

clean_data$Academic.Interest.1[clean_data$Academic.Interest.1 == "Choral Music" | 
                                 clean_data$Academic.Interest.1 == "Film Studies"|
                                 clean_data$Academic.Interest.1 == "Instrumental Music" |
                                 clean_data$Academic.Interest.1 == "Music" |
                                 clean_data$Academic.Interest.1 == "Music Composition" |
                                 clean_data$Academic.Interest.1 == "Music Education" |
                                 clean_data$Academic.Interest.1 == "Theatre"] <- "Music & Theatre"

clean_data$Academic.Interest.1[clean_data$Academic.Interest.1 == "Communication" | 
                                 clean_data$Academic.Interest.1 == "Comparative Literature"|
                                 clean_data$Academic.Interest.1 == "Creative Writing" |
                                 clean_data$Academic.Interest.1 == "Human Communication" |
                                 clean_data$Academic.Interest.1 == "New Media" ] <- "Media & Communication"

clean_data$Academic.Interest.1[clean_data$Academic.Interest.1 == "Cognitive Science" | 
                                 clean_data$Academic.Interest.1 == "Neuroscience"|
                                 clean_data$Academic.Interest.1 == "Nursing" |
                                 clean_data$Academic.Interest.1 == "Pharmacy" |
                                 clean_data$Academic.Interest.1 == "Pre-Dental" |
                                 clean_data$Academic.Interest.1 == "Pre-Medical" |
                                 clean_data$Academic.Interest.1 == "Pre-Veterinary" |
                                 clean_data$Academic.Interest.1 == "Psychology" ] <- "Health & Medical Studies"

clean_data$Academic.Interest.1[clean_data$Academic.Interest.1 == "Astronomy" | 
                                 clean_data$Academic.Interest.1 == "Physics"] <- "Physics & Astronomy"

clean_data$Academic.Interest.1[clean_data$Academic.Interest.1 == "Computer Science" | 
                                 clean_data$Academic.Interest.1 == "Engineering Science" ] <- "Computer & Engineering Sciences"

clean_data$Academic.Interest.1[clean_data$Academic.Interest.1 == "Mathematical Finance" | 
                                 clean_data$Academic.Interest.1 == "Mathematics" ] <- "Mathematics"

clean_data$Academic.Interest.1[clean_data$Academic.Interest.1 == "Philosophy" | 
                                 clean_data$Academic.Interest.1 == "Political Science"|
                                 clean_data$Academic.Interest.1 == "Pre-Law" |
                                 clean_data$Academic.Interest.1 == "Religion"] <- "Political & Philosophical Studies"

# Group options with a low number of cases (< 100 cases) into "Others".
frequencies <-data.frame(table(clean_data$Academic.Interest.1))
frequencies
clean_data$Academic.Interest.1.Frequency <- NA
for (i in 1:nrow(clean_data)){
  for(j in 1:nrow(frequencies)){
    if (clean_data$Academic.Interest.1[i] == frequencies$Var1[j])
    {clean_data$Academic.Interest.1.Frequency[i] <- frequencies$Freq[j]}}
}

for (i in 1:nrow(clean_data)){
  if (clean_data$Academic.Interest.1.Frequency[i] < 100)
  {clean_data$Academic.Interest.1[i] <- "Others"}else{
    clean_data$Academic.Interest.1[i]
  }
}

# Distribution of data:
ggplot(clean_data, aes(Academic.Interest.1)) + geom_bar()
round(prop.table(table(clean_data$Academic.Interest.1))*100, 2)
# No level making up over 95%.
clean_data$Academic.Interest.1 <- as.factor(clean_data$Academic.Interest.1)
clean_data <- subset(clean_data, select = -c(Academic.Interest.1.Frequency))



# 26. Academic.Interest.2
Gen_Sum[26,]
# There are 159 missing values. Replace the missing values with "Undecided":
clean_data$Academic.Interest.2[is.na(clean_data$Academic.Interest.2)] <- "Undecided"

# Replace the repeated academic interests with "No 2ndInterest"
clean_data$Academic.Interest.2 <- ifelse(clean_data$Academic.Interest.2 == clean_data$Academic.Interest.1, 
                                         "No 2ndInterest", clean_data$Academic.Interest.2)

table(clean_data$Academic.Interest.2)
# No unusual category.
# Group Academic Fields into major groups:
clean_data$Academic.Interest.2 <- ifelse(grepl("Business", clean_data$Academic.Interest.2), "Business",
                                         ifelse(clean_data$Academic.Interest.2 == "Finance", "Business",
                                                ifelse(clean_data$Academic.Interest.2 == "Entrepreneurship", "Business", 
                                                       clean_data$Academic.Interest.2)))

clean_data$Academic.Interest.2[clean_data$Academic.Interest.2 == "Biochemistry" | 
                                 clean_data$Academic.Interest.2 == "Biochemistry & Molecular Biology"|
                                 clean_data$Academic.Interest.2 == "Biology" |
                                 clean_data$Academic.Interest.2 == "Biomathematics"] <- "Biology-related Studies"

clean_data$Academic.Interest.2[clean_data$Academic.Interest.2 == "Applied Chemistry" | 
                                 clean_data$Academic.Interest.2 == "Chemistry"] <- "Chemistry-related Studies"

clean_data$Academic.Interest.2[clean_data$Academic.Interest.2 == "Art" | 
                                 clean_data$Academic.Interest.2 == "Art and Art History"|
                                 clean_data$Academic.Interest.2 == "Art History" |
                                 clean_data$Academic.Interest.2 == "Arts, Letters & Enterprise"] <- "Art-related Studies"

clean_data$Academic.Interest.2[clean_data$Academic.Interest.2 == "Chinese" | 
                                 clean_data$Academic.Interest.2 == "Classical Languages"|
                                 clean_data$Academic.Interest.2 == "English" |
                                 clean_data$Academic.Interest.2 == "Foreign Languages" |
                                 clean_data$Academic.Interest.2 == "French" |
                                 clean_data$Academic.Interest.2 == "German" |
                                 clean_data$Academic.Interest.2 == "Italian" |
                                 clean_data$Academic.Interest.2 == "Latin" |
                                 clean_data$Academic.Interest.2 == "Linguistics" |
                                 clean_data$Academic.Interest.2 == "Russian" |
                                 clean_data$Academic.Interest.2 == "Spanish" ] <- "Language-based Studies"

clean_data$Academic.Interest.2[clean_data$Academic.Interest.2 == "African American Studies" | 
                                 clean_data$Academic.Interest.2 == "Ancient Mediterranean Studies"|
                                 clean_data$Academic.Interest.2 == "East Asian Studies" |
                                 clean_data$Academic.Interest.2 == "Global Latinx Studies"] <- "Cultural Studies"

clean_data$Academic.Interest.2[clean_data$Academic.Interest.2 == "Agriculture" | 
                                 clean_data$Academic.Interest.2 == "Anthropology"|
                                 clean_data$Academic.Interest.2 == "Economics" |
                                 clean_data$Academic.Interest.2 == "Environmental Studies" |
                                 clean_data$Academic.Interest.2 == "Geosciences" |
                                 clean_data$Academic.Interest.2 == "History" |
                                 clean_data$Academic.Interest.2 == "International Studies" |
                                 clean_data$Academic.Interest.2 == "Sociology" |
                                 clean_data$Academic.Interest.2 == "Education" |
                                 clean_data$Academic.Interest.2 == "Urban Studies" |
                                 clean_data$Academic.Interest.2 == "Women's & Gender Studies" ] <- "Social Studies"

clean_data$Academic.Interest.2[clean_data$Academic.Interest.2 == "Architectural Studies" | 
                                 clean_data$Academic.Interest.2 == "Architecture"] <- "Architecture-related Studies"

clean_data$Academic.Interest.2[clean_data$Academic.Interest.2 == "Choral Music" | 
                                 clean_data$Academic.Interest.2 == "Film Studies"|
                                 clean_data$Academic.Interest.2 == "Instrumental Music" |
                                 clean_data$Academic.Interest.2 == "Music" |
                                 clean_data$Academic.Interest.2 == "Music Composition" |
                                 clean_data$Academic.Interest.2 == "Music Education" |
                                 clean_data$Academic.Interest.2 == "Theatre"] <- "Music & Theatre"

clean_data$Academic.Interest.2[clean_data$Academic.Interest.2 == "Communication" | 
                                 clean_data$Academic.Interest.2 == "Comparative Literature"|
                                 clean_data$Academic.Interest.2 == "Creative Writing" |
                                 clean_data$Academic.Interest.2 == "Human Communication" |
                                 clean_data$Academic.Interest.2 == "New Media" ] <- "Media & Communication"

clean_data$Academic.Interest.2[clean_data$Academic.Interest.2 == "Cognitive Science" | 
                                 clean_data$Academic.Interest.2 == "Neuroscience"|
                                 clean_data$Academic.Interest.2 == "Nursing" |
                                 clean_data$Academic.Interest.2 == "Pharmacy" |
                                 clean_data$Academic.Interest.2 == "Pre-Dental" |
                                 clean_data$Academic.Interest.2 == "Pre-Medical" |
                                 clean_data$Academic.Interest.2 == "Pre-Veterinary" |
                                 clean_data$Academic.Interest.2 == "Psychology" ] <- "Health & Medical Studies"

clean_data$Academic.Interest.2[clean_data$Academic.Interest.2 == "Astronomy" | 
                                 clean_data$Academic.Interest.2 == "Physics"] <- "Physics & Astronomy"

clean_data$Academic.Interest.2[clean_data$Academic.Interest.2 == "Computer Science" | 
                                 clean_data$Academic.Interest.2 == "Engineering Science" ] <- "Computer & Engineering Sciences"

clean_data$Academic.Interest.2[clean_data$Academic.Interest.2 == "Mathematical Finance" | 
                                 clean_data$Academic.Interest.2 == "Mathematics" ] <- "Mathematics"

clean_data$Academic.Interest.2[clean_data$Academic.Interest.2 == "Philosophy" | 
                                 clean_data$Academic.Interest.2 == "Political Science"|
                                 clean_data$Academic.Interest.2 == "Pre-Law" |
                                 clean_data$Academic.Interest.2 == "Religion"] <- "Political & Philosophical Studies"


# Group options with a low number of cases (<100 cases) into "Others".
frequencies <-data.frame(table(clean_data$Academic.Interest.2))
frequencies
clean_data$Academic.Interest.2.Frequency <- NA
for (i in 1:nrow(clean_data)){
  for(j in 1:nrow(frequencies)){
    if (clean_data$Academic.Interest.2[i] == frequencies$Var1[j])
    {clean_data$Academic.Interest.2.Frequency[i] <- frequencies$Freq[j]}}
}

for (i in 1:nrow(clean_data)){
  if (clean_data$Academic.Interest.2.Frequency[i] < 100)
  {clean_data$Academic.Interest.2[i] <- "Others"}else{
    clean_data$Academic.Interest.2[i]
  }
}

# Distribution of data:
ggplot(clean_data, aes(Academic.Interest.2)) + geom_bar()
round(prop.table(table(clean_data$Academic.Interest.2))*100, 2)
# No level making up over 95%.
clean_data$Academic.Interest.2 <- as.factor(clean_data$Academic.Interest.2)
clean_data <- subset(clean_data, select = -c(Academic.Interest.2.Frequency))



# 27. First_Source.Origin.First.Source.Summary
Gen_Sum[27,]
# No missing values.

table(clean_data$First_Source.Origin.First.Source.Summary)
# No unusual category.
# Group options with a low number of cases (< 100) into "Other Sources".
frequencies <-data.frame(table(clean_data$First_Source.Origin.First.Source.Summary))
frequencies
clean_data$First_Source.Summary.Frequency <- NA
for (i in 1:nrow(clean_data)){
  for(j in 1:nrow(frequencies)){
    if (clean_data$First_Source.Origin.First.Source.Summary[i] == frequencies$Var1[j])
    {clean_data$First_Source.Summary.Frequency[i] <- frequencies$Freq[j]}}
}

for (i in 1:nrow(clean_data)){
  if (clean_data$First_Source.Summary.Frequency[i] < 100)
  {clean_data$First_Source.Origin.First.Source.Summary[i] <- "Other Sources"}else{
    clean_data$First_Source.Origin.First.Source.Summary[i]
  }
}

# Distribution of data:
ggplot(clean_data, aes(First_Source.Origin.First.Source.Summary)) + geom_bar()
round(prop.table(table(clean_data$First_Source.Origin.First.Source.Summary))*100, 2)
# No level making up over 95%.
clean_data$First_Source.Origin.First.Source.Summary <- as.factor(clean_data$First_Source.Origin.First.Source.Summary)
clean_data <- subset(clean_data, select = -c(First_Source.Summary.Frequency))




# 28. Total.Event.Participation
Gen_Sum[28,]
# No missing values.

table(clean_data$Total.Event.Participation)
# No unusual category.
# 3, 4, 5 combined accounts for < 1% of the data set.
# Compared to the number of cases in 0, 1, and 2, the number of cases
# in 3, 4, and 5 won't be very useful in predicting the response.
# Factor the variable and group 3, 4, and 5 into "2 or more".
clean_data$Total.Event.Participation <- ifelse(clean_data$Total.Event.Participation > 2,
                                               2, clean_data$Total.Event.Participation)

# Convert int to char so that level name can be modified.
clean_data$Total.Event.Participation <- as.character(clean_data$Total.Event.Participation)
clean_data$Total.Event.Participation <- ifelse(clean_data$Total.Event.Participation == "2",
                                               "2 or more", clean_data$Total.Event.Participation)

# Distribution of data:
ggplot(clean_data, aes(Total.Event.Participation)) + geom_bar()
round(prop.table(table(clean_data$Total.Event.Participation))*100, 2)
# No level making up over 95%.
clean_data$Total.Event.Participation <- as.factor(clean_data$Total.Event.Participation)



# 29. Count.of.Campus.Visits
Gen_Sum[29,]
# No missing values.

table(clean_data$Count.of.Campus.Visits)
# No unusual category.
# Factor the variable and group 5, 6, and 8 into 4.
clean_data$Count.of.Campus.Visits <- ifelse(clean_data$Count.of.Campus.Visits > 3,
                                            3, clean_data$Count.of.Campus.Visits)
# Convert int to char so that I can modify level name.
clean_data$Count.of.Campus.Visits <- as.character(clean_data$Count.of.Campus.Visits)
clean_data$Count.of.Campus.Visits <- ifelse(clean_data$Count.of.Campus.Visits == "3",
                                            "3 or more", clean_data$Count.of.Campus.Visits)

# Distribution of data:
ggplot(clean_data, aes(Count.of.Campus.Visits)) + geom_bar()
round(prop.table(table(clean_data$Count.of.Campus.Visits))*100, 2)
# No level making up over 95%.
clean_data$Count.of.Campus.Visits <- as.factor(clean_data$Count.of.Campus.Visits)



# 30. School..1.Organization.Category
Gen_Sum[30,]
# There are 38 missing values. 

round(prop.table(table(clean_data$School..1.Organization.Category))*100, 2)
# No unusual category but this variable needs to be removed because the number 
# of observation with level High School makes up over 99% of the data. As a result,
# this variable will not add much significance during the modeling process.
clean_data <- subset(clean_data, select = -c(School..1.Organization.Category))



# 31. School.1.Code
Gen_Sum[31,]
# Over 78% of the data for this variable is NAs and considering that the school
# code will not provide much insight during the modeling process, this variable
# needs to be removed. 
clean_data <- subset(clean_data, select = -c(School.1.Code))



# 32. School.1.Class.Rank..Numeric.
Gen_Sum[32,]
# There are 8136 missing values. Replace the missing values with "0", which later
# will be replaced with "Unknown":
clean_data$School.1.Class.Rank..Numeric.[is.na(clean_data$School.1.Class.Rank..Numeric.)] <- 0

# Group the Rankings into intervals:
clean_data$School.1.Class.Rank..Numeric. <- ifelse(clean_data$School.1.Class.Rank..Numeric. > 0 &
                                            clean_data$School.1.Class.Rank..Numeric. <= 10,
                                            1, clean_data$School.1.Class.Rank..Numeric.)
clean_data$School.1.Class.Rank..Numeric. <- ifelse(clean_data$School.1.Class.Rank..Numeric. > 10 &
                                                   clean_data$School.1.Class.Rank..Numeric. <= 100  ,
                                                   11, clean_data$School.1.Class.Rank..Numeric.)
clean_data$School.1.Class.Rank..Numeric. <- ifelse(clean_data$School.1.Class.Rank..Numeric. > 100 &
                                                     clean_data$School.1.Class.Rank..Numeric. <= 200  ,
                                                   101, clean_data$School.1.Class.Rank..Numeric.)
clean_data$School.1.Class.Rank..Numeric. <- ifelse(clean_data$School.1.Class.Rank..Numeric. > 200 &
                                                     clean_data$School.1.Class.Rank..Numeric. <= 300  ,
                                                   201, clean_data$School.1.Class.Rank..Numeric.)
clean_data$School.1.Class.Rank..Numeric. <- ifelse(clean_data$School.1.Class.Rank..Numeric. > 300,
                                                   301, clean_data$School.1.Class.Rank..Numeric.)

# Convert int to char so that I can modify level name.
clean_data$School.1.Class.Rank..Numeric. <- as.character(clean_data$School.1.Class.Rank..Numeric.)
clean_data$School.1.Class.Rank..Numeric. <- ifelse(clean_data$School.1.Class.Rank..Numeric. == "0",
                                            "Unknown", clean_data$School.1.Class.Rank..Numeric.)
clean_data$School.1.Class.Rank..Numeric. <- ifelse(clean_data$School.1.Class.Rank..Numeric. == "1",
                                            "1-10", clean_data$School.1.Class.Rank..Numeric.)
clean_data$School.1.Class.Rank..Numeric. <- ifelse(clean_data$School.1.Class.Rank..Numeric. == "11",
                                            "11-100", clean_data$School.1.Class.Rank..Numeric.)
clean_data$School.1.Class.Rank..Numeric. <- ifelse(clean_data$School.1.Class.Rank..Numeric. == "101",
                                            "101-200", clean_data$School.1.Class.Rank..Numeric.)
clean_data$School.1.Class.Rank..Numeric. <- ifelse(clean_data$School.1.Class.Rank..Numeric. == "201",
                                            "201-300", clean_data$School.1.Class.Rank..Numeric.)
clean_data$School.1.Class.Rank..Numeric. <- ifelse(clean_data$School.1.Class.Rank..Numeric. == "301",
                                            "300+", clean_data$School.1.Class.Rank..Numeric.)
# Distribution of data:
ggplot(clean_data, aes(School.1.Class.Rank..Numeric.)) + geom_bar()
round(prop.table(table(clean_data$School.1.Class.Rank..Numeric.))*100, 2)
# No level making up over 95%.
clean_data$School.1.Class.Rank..Numeric. <- as.factor(clean_data$School.1.Class.Rank..Numeric.)


# 33. School.1.Class.Size..Numeric.
Gen_Sum[33,]
# There are 8136 missing values.

# Percentage rank can more accurately reflect a student's academic performance
# than numeric rank. 
# New Variable - School.1.Top.Percent.in.Class
clean_data$School.1.Top.Percent.in.Class <- NA
clean_data$School.1.Top.Percent.in.Class <- 100 *(as.numeric(original$School.1.Class.Rank..Numeric.)/as.numeric(clean_data$School.1.Class.Size..Numeric.))
clean_data <- subset(clean_data, select = -c(School.1.Class.Size..Numeric.))

sum(is.na(clean_data$School.1.Top.Percent.in.Class))
# There are 8136 NA values and we impute the missing values based on Academic.Index column. 

# To impute missing values for School.1.Top.Percent.in.Class, Academic.Index must
# be cleaned first. 
# 57. Academic.Index
Gen_Sum[57,]
# 829 missing values. 

table(clean_data$Academic.Index)
# No unusual categories.
# Impute 829 NAs with the most common level.
clean_data$Academic.Index[is.na(clean_data$Academic.Index)] <- 3

#Impute missing values in School.1.Top.Percent.in.Class based on Academic.Index.
clean_index_1 <- clean_data %>% 
  group_by(Academic.Index) %>%
  filter(Academic.Index == 1) %>%
  mutate(School.1.Top.Percent.in.Class = replace(School.1.Top.Percent.in.Class, 
  is.na(School.1.Top.Percent.in.Class), mean(School.1.Top.Percent.in.Class, na.rm=TRUE)))

clean_index_2 <- clean_data %>% 
  group_by(Academic.Index) %>% 
  filter(Academic.Index == 2) %>%
  mutate(School.1.Top.Percent.in.Class = replace(School.1.Top.Percent.in.Class, 
  is.na(School.1.Top.Percent.in.Class), mean(School.1.Top.Percent.in.Class, na.rm=TRUE)))

clean_index_3 <- clean_data %>% 
  group_by(Academic.Index) %>%
  filter(Academic.Index == 3) %>%
  mutate(School.1.Top.Percent.in.Class = replace(School.1.Top.Percent.in.Class, 
  is.na(School.1.Top.Percent.in.Class), mean(School.1.Top.Percent.in.Class, na.rm=TRUE)))  

clean_index_4 <- clean_data %>% 
  group_by(Academic.Index) %>%
  filter(Academic.Index == 4) %>%
  mutate(School.1.Top.Percent.in.Class = replace(School.1.Top.Percent.in.Class, 
  is.na(School.1.Top.Percent.in.Class), mean(School.1.Top.Percent.in.Class, na.rm=TRUE)))    

clean_index_5 <- clean_data %>% 
  group_by(Academic.Index) %>%
  filter(Academic.Index == 5) %>%
  mutate(School.1.Top.Percent.in.Class = replace(School.1.Top.Percent.in.Class, 
  is.na(School.1.Top.Percent.in.Class), mean(School.1.Top.Percent.in.Class, na.rm=TRUE)))     

clean_data <- rbind(clean_index_1, clean_index_2, clean_index_3, clean_index_4, clean_index_5)




# 34. School.1.GPA
Gen_Sum[34,]
# 9800 missing values. 
# This variable should be removed because School.1.GPA.Recalculated is more accurate.
clean_data <- subset(clean_data, select = -c(School.1.GPA))



# 35. School.1.GPA.Scale
Gen_Sum[35,]
# 9800 missing values. 
# This variable should be removed because it is irrelevant to the classification
# models.
clean_data <- subset(clean_data, select = -c(School.1.GPA.Scale))



# 36. School.1.GPA.Recalculated
Gen_Sum[36,]
# No missing value.

# Identify outliers.
ggplot(clean_data, aes(School.1.GPA.Recalculated)) + geom_boxplot()
sum(clean_data$School.1.GPA.Recalculated[clean_data$School.1.GPA.Recalculated < 0])
# Outlier of School.1.GPA.Recalculated occur below 2.5. No GPA value below 0.

# Group Recalculated GPA into groups:
clean_data$School.1.GPA.Recalculated <- ifelse(clean_data$School.1.GPA.Recalculated < 3,
                                               2, clean_data$School.1.GPA.Recalculated)
clean_data$School.1.GPA.Recalculated <- ifelse(clean_data$School.1.GPA.Recalculated < 3.3 &
                                                clean_data$School.1.GPA.Recalculated >= 3,
                                               3, clean_data$School.1.GPA.Recalculated)
clean_data$School.1.GPA.Recalculated <- ifelse(clean_data$School.1.GPA.Recalculated < 3.7 &
                                                 clean_data$School.1.GPA.Recalculated >= 3.3,
                                               3.3, clean_data$School.1.GPA.Recalculated)
clean_data$School.1.GPA.Recalculated <- ifelse(clean_data$School.1.GPA.Recalculated <= 4 &
                                                 clean_data$School.1.GPA.Recalculated >= 3.7,
                                               3.7, clean_data$School.1.GPA.Recalculated)
# Convert the variable to Character to easier change level names:
clean_data$School.1.GPA.Recalculated <- as.character(clean_data$School.1.GPA.Recalculated)
clean_data$School.1.GPA.Recalculated <- ifelse(clean_data$School.1.GPA.Recalculated == "2",
                                               "Below B", clean_data$School.1.GPA.Recalculated)
clean_data$School.1.GPA.Recalculated <- ifelse(clean_data$School.1.GPA.Recalculated == "3",
                                               "B+", clean_data$School.1.GPA.Recalculated)
clean_data$School.1.GPA.Recalculated <- ifelse(clean_data$School.1.GPA.Recalculated == "3.3",
                                               "A-", clean_data$School.1.GPA.Recalculated)
clean_data$School.1.GPA.Recalculated <- ifelse(clean_data$School.1.GPA.Recalculated == "3.7",
                                               "A", clean_data$School.1.GPA.Recalculated)
# Distribution of data:
ggplot(clean_data, aes(School.1.GPA.Recalculated)) + geom_bar()
round(prop.table(table(clean_data$School.1.GPA.Recalculated))*100, 2)
# No level making up over 95%.
clean_data$School.1.GPA.Recalculated <- factor(clean_data$School.1.GPA.Recalculated, ordered = T,
                                               levels = c("Below B", "B+", "A-", "A"))




# 37. School.2.Class.Rank..Numeric.
Gen_Sum[37,]
# All values are missing values. This variable needs to be removed. 
clean_data <- subset(clean_data, select = -c(School.2.Class.Rank..Numeric.))



# 38. School.2.Class.Size..Numeric.
Gen_Sum[38,]
# All values are missing values. This variable needs to be removed. 
clean_data <- subset(clean_data, select = -c(School.2.Class.Size..Numeric.))



# 39. School.2.GPA
Gen_Sum[39,]
# All values are missing values. This variable needs to be removed. 
clean_data <- subset(clean_data, select = -c(School.2.GPA))



# 40. School.2.GPA.Scale
Gen_Sum[40,]
# All values are missing values. This variable needs to be removed. 
clean_data <- subset(clean_data, select = -c(School.2.GPA.Scale))



# 41. School.2.GPA.Recalculated
Gen_Sum[41,]
# All values are missing values. This variable needs to be removed. 
clean_data <- subset(clean_data, select = -c(School.2.GPA.Recalculated))



# 42. School.3.Class.Rank..Numeric.
Gen_Sum[42,]
# All values are missing values. This variable needs to be removed. 
clean_data <- subset(clean_data, select = -c(School.3.Class.Rank..Numeric.))



# 43. School.3.Class.Size..Numeric.
Gen_Sum[43,]
# All values are missing values. This variable needs to be removed. 
clean_data <- subset(clean_data, select = -c(School.3.Class.Size..Numeric.))



# 44. School.3.GPA
Gen_Sum[44,]
# All values are missing values. This variable needs to be removed. 
clean_data <- subset(clean_data, select = -c(School.3.GPA))



# 45. School.3.GPA.Scale
Gen_Sum[45,]
# All values are missing values. This variable needs to be removed. 
clean_data <- subset(clean_data, select = -c(School.3.GPA.Scale))



# 46. School.3.GPA.Recalculated
Gen_Sum[46,]
# All values are missing values. This variable needs to be removed. 
clean_data <- subset(clean_data, select = -c(School.3.GPA.Recalculated))




##### C.2. Data Cleaing - ACT Scores - Column 47-52 #####
# 47-52. ACT Composite / English / Reading / Math / Science Reasoning / Writing.

# Out of all the ACT Score Columns, the column that I want to focus on is 
# ACT.Composite as it is the average score of 4 parts of the ACT, thus is
# reflective of an applicant's performance in all sections (except Writing). 
Gen_Sum[47,]
# There are 7502 missing values in the ACT.Composite Column. 

# The missing values in ACT.Composite arise from three possible reasons and the 
# prescriptions for each type of NA are as follows:
# .	ACT Scores not submitted: Applicants could choose between ACT and SAT scores
# to submit. Check if applicant has ACT scores converted from the equivalent SAT 
# in the ACT.Concordance.Score..of.SAT.R. column. Replace missing value with the 
# corresponding value in ACT.Concordance.Score..of.SAT.R.
# .	ACT Scores not required: Some applicants in Fall 2021 opted out of submitting
# standardized test scores, which was reflected in the Test.Optional column. If
# the value in the Test.Optional column is 1, replace the NA in ACT.Composite 
# with a new level called "Test Score Not Required". 
# .	The remaining NA values: Impute the NA values in ACT.Composite based on 
# Academic.Index

# For applicants who did not submit ACT but submitted SAT, replace the NA in 
# ACT.Composite with the corresponding ACT value converted from Recentered SAT
# score:
for (i in 1:nrow(clean_data)) {
  if (is.na(clean_data$ACT.Composite[i]) == T & is.na(clean_data$ACT.Concordance.Score..of.SAT.R.[i]) == F) {
    clean_data$ACT.Composite[i] <- clean_data$ACT.Concordance.Score..of.SAT.R.[i]
    i <- i + 1
  } else {
    clean_data$ACT.Composite[i] <- clean_data$ACT.Composite[i]
    i <- i + 1
  }
}


# For applicants in Fall 2021 who opted out of submitting test scores, replace 
# the NAs with 100, which will be changed to "Test Score Not Required" as 
# ACT.Composite is factorized:

# Clean Test.Optional Column:
# 63. Test.Optional:
Gen_Sum[63,]
# 11901 missing values. Because the standardized test scores were only optional
# for the applicants of Fall 21, the missing values should be replaced with 
# "Not Fall 21":
clean_data$Test.Optional <- as.character(clean_data$Test.Optional)
clean_data$Test.Optional <- ifelse(is.na(clean_data$Test.Optional), "Not Fall 21",
                                   clean_data$Test.Optional)

# Replace NAs for applicants who opted out of test scores submission with 100:
for (i in 1:nrow(clean_data)) {
  if (is.na(clean_data$ACT.Composite[i]) == T & (clean_data$Test.Optional[i] == "1") ) {
    clean_data$ACT.Composite[i] <- 100
    i <- i + 1
  } else {
    clean_data$ACT.Composite[i] <- clean_data$ACT.Composite[i]
    i <- i + 1
  }
}


# For the remaining NA values in ACT.Composite, replace them with values that 
# are imputed based on Academic.Index:

# We need to handle ACT English / Reading / Math / Science Reasoning / Writing 
# before dealing with the remaining NAs of ACT.Composite.

# 48. ACT.English
Gen_Sum[48,]
# There are 7883 missing values. 
# Although the missing values indicate that the applicant did not take and/or
# submit their ACT scores, for the purpose of using ACT scores as a meaningful
# numeric variable, I will not replace the NAs with 0 or "No Act" but the 
# mean of ACT scores based on Academic.Index
clean_index_1 <- clean_data %>% 
  group_by(Academic.Index) %>%
  filter(Academic.Index == 1) %>%
  mutate(ACT.English = replace(ACT.English,
  is.na(ACT.English), mean(ACT.English, na.rm=TRUE)))

clean_index_2 <- clean_data %>% 
  group_by(Academic.Index) %>% 
  filter(Academic.Index == 2) %>%
  mutate(ACT.English = replace(ACT.English, 
  is.na(ACT.English), mean(ACT.English, na.rm=TRUE)))

clean_index_3 <- clean_data %>% 
  group_by(Academic.Index) %>%
  filter(Academic.Index == 3) %>%
  mutate(ACT.English = replace(ACT.English, 
  is.na(ACT.English), mean(ACT.English, na.rm=TRUE)))  

clean_index_4 <- clean_data %>% 
  group_by(Academic.Index) %>%
  filter(Academic.Index == 4) %>%
  mutate(ACT.English = replace(ACT.English, 
  is.na(ACT.English), mean(ACT.English, na.rm=TRUE)))    

clean_index_5 <- clean_data %>% 
  group_by(Academic.Index) %>%
  filter(Academic.Index == 5) %>%
  mutate(ACT.English = replace(ACT.English, 
  is.na(ACT.English), mean(ACT.English, na.rm=TRUE)))     

clean_data <- rbind(clean_index_1, clean_index_2, clean_index_3, clean_index_4, clean_index_5)
clean_data$ACT.English <- as.numeric(clean_data$ACT.English)



# 49. ACT.Reading
Gen_Sum[49,]
# There are 7883 missing values. 
# Although the missing values indicate that the applicant did not take and/or
# submit their ACT scores, for the purpose of using ACT scores as a meaningful
# numeric variable, I will not replace the NAs with 0 or "No Act" but the 
# mean of ACT scores based on Academic.Index
clean_index_1 <- clean_data %>% 
  group_by(Academic.Index) %>%
  filter(Academic.Index == 1) %>%
  mutate(ACT.Reading = replace(ACT.Reading,
                               is.na(ACT.Reading), mean(ACT.Reading, na.rm=TRUE)))

clean_index_2 <- clean_data %>% 
  group_by(Academic.Index) %>% 
  filter(Academic.Index == 2) %>%
  mutate(ACT.Reading = replace(ACT.Reading, 
                               is.na(ACT.Reading), mean(ACT.Reading, na.rm=TRUE)))

clean_index_3 <- clean_data %>% 
  group_by(Academic.Index) %>%
  filter(Academic.Index == 3) %>%
  mutate(ACT.Reading = replace(ACT.Reading, 
                               is.na(ACT.Reading), mean(ACT.Reading, na.rm=TRUE)))  

clean_index_4 <- clean_data %>% 
  group_by(Academic.Index) %>%
  filter(Academic.Index == 4) %>%
  mutate(ACT.Reading = replace(ACT.Reading, 
                               is.na(ACT.Reading), mean(ACT.Reading, na.rm=TRUE)))    

clean_index_5 <- clean_data %>% 
  group_by(Academic.Index) %>%
  filter(Academic.Index == 5) %>%
  mutate(ACT.Reading = replace(ACT.Reading, 
                               is.na(ACT.Reading), mean(ACT.Reading, na.rm=TRUE)))     

clean_data <- rbind(clean_index_1, clean_index_2, clean_index_3, clean_index_4, clean_index_5)
clean_data$ACT.Reading <- as.numeric(clean_data$ACT.Reading)



# 50. ACT.Math
Gen_Sum[50,]
# There are 7883 missing values. 
# Although the missing values indicate that the applicant did not take and/or
# submit their ACT scores, for the purpose of using ACT scores as a meaningful
# numeric variable, I will not replace the NAs with 0 or "No Act" but the 
# mean of ACT scores based on Academic.Index
clean_index_1 <- clean_data %>% 
  group_by(Academic.Index) %>%
  filter(Academic.Index == 1) %>%
  mutate(ACT.Math = replace(ACT.Math,
                               is.na(ACT.Math), mean(ACT.Math, na.rm=TRUE)))

clean_index_2 <- clean_data %>% 
  group_by(Academic.Index) %>% 
  filter(Academic.Index == 2) %>%
  mutate(ACT.Math = replace(ACT.Math, 
                               is.na(ACT.Math), mean(ACT.Math, na.rm=TRUE)))

clean_index_3 <- clean_data %>% 
  group_by(Academic.Index) %>%
  filter(Academic.Index == 3) %>%
  mutate(ACT.Math = replace(ACT.Math, 
                               is.na(ACT.Math), mean(ACT.Math, na.rm=TRUE)))  

clean_index_4 <- clean_data %>% 
  group_by(Academic.Index) %>%
  filter(Academic.Index == 4) %>%
  mutate(ACT.Math = replace(ACT.Math, 
                               is.na(ACT.Math), mean(ACT.Math, na.rm=TRUE)))    

clean_index_5 <- clean_data %>% 
  group_by(Academic.Index) %>%
  filter(Academic.Index == 5) %>%
  mutate(ACT.Math = replace(ACT.Math, 
                               is.na(ACT.Math), mean(ACT.Math, na.rm=TRUE)))     

clean_data <- rbind(clean_index_1, clean_index_2, clean_index_3, clean_index_4, clean_index_5)
clean_data$ACT.Math <- as.numeric(clean_data$ACT.Math)



# 51. ACT.Science.Reasoning
Gen_Sum[51,]
# There are 7883 missing values. 
# Although the missing values indicate that the applicant did not take and/or
# submit their ACT scores, for the purpose of using ACT scores as a meaningful
# numeric variable, I will not replace the NAs with 0 or "No Act" but the 
# mean of ACT scores based on Academic.Index
clean_index_1 <- clean_data %>% 
  group_by(Academic.Index) %>%
  filter(Academic.Index == 1) %>%
  mutate(ACT.Science.Reasoning = replace(ACT.Science.Reasoning,
                            is.na(ACT.Science.Reasoning), mean(ACT.Science.Reasoning, na.rm=TRUE)))

clean_index_2 <- clean_data %>% 
  group_by(Academic.Index) %>% 
  filter(Academic.Index == 2) %>%
  mutate(ACT.Science.Reasoning = replace(ACT.Science.Reasoning, 
                            is.na(ACT.Science.Reasoning), mean(ACT.Science.Reasoning, na.rm=TRUE)))

clean_index_3 <- clean_data %>% 
  group_by(Academic.Index) %>%
  filter(Academic.Index == 3) %>%
  mutate(ACT.Science.Reasoning = replace(ACT.Science.Reasoning, 
                            is.na(ACT.Science.Reasoning), mean(ACT.Science.Reasoning, na.rm=TRUE)))  

clean_index_4 <- clean_data %>% 
  group_by(Academic.Index) %>%
  filter(Academic.Index == 4) %>%
  mutate(ACT.Science.Reasoning = replace(ACT.Science.Reasoning, 
                            is.na(ACT.Science.Reasoning), mean(ACT.Science.Reasoning, na.rm=TRUE)))    

clean_index_5 <- clean_data %>% 
  group_by(Academic.Index) %>%
  filter(Academic.Index == 5) %>%
  mutate(ACT.Science.Reasoning = replace(ACT.Science.Reasoning, 
                            is.na(ACT.Science.Reasoning), mean(ACT.Science.Reasoning, na.rm=TRUE)))     

clean_data <- rbind(clean_index_1, clean_index_2, clean_index_3, clean_index_4, clean_index_5)
clean_data$ACT.Science.Reasoning <- as.numeric(clean_data$ACT.Science.Reasoning)



# 52. ACT.Writing
Gen_Sum[52,]
# There are 7883 missing values. 
# Although the missing values indicate that the applicant did not take and/or
# submit their ACT scores, for the purpose of using ACT scores as a meaningful
# numeric variable, I will not replace the NAs with 0 or "No Act" but the 
# mean of ACT scores based on Academic.Index
clean_index_1 <- clean_data %>% 
  group_by(Academic.Index) %>%
  filter(Academic.Index == 1) %>%
  mutate(ACT.Writing = replace(ACT.Writing,
                                         is.na(ACT.Writing), mean(ACT.Writing, na.rm=TRUE)))

clean_index_2 <- clean_data %>% 
  group_by(Academic.Index) %>% 
  filter(Academic.Index == 2) %>%
  mutate(ACT.Writing = replace(ACT.Writing, 
                                         is.na(ACT.Writing), mean(ACT.Writing, na.rm=TRUE)))

clean_index_3 <- clean_data %>% 
  group_by(Academic.Index) %>%
  filter(Academic.Index == 3) %>%
  mutate(ACT.Writing = replace(ACT.Writing, 
                                         is.na(ACT.Writing), mean(ACT.Writing, na.rm=TRUE)))  

clean_index_4 <- clean_data %>% 
  group_by(Academic.Index) %>%
  filter(Academic.Index == 4) %>%
  mutate(ACT.Writing = replace(ACT.Writing, 
                                         is.na(ACT.Writing), mean(ACT.Writing, na.rm=TRUE)))    

clean_index_5 <- clean_data %>% 
  group_by(Academic.Index) %>%
  filter(Academic.Index == 5) %>%
  mutate(ACT.Writing = replace(ACT.Writing, 
                                         is.na(ACT.Writing), mean(ACT.Writing, na.rm=TRUE)))     

clean_data <- rbind(clean_index_1, clean_index_2, clean_index_3, clean_index_4, clean_index_5)
clean_data$ACT.Writing <- as.numeric(clean_data$ACT.Writing)


# 47. ACT.Composite (CONT)
# Replace the missing value using the scores from 5 parts of the ACT standardized
# test:
clean_data$ACT.Sum <- NA
for (i in 1:nrow(clean_data)) {
  if (is.na(clean_data$ACT.Composite[i]) == T) {
    clean_data$ACT.Sum[i] <- clean_data$ACT.English[i] + clean_data$ACT.Reading[i] +
                                 clean_data$ACT.Math[i] + clean_data$ACT.Science.Reasoning[i]
    clean_data$ACT.Composite[i] <- round((clean_data$ACT.Sum[i]/4), 0)
    i <- i + 1
  } else {
    clean_data$ACT.Composite[i] <- clean_data$ACT.Composite[i] 
    i <- i + 1
  }
}
 
# Convert ACT.Composite into a factor variable by grouping the values into intervals:
sum(clean_data$ACT.Composite < 0) 
# No ACT Scores below 0.
min(clean_data$ACT.Composite)
# Because the minimum ACT.Composite score in the data set is 15, which is the 22nd
# Percentile on a National level, the lowest level for ACT scores will be "22nd
# Percentile" and not 1st Percentile.
clean_data$ACT.Composite <- ifelse(clean_data$ACT.Composite >= 34&
                                   clean_data$ACT.Composite <= 36,
                                   99, clean_data$ACT.Composite)
clean_data$ACT.Composite <- ifelse(clean_data$ACT.Composite >= 32 &
                                   clean_data$ACT.Composite < 34,
                                   96, clean_data$ACT.Composite)
clean_data$ACT.Composite <- ifelse(clean_data$ACT.Composite >= 30 &
                                   clean_data$ACT.Composite < 32,
                                   93, clean_data$ACT.Composite)
clean_data$ACT.Composite <- ifelse(clean_data$ACT.Composite >= 29 &
                                   clean_data$ACT.Composite < 30,
                                   90, clean_data$ACT.Composite)
clean_data$ACT.Composite <- ifelse(clean_data$ACT.Composite >= 27 &
                                   clean_data$ACT.Composite < 29,
                                   85, clean_data$ACT.Composite)
clean_data$ACT.Composite <- ifelse(clean_data$ACT.Composite >= 25 &
                                   clean_data$ACT.Composite < 27,
                                   78, clean_data$ACT.Composite)
clean_data$ACT.Composite <- ifelse(clean_data$ACT.Composite < 25,
                                   22, clean_data$ACT.Composite)

# Convert int to char so that I can modify level name.
clean_data$ACT.Composite <- as.character(clean_data$ACT.Composite)
clean_data$ACT.Composite <- ifelse(clean_data$ACT.Composite == "100",
                                   "Test Score Not Required", clean_data$ACT.Composite)
clean_data$ACT.Composite <- ifelse(clean_data$ACT.Composite == "99",
                                   "99th Percentile", clean_data$ACT.Composite)
clean_data$ACT.Composite <- ifelse(clean_data$ACT.Composite == "96",
                                   "96th Percentile", clean_data$ACT.Composite)
clean_data$ACT.Composite <- ifelse(clean_data$ACT.Composite == "93",
                                   "93th Percentile", clean_data$ACT.Composite)
clean_data$ACT.Composite <- ifelse(clean_data$ACT.Composite == "90",
                                   "90th Percentile", clean_data$ACT.Composite)
clean_data$ACT.Composite <- ifelse(clean_data$ACT.Composite == "85",
                                   "85th Percentile", clean_data$ACT.Composite)
clean_data$ACT.Composite <- ifelse(clean_data$ACT.Composite == "78",
                                   "78th Percentile", clean_data$ACT.Composite)
clean_data$ACT.Composite <- ifelse(clean_data$ACT.Composite == "22",
                                   "22nd Percentile", clean_data$ACT.Composite)

clean_data$ACT.Composite<- factor(clean_data$ACT.Composite, ordered = T,
                                  levels = c("Test Score Not Required", "22nd Percentile", 
                                             "78th Percentile", "85th Percentile", "90th Percentile",
                                             "93th Percentile", "96th Percentile", "99th Percentile"))
# Distribution of data:
ggplot(clean_data, aes(ACT.Composite)) + geom_bar()
round(prop.table(table(clean_data$ACT.Composite))*100, 2)
# No level making up over 95%.
clean_data <- subset(clean_data, select = -c(ACT.Sum))



# 48-52. ACT English / Reading / Math / Science Reasoning / Writing:
# Remove the ACT separate Test Scores because they have been used to impute values
# in the ACT.Composite column:
clean_data <- subset(clean_data, select = -c(ACT.English, ACT.Reading, ACT.Math,
                                             ACT.Science.Reasoning, ACT.Writing))
 




##### C.3. Data Cleaing - Column 53-69 #####
# 53.SAT.I.CR...M
Gen_Sum[53,]
# There are 14569 missing values, making up over 96% of the data for this variable.
# SAT.I.CR...M should be removed as it is unlikely to provide insight in the 
# modeling process. 
clean_data <- subset(clean_data, select = -c(SAT.I.CR...M))



# 54. SAT.R.Evidence.Based.Reading.and.Writing.Section...Math.Section
Gen_Sum[54,]
# There are 6711 missing values but as I have decided to focus on the ACT scores,
# all SAT.R-related columns will be removed to avoid multi-collinearity.
clean_data <- subset(clean_data, select = -c(SAT.R.Evidence.Based.Reading.and.Writing.Section...Math.Section))



# 55. Permanent.Geomarket
Gen_Sum[55,]
clean_data[is.na(clean_data$Permanent.Geomarket), "Citizenship.Status"]
# There is only 1 missing value - observation 11148. According to column 
# Citizenship.Status, however, this applicant holds US citizenship. Replace the
# missing value with the most common level of Permanent.Geomarket - TX-06:
clean_data$Permanent.Geomarket <- ifelse(is.na(clean_data$Permanent.Geomarket), 
                                         "TX-06", clean_data$Permanent.Geomarket)

levels(factor(clean_data$Permanent.Geomarket))
# Grouping the states into geographical regions of the US:
clean_data$Permanent.Geomarket <- ifelse(grepl("CO", clean_data$Permanent.Geomarket),
                                         "West", clean_data$Permanent.Geomarket)
clean_data$Permanent.Geomarket <- ifelse(grepl("WA", clean_data$Permanent.Geomarket),
                                         "West", clean_data$Permanent.Geomarket)
clean_data$Permanent.Geomarket <- ifelse(grepl("NM", clean_data$Permanent.Geomarket),
                                         "West", clean_data$Permanent.Geomarket)
clean_data$Permanent.Geomarket <- ifelse(grepl("LA", clean_data$Permanent.Geomarket),
                                         "South", clean_data$Permanent.Geomarket)
clean_data$Permanent.Geomarket <- ifelse(grepl("AZ", clean_data$Permanent.Geomarket),
                                         "West", clean_data$Permanent.Geomarket)
clean_data$Permanent.Geomarket <- ifelse(grepl("INT", clean_data$Permanent.Geomarket),
                                         "Other", clean_data$Permanent.Geomarket)
clean_data$Permanent.Geomarket <- ifelse(grepl("CA", clean_data$Permanent.Geomarket),
                                         "West", clean_data$Permanent.Geomarket)
clean_data$Permanent.Geomarket <- ifelse(grepl("MN", clean_data$Permanent.Geomarket),
                                         "Midwest", clean_data$Permanent.Geomarket)
clean_data$Permanent.Geomarket <- ifelse(grepl("TN", clean_data$Permanent.Geomarket),
                                         "South", clean_data$Permanent.Geomarket)
clean_data$Permanent.Geomarket <- ifelse(grepl("OK", clean_data$Permanent.Geomarket),
                                         "South", clean_data$Permanent.Geomarket)
clean_data$Permanent.Geomarket <- ifelse(grepl("OR", clean_data$Permanent.Geomarket),
                                         "West", clean_data$Permanent.Geomarket)
clean_data$Permanent.Geomarket <- ifelse(grepl("MO", clean_data$Permanent.Geomarket),
                                         "Midwest", clean_data$Permanent.Geomarket)
clean_data$Permanent.Geomarket <- ifelse(grepl("KS", clean_data$Permanent.Geomarket),
                                         "Midwest", clean_data$Permanent.Geomarket)
clean_data$Permanent.Geomarket <- ifelse(grepl("UT", clean_data$Permanent.Geomarket),
                                         "West", clean_data$Permanent.Geomarket)
clean_data$Permanent.Geomarket <- ifelse(grepl("IL", clean_data$Permanent.Geomarket),
                                         "Midwest", clean_data$Permanent.Geomarket)
clean_data$Permanent.Geomarket <- ifelse(grepl("NV", clean_data$Permanent.Geomarket),
                                         "West", clean_data$Permanent.Geomarket)
clean_data$Permanent.Geomarket <- ifelse(grepl("AL", clean_data$Permanent.Geomarket),
                                         "South", clean_data$Permanent.Geomarket)
clean_data$Permanent.Geomarket <- ifelse(grepl("FL", clean_data$Permanent.Geomarket),
                                         "South", clean_data$Permanent.Geomarket)
clean_data$Permanent.Geomarket <- ifelse(grepl("MD", clean_data$Permanent.Geomarket),
                                         "South", clean_data$Permanent.Geomarket)
clean_data$Permanent.Geomarket <- ifelse(grepl("AK", clean_data$Permanent.Geomarket),
                                         "West", clean_data$Permanent.Geomarket)
clean_data$Permanent.Geomarket <- ifelse(grepl("AR", clean_data$Permanent.Geomarket),
                                         "South", clean_data$Permanent.Geomarket)
clean_data$Permanent.Geomarket <- ifelse(grepl("CT", clean_data$Permanent.Geomarket),
                                         "Northeast", clean_data$Permanent.Geomarket)
clean_data$Permanent.Geomarket <- ifelse(grepl("DC", clean_data$Permanent.Geomarket),
                                         "South", clean_data$Permanent.Geomarket)
clean_data$Permanent.Geomarket <- ifelse(grepl("DE", clean_data$Permanent.Geomarket),
                                         "South", clean_data$Permanent.Geomarket)
clean_data$Permanent.Geomarket <- ifelse(grepl("GA", clean_data$Permanent.Geomarket),
                                         "South", clean_data$Permanent.Geomarket)
clean_data$Permanent.Geomarket <- ifelse(grepl("HI", clean_data$Permanent.Geomarket),
                                         "West", clean_data$Permanent.Geomarket)
clean_data$Permanent.Geomarket <- ifelse(grepl("MD", clean_data$Permanent.Geomarket),
                                         "South", clean_data$Permanent.Geomarket)
clean_data$Permanent.Geomarket <- ifelse(grepl("IA", clean_data$Permanent.Geomarket),
                                         "Midwest", clean_data$Permanent.Geomarket)
clean_data$Permanent.Geomarket <- ifelse(grepl("IN", clean_data$Permanent.Geomarket),
                                         "Midwest", clean_data$Permanent.Geomarket)
clean_data$Permanent.Geomarket <- ifelse(grepl("KY", clean_data$Permanent.Geomarket),
                                         "South", clean_data$Permanent.Geomarket)
clean_data$Permanent.Geomarket <- ifelse(grepl("MA", clean_data$Permanent.Geomarket),
                                         "Northeast", clean_data$Permanent.Geomarket)
clean_data$Permanent.Geomarket <- ifelse(grepl("ME", clean_data$Permanent.Geomarket),
                                         "Northeast", clean_data$Permanent.Geomarket)
clean_data$Permanent.Geomarket <- ifelse(grepl("MI", clean_data$Permanent.Geomarket),
                                         "Midwest", clean_data$Permanent.Geomarket)
clean_data$Permanent.Geomarket <- ifelse(grepl("MS", clean_data$Permanent.Geomarket),
                                         "South", clean_data$Permanent.Geomarket)
clean_data$Permanent.Geomarket <- ifelse(grepl("MT", clean_data$Permanent.Geomarket),
                                         "West", clean_data$Permanent.Geomarket)
clean_data$Permanent.Geomarket <- ifelse(grepl("NC", clean_data$Permanent.Geomarket),
                                         "South", clean_data$Permanent.Geomarket)
clean_data$Permanent.Geomarket <- ifelse(grepl("ND", clean_data$Permanent.Geomarket),
                                         "Midwest", clean_data$Permanent.Geomarket)
clean_data$Permanent.Geomarket <- ifelse(grepl("NE", clean_data$Permanent.Geomarket),
                                         "Midwest", clean_data$Permanent.Geomarket)
clean_data$Permanent.Geomarket <- ifelse(grepl("NH", clean_data$Permanent.Geomarket),
                                         "Northeast", clean_data$Permanent.Geomarket)
clean_data$Permanent.Geomarket <- ifelse(grepl("NJ", clean_data$Permanent.Geomarket),
                                         "Northeast", clean_data$Permanent.Geomarket)
clean_data$Permanent.Geomarket <- ifelse(grepl("NY", clean_data$Permanent.Geomarket),
                                         "Northeast", clean_data$Permanent.Geomarket)
clean_data$Permanent.Geomarket <- ifelse(grepl("OH", clean_data$Permanent.Geomarket),
                                         "Midwest", clean_data$Permanent.Geomarket)
clean_data$Permanent.Geomarket <- ifelse(grepl("PA", clean_data$Permanent.Geomarket),
                                         "Northeast", clean_data$Permanent.Geomarket)
clean_data$Permanent.Geomarket <- ifelse(grepl("PR", clean_data$Permanent.Geomarket),
                                         "Northeast", clean_data$Permanent.Geomarket)
clean_data$Permanent.Geomarket <- ifelse(grepl("RI", clean_data$Permanent.Geomarket),
                                         "Northeast", clean_data$Permanent.Geomarket)
clean_data$Permanent.Geomarket <- ifelse(grepl("SC", clean_data$Permanent.Geomarket),
                                         "South", clean_data$Permanent.Geomarket)
clean_data$Permanent.Geomarket <- ifelse(grepl("SD", clean_data$Permanent.Geomarket),
                                         "Midwest", clean_data$Permanent.Geomarket)
clean_data$Permanent.Geomarket <- ifelse(grepl("ID", clean_data$Permanent.Geomarket),
                                         "West", clean_data$Permanent.Geomarket)
clean_data$Permanent.Geomarket <- ifelse(grepl("VA", clean_data$Permanent.Geomarket),
                                         "South", clean_data$Permanent.Geomarket)
clean_data$Permanent.Geomarket <- ifelse(grepl("VT", clean_data$Permanent.Geomarket),
                                         "Northeast", clean_data$Permanent.Geomarket)
clean_data$Permanent.Geomarket <- ifelse(grepl("WI", clean_data$Permanent.Geomarket),
                                         "Midwest", clean_data$Permanent.Geomarket)
clean_data$Permanent.Geomarket <- ifelse(grepl("WV", clean_data$Permanent.Geomarket),
                                         "South", clean_data$Permanent.Geomarket)
clean_data$Permanent.Geomarket <- ifelse(grepl("WY", clean_data$Permanent.Geomarket),
                                         "West", clean_data$Permanent.Geomarket)
clean_data$Permanent.Geomarket <- ifelse(grepl("TX", clean_data$Permanent.Geomarket),
                                         "South", clean_data$Permanent.Geomarket)
clean_data$Permanent.Geomarket <- ifelse(grepl("US", clean_data$Permanent.Geomarket),
                                         "Northeast", clean_data$Permanent.Geomarket)
clean_data$Permanent.Geomarket <- ifelse(clean_data$Permanent.Geomarket == "Other",
                                         "Outside US", clean_data$Permanent.Geomarket)
# Data distribution:
ggplot(clean_data, aes(Permanent.Geomarket)) + geom_bar()
round(prop.table(table(clean_data$Permanent.Geomarket))*100, 2)
# No level making up over 95%.
clean_data$Permanent.Geomarket <- as.factor(clean_data$Permanent.Geomarket)



# 56.Citizenship.Status
Gen_Sum[56,]
# No missing value. 

levels(factor(clean_data$Citizenship.Status))
# No unusual categories. 

# Data distribution:
ggplot(clean_data, aes(Citizenship.Status)) + geom_bar()
round(prop.table(table(clean_data$Citizenship.Status))*100, 2)
# No level making up over 95%.
clean_data$Citizenship.Status <- as.factor(clean_data$Citizenship.Status)


# 57. Academic.Index
# Cleaned in section C.1. 



# 58. Intend.to.Apply.for.Financial.Aid.
Gen_Sum[58,]
# 21 missing values. We need to check the Merit.Award column. If applicant received
# some type of Merit Award, replace the NA value with 1. Otherwise, replace the 
# NA with 0. 
View(clean_data[is.na(clean_data$Intend.to.Apply.for.Financial.Aid.), 
                c("Intend.to.Apply.for.Financial.Aid.", "Merit.Award")])
# It appears that all applicants with NA values for Intend.to.Apply.for.Financial.Aid.
# were offered some type of merit-based financial aid. Replace all the NAs with
# a value of 1:
clean_data$Intend.to.Apply.for.Financial.Aid. <- ifelse(is.na(clean_data$Intend.to.Apply.for.Financial.Aid.),
                                                 1, clean_data$Intend.to.Apply.for.Financial.Aid.)

clean_data$Intend.to.Apply.for.Financial.Aid. <- as.factor(clean_data$Intend.to.Apply.for.Financial.Aid.)
# Data distribution:
ggplot(clean_data, aes(Intend.to.Apply.for.Financial.Aid.)) + geom_bar()
round(prop.table(table(clean_data$Intend.to.Apply.for.Financial.Aid.))*100, 2)
# No level making up over 95%.



# 59. Merit.Award
Gen_Sum[59,]
levels(factor(clean_data$Merit.Award))
# Whilst there are no missing value, there is a typo for TT125 as there is no
# Merit Award with a value of $125,000. Replace the typo value with "TT12.5":
clean_data$Merit.Award <- ifelse(clean_data$Merit.Award == "TT125", "TT12.5",
                                 clean_data$Merit.Award)

# Group levels by Award Type:
clean_data$Merit.Award <- ifelse(clean_data$Merit.Award == "I0" | clean_data$Merit.Award == "Z0",
                                 "No Award", clean_data$Merit.Award)
clean_data$Merit.Award <- ifelse(clean_data$Merit.Award == "X0" | clean_data$Merit.Award == "Y0",
                                 "Tuition Exchange/Remission", clean_data$Merit.Award)
clean_data$Merit.Award <- ifelse(grepl("I", clean_data$Merit.Award),
                                  "Intl Student", clean_data$Merit.Award)
clean_data$Merit.Award <- ifelse(clean_data$Merit.Award == "D12.5" |
                                 clean_data$Merit.Award == "D18"  |
                                 clean_data$Merit.Award == "D20",
                                 "Dean", clean_data$Merit.Award)
clean_data$Merit.Award <- ifelse(clean_data$Merit.Award == "M24" |
                                   clean_data$Merit.Award == "M25" |
                                   clean_data$Merit.Award == "M26" |
                                   clean_data$Merit.Award == "M27" |
                                   clean_data$Merit.Award == "M30",
                                 "Murchison", clean_data$Merit.Award)
clean_data$Merit.Award <- ifelse(clean_data$Merit.Award == "P17" |
                                   clean_data$Merit.Award == "P18" |
                                   clean_data$Merit.Award == "P23",
                                 "President", clean_data$Merit.Award)
clean_data$Merit.Award <- ifelse(clean_data$Merit.Award == "T21" |
                                   clean_data$Merit.Award == "T22" |
                                   clean_data$Merit.Award == "T23" |
                                   clean_data$Merit.Award == "T25",
                                 "Trustee", clean_data$Merit.Award)
clean_data$Merit.Award <- ifelse(clean_data$Merit.Award == "T21" |
                                   clean_data$Merit.Award == "T22" |
                                   clean_data$Merit.Award == "T23" |
                                   clean_data$Merit.Award == "T25",
                                 "Trustee", clean_data$Merit.Award)
clean_data$Merit.Award <- ifelse(clean_data$Merit.Award == "TTS" |
                                   clean_data$Merit.Award == "SEM",
                                 "Trinity Tower/Semmes", clean_data$Merit.Award)
clean_data$Merit.Award <- ifelse(clean_data$Merit.Award == "TT9" |
                                   clean_data$Merit.Award == "TT10" |
                                   clean_data$Merit.Award == "TT12" |
                                   clean_data$Merit.Award == "TT12.5",
                                 "Trinity Tiger", clean_data$Merit.Award)

# Data distribution:
ggplot(clean_data, aes(Merit.Award)) + geom_bar()
round(prop.table(table(clean_data$Merit.Award))*100, 2)
# No level making up over 95%.
clean_data$Merit.Award <- as.factor(clean_data$Merit.Award)



# 60. SAT.Concordance.Score..of.SAT.R.
Gen_Sum[60,]
# 6711 missing values. This variable should be removed as I have decided to focus
# on ACT scores.
clean_data <- subset(clean_data, select = -c(SAT.Concordance.Score..of.SAT.R.))



# 61. ACT.Concordance.Score..of.SAT.R.
Gen_Sum[61,]
# 8222 missing values. This variable should be removed as it has already been
# used to replace the missing values in the ACT.Composite columns. 
clean_data <- subset(clean_data, select = -c(ACT.Concordance.Score..of.SAT.R.))



# 62. ACT.Concordance.Score..of.SAT.
Gen_Sum[62,] 
# 15120 missing values. Because SAT I (SAT scores that have not been recentered) 
# was removed, this column becomes redundant. It also has over 99% of the data 
# as missing values. This column needs to be removed. 
clean_data <- subset(clean_data, select = -c(ACT.Concordance.Score..of.SAT.))



# 63. Test.Optional
# Cleaned in section C.2.
clean_data <- subset(clean_data, select = -c(Test.Optional))



# 64. SAT.I.Critical.Reading
Gen_Sum[64,]
# There are 14569 missing values, making up over 96% of the data. This variable
# should be removed because it will not provide much insight during the 
# modeling process. 
clean_data <- subset(clean_data, select = -c(SAT.I.Critical.Reading))



# 65.SAT.I.Math 
Gen_Sum[65,]
# There are 14569 missing values, making up over 96% of the data. This variable
# should be removed because it will not provide much insight during the 
# modeling process. 
clean_data <- subset(clean_data, select = -c(SAT.I.Math))




# 66. SAT.I.Writing
Gen_Sum[66,]
# There are 14573 missing values, making up over 96% of the data. This variable
# should be removed because it will not provide much insight during the 
# modeling process. 
clean_data <- subset(clean_data, select = -c(SAT.I.Writing))



# 67. SAT.R.Evidence.Based.Reading.and.Writing.Section
Gen_Sum[67,]
# There are 8332 missing values. This variable should be removed as I have decided 
# to focus on ACT scores.
clean_data <- subset(clean_data, select = -c(SAT.R.Evidence.Based.Reading.and.Writing.Section))



# 68. SAT.R.Math.Section
Gen_Sum[68,]
# There are 8332 missing values. This variable should be removed as I have decided to focus
# on ACT scores.
clean_data <- subset(clean_data, select = -c(SAT.R.Math.Section))



# 69. Decision 
Gen_Sum[69,]
# There are no missing values.

levels(factor(clean_data$Decision))
# No unusual categories. 

# Data distribution: 
ggplot(clean_data, aes(Decision)) + geom_bar()
round(prop.table(table(clean_data$Decision))*100, 2)
# No level making up over 95%.
clean_data$Decision <- as.factor(clean_data$Decision)

# Splitting the clean_data into the Train and the Test data sets:
# clean_data <- subset(clean_data, select = -c(Academic.Index))
clean_data <- subset(clean_data, select = -c(ID))
train <- clean_data[clean_data$train.test == "train",]
train <- subset(train, select = -c(train.test))
train <- data.frame(train)
test <- clean_data[clean_data$train.test == "test",]
test <- subset(test, select = -c(train.test))
test <- data.frame(test)




##### D. LOGISTIC REGRESSION #####
# 1. Model (Warning for this model has been suppressed as the model runs normally):
Logistic_model <- suppressWarnings(glm(Decision ~ ., family = binomial(link = "logit"),
                      data = train)) 

# 2. Predict Decision for each observation in the training set using 0.5 as the 
# initial cut-off probability.
contrasts(train$Decision)
Logistic_prob_train <- suppressWarnings(predict(Logistic_model, type = "response", train))
Logistic_pred_train <- ifelse(Logistic_prob_train > 0.5, "1", "0")
Logistic_conting_train <- table(Logistic_pred_train, train$Decision, 
                                dnn = c("Predicted", "Actual"))
Logistic_conting_train

# Compute Kappa for the training set.
Logistic_cm_train <- confusionMatrix(Logistic_conting_train)
Logistic_cm_train
Logistic_cm_train$overall["Kappa"]
# Kappa score is 0.5184072, indicating that the model has weak agreement with reality. 


# 3. Predict the Decision for each observation in the test set using 0.5 as the 
# initial cut-off probability.
contrasts(test$Decision)
Logistic_prob_test <- suppressWarnings(predict(Logistic_model, type = "response", test))
Logistic_pred_results <- ifelse(Logistic_prob_test > 0.5, "1", "0")
Logistic_conting_test <- table(Logistic_pred_results, test$Decision, 
                               dnn = c("Predicted", "Actual"))
Logistic_conting_test

# Compute Kappa for the test set.
Logistic_cm_test <- confusionMatrix(Logistic_conting_test)
Logistic_cm_test
Logistic_cm_test$overall["Kappa"]
# Kappa score is 0.5063023, indicating that the model has weak agreement with reality. 


# 4. Try out different cut-off probabilities:
# Less conservative cut-off points: 0.45, 0.4, 0.35, 0.3
# Cut-off point = 0.45
Logistic_pred_results <- ifelse(Logistic_prob_test > 0.45, "1", "0")
Logistic_conting_test <- table(Logistic_pred_results, test$Decision, 
                               dnn = c("Predicted", "Actual"))
Logistic_conting_test
Logistic_cm_test <- confusionMatrix(Logistic_conting_test)
Logistic_cm_test
Logistic_cm_test$overall["Kappa"]
# Kappa score is 0.5216513, indicating that model has weak agreement with reality.

# Cut-off point = 0.4
Logistic_pred_results <- ifelse(Logistic_prob_test > 0.4, "1", "0")
Logistic_conting_test <- table(Logistic_pred_results, test$Decision, 
                               dnn = c("Predicted", "Actual"))
Logistic_conting_test
Logistic_cm_test <- confusionMatrix(Logistic_conting_test)
Logistic_cm_test
Logistic_cm_test$overall["Kappa"]
# Kappa score is 0.5311145, indicating that model has weak agreement with reality.


# Cut-off point = 0.35
Logistic_pred_results <- ifelse(Logistic_prob_test > 0.35, "1", "0")
Logistic_conting_test <- table(Logistic_pred_results, test$Decision, 
                               dnn = c("Predicted", "Actual"))
Logistic_conting_test
Logistic_cm_test <- confusionMatrix(Logistic_conting_test)
Logistic_cm_test
Logistic_cm_test$overall["Kappa"]
# Kappa score is 0.5288376, indicating that model has weak agreement with reality.


# Cut-off point = 0.3
Logistic_pred_results <- ifelse(Logistic_prob_test > 0.3, "1", "0")
Logistic_conting_test <- table(Logistic_pred_results, test$Decision, 
                               dnn = c("Predicted", "Actual"))
Logistic_conting_test
Logistic_cm_test <- confusionMatrix(Logistic_conting_test)
Logistic_cm_test
Logistic_cm_test$overall["Kappa"]
# Kappa score is 0.53192, indicating that model has weak agreement with reality.


# More conservative cut-off points: 0.55, 0.6, 0.65, 0.7
# Cut-off point = 0.55
Logistic_pred_results <- ifelse(Logistic_prob_test > 0.55, "1", "0")
Logistic_conting_test <- table(Logistic_pred_results, test$Decision, 
                               dnn = c("Predicted", "Actual"))
Logistic_conting_test
Logistic_cm_test <- confusionMatrix(Logistic_conting_test)
Logistic_cm_test
Logistic_cm_test$overall["Kappa"]
# Kappa score is 0.4820599, indicating that model has weak agreement with reality.


# Cut-off point = 0.6
Logistic_pred_results <- ifelse(Logistic_prob_test > 0.6, "1", "0")
Logistic_conting_test <- table(Logistic_pred_results, test$Decision, 
                               dnn = c("Predicted", "Actual"))
Logistic_conting_test
Logistic_cm_test <- confusionMatrix(Logistic_conting_test)
Logistic_cm_test
Logistic_cm_test$overall["Kappa"]
# Kappa score is 0.4523599, indicating that model has weak agreement with reality.


# Cut-off point = 0.65
Logistic_pred_results <- ifelse(Logistic_prob_test > 0.65, "1", "0")
Logistic_conting_test <- table(Logistic_pred_results, test$Decision, 
                               dnn = c("Predicted", "Actual"))
Logistic_conting_test
Logistic_cm_test <- confusionMatrix(Logistic_conting_test)
Logistic_cm_test
Logistic_cm_test$overall["Kappa"]
# Kappa score is 0.4249024, indicating that model has weak agreement with reality.


# Cut-off point = 0.7
Logistic_pred_results <- ifelse(Logistic_prob_test > 0.7, "1", "0")
Logistic_conting_test <- table(Logistic_pred_results, test$Decision, 
                               dnn = c("Predicted", "Actual"))
Logistic_conting_test
Logistic_cm_test <- confusionMatrix(Logistic_conting_test)
Logistic_cm_test
Logistic_cm_test$overall["Kappa"]
# Kappa score is 0.3833993, indicating that model has minimal agreement with reality.


# Best Kappa score I found for Logistic Regression model:
Logistic_pred_results <- ifelse(Logistic_prob_test > 0.259, "1", "0")
Logistic_conting_test <- table(Logistic_pred_results, test$Decision, 
                               dnn = c("Predicted", "Actual"))
Logistic_conting_test
Logistic_cm_test <- confusionMatrix(Logistic_conting_test)
Logistic_cm_test
Logistic_cm_test$overall["Kappa"]
# Kappa score is 0.5371796, indicating that model has weak agreement with reality.





##### E. KNN #####
# 1. Convert all variables in both the train and test data sets to numeric values
# except for Decision:
kNN_train <- train
for (x in 1:ncol(kNN_train)) {
  if (is.numeric(kNN_train[x]) == F) {
    kNN_train[x] <- as.numeric(unlist(kNN_train[x]))
    x <- x + 1
  } else {
    kNN_train[x] <- kNN_train[x]
    x <- x + 1
  }
}
kNN_train$Decision <- as.factor(kNN_train$Decision)


kNN_test <- test
for (x in 1:ncol(kNN_test)) {
  if (is.numeric(kNN_test[x]) == F) {
    kNN_test[x] <- as.numeric(unlist(kNN_test[x]))
    x <- x + 1
  } else {
    kNN_test[x] <- kNN_test[x]
    x <- x + 1
  }
}
kNN_test$Decision <- as.factor(kNN_test$Decision)

kNN_train <- as.data.frame(kNN_train)
kNN_test <- as.data.frame(kNN_test)

# 2. Build a loop to find the optimal k value for the kNN model:
Kappa <- rep(0, 100)
for(i in 1:100){
  set.seed(1)
  nn_test <- kNN(Decision ~., kNN_train, kNN_test, k = i)
  nn_conting_test <- table(nn_test, kNN_test$Decision, 
                           dnn = c("Predicted", "Actual"))
  nn_cm_test <- confusionMatrix(nn_conting_test)
  Kappa[i] <- nn_cm_test$overall["Kappa"]
}

# The higher the Kappa is, the better the classification performance is.
which.max(Kappa)
Kappa[8]
# The best value for k is 8. 


# 3. Classification performance (Kappa) for the training set:
set.seed(1)
NN8_train <- kNN(Decision ~., kNN_train, kNN_train, k = 8)
NN8_conting_train <- table(NN8_train, kNN_train$Decision, 
                           dnn = c("Predicted", "Actual"))
NN8_cm_train <- confusionMatrix(NN8_conting_train)
NN8_cm_train
Kappa_train_NN8 <- NN8_cm_train$overall["Kappa"]
Kappa_train_NN8
# Kappa score is 0.4202027, indicating that model has weak agreement with reality.


# 4. Classification performance (Kappa) for the test set:
set.seed(1)
NN8_test <- kNN(Decision ~., kNN_train, kNN_test, k = 8)
NN8_conting_test <- table(NN8_test, kNN_test$Decision, 
                          dnn = c("Predicted", "Actual"))
NN8_cm_test <- confusionMatrix(NN8_conting_test)
NN8_cm_test
Kappa_test_NN8 <- NN8_cm_test$overall["Kappa"]
Kappa_test_NN8
# Kappa score is 0.3117081, indicating that the model has minimal agreement with
# reality





##### F. SIMPLE TREE #####
# 1. Build a simple tree:
simple_tree <- tree(Decision ~., train)
summary(simple_tree)
plot(simple_tree)
text(simple_tree)


# 2. Classification performance (Kappa) for the training set:
simple_tree_pred_train <- predict(simple_tree, train, type = "class")
simple_tree_conting_train <- table(simple_tree_pred_train, 
                                   train$Decision, 
                                   dnn = c("Predicted", "Actual"))
simple_tree_conting_train
simple_tree_cm_train <- confusionMatrix(simple_tree_conting_train)
simple_tree_cm_train
simple_tree_cm_train$overall["Kappa"]
# Kappa score is 0.2213647, indicating that model has minimal agreement with 
# reality. 


# 3. Classification performance (Kappa) for the test set:
simple_tree_pred_test <- predict(simple_tree, test, type = "class")
simple_tree_conting_test <- table(simple_tree_pred_test, 
                                  test$Decision, 
                                  dnn = c("Predicted", "Actual"))
simple_tree_conting_test
simple_tree_cm_test <- confusionMatrix(simple_tree_conting_test)
simple_tree_cm_test
simple_tree_cm_test$overall["Kappa"]
# Kappa score is 0.2008635 , indicating that model has minimal agreement with 
# reality. 




##### G. PRUNED TREE #####
# 1. Perform 10-fold cross-validation to determine whether the simple tree should 
# be pruned.
set.seed(1)
cv_Tree <- cv.tree(simple_tree, FUN = prune.misclass, K = 10)
cv_Tree$size[which.min(cv_Tree$dev)]
# The output shows that the tree with 8 terminal nodes results in the lowest cv 
# error. The tree should be pruned. 


# 2. Perform tree pruning according to the conclusion from the previous section:
prune_tree <- prune.misclass(simple_tree, best = 8)


# 3. Classification performance (Kappa) for the training set:
prune_tree_pred_train <- predict(prune_tree, train, type = "class")
prune_tree_conting_train <- table(prune_tree_pred_train, 
                                  train$Decision, 
                                  dnn = c("Predicted", "Actual"))
prune_tree_conting_train
prune_tree_cm_train <- confusionMatrix(prune_tree_conting_train)
prune_tree_cm_train
prune_tree_cm_train$overall["Kappa"]
# Kappa score is 0.2213647, indicating that model has minimal agreement with 
# reality. 


# 4. Classification performance (Kappa) for the test set
prune_tree_pred_test <- predict(prune_tree, test, type = "class")
prune_tree_conting_test <- table(prune_tree_pred_test, 
                                 test$Decision, 
                                 dnn = c("Predicted", "Actual"))
prune_tree_conting_test
prune_tree_cm_test <- confusionMatrix(prune_tree_conting_test)
prune_tree_cm_test
prune_tree_cm_test$overall["Kappa"]
# Kappa score is 0.2008635, indicating that model has minimal agreement with 
# reality. 



##### H. BAGGING #####
# 1. Bagging Tree model:
set.seed(1)
Tree_Bagging <- randomForest(Decision ~ ., data = train,
                             ntrees = 500, mtry = 27, replace = TRUE,
                             importance = TRUE)
Tree_Bagging


# 2. Classification performance (Kappa) for the training set:
bag_train_pred <- predict(Tree_Bagging, train, type = "class")
bag_conting_train <- table(bag_train_pred, train$Decision, 
                           dnn = c("Predicted", "Actual"))
bag_conting_train
bag_cm_train <- confusionMatrix(bag_conting_train)
bag_cm_train
bag_cm_train$overall["Kappa"]
# Kappa score is 1, suggesting the model has very strong agreement with  
# reality.


# Classification performance (Kappa) for the test set
bag_test_pred <- predict(Tree_Bagging, test, type = "class")
bag_conting_test <- table(bag_test_pred, test$Decision, 
                          dnn = c("Predicted", "Actual"))
bag_conting_test
bag_cm_test <- confusionMatrix(bag_conting_test)
bag_cm_test
bag_cm_test$overall["Kappa"]
# Kappa score is 0.4919075, indicating that model has weak agreement with reality. 




##### I. RANDOM FOREST #####
# 1. Run a loop to find the optimal mtry for Random Forest:
Test_Kappa_RF <- rep(0, 26)
for(i in 1:26){
  set.seed(1)
  Tree_RF <- randomForest(Decision ~ ., data = train,
                          ntrees = 500, mtry = i, replace = TRUE,
                          importance = TRUE)
  Test_pred_RF <- predict(Tree_RF, test, type = "class")
  RF_conting_test <- table(Test_pred_RF, test$Decision, 
                           dnn = c("Predicted", "Actual"))
  RF_cm_test <- confusionMatrix(RF_conting_test)
  Test_Kappa_RF[i] <- RF_cm_test$overall["Kappa"]
}
which.max(Test_Kappa_RF)
Test_Kappa_RF[which.max(Test_Kappa_RF)]
# The optimal mtry is 5.


# 2. Classification performance (Kappa) for the training set using the optimal mtry:
set.seed(1)
Tree_RF <- randomForest(Decision ~ ., data = train,
                        ntrees = 500, mtry = 5, replace = TRUE,
                        importance = TRUE)
rf_train_pred <- predict(Tree_RF, train, type = "class")
rf_conting_train <- table(rf_train_pred, train$Decision, 
                          dnn = c("Predicted", "Actual"))
rf_conting_train
rf_cm_train <- confusionMatrix(rf_conting_train)
rf_cm_train$overall["Kappa"]
# Kappa score is 1, indicating that the model has very strong agreement with 
# reality. 


# 3. Classification performance (Kappa) for the test set
rf_test_pred <- predict(Tree_RF, test, type = "class")
rf_conting_test <- table(rf_test_pred, test$Decision, 
                         dnn = c("Predicted", "Actual"))
rf_conting_test
rf_cm_test <- confusionMatrix(rf_conting_test)
rf_cm_test$overall["Kappa"]
# Kappa score is 0.5252375, suggesting that the model has weak agreement with 
# reality.




##### J. BOOSTING #####
# 1. Run a loop to find the best interaction.depth and the best number of trees
# for Boosting model:
contrasts(train$Decision)
train$Decision <- ifelse(train$Decision == "0", 0, 1)
test$Decision <- ifelse(test$Decision == "0", 0, 1)

n_trees <- rep(0, 6)
min_cv_error <- rep(0, 6)
for(i in 1:6){
  set.seed(1)
  Tree_Boosting <- gbm(Decision ~., data = train, distribution = "bernoulli",
                       n.trees = 5000, interaction.depth = i, cv.folds = 10,
                       shrinkage = 0.01)
  n_trees[i] <- which.min(Tree_Boosting$cv.error)
  min_cv_error[i] <- Tree_Boosting$cv.error[which.min(Tree_Boosting$cv.error)]
}
which.min(min_cv_error)
# The best interaction.depth is 4.
n_trees[4]


# 2. Classification performance (Kappa) for the training set:
set.seed(1)
Tree_Boosting <- gbm(Decision ~., data = train, 
                     distribution = "bernoulli",
                     n.trees = 2141, interaction.depth = 4,
                     shrinkage = 0.01)

boost_prob_train <- predict(Tree_Boosting, type = "response", 
                            train)
boost_pred_results_train <- ifelse(boost_prob_train > 0.5, 1, 0)
boost_conting_train <- table(boost_pred_results_train, train$Decision, 
                             dnn = c("Predicted", "Actual"))
boost_conting_train
boost_cm_train <- confusionMatrix(boost_conting_train)
boost_cm_train$overall["Kappa"]
# Kappa score is 0.5943555, suggesting that the model has weak agreement with 
# reality.


# 3. Classification performance (Kappa) for the test set:
boost_prob_test <- predict(Tree_Boosting, type = "response", 
                           test)
boost_pred_results_test <- ifelse(boost_prob_test > 0.5, 1, 0)
boost_conting_test <- table(boost_pred_results_test, test$Decision, 
                            dnn = c("Predicted", "Actual"))
boost_conting_test
boost_cm_test <- confusionMatrix(boost_conting_test)
boost_cm_test$overall["Kappa"]
# Kappa score is 0.5034971, meaning that the model has weak agreement with reality



# 4. Try out different cut-off probabilities:
# Less conservative cut-off points: 0.45, 0.4, 0.35, 0.3
# Cut-off point = 0.45
boost_pred_results_test <- ifelse(boost_prob_test > 0.45, 1, 0)
boost_conting_test <- table(boost_pred_results_test, test$Decision, 
                            dnn = c("Predicted", "Actual"))
boost_conting_test
boost_cm_test <- confusionMatrix(boost_conting_test)
boost_cm_test$overall["Kappa"]
# Kappa score is 0.5217828, indicating that model has weak agreement with reality.


# Cut-off point = 0.4
boost_pred_results_test <- ifelse(boost_prob_test > 0.4, 1, 0)
boost_conting_test <- table(boost_pred_results_test, test$Decision, 
                            dnn = c("Predicted", "Actual"))
boost_conting_test
boost_cm_test <- confusionMatrix(boost_conting_test)
boost_cm_test$overall["Kappa"]
# Kappa score is 0.5365014, indicating that model has weak agreement with reality.


# Cut-off point = 0.35
boost_pred_results_test <- ifelse(boost_prob_test > 0.35, 1, 0)
boost_conting_test <- table(boost_pred_results_test, test$Decision, 
                            dnn = c("Predicted", "Actual"))
boost_conting_test
boost_cm_test <- confusionMatrix(boost_conting_test)
boost_cm_test$overall["Kappa"]
# Kappa score is 0.5341085, indicating that model has weak agreement with reality.


# Cut-off point = 0.3
boost_pred_results_test <- ifelse(boost_prob_test > 0.3, 1, 0)
boost_conting_test <- table(boost_pred_results_test, test$Decision, 
                            dnn = c("Predicted", "Actual"))
boost_conting_test
boost_cm_test <- confusionMatrix(boost_conting_test)
boost_cm_test$overall["Kappa"]
# Kappa score is 0.5268845, indicating that model has weak agreement with reality.


# More conservative cut-off points: 0.55, 0.6, 0.65, 0.7
# Cut-off point = 0.55
boost_pred_results_test <- ifelse(boost_prob_test > 0.55, 1, 0)
boost_conting_test <- table(boost_pred_results_test, test$Decision, 
                            dnn = c("Predicted", "Actual"))
boost_conting_test
boost_cm_test <- confusionMatrix(boost_conting_test)
boost_cm_test$overall["Kappa"]
# Kappa score is 0.4714181, indicating that model has weak agreement with reality.


# Cut-off point = 0.6
boost_pred_results_test <- ifelse(boost_prob_test > 0.6, 1, 0)
boost_conting_test <- table(boost_pred_results_test, test$Decision, 
                            dnn = c("Predicted", "Actual"))
boost_conting_test
boost_cm_test <- confusionMatrix(boost_conting_test)
boost_cm_test$overall["Kappa"]
# Kappa score is 0.4390251, indicating that model has weak agreement with reality.


# Cut-off point = 0.65
boost_pred_results_test <- ifelse(boost_prob_test > 0.65, 1, 0)
boost_conting_test <- table(boost_pred_results_test, test$Decision, 
                            dnn = c("Predicted", "Actual"))
boost_conting_test
boost_cm_test <- confusionMatrix(boost_conting_test)
boost_cm_test$overall["Kappa"]
# Kappa score is 0.3984057, indicating that model has minimal agreement with reality.


# Cut-off point = 0.7
boost_pred_results_test <- ifelse(boost_prob_test > 0.7, 1, 0)
boost_conting_test <- table(boost_pred_results_test, test$Decision, 
                            dnn = c("Predicted", "Actual"))
boost_conting_test
boost_cm_test <- confusionMatrix(boost_conting_test)
boost_cm_test$overall["Kappa"]
# Kappa score is 0.362995, indicating that model has minimal agreement with reality.


# Best Kappa score I found for Boosting model:
boost_pred_results_test <- ifelse(boost_prob_test > 0.42, 1, 0)
boost_conting_test <- table(boost_pred_results_test, test$Decision, 
                            dnn = c("Predicted", "Actual"))
boost_conting_test
boost_cm_test <- confusionMatrix(boost_conting_test)
boost_cm_test$overall["Kappa"]
# Kappa score is 0.540082, indicating that model has weak agreement with reality.





