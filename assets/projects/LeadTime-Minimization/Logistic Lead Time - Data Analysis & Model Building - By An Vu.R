#An file
library(reshape2)
library(tidyverse)
library(fastDummies)
library(car)
library(readxl)
library(zoo)
library(dplyr)
library(ggplot2)
library(knitr)
library(kableExtra)
options(scipen = 100)

Raw_Data <- read_excel("Regression_Final_Raw.xlsx", sheet = "Raw-Data")
Calendar <- read_excel("Regression_Final_Raw.xlsx", sheet = "Calendar")


Raw_Data$Intransit_Lead_Time <- NA
Raw_Data$Manufacturing_Time <- NA
Raw_Data$Quarter <- NA
Raw_Data$Year <- NA

#intransit and manufacturing
for (m in 1:nrow(Raw_Data)) {
  Raw_Data$Intransit_Lead_Time[m] = Raw_Data$`Receipt Date`[m] - Raw_Data$`Ship Date`[m]
  Raw_Data$Manufacturing_Time[m] =  Raw_Data$`Ship Date`[m] - Raw_Data$`PO Download Date`[m]
  #m = m + 1
}

#good lead times 
Raw_Data$LOBxOriginxShipMode <- paste(Raw_Data$LOB, Raw_Data$Origin, Raw_Data$`Ship Mode`)
Raw_Data_good_vals <- Raw_Data[Raw_Data$Intransit_Lead_Time > 0 & 
                                 is.na(Raw_Data$Intransit_Lead_Time) == FALSE, ]

Raw_Data_good_vals <- Raw_Data_good_vals[Raw_Data_good_vals$Manufacturing_Time >= 0 & 
                                           is.na(Raw_Data_good_vals$Manufacturing_Time) == FALSE, ]

Raw_Data_good_vals <- Raw_Data_good_vals[Raw_Data_good_vals$Manufacturing_Time < 100, ]

#est time for intransit and manu
Est_Time <- Raw_Data_good_vals %>% 
  group_by(LOBxOriginxShipMode) %>%
  summarize(Est_ILT = round(mean(Intransit_Lead_Time), 0),
            Est_MT = round(mean(Manufacturing_Time), 0))


#impute values
k <- 1
for (k in 1:nrow(Est_Time)) {
  Raw_Data$Intransit_Lead_Time <- ifelse(is.na(Raw_Data$Intransit_Lead_Time) == T & 
                                           Raw_Data$LOBxOriginxShipMode == Est_Time$LOBxOriginxShipMode[k],
                                         Est_Time$Est_ILT[k],
                                         Raw_Data$Intransit_Lead_Time) 
  Raw_Data$Intransit_Lead_Time <- ifelse(Raw_Data$Intransit_Lead_Time <= 0 & 
                                           Raw_Data$LOBxOriginxShipMode == Est_Time$LOBxOriginxShipMode[k],
                                         Est_Time$Est_ILT[k],
                                         Raw_Data$Intransit_Lead_Time) 
  Raw_Data$Manufacturing_Time <- ifelse(is.na(Raw_Data$Manufacturing_Time) == T &
                                          Raw_Data$LOBxOriginxShipMode == Est_Time$LOBxOriginxShipMode[k],
                                        Est_Time$Est_MT[k],
                                        Raw_Data$Manufacturing_Time)
  Raw_Data$Manufacturing_Time <- ifelse(Raw_Data$Manufacturing_Time < 0 &
                                          Raw_Data$LOBxOriginxShipMode == Est_Time$LOBxOriginxShipMode[k],
                                        Est_Time$Est_MT[k],
                                        Raw_Data$Manufacturing_Time)
  Raw_Data$Manufacturing_Time <- ifelse(Raw_Data$Manufacturing_Time >= 100 &
                                          Raw_Data$LOBxOriginxShipMode == Est_Time$LOBxOriginxShipMode[k],
                                        Est_Time$Est_MT[k],
                                        Raw_Data$Manufacturing_Time)
} 

#ocean transit
which(Raw_Data$`Ship Mode`=="OCEAN" & Raw_Data$Intransit_Lead_Time<=10)
Raw_Data[c(4073, 4324, 5468), "Intransit_Lead_Time"] <- Est_Time[3,2]


#ship and receipt date
n <- 1
for (n in 1:nrow(Est_Time)) {
  Raw_Data$`Ship Date` <- ifelse(is.na(Raw_Data$`Ship Date`) == T,
                                 as.Date(Raw_Data$`PO Download Date`) + Raw_Data$Manufacturing_Time,
                                 as.Date(Raw_Data$`Ship Date`))
  Raw_Data$`Receipt Date` <- ifelse(is.na(Raw_Data$`Receipt Date`) == T,
                                    as.Date(Raw_Data$`PO Download Date`) + 
                                      Raw_Data$Intransit_Lead_Time +  Raw_Data$Manufacturing_Time,
                                    as.Date(Raw_Data$`Receipt Date`))
}
Raw_Data$`Receipt Date` <- as.Date(Raw_Data$`Receipt Date`)
Raw_Data$`Ship Date` <- as.Date(Raw_Data$`Ship Date`)


#quarter and year
i<-1
for (i in 1:nrow(Calendar)) {
  Raw_Data$Quarter <- ifelse(Raw_Data$`Receipt Date` >= Calendar$Start_Date[i] 
                             & Raw_Data$`Receipt Date` <= Calendar$End_date[i],
                             Calendar$Quarter[i],
                             Raw_Data$Quarter) 
  Raw_Data$Year <- ifelse(Raw_Data$`Receipt Date` >= Calendar$Start_Date[i] 
                          & Raw_Data$`Receipt Date` <= Calendar$End_date[i],
                          Calendar$Year[i],
                          Raw_Data$Year) 
}


#one hot encoding
Raw_Data_onehot <- Raw_Data[ , c("LOB", "Origin", "Ship Mode", "Quarter")]
Raw_Data_onehot <- dummy_cols(Raw_Data_onehot, c("LOB", "Origin", "Ship Mode", "Quarter"))
Raw_Data_onehot <- select_if(Raw_Data_onehot, is.numeric)
Raw_Data_onehot$Intransit_Lead_Time <- Raw_Data$Intransit_Lead_Time
Raw_Data_onehot$Year<- Raw_Data$Year


cor_mtr <- round(cor(Raw_Data_onehot[c(1:17)]), 3)

Correlation_Matrix <- as.data.frame(cor_mtr) %>%
  arrange(desc(Intransit_Lead_Time))

get_upper_tri <- function(cor_matrix) {
  cor_matrix[lower.tri(cor_matrix)] <- " " 
  return(cor_matrix)
}

Correlation_Matrix <- get_upper_tri(Correlation_Matrix)
cor_mtr_melted <- melt(get_upper_tri(cor_mtr), na.rm = T)
cor_mtr_melted$value <- as.numeric(cor_mtr_melted$value)
cor_mtr_melted$value <- round(cor_mtr_melted$value, 2)
ggplot(data = cor_mtr_melted, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0,limit = c(-1,1), space = "Lab",
                       name="Pearson\nCorrelation") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                   size = 12, hjust = 1))+
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4, na.rm = T) +
  coord_fixed()
#took out the correlation plots code

cor_mtr_melted2 <- melt(cor_mtr)

Correlation_Table <- as.data.frame(filter(cor_mtr_melted2, Var1 == "Intransit_Lead_Time" & value < 1) %>% arrange(desc(value)))

Correlation_Table

Cor_Predictor_Pairs <- as.data.frame(filter(cor_mtr_melted2, 
                                            value >= 0.6  | value <= -0.6) %>%
                                       arrange(desc(value)))
Cor_Predictor_Pairs <- Cor_Predictor_Pairs[-c(1:7, 9:19, 21, 23, 25, 27, 29, 31, 33), ]
Cor_Predictor_Pairs

Raw_Data %>% filter(`Ship Mode` == "GROUND" | `Ship Mode` == "AIR") %>%
  group_by(Quarter, `Ship Mode`) %>%
  summarize(mean(Intransit_Lead_Time))

#modeling (all variables included)
model<- lm(Intransit_Lead_Time~., data = Raw_Data_onehot)
summary(model)


modelA <- lm(Intransit_Lead_Time ~ `LOB_Product A`:`Origin_Site A` + `LOB_Product B`:`Ship Mode_OCEAN` +
               `LOB_Product C`+`Origin_Site B`+ `Origin_Site C` + 
               `Origin_Site D`:`Ship Mode_GROUND`+ `Ship Mode_FASTBOAT`+
               `Ship Mode_AIR` + Quarter_Q1 +Quarter_Q3  + Quarter_Q4,
             data = Raw_Data_onehot) 
summary(modelA)
vif(modelA)


modelB <- lm(Intransit_Lead_Time ~ `LOB_Product A`:`Origin_Site A` + `LOB_Product B`:`Ship Mode_OCEAN` +
               `LOB_Product C` + `Origin_Site B`+  
               `Origin_Site D`:`Ship Mode_GROUND`+ `Ship Mode_FASTBOAT` +
               `Ship Mode_AIR` + Quarter_Q1 +Quarter_Q3  + Quarter_Q4,
             data = Raw_Data_onehot) 
summary(modelB)
vif(modelB)


modelC <- lm(Intransit_Lead_Time ~ `Origin_Site A`:`LOB_Product B`+`Ship Mode_GROUND`+ 
               `LOB_Product C` + `Origin_Site B`+  `Ship Mode_OCEAN` +
               `Origin_Site C`+ `Ship Mode_FASTBOAT` + 
               + Quarter_Q1 + Quarter_Q4 +Quarter_Q3,
             data = Raw_Data_onehot) 
summary(modelC)
vif(modelC)





