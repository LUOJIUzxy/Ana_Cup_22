#loading packages

library(tidyverse)
library(readr)
library(dplyr)
library(lubridate)
library(caret)
library(mlr)

#setting working directory and clearing working enviromenent
rm(list=ls())
getwd()
setwd('C:/Users/zxy/Documents/Analytics_Cup')

#laoding in all the data
test_data <- read_csv("Test_Data_AC2022/pub_f6Xd8II.csv")
customers <- read_csv("Training_Data_AC2022/customers.csv", col_types = cols(CUSTOMER = col_character(),REV_CURRENT_YEAR = col_number()))
geo <- read_csv("Training_Data_AC2022/geo.csv")
sumbission_random <- read_csv("Training_Data_AC2022/submission_random.csv")
transactions <- read_csv("Training_Data_AC2022/transactions.csv",  col_types = cols(CUSTOMER = col_character()))

#merging data

geo[geo == "CH"] <- "Switzerland"
geo[geo == "FR"] <- "France"

transactions$CUSTOMER = str_replace(transactions$CUSTOMER,"\"","")
transactions$CUSTOMER = str_replace(transactions$CUSTOMER,"\"","")


df_full = merge(transactions,geo,by="SALES_LOCATION")
df_full = full_join(df_full, customers, by= "CUSTOMER")
#view(df_full)


## find missing values
sapply(df_full, function(x) sum(is.na(x)))

## droping df_full$TEST_SET_ID
df_full <- subset(df_full, select = -c(TEST_SET_ID, END_CUSTOMER ))

#dropping all the missing data
df_full2 <- df_full[complete.cases(df_full),]

##OFFER_STATUS setting to all caps 
df_full2$OFFER_STATUS <- toupper(df_full2$OFFER_STATUS)
#df_full2$OFFER_STATUS

##OFFER_STATUS "WIN" = "WON", "LOSE" = "LOST"
df_full2$OFFER_STATUS = str_replace(df_full2$OFFER_STATUS,"WON","WIN")
df_full2$OFFER_STATUS = str_replace(df_full2$OFFER_STATUS,"LOSE","LOST")
#df_full2$OFFER_STATUS
#str(df_full2$OFFER_STATUS)

## convert offer status to binary

#as.factor works
df_full2$OFFER_STATUS <- as.factor(df_full2$OFFER_STATUS)

#recoding to binary 
df_full2 <- df_full2 %>% 
  mutate(OFFER_STATUS = recode(OFFER_STATUS, 
                               "LOST" = "0", 
                               "WIN" = "1"))
#df_full2$OFFER_STATUS

# set REV_CURRENT_YEAR as double 
df_full2$REV_CURRENT_YEAR <- as.double(df_full2$REV_CURRENT_YEAR)


# convert all nomimal values such as OWNERSHIP AND BUSINESS TYPE TO FACTORS
# all nominal values = "COUNTRY.x", "COUNTRY.y", "CURRENCY", "OWNERSHIP", "SALES_BRANCH", "SALES_OFFICE", "BUSINESS_TYPE", "OFFER_TYPE", "TECH", "PRICE_LIST", "SALES_LOCATION"
nom_val = c("COUNTRY.x", "COUNTRY.y", "CURRENCY", "OWNERSHIP", "SALES_BRANCH", "SALES_OFFICE", "BUSINESS_TYPE", "OFFER_TYPE", "TECH", "PRICE_LIST", "SALES_LOCATION")                                                                                                                                                                                                                           
df_full2$SALES_BRANCH

df_full2 <- df_full2 %>%
  mutate_at(vars("COUNTRY.x", "COUNTRY.y", "CURRENCY", "OWNERSHIP", "SALES_BRANCH", "SALES_OFFICE", "BUSINESS_TYPE", "OFFER_TYPE", "TECH", "PRICE_LIST", "SALES_LOCATION"), factor)


## doing the currency transformations
levels(df_full2$CURRENCY)
## looking up conversion ratios for Yuan/Pound/Dollar -> Euro 
# 7.19 YUAN = 1 Euro => /7.19
# 1 Pound = 1.19 Euro => *1.19 OR /0.8403
# 1.13 dollar = 1 Euro => /1.13


#set all 3 collums to euro
df_full2<- df_full2 %>%
  mutate(rev_ycur_eu = case_when(CURRENCY == "Chinese Yuan" ~ df_full2$REV_CURRENT_YEAR/7.19,
                                 CURRENCY == "Euro" ~ df_full2$REV_CURRENT_YEAR,
                                 CURRENCY == "US Dollar" ~ df_full2$REV_CURRENT_YEAR/1.13,
                                 CURRENCY == "Pound Sterling" ~ df_full2$REV_CURRENT_YEAR/0.8403))
df_full2<- df_full2 %>%
  mutate(rev_y1_eu = case_when(CURRENCY == "Chinese Yuan" ~ df_full2$REV_CURRENT_YEAR.1/7.19,
                               CURRENCY == "Euro" ~ df_full2$REV_CURRENT_YEAR.1,
                               CURRENCY == "US Dollar" ~ df_full2$REV_CURRENT_YEAR.1/1.13,
                               CURRENCY == "Pound Sterling" ~ df_full2$REV_CURRENT_YEAR.1/0.8403))

df_full2<- df_full2 %>%
  mutate(rev_y2_eu = case_when(CURRENCY == "Chinese Yuan" ~ df_full2$REV_CURRENT_YEAR.2/7.19,
                               CURRENCY == "Euro" ~ df_full2$REV_CURRENT_YEAR.2,
                               CURRENCY == "US Dollar" ~ df_full2$REV_CURRENT_YEAR.2/1.13,
                               CURRENCY == "Pound Sterling" ~ df_full2$REV_CURRENT_YEAR.2/0.8403))





####################### TO DO ##########################

df_full2$avg_rev <- rowMeans(df_full2[32:34], na.rm=TRUE)
df_full2$avg_rev


####################### TO DO ##########################



### reformatting the dates

# to not run all the code again
#df_full2_5 = df_full2
df_full2 = df_full2_5

#clean the dates
for(i in 1:6){
  df_full2$CREATION_YEAR = str_replace(df_full2$CREATION_YEAR,".","")
}
df_full2$CREATION_YEAR <- as.numeric(df_full2$CREATION_YEAR)

df_full2$CREATION_YEAR


####################### TO DO ##########################

# PART BELOW DOESNT WORK YET

####################### TO DO ##########################


#figure out why this string is not in a standard unambiguous format??

#different idea to just remove the first 6 chars in date to be only left wit the year
#df_full2$CREATION_YEAR = str_replace(df_full2$CREATION_YEAR,".","")



#checking if its all good
df_full2$CREATION_YEAR
str(df_full2$CREATION_YEAR)


#date_format = "%d-%m-%Y"
#df_full2$MO_CREATED_DATE <- as.Date("df_full2$MO_CREATED_DATE",)
#df_full2$SO_CREATED_DATE <- as.Date("df_full2$SO_CREATED_DATE",)

######################## Correlation ######################################### 

# transform the date formate(SO_CREATED_DATE) into numerics
SO_CREATED_DATE_YEAR = as.numeric(format(as.Date(df_full2$SO_CREATED_DATE), "%Y"))

## calculate the cooperation time(creation year - SO created)
df_full2$COOP_TIME <- SO_CREATED_DATE_YEAR - df_full2$CREATION_YEAR

df_full2$TECH = as.numeric(df_full2$TECH)

## transform type of 'TECH' from string to numeric
df_full2 <- df_full2 %>%
  mutate(TECH = recode(TECH, "F" = "0", 
                             "S" = "1",
                              "C" = "2"))
df_full2$TECH = as.numeric(df_full2$TECH)

## transform type of 'BUSINESS_TYPE' from string to numeric
df_full2 <- df_full2 %>% 
  mutate(BUSINESS_TYPE = recode(BUSINESS_TYPE, "New" = "N", 
                       "Mig" = "M",
                       "Exp" = "E"))

df_full2 <- df_full2 %>% 
  mutate(BUSINESS_TYPE = recode(BUSINESS_TYPE, "N" = "0", 
                                "M" = "1",
                                "E" = "2"))

df_full2$BUSINESS_TYPE = as.numeric(df_full2$BUSINESS_TYPE)

## transform type of 'OWNERSHIP' from string to numeric
df_full2 <- df_full2 %>% 
  mutate(OWNERSHIP = recode(OWNERSHIP, "Privately Owned/Publicly Traded" = "0", 
                                "Governmental" = "1"))

df_full2$OWNERSHIP = as.numeric(df_full2$OWNERSHIP)


feature_columns = c("OFFER_PRICE", "avg_rev", "TECH", "BUSINESS_TYPE", "OWNERSHIP", "COOP_TIME")
feature_columns_correlation = cor(df_full2[feature_columns])
feature_columns_correlation

high_cor_columns = findCorrelation(feature_columns_correlation)
high_cor_columns

####################### Modeling #######################################

tc <- trainControl("repeatedcv", number=10, repeats=10, classProbs=TRUE, savePred=T) 
InTrain<-createDataPartition(y=df_full2$OFFER_STATUS,p=0.3,list=FALSE)
training1<-df_full2[InTrain,]

# Missing family status causes drop from 14 % -> 17%
rf_model<-train(OFFER_STATUS~OFFER_PRICE+BUSINESS_TYPE+TECH+OWNERSHIP+COOP_TIME,data=df_full2,method="rf",
                trControl=trainControl(method="cv",number=5),
                prox=TRUE,allowParallel=TRUE)

print(rf_model)
print(rf_model$finalModel)
