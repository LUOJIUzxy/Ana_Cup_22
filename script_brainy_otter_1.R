#loading packages

library(tidyverse)
library(readr)
library(dplyr)
library(lubridate)
library(caret)
library(mlr)

set.seed(2022)

#setting working directory and clearing working enviromenent
rm(list=ls())
#getwd()
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

transactions$CUSTOMER = str_replace_all(transactions$CUSTOMER,"\"","")

df_full = merge(transactions,geo,by="SALES_LOCATION")
df_full = left_join(df_full, customers, by= c("CUSTOMER", "COUNTRY"))
#view(df_full)

df_full2 = df_full
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
                               "WIN" = "1")) %>% 
  mutate(OFFER_STATUS = as.factor(OFFER_STATUS))

# set REV_CURRENT_YEAR as double 
df_full2$REV_CURRENT_YEAR <- as.double(df_full2$REV_CURRENT_YEAR)


# convert all nomimal values such as OWNERSHIP AND BUSINESS TYPE TO FACTORS
# all nominal values = "COUNTRY.x", "COUNTRY.y", "CURRENCY", "OWNERSHIP", "SALES_BRANCH", "SALES_OFFICE", "BUSINESS_TYPE", "OFFER_TYPE", "TECH", "PRICE_LIST", "SALES_LOCATION"

#nom_val = c("COUNTRY.x", "COUNTRY.y", "CURRENCY", "OWNERSHIP", "SALES_BRANCH", "SALES_OFFICE", "BUSINESS_TYPE", "OFFER_TYPE", "TECH", "PRICE_LIST", "SALES_LOCATION")                                                                                                                                                                                                                           
#df_full2$SALES_BRANCH

df_full2 <- df_full2 %>%
  mutate_at(vars("COUNTRY", "CURRENCY", "OWNERSHIP", "SALES_BRANCH", "SALES_OFFICE", "BUSINESS_TYPE", "OFFER_TYPE", "TECH", "PRICE_LIST", "SALES_LOCATION"), factor)


## doing the currency transformations
levels(df_full2$CURRENCY)
## looking up conversion ratios for Yuan/Pound/Dollar -> Euro 
# 7.19 "Chinese Yuan" = 1 Euro => /7.19
# 1 "Pound Sterling" = 1.19 Euro =>/0.8403
# 1.13 "US Dollar" = 1 Euro => /1.13
# 1 "Euro" = 1 "Euro"

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

df_full2$avg_rev <- rowMeans(df_full2[33:35], na.rm=TRUE)
df_full2$avg_rev

####################### TO DO ##########################


### reformatting the dates

# to not run all the code again
#df_full2_5 = df_full2
#df_full2 = df_full2_5

#clean the dates
for(i in 1:6){
  df_full2$CREATION_YEAR = str_replace(df_full2$CREATION_YEAR,".","")
}
df_full2$CREATION_YEAR <- as.numeric(df_full2$CREATION_YEAR)

df_full2$CREATION_YEAR

# transform the date format(SO_CREATED_DATE) into numerics
df_full2$SO_CREATED_DATE_YEAR = as.numeric(format(as_datetime(df_full2$SO_CREATED_DATE), "%Y"))
## calculate the cooperation time(creation year - SO created)
df_full2$COOP_TIME <- df_full2$SO_CREATED_DATE_YEAR - df_full2$CREATION_YEAR

######################## REPLACE MISSING VALUES ##############################
# COOP_TIME
avg_coop_time <- mean(df_full2$COOP_TIME, na.rm = TRUE)
avg_coop_time

df_full2[c("COOP_TIME")][is.na(df_full2[c("COOP_TIME")])] <- avg_coop_time

# avg_rev
avg_all_rev <- mean(df_full2$avg_rev, na.rm = TRUE)
avg_all_rev
df_full2[c("avg_rev")][is.na(df_full2[c("avg_rev")])] <- avg_all_rev

# OWNERSHIP
df_full2 %>% count(OWNERSHIP)
# Replace NA value with "No information"
df_full2[c("OWNERSHIP")][is.na(df_full2[c("OWNERSHIP")])] <- "No information"

######################## Correlation ######################################### 
# TODO: Save this correlation data into another variable, instead of df_full2
df_full3 <- df_full2

## transform type of 'TECH' from string to numeric
df_full3 <- df_full3 %>%
  mutate(TECH = recode(TECH, "F" = "0", 
                       "S" = "1",
                       "C" = "2"))
df_full3$TECH = as.numeric(df_full3$TECH)

## transform type of 'BUSINESS_TYPE' from string to numeric
df_full3 <- df_full3 %>% 
  mutate(BUSINESS_TYPE = recode(BUSINESS_TYPE, "New" = "N", 
                                "Mig" = "M",
                                "Exp" = "E"))

df_full3 <- df_full3 %>% 
  mutate(BUSINESS_TYPE = recode(BUSINESS_TYPE, "N" = "0", 
                                "M" = "1",
                                "E" = "2"))

df_full3$BUSINESS_TYPE = as.numeric(df_full3$BUSINESS_TYPE)

## transform type of 'OWNERSHIP' from string to numeric
df_full3 <- df_full3 %>% 
  mutate(OWNERSHIP = recode(OWNERSHIP, "Privately Owned/Publicly Traded" = "0", 
                            "Governmental" = "1"))

df_full3$OWNERSHIP = as.numeric(df_full3$OWNERSHIP)


feature_columns = c("OFFER_PRICE", "avg_rev", "TECH", "BUSINESS_TYPE", "OWNERSHIP", "COOP_TIME")
feature_columns_correlation = cor(df_full3[feature_columns])
feature_columns_correlation

high_cor_columns = findCorrelation(feature_columns_correlation)
high_cor_columns
########################## Modeling #######################################
columns_drop <- c(
  "SALES_LOCATION",
  "SO_ID",
  "MO_ID",
  "SERVICE_LIST_PRICE",
  "MATERIAL_COST",
  "SERVICE_COST",
  "PRICE_LIST",
  "ISIC",
  "MO_CREATED_DATE",
  "SO_CREATED_DATE",
  "OFFER_TYPE",
  "COSTS_PRODUCT_A",
  "COSTS_PRODUCT_B",
  "COSTS_PRODUCT_C",
  "COSTS_PRODUCT_D",
  "COSTS_PRODUCT_E",
  "COUNTRY",
  "SALES_OFFICE",
  "SALES_BRANCH",
  "REV_CURRENT_YEAR",
  "REV_CURRENT_YEAR.1",
  "REV_CURRENT_YEAR.2",
  "CREATION_YEAR",
  "CURRENCY",
  "rev_ycur_eu",
  "rev_y1_eu",
  "rev_y2_eu",
  "CUSTOMER",
  "END_CUSTOMER",
  "SO_CREATED_DATE_YEAR"
)

df_full2 <- df_full2 %>% 
  select(-columns_drop)

test = merge(df_full2, test_data, by.x= "TEST_SET_ID", by.y="id")
test

## find missing values
sapply(test, function(x) sum(is.na(x)))

## droping df_full$TEST_SET_ID
train_data <- subset(df_full2, select = -c(TEST_SET_ID))
sapply(train_data, function(x) sum(is.na(x)))

test <- subset(test, select = -c(TEST_SET_ID, prediction))

#dropping all the missing data
train_data <- train_data[complete.cases(train_data),]
sapply(train_data, function(x) sum(is.na(x)))


# Make a task
#task = makeRegrTask(data = train_data, target = "OFFER_STATUS")
task <- makeClassifTask(data = train_data,target = "OFFER_STATUS")
task

#learner = makeLearner("regr.cvglmnet")
#learner = makeLearner("regr.randomForest")
learner <- makeLearner("classif.logreg",predict.type = "response")
learner

#cv.logistic <- crossval(learner = learner, task = task,
#                        iters = 3,stratify = TRUE,measures = acc, show.info = F)

mod = train(learner, task)
mod

preds <- predict(mod, newdata = test)
preds

predictions = data.frame(id=test_data$id, prediction=preds$data$response)
predictions

######################   Export the Predictions   ################################
write.csv(predictions, file="predictions_brainy_otter_1.csv", row.names=FALSE)