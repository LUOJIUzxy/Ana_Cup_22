library(tidyverse)
library(readr)
library(lubridate)

train_df_cust <- read_csv("C:/Users/zxy/Documents/Analytics_Cup/Training_Data_AC2022_pDULzZv/customers.csv", col_types = cols(CUSTOMER = col_character(), REV_CURRENT_YEAR = col_number()))
view(train_df_cust)

train_df_trans <- read_csv("C:/Users/zxy/Documents/Analytics_Cup/Training_Data_AC2022_pDULzZv/transactions.csv")

train_df_geo <- read_csv("C:/Users/zxy/Documents/Analytics_Cup/Training_Data_AC2022_pDULzZv/geo.csv")
view(train_df_geo)

train_df_geo[train_df_geo == "CH"] <- "Switzerland"
train_df_geo[train_df_geo == "FR"] <- "France"

train_df_trans$CUSTOMER = str_replace(train_df_trans$CUSTOMER,"\"","")
train_df_trans$CUSTOMER = str_replace(train_df_trans$CUSTOMER,"\"","")

df_full = merge(train_df_trans,train_df_geo,by="SALES_LOCATION")
df_full = full_join(df_full, train_df_cust, by="CUSTOMER")

view(df_full)

## find missing values
sapply(df_full, function(train_df_cust) sum(is.na(train_df_cust)))

## droping df_full$TEST_SET_ID
df_full <- subset(df_full, select = -c(TEST_SET_ID, END_CUSTOMER ))

#dropping all the missing data
df_full2 <- df_full[complete.cases(df_full),]

## setting to all caps 
df_full2$OFFER_STATUS <- toupper(df_full2$OFFER_STATUS)
df_full2$OFFER_STATUS


nom_val = c("COUNTRY.x", "COUNTRY.y", "CURRENCY", "OWNERSHIP", "SALES_BRANCH", "SALES_OFFICE", "BUSINESS_TYPE", "OFFER_TYPE", "TECH", "PRICE_LIST", "SALES_LOCATION")                                                                                                                                                                                                                           


for(i in nom_val)
{df_full2$i <- as.factor(df_full2$i)}

df_full2 <- df_full2 %>% mutate_at(vars(CREATION_YEAR), as_datetime)

lct <- Sys.getlocale("LC_TIME")
Sys.setlocale("LC_TIME", "C")
lct

#clean the dates
df_full2$CREATION_YEAR = str_replace_all(df_full2$CREATION_YEAR,"/",".")
df_full2$CREATION_YEAR = str_replace_all(df_full2$CREATION_YEAR,"[.]","-")
view(df_full2$CREATION_YEAR)
#reformat the CREATION_YEAR

df_full2 <- df_full2 %>% mutate_at(vars(CREATION_YEAR), as_datetime)

date_format = "%d-%m-%Y"

df_full2$CREATION_YEAR = as.Date(df_full2$CREATION_YEAR, date_format)
#df_full2$CREATION_YEAR = as.Date("df_full2$CREATION_YEAR", '%d-%m-%Y')
df_full2$CREATION_YEAR
str(df_full2$CREATION_YEAR)

#clean the dates
for(i in 1:6){
  df_full2$CREATION_YEAR = str_replace(df_full2$CREATION_YEAR,".","")
}
df_full2$CREATION_YEAR <- as.numeric(df_full2$CREATION_YEAR)

df_full2$CREATION_YEAR

view(df_full2$CREATION_YEAR)
