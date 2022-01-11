library(tidyverse)
library(tidymodels)

set.seed(2022)
options(dplyr.width = Inf) # show all columns when printing to console

# read csv
cust <- read_csv("customers.csv")
trans <- read_csv("transactions.csv")
geo <- read_csv("geo.csv")

# deal with key issues
trans <- trans %>% mutate(CUSTOMER = as.integer(gsub("\"", "", CUSTOMER)))
trans <- trans %>% mutate(END_CUSTOMER = toupper(END_CUSTOMER))
trans <- trans %>% mutate(END_CUSTOMER = case_when(
  is.na(END_CUSTOMER) ~ as.integer("a"), #unknown end customer
  END_CUSTOMER == "NO" ~ as.integer("a"), #unknown end customer != customer TODO maybe handle this differently
  END_CUSTOMER == "YES" ~ CUSTOMER, #customer is end customer
  TRUE ~ as.integer(END_CUSTOMER) 
))
cust <- cust %>% mutate(CUSTOMER = as.integer(CUSTOMER))
geo <- geo %>% mutate(COUNTRY = case_when(
  COUNTRY=="CH" ~ "Switzerland", 
  COUNTRY=="FR" ~ "France",
  TRUE ~ "NA")
  )

# left join all three csv files
df <- trans %>% left_join(geo) %>% left_join(cust)
#join cust a second time on END_CUSTOMER
names(cust) <- paste0("END_", names(cust))
df <- df %>% left_join(cust, c("END_CUSTOMER" = "END_CUSTOMER", "COUNTRY" = "END_COUNTRY"))

# check missing values from different attributes
df %>% summarize_all(function(x) sum(is.na(x)))

# split train and test
train <- df %>% filter(is.na(TEST_SET_ID)) 
test <- df %>% anti_join(train) %>% select(-OFFER_STATUS)

#test ids for output file
output_ids <- test$TEST_SET_ID

#  Check whether the test set matches the submission template
submission_template <- read_csv('submission_random.csv')

template_ids <- submission_template %>% arrange(id) %>% pull(id)
test_ids <- test %>% arrange(TEST_SET_ID) %>% pull(TEST_SET_ID)
all(template_ids == test_ids) #true if all the same, otherwise falls

#read training set
#trans_training <- read_csv("transactions.csv")
#trans_training <- trans_training %>% filter(is.na(TEST_SET_ID))
#remove test_ID column from training set
#trans_training <- trans_training %>% subset(select= - c(TEST_SET_ID))
### DATA PREPARATION on geo data
#if country were unknown we can't identify customer
#geo <- geo %>% filter(!is.na(COUNTRY))

train <- train %>% mutate(
  OFFER_STATUS = as.factor(case_when(
    toupper(OFFER_STATUS) == "LOSE" ~ 0,
    toupper(OFFER_STATUS) == "LOST" ~ 0,
    TRUE ~ 1)
  ))

train <- train %>% select(-REV_CURRENT_YEAR, -END_REV_CURRENT_YEAR, -TEST_SET_ID)

#train <- train %>% select(MO_ID, SO_ID, 
#                          TECH, OFFER_TYPE, BUSINESS_TYPE,
#                          CREATION_YEAR, END_CREATION_YEAR,
#                          MO_CREATED_DATE, SO_CREATED_DATE,
#                          OFFER_STATUS)

### DATA PREPARATION with recipe
library(lubridate)
library(quantmod)

rec <- recipe(
  OFFER_STATUS ~ ., data = train) %>%
  # step_mutate(OFFER_ID = ifelse(is.na(SO_ID), MO_ID, SO_ID), role = "ID") %>%
  update_role(MO_ID, SO_ID, new_role = "ID") %>%
  step_mutate_at(all_nominal(), -all_outcomes(), -has_role("ID"), fn = toupper) %>%
 
  #binning
  step_mutate(OFFER_PRICE = case_when(
    is.na(OFFER_PRICE) ~ "NA",
    OFFER_PRICE < 6000 ~ "<6k",
    TRUE ~ ">=6k"
  )) %>% 
  
  #step_mutate(OFFER_STATUS = as.factor(case_when(
  #  toupper(OFFER_STATUS) == "LOSE" ~ 0,
  #  toupper(OFFER_STATUS) == "LOST" ~ 0,
  #  TRUE ~ 1)), skip = TRUE) %>%
  #impute missing dates with default value
  step_mutate_at(MO_CREATED_DATE,SO_CREATED_DATE, fn = ~replace_na(.,"0:0:0 0:0")) %>% 
  
  #data types of dates
  step_mutate_at(CREATION_YEAR, END_CREATION_YEAR, fn = function(x) parse_date_time(x,orders="dmY") %>% year()) %>%
  step_mutate_at(MO_CREATED_DATE, SO_CREATED_DATE, fn = function(x) parse_date_time(gsub(pattern="[[:punct:]]", ":", x),orders=c("d:m:Y H:M", "Y:m:d H:M:S"))) %>%
    
  #exchange currencies to EUR
  #create exchange rate column and multiply prices
  # step_mutate_at(CURRENCY, END_CURRENCY, fn= function(x) case_when(
  #   (x == "EURO") ~ "EUR",
  #   (x == "CHINESE YUAN") ~ "CNY",
  #   (x == "US DOLLAR") ~ "USD",
  #   (x == "POUND STERLING") ~ "GBP",
  #   TRUE ~ "NA"
  # )) %>% 
  # step_mutate(EXCHANGE_RATE = getFX(str_replace_all(paste("EUR/",CURRENCY), " ", "")
  #                                   , from = format(SO_CREATED_DATE %>% , format="%Y-%M-%D"))) %>% 
  # 
  #binary dependent
  
  #remove cols
  #step_select(-REV_CURRENT_YEAR, -END_REV_CURRENT_YEAR, -TEST_SET_ID) %>%  # REV_CURRENT_YEAR.1 is just a rounded number, correlation = 1
  step_impute_mean(all_numeric_predictors(), -all_outcomes()) %>%
  step_novel(all_nominal(), -all_outcomes(), -has_role("ID"), new_level="new") %>%
  step_unknown(all_nominal(), -all_outcomes(), new_level = "none") %>% 
  #data type of nominal attributes
  #step_mutate_at(TECH, BUSINESS_TYPE, PRICE_LIST, OWNERSHIP, END_OWNERSHIP, COUNTRY, CURRENCY, END_CURRENCY, fn = as.factor) %>%
  step_string2factor(all_nominal(), -all_outcomes(), -has_role("ID")) %>%
  step_dummy(all_nominal_predictors(), -all_outcomes()) %>%
  step_naomit(all_predictors(), -all_outcomes(), -has_role("ID"), skip = TRUE) %>%
  step_zv(all_predictors(), -all_outcomes())

rec_data <- rec %>% prep() %>% bake(NULL)
rec_test <- rec %>% prep() %>% bake(new_data=test)

View(rec_test%>% mutate_all(is.na) %>% summarize_all(sum))

#train a random forest model
train_model <- rand_forest(mode = "classification", mtry = 3, trees = 500) %>%
  set_engine("ranger")                  

train_model

wflow <- 
  workflow() %>%
  #model definition
  add_model(train_model) %>% 
  add_recipe(rec) 
  #evaluation

wflow


fitted <- wflow %>% fit(data=train)

# try
train_set_with_predictions <-
  bind_cols(
    train,
    fitted %>% predict(train)
  )
train_set_with_predictions

bal_accuracy_vec(train_set_with_predictions$OFFER_STATUS, train_set_with_predictions$.pred_class)

test_set_with_predictions <-
  bind_cols(
    test,
    fitted %>% predict(test)
  )
test_set_with_predictions
test_predictions <- fitted %>% predict(test)
test_predictions

all(template_ids == output_ids)

#create output file
output <- data.frame(id=output_ids,prediction=test_predictions)
output <- rename(output, prediction=.pred_class)

output_file <- write.csv(output, file="predictions_the_gr8ful_gr8_8.csv", row.names=FALSE)

output_file <- read_csv("predictions_the_gr8ful_gr8_8.csv")
View(output_file)

truth <- test$OFFER_STATUS
truth_train <- train$OFFER_STATUS

pred <- predict(fitted, test)
pred_train <- predict(fitted, train)
bac <- bal_accuracy_vec(truth, pred)#balanced accuracy
bac_train <- bal_accuracy_vec(truth_train, pred_train)#balanced accuracy

bac_train

error_rate = nrow(test %>% filter(as.character(OFFER_STATUS_BIN)!=pred))/nrow(test)
error_rate 

# Check REV_CURRENT_YEAR & REV_CURRENT_YEAR.1
#dft <- df %>% filter(!is.na(REV_CURRENT_YEAR))
#dft$REV_CURRENT_YEAR = as.numeric(gsub("\"", "", dft$REV_CURRENT_YEAR))
#cor(dft$REV_CURRENT_YEAR, dft$REV_CURRENT_YEAR.1)

#fix casing for string attributes
#customers <- customers %>% mutate_at(vars(OWNERSHIP, COUNTRY, CURRENCY), toupper)

#correct data types of nominal attributes, not best practice
#customers <- customers %>% mutate(
#  OWNERSHIP = as.factor(OWNERSHIP),
#  COUNTRY = as.factor(COUNTRY),
#  COUNTRY = as.factor(COUNTRY)
#)

# remove \" from REV_CURRENT_YEAR and convert to numeric
#customers <- (customers %>% mutate(REV_CURRENT_YEAR = gsub("\"", "", REV_CURRENT_YEAR)))
#customers$REV_CURRENT_YEAR <- as.numeric(customers$REV_CURRENT_YEAR)

# convert CREATION_YEAR to date
#customers$CREATION_YEAR <- gsub(pattern="[[:punct:]]", ":", customers$CREATION_YEAR)
#customers$CREATION_YEAR <- as_date(customers$CREATION_YEAR, format="%d:%m:%Y")

### DATA PREPARATION on transaction data

#correct data types of nominal attributes, not best practice
#trans_training <- trans_training %>% mutate(
#  TECH = as.factor(TECH),
#  BUSINESS_TYPE = as.factor(BUSINESS_TYPE),
#  PRICE_LIST = as.factor(PRICE_LIST)
#)

#omit rows with ISIC = NA or SALES_LOCATION = NA
trans_training <- trans_training %>% filter(!is.na(ISIC) & !is.na(SALES_LOCATION))

#map training labels -> 0/1
#trans_training <- trans_training %>% mutate(OFFER_STATUS = toupper(OFFER_STATUS))
#new column mapping losses->0, wins->1
#trans_training <- trans_training %>% mutate(OFFER_STATUS_BIN = case_when(
#  OFFER_STATUS == "LOSE" ~ 0,
#  OFFER_STATUS == "LOST" ~ 0,
#  TRUE ~ 1),
#  OFFER_STATUS_BIN = as.factor(OFFER_STATUS_BIN)
#)
#trans_training <- trans_training %>% subset(select= - c(OFFER_STATUS))

#remove \" from CUSTOMER and END_CUSTOMER values
#trans_training <- trans_training %>% mutate(CUSTOMER = gsub("\"", "", CUSTOMER))
#trans_training <- trans_training %>% mutate(END_CUSTOMER = gsub("\"", "", END_CUSTOMER))

#handle missing values in END_CUSTOMER: NA -> 0
#trans_training <- trans_training %>% mutate(END_CUSTOMER = toupper(END_CUSTOMER))
#trans_training <- trans_training %>% mutate(END_CUSTOMER = case_when(
#  is.na(END_CUSTOMER) ~ "0", #unknown end customer
#  END_CUSTOMER == "NO" ~ "0", #unknown end customer != customer TODO maybe handle this differently
#  END_CUSTOMER == "YES" ~ CUSTOMER, #customer is end customer
#  TRUE ~ END_CUSTOMER 
#))

#convert datestrings to datetime
#trans_training$MO_CREATED_DATE <- gsub(pattern="[[:punct:]]", ":", trans_training$MO_CREATED_DATE)
#trans_training$SO_CREATED_DATE <- gsub(pattern="[[:punct:]]", ":", trans_training$SO_CREATED_DATE)

#trans_training <- trans_training %>% mutate_at(vars(MO_CREATED_DATE, SO_CREATED_DATE), as_datetime)

# convert CUSTOMER IDs to numeric, unavailable values -> NA TODO: should these rows be ignored?
#trans_training$CUSTOMER <- as.numeric(trans_training$CUSTOMER)
#trans_training$END_CUSTOMER <- as.numeric(trans_training$END_CUSTOMER)

#merge geo data using SALES_LOCATION
#trans_training_joint <- merge(x = trans_training, y=geo, by='SALES_LOCATION', all.x=TRUE)
#View(trans_training_joint %>% summarise(across(.cols = everything(), .fns = ~sum(is.na(.)))))

### TRAINING

#train a random forest model
train_model <- rand_forest(mode = "classification", mtry = 3, trees = 500) %>%
  set_engine("ranger") %>%
  fit(
    as.factor(OFFER_STATUS) ~ OFFER_PRICE,
    data = train
  )

pred <- predict(train_model, train)
error_rate = nrow(trans_training %>% filter(as.character(OFFER_STATUS_BIN)!=pred))/nrow(trans_training)
error_rate 

