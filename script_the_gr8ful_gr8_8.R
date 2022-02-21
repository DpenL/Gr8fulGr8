library(tidyverse)
library(tidymodels)
library(rsample)
library(lubridate)
library(quantmod) # for exchange rates
library(priceR) #dito

set.seed(2022)
options(dplyr.width = Inf) # show all columns when printing to console

# read csv
cust <- read_csv("customers.csv")
trans <- read_csv("transactions.csv")
geo <- read_csv("geo.csv")

# deal with key issues
trans <- trans %>% mutate(CUSTOMER = as.integer(gsub("\"", "", CUSTOMER)))
trans <- trans %>% mutate(END_CUSTOMER = case_when(
  is.na(END_CUSTOMER) ~ CUSTOMER, #unknown end_customer, assume same as customer
  toupper(END_CUSTOMER) == "NO" ~ as.integer("a"), #unknown end customer != customer TODO maybe handle this differently
  toupper(END_CUSTOMER) == "YES" ~ CUSTOMER, #customer is end customer
  TRUE ~ as.integer(END_CUSTOMER) 
))
#feature: is customer end_customer?
trans <- trans %>% mutate(DIFFERENT_END_CUSTOMER = case_when(
  CUSTOMER == END_CUSTOMER ~ 0,
  TRUE ~ 1
))
#remove END_CUSTOMER column
trans <- trans %>% select(-END_CUSTOMER)

#split ISIC into hierarchical parts
trans <- trans %>% mutate(ISIC.1 = case_when(
  is.na(ISIC) ~ "NA",
  TRUE ~ as.character(as.numeric(ISIC) %/% 100)
))

trans <- trans %>% mutate(TOTAL_COST = MATERIAL_COST + SERVICE_COST)


#prepare customer keys
cust <- cust %>% mutate(CUSTOMER = as.integer(CUSTOMER))
geo <- geo %>% mutate(COUNTRY = case_when(
  COUNTRY=="CH" ~ "Switzerland", 
  COUNTRY=="FR" ~ "France",
  TRUE ~ "NA")
  )

cust <- cust %>% mutate(
  REV_SUM = REV_CURRENT_YEAR.1 + REV_CURRENT_YEAR.2,
  DELTA_REV = REV_CURRENT_YEAR.1 - REV_CURRENT_YEAR.2
  )

cust <- cust %>% select(-REV_CURRENT_YEAR, -REV_CURRENT_YEAR.1, -REV_CURRENT_YEAR.2)
#feature: is customers different from that of their country?
#assumes France uses Euro
cust <- cust %>% mutate(FOREIGN_CURRENCY=case_when(
  toupper(CURRENCY) == "EURO" ~ 0,
  TRUE ~ 1
))


# left join all three csv files
df <- trans %>% left_join(geo) %>% left_join(cust)

# check missing values from different attributes
df %>% summarize_all(function(x) sum(is.na(x)))

# split train and test
train <- df %>% filter(is.na(TEST_SET_ID)) 
test <- df %>% anti_join(train) %>% select(-OFFER_STATUS)

#test_ids for output file
output_ids <- test$TEST_SET_ID

#  Check whether the test set matches the submission template
submission_template <- read_csv('submission_random.csv')

template_ids <- submission_template %>% arrange(id) %>% pull(id)
test_ids <- test %>% arrange(TEST_SET_ID) %>% pull(TEST_SET_ID)
all(template_ids == test_ids) #true if all the same, otherwise falls

train <- train %>% mutate(
  OFFER_STATUS = as.factor(case_when(
    toupper(OFFER_STATUS) == "LOSE" ~ 0,
    toupper(OFFER_STATUS) == "LOST" ~ 0,
    TRUE ~ 1)
  ))

#final features before recipe
train <- train %>% select(MO_ID, SO_ID, 
                          TECH, OFFER_TYPE, BUSINESS_TYPE,
                          OFFER_PRICE, SERVICE_LIST_PRICE,
                          PRICE_LIST, ISIC.1, ISIC,
                          TOTAL_COST,
                          COSTS_PRODUCT_A,
                          COSTS_PRODUCT_B,
                          COSTS_PRODUCT_C,
                          COSTS_PRODUCT_D,
                          COSTS_PRODUCT_E,
                          CREATION_YEAR,
                          MO_CREATED_DATE, SO_CREATED_DATE,
                          DIFFERENT_END_CUSTOMER,
                          REV_SUM,
                          DELTA_REV,
                          CURRENCY, FOREIGN_CURRENCY,
                          SALES_OFFICE,
                          SALES_LOCATION,
                          COUNTRY,
                          OFFER_STATUS)

#fold cross evaluation
folds <- train %>% vfold_cv(v=5)

### DATA PREPARATION with recipe

CNY <- filter(exchange_rate_latest("CNY"), currency=="EUR")[,2]
USD <- filter(exchange_rate_latest("USD"), currency=="EUR")[,2]
GBP <- filter(exchange_rate_latest("GBP"), currency=="EUR")[,2]

rec <- recipe(
  OFFER_STATUS ~ ., data = train) %>%
  update_role(MO_ID, SO_ID, new_role = "ID") %>%
  step_mutate_at(all_nominal(), -all_outcomes(), -has_role("ID"), fn = toupper) %>%
 
  #replace missing offer prices with list price
  step_mutate(OFFER_PRICE = case_when(
    is.na(OFFER_PRICE) ~ SERVICE_LIST_PRICE,
    TRUE ~ OFFER_PRICE
  )) %>% 

  #data types of dates, impute missing with mean
  step_mutate_at(CREATION_YEAR, fn = function(x) parse_date_time(x,orders="dmY") %>% year()) %>%
  step_mutate_at(CREATION_YEAR, fn = ~replace_na(.,as.integer(mean(CREATION_YEAR)))) %>% 
  step_mutate_at(MO_CREATED_DATE, SO_CREATED_DATE, fn = function(x) parse_date_time(gsub(pattern="[[:punct:]]", ":", x),orders=c("d:m:Y H:M", "Y:m:d H:M:S"))) %>%

  
  step_mutate(REV_SUM = case_when(
    is.na(CURRENCY) || is.nan(CURRENCY) ~ NA_real_,
    CURRENCY == "EURO" ~ REV_SUM,
    CURRENCY == "CHINESE YUAN" ~ CNY*REV_SUM,
    CURRENCY == "US DOLLAR" ~ USD*REV_SUM,
    CURRENCY == "POUND STERLING" ~ GBP*REV_SUM,
    TRUE ~ NA_real_
  )) %>% 
  step_mutate(REV_SUM =  case_when(
      is.na(REV_SUM) || is.nan(REV_SUM) ~ 0,
      REV_SUM == 0.0 ~ 0,
      TRUE ~ log10(REV_SUM)
    )) %>%

  step_mutate(
    DELTA_REV = case_when(
      is.na(CURRENCY) || is.nan(CURRENCY) ~ NA_real_,
      CURRENCY == "EURO" ~ DELTA_REV,
      CURRENCY == "CHINESE YUAN" ~ CNY*DELTA_REV,
      CURRENCY == "US DOLLAR" ~ USD*DELTA_REV,
      CURRENCY == "POUND STERLING" ~ GBP*DELTA_REV,
      TRUE ~ NA_real_
    ),
    DELTA_REV = case_when(
      is.na(DELTA_REV) || is.nan(DELTA_REV) ~ 0,
      DELTA_REV == 0.0 ~ 0,
      TRUE ~ log10(DELTA_REV)
    )) %>%
  
  
  step_mutate(CURRENCY = as.factor(CURRENCY)) %>% 
  
  #impute numerics with mean
  step_impute_mean(all_numeric_predictors(), -all_outcomes()) %>%
  
  step_novel(all_nominal(), -all_outcomes(), -has_role("ID"), new_level="new") %>%
  step_unknown(FOREIGN_CURRENCY, all_nominal(), -all_outcomes(), new_level = "none") %>% 
  #data type of nominal attributes
  step_string2factor(all_nominal(), -all_outcomes(), -has_role("ID")) %>%
  step_dummy(all_nominal_predictors(), -all_outcomes(), -has_role("ID")) %>%
  step_naomit(all_predictors(), -all_outcomes(), -has_role("ID"), skip = TRUE) %>%
  #step_pca(all_numeric_predictors(), -all_outcomes()) %>%
  step_zv(all_predictors(), -all_outcomes())

rec_data <- rec %>% prep() %>% bake(NULL)
rec_test <- rec %>% prep() %>% bake(new_data=test)

View(rec_test%>% mutate_all(is.na) %>% summarize_all(sum))

#train a random forest model
train_model <- rand_forest(mode = "classification", mtry = 5, trees = 500) %>%
  set_engine("ranger",  importance = "impurity")                  

train_model

wflow <- 
  workflow() %>%
  #model definition
  add_model(train_model) %>% 
  add_recipe(rec) 
  #evaluation

wflow


fitted <- wflow %>% fit_resamples(folds, metrics = metric_set(yardstick::bal_accuracy))

best_config <- fitted %>%
  # find the best tried configuration for a certain criterion
  select_best('bal_accuracy')
final_workflow <- wflow %>% 
  finalize_workflow(best_config)

final_workflow

trained_model <- final_workflow %>% 
  fit(data=train)

train_set_with_predictions <-
  bind_cols(
    train,
    trained_model %>% predict(train, type="prob")
  )
train_set_with_predictions

train_set_with_predictions <- train_set_with_predictions %>%
  mutate(pred=as.factor(ifelse(.pred_0>0.2,0,1)))

bal_accuracy_vec(train_set_with_predictions$OFFER_STATUS, train_set_with_predictions$pred)

test_set_with_predictions <-
  bind_cols(
    test,
    trained_model %>% predict(test, type="prob")
  )
test_set_with_predictions

test_set_with_predictions <- test_set_with_predictions %>%
  mutate(pred=as.factor(ifelse(.pred_0>0.2,0,1)))

test_predictions <- trained_model %>% predict(test, type="prob")
test_predictions <- test_predictions %>%
  mutate(pred=as.factor(ifelse(.pred_0>0.2,0,1)))
test_predictions

all(template_ids == output_ids)

#create output file
output <- data.frame(id=output_ids,prediction=test_predictions$pred)

output_file <- write.csv(output, file="predictions_the_gr8ful_gr8_8.csv", row.names=FALSE)

output_file <- read_csv("predictions_the_gr8ful_gr8_8.csv")
View(output_file)
