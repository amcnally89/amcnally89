- 👋 Hi, I’m @amcnally89
- Below you will find the code that I am attempting to use for the R Capstone Project. It appears that there is an warning with the test_results_all predict. But the main issue that I am having is with the RSQ and RSME functions.

library("tidymodels")
library("tidyverse")
library("rlang")
library("stringr")

dataset_url <- "https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBMDeveloperSkillsNetwork-RP0321EN-SkillsNetwork/labs/datasets/seoul_bike_sharing_converted_normalized.csv"
bike_sharing_df <- read_csv(dataset_url)
spec(bike_sharing_df)

bike_sharing_df <- bike_sharing_df %>% 
                   select(-DATE, -FUNCTIONING_DAY)

set.seed(1234)
bike_data_split <- initial_split(bike_sharing_df,prop = 4/5)
train_data <- training(bike_data_split)
test_data <- testing(bike_data_split)

lm_model_weather <- linear_reg(mode = "regression") %>% set_engine(engine = "lm")
lm_model_weather 

train_fit_weather <- lm_model_weather  %>% 
    fit(RENTED_BIKE_COUNT ~ TEMPERATURE + HUMIDITY + WIND_SPEED + VISIBILITY + DEW_POINT_TEMPERATURE + SOLAR_RADIATION + RAINFALL + SNOWFALL, data = train_data)
train_fit_weather 
#print(train_fit_weather$fit)

lm_model_all <- linear_reg(mode = "regression") %>%  set_engine(engine = "lm")
lm_model_all

train_fit_all <- lm_model_all %>% fit(RENTED_BIKE_COUNT ~ ., data = train_data)
train_fit_all 
#print(train_fit_all$fit)
#summary(lm_model_all$fit)

test_results_weather <- train_fit_weather %>%
  predict(new_data = train_data) %>%
  mutate(truth = train_data$RENTED_BIKE_COUNT)

test_results_all <- train_fit_all %>%
  predict(new_data = train_data) %>%
  mutate(truth = train_data$RENTED_BIKE_COUNT)

rsq_weather <- rsq(test_results_weather, truth = truth, estimate = .pred)
rmse_weather <- rmse(test_results_weather, truth = truth, estimate = .pred)

rsq_all <- rsq( test_results_all , truth = truth, estimate = .pred)
rmse_all <- rmse( test_results_all , truth = truth, estimate = .pred)

##### This is the portion of the code that is causing the error. #####
train_fit_all %>%
     mutate(name = fct_reorder(labels(train_fit_all$fit$coefficients), train_fit_all$fit$coefficients)) %>%
     ggplot( aes(x=fit, y=coefficients)) +
     geom_bar()


<!---
amcnally89/amcnally89 is a ✨ special ✨ repository because its `README.md` (this file) appears on your GitHub profile.
You can click the Preview link to take a look at your changes.
--->
