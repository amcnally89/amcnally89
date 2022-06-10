- 👋 Hi, I’m @amcnally89
- Below you will find the code that I am attempting to use for the R Capstone Project. It appears that there is an warning with the test_results_all predict. But the main issue that I am having is with the RSQ and RSME functions.

library("tidymodels")
library("tidyverse")
library("rlang")
library("stringr")

# Dataset URL
dataset_url <- "https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBMDeveloperSkillsNetwork-RP0321EN-SkillsNetwork/labs/datasets/seoul_bike_sharing_converted_normalized.csv"
bike_sharing_df <- read_csv(dataset_url)
spec(bike_sharing_df)

bike_sharing_df <- bike_sharing_df %>% select(-DATE, -FUNCTIONING_DAY)

lm_spec <- linear_reg() %>% set_engine("lm") %>% set_mode("regression")

set.seed(1234)
data_split <- initial_split(bike_sharing_df, prop = 4/5)
train_data <- training(data_split)
test_data <- testing(data_split)

#/***** LM1 *****/
sample_lm_poly10 <-lm_spec %>% fit(RENTED_BIKE_COUNT ~ poly(TEMPERATURE, 6) + poly(RAINFALL, 4) , data = train_data)

test_results_sample_poly10 <- sample_lm_poly10 %>%
	predict(new_data = train_data) %>%
	mutate(truth = train_data$RENTED_BIKE_COUNT)
test_results_sample_poly10[test_results_sample_poly10<0] <- 0

rsq_sample_poly10 <- rsq(test_results_sample_poly10, truth = truth, estimate = .pred)
rmse_sample_poly10 <- rmse(test_results_sample_poly10, truth = truth, estimate = .pred)

sample_lm_poly11 <-lm_spec %>% fit(RENTED_BIKE_COUNT ~ TEMPERATURE*RAINFALL , data = train_data)

test_results_sample_poly11 <- sample_lm_poly11 %>%
	predict(new_data = train_data) %>%
	mutate(truth = train_data$RENTED_BIKE_COUNT)
test_results_sample_poly11[test_results_sample_poly11<0] <- 0

rsq_sample_poly11 <- rsq(test_results_sample_poly11, truth = truth, estimate = .pred)
rmse_sample_poly11 <- rmse(test_results_sample_poly11, truth = truth, estimate = .pred)

#/***** LM2 *****/
sample_lm_poly20 <-lm_spec %>% fit(RENTED_BIKE_COUNT ~ poly(TEMPERATURE, 6) + poly(SNOWFALL, 4) , data = train_data)

test_results_sample_poly20 <- sample_lm_poly20 %>%
	predict(new_data = train_data) %>%
	mutate(truth = train_data$RENTED_BIKE_COUNT)
test_results_sample_poly20[test_results_sample_poly10<0] <- 0

rsq_sample_poly20 <- rsq(test_results_sample_poly20, truth = truth, estimate = .pred)
rmse_sample_poly20 <- rmse(test_results_sample_poly20, truth = truth, estimate = .pred)

sample_lm_poly21 <-lm_spec %>% fit(RENTED_BIKE_COUNT ~ TEMPERATURE*SNOWFALL , data = train_data)

test_results_sample_poly21 <- sample_lm_poly21 %>%
	predict(new_data = train_data) %>%
	mutate(truth = train_data$RENTED_BIKE_COUNT)
test_results_sample_poly21[test_results_sample_poly21<0] <- 0

rsq_sample_poly21 <- rsq(test_results_sample_poly21, truth = truth, estimate = .pred)
rmse_sample_poly21 <- rmse(test_results_sample_poly21, truth = truth, estimate = .pred)

#/***** LM3 *****/
sample_lm_poly30 <-lm_spec %>% fit(RENTED_BIKE_COUNT ~ poly(TEMPERATURE, 6) + poly(WIND_SPEED, 4) , data = train_data)

test_results_sample_poly30 <- sample_lm_poly30 %>%
	predict(new_data = train_data) %>%
	mutate(truth = train_data$RENTED_BIKE_COUNT)
test_results_sample_poly30[test_results_sample_poly30<0] <- 0

rsq_sample_poly30 <- rsq(test_results_sample_poly30, truth = truth, estimate = .pred)
rmse_sample_poly30 <- rmse(test_results_sample_poly30, truth = truth, estimate = .pred)

sample_lm_poly31 <-lm_spec %>% fit(RENTED_BIKE_COUNT ~ TEMPERATURE*WIND_SPEED, data = train_data)

test_results_sample_poly31 <- sample_lm_poly31 %>%
	predict(new_data = train_data) %>%
	mutate(truth = train_data$RENTED_BIKE_COUNT)
test_results_sample_poly31[test_results_sample_poly31<0] <- 0

rsq_sample_poly31 <- rsq(test_results_sample_poly31, truth = truth, estimate = .pred)
rmse_sample_poly31 <- rmse(test_results_sample_poly31, truth = truth, estimate = .pred)

#/***** LM4 *****/
sample_lm_poly40 <-lm_spec %>% fit(RENTED_BIKE_COUNT ~ poly(TEMPERATURE, 6) + poly(SOLAR_RADIATION, 4) , data = train_data)

test_results_sample_poly40 <- sample_lm_poly40 %>%
	predict(new_data = train_data) %>%
	mutate(truth = train_data$RENTED_BIKE_COUNT)
test_results_sample_poly40[test_results_sample_poly40<0] <- 0

rsq_sample_poly40 <- rsq(test_results_sample_poly40, truth = truth, estimate = .pred)
rmse_sample_poly40 <- rmse(test_results_sample_poly40, truth = truth, estimate = .pred)

sample_lm_poly41 <-lm_spec %>% fit(RENTED_BIKE_COUNT ~ TEMPERATURE*SOLAR_RADIATION, data = train_data)

test_results_sample_poly41 <- sample_lm_poly41 %>%
	predict(new_data = train_data) %>%
	mutate(truth = train_data$RENTED_BIKE_COUNT)
test_results_sample_poly41[test_results_sample_poly41<0] <- 0

rsq_sample_poly41 <- rsq(test_results_sample_poly41, truth = truth, estimate = .pred)
rmse_sample_poly41 <- rmse(test_results_sample_poly41, truth = truth, estimate = .pred)

#/***** LM5 *****/
sample_lm_poly50 <-lm_spec %>% fit(RENTED_BIKE_COUNT ~ poly(TEMPERATURE, 6) + poly(VISIBILITY, 4) , data = train_data)

test_results_sample_poly50 <- sample_lm_poly50 %>%
	predict(new_data = train_data) %>%
	mutate(truth = train_data$RENTED_BIKE_COUNT)
test_results_sample_poly50[test_results_sample_poly50<0] <- 0

rsq_sample_poly50 <- rsq(test_results_sample_poly50, truth = truth, estimate = .pred)
rmse_sample_poly50 <- rmse(test_results_sample_poly50, truth = truth, estimate = .pred)

sample_lm_poly51 <-lm_spec %>% fit(RENTED_BIKE_COUNT ~ TEMPERATURE*VISIBILITY, data = train_data)

test_results_sample_poly51 <- sample_lm_poly51 %>%
	predict(new_data = train_data) %>%
	mutate(truth = train_data$RENTED_BIKE_COUNT)
test_results_sample_poly51[test_results_sample_poly51<0] <- 0

rsq_sample_poly51 <- rsq(test_results_sample_poly51, truth = truth, estimate = .pred)
rmse_sample_poly51 <- rmse(test_results_sample_poly51, truth = truth, estimate = .pred)

/*****/



first_column <-c(rsq_sample_poly10$.estimate,rsq_sample_poly11$.estimate,rsq_sample_poly20$.estimate,rsq_sample_poly21$.estimate,rsq_sample_poly30$.estimate,rsq_sample_poly31$.estimate,rsq_sample_poly40$.estimate,rsq_sample_poly41$.estimate,rsq_sample_poly50$.estimate,rsq_sample_poly51$.estimate,rmse_sample_poly10$.estimate,rmse_sample_poly11$.estimate,rmse_sample_poly20$.estimate,rmse_sample_poly21$.estimate,rmse_sample_poly30$.estimate,rmse_sample_poly31$.estimate,rmse_sample_poly40$.estimate,rmse_sample_poly41$.estimate,rmse_sample_poly50$.estimate,rmse_sample_poly51$.estimate)

second_column <-c(rsq_sample_poly10$.metric,rsq_sample_poly11$.metric,rsq_sample_poly20$.metric,rsq_sample_poly21$.metric,rsq_sample_poly30$.metric,rsq_sample_poly31$.metric,rsq_sample_poly40$.metric,rsq_sample_poly41$.metric,rsq_sample_poly50$.metric,rsq_sample_poly51$.metric,rmse_sample_poly10$.metric,rmse_sample_poly11$.metric,rmse_sample_poly20$.metric,rmse_sample_poly21$.metric,rmse_sample_poly30$.metric,rmse_sample_poly31$.metric,rmse_sample_poly40$.metric,rmse_sample_poly41$.metric,rmse_sample_poly50$.metric,rmse_sample_poly51$.metric)

names(first_column) <- "Estimator"
first_column <- as.data.frame(first_column)
names(second_column) <- "RSQ/RMSE"
second_column <- as.data.frame(second_column)

RSQ_RMSE_df <- as.data.frame(c( first_column, second_column))
names(RSQ_RMSE_df) <- c("Estimator","RSQ/RMSE")



ggplot(RSQ_RMSE_df, aes(fill=RSQ/RMSE, y=Estimator)) + 
    geom_bar(position="dodge", stat="identity")


<!---
amcnally89/amcnally89 is a ✨ special ✨ repository because its `README.md` (this file) appears on your GitHub profile.
You can click the Preview link to take a look at your changes.
--->
