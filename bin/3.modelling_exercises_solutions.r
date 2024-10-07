#autor:      Joao Sollari Lopes
#local:      INE, Lisboa
#Rversion:   4.3.1
#criado:     26.09.2024
#modificado: 07.10.2024

# 0. INDEX
{
# 1. BUILD A SIMPLE MODEL
# 1.1. LOOK AT DATA
# 1.2. FIT A MODEL
# 1.3. USE MODEL TO PREDICT
# 2. BUILD A MODEL USING WORKFLOW
# 2.1. LOOK AT DATA
# 2.2. SPLIT DATA
# 2.3. CREATE WORKFLOW
# 2.4. FIT A MODEL
# 2.6. EVALUATE MODEL
# 3. BUILD VARIOUS MODELS USING WORKFLOW
# 3.1. LOOK AT DATA
# 3.2. SPLIT DATA
# 2.3. CREATE WORKFLOW AND FIT MODEL 1
# 3.4. CREATE WORKFLOW AND FIT MODEL 2
# 3.5. EVALUATE LAST MODEL

}
# 1. BUILD A MODEL
{
library("dotwhisker") 
library("tidymodels")
library("tidyverse")


## 1.1. LOOK AT DATA [no exercises]

## 1.2. FIT A MODEL

# a) Evaluate the following model fitted to the diamonds datasets using native R
# function summary(). Evaluate it also using function augment() and rsq() from
# tidymodels. What are the possible problems of using the same data to fit and 
# to evaluate the model? Think about when you have completely new data that
# you want to make predictions of.
#
#set.seed(123)
#diamonds_data <- diamonds |>
#  mutate(
#    log_price = log(price),
#    log_carat = log(carat),
#    fct_cut = factor(cut, ordered = FALSE),
#    .keep = "used"
#  ) |>
#  slice_sample(n = 1000)
#lm_fit <- linear_reg() |>
#  fit(log_price ~ log_carat + fct_cut, data = diamonds_data)
set.seed(123)
diamonds_data <- diamonds |>
  mutate(
    log_price = log(price),
    log_carat = log(carat),
    fct_cut = factor(cut, ordered = FALSE),
    .keep = "used"
  ) |>
  slice_sample(n = 1000)
lm_fit <- linear_reg() |>
  fit(log_price ~ log_carat + fct_cut, data = diamonds_data)

summary(lm_fit$fit)
summary(lm_fit$fit)$r.sq
augment(lm_fit, new_data = diamonds_data) |>
  rsq(truth = log_price, estimate = .pred)
# Using same data to fit and evaluate can lead to overestimate evaluation.

# b) How can you avoid over estimating the evaluation of the model? Consider 
# taking a new sample of the data.
set.seed(456)
diamonds_data2 <- diamonds |>
  mutate(
    log_price = log(price),
    log_carat = log(carat),
    fct_cut = factor(cut, ordered = FALSE),
    .keep = "used"
  ) |>
  slice_sample(n = 1000)

augment(lm_fit, new_data = diamonds_data2) |>
  rsq(truth = log_price, estimate = .pred)

# c) Using the same fitted model, build by hand a plot showing how the residuals
# vary a long the values of the predictors.
augment(lm_fit, new_data = diamonds_data) |>
  ggplot(aes(y = .resid, x = .pred)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "blue")

## 1.3. USE MODEL TO PREDICT

# a) Using the model from the previous section, predict the price of diamonds 
# with 1, 2 and 3 carat for cuts "Fair", "Very Good" and "Ideal". Build a plot
# of the predictions using a confidence interval of 90%.
new_points2 <- expand.grid(
  log_carat = log(1:3),
  fct_cut = c("Fair", "Very Good", "Ideal")
  )

bind_cols(
  new_points2,
  predict(lm_fit, new_data = new_points2),
  predict(lm_fit, new_data = new_points2, type = "conf_int", level = 0.90)
) |>
  ggplot(aes(x = exp(log_carat), color = fct_cut)) + 
  geom_point(aes(y = exp(.pred))) + 
  geom_errorbar(
    aes(ymin = exp(.pred_lower), ymax = exp(.pred_upper)),
    width = .1
  ) + 
  labs(x = "diamond carat", y = "diamond price", color = "diamond cut")
  
}
# 2. BUILD A MODEL USING WORKFLOW
{
library("nycflights13")
library("tidymodels")
library("tidyverse")

## 2.1. LOOK AT DATA [no exercises]

## 2.2. SPLIT DATA [no exercises]

## 2.3. CREATE WORKFLOW

# a) Considering the following workflow, build a new workflow without the steps
# of the recipe. Look at the function add_formula().
#flights_rec <- 
#  recipe(arr_delay ~ ., data = train_data) |>
#  step_date(date, features = c("dow", "month")) |>
#  step_holiday(date, holidays = timeDate::listHolidays("US")) |>
#  step_rm(date) |>
#  step_dummy(all_nominal_predictors()) |>
#  step_zv(all_predictors()) |>
#  step_normalize(all_numeric_predictors())
#flights_mod <- logistic_reg(engine = "glm")
#flights_wflow <-
#  workflow() |>
#  add_model(flights_mod) |>
#  add_recipe(flights_rec)
flights_mod <- logistic_reg(engine = "glm")
flights_wflow2 <-
  workflow() |>
  add_model(flights_mod) |>
  add_formula(arr_delay ~ .)

## 2.4. FIT A MODEL

# a) Using the given workflow and the newly created one, fit the two models to
# the following data set. Don't forget to split the data in train and test sets.
#set.seed(123)
#flight_data <- flights |> 
#  slice_sample(n = 10000) |>
#  mutate(
#    arr_delay = factor(ifelse(arr_delay >= 30, "late", "on_time")),
#    date = lubridate::as_date(time_hour)
#  ) |> 
#  select(dep_time, origin, dest, distance, carrier, date, arr_delay) |> 
#  na.omit() |> 
#  mutate(across(where(is.character), as.factor))
#set.seed(456)
#data_split <- initial_split(flight_data, prop = 3/4)
#train_data <- training(data_split)
#test_data  <- testing(data_split)
set.seed(123)
flight_data <- flights |> 
  slice_sample(n = 10000) |>
  mutate(
    arr_delay = factor(ifelse(arr_delay >= 30, "late", "on_time")),
    date = lubridate::as_date(time_hour)
  ) |> 
  inner_join(weather, by = c("origin", "time_hour")) |> 
  select(
    arr_delay,
    dep_time, origin, air_time, distance, date,
    humid, wind_dir, wind_speed, precip, pressure, visib
  ) |> 
  na.omit() |> 
  mutate(across(where(is.character), as.factor))

set.seed(456)
data_split <- initial_split(flight_data, prop = 3/4)
train_data <- training(data_split)
test_data  <- testing(data_split)

flights_mod <- logistic_reg(engine = "glm")

flights_rec <- 
  recipe(arr_delay ~ ., data = train_data) |>
  step_date(date, features = c("dow", "month")) |>
  step_holiday(date, holidays = timeDate::listHolidays("US")) |>
  step_rm(date) |>
  step_dummy(all_nominal_predictors()) |>
  step_zv(all_predictors()) |>
  step_normalize(all_numeric_predictors())

flights_wflow <-
  workflow() |>
  add_model(flights_mod) |>
  add_recipe(flights_rec)

flights_wflow2 <-
  workflow() |>
  add_model(flights_mod) |>
  add_formula(arr_delay ~ .)

flights_fit <- flights_wflow |> fit(data = train_data)
flights_fit2 <- flights_wflow2 |>  fit(data = train_data)

## 2.5. EVALUATE MODEL

# a) How did the previously fitted models perform? Use the appropriate
# statistic.
flights_aug <- augment(flights_fit, new_data = test_data)
flights_aug |> roc_auc(truth = arr_delay, .pred_late)
flights_aug2 <- augment(flights_fit2, new_data = test_data)
flights_aug2 |> roc_auc(truth = arr_delay, .pred_late)

# b) Calculate a confusion matrix by hand to the model that considers the steps
# of the recipe in the workflow. Which probability threshold was used for
# predicting the classes?
flights_aug |> count(arr_delay, .pred_class) |>
  pivot_wider(
    names_from = .pred_class,
    values_from = n
  ) |>
  rename(Real = arr_delay)
# The confusion matrix uses the classic 50% threshold.

# c) Use probability threshold for predicting the classes of 0.40 and 0.60. How
# do the threshold impact on the confusion matrix?
flights_aug |>
  mutate(
    .pred_class = factor(ifelse(.pred_on_time > 0.40, "on_time", "late"))
  ) |>
  count(arr_delay, .pred_class) |>
  pivot_wider(
    names_from = .pred_class,
    values_from = n
  ) |>
  rename(Real = arr_delay)
# A lower threshold leads to more false positives and less false negatives.

flights_aug |>
  mutate(
    .pred_class = factor(ifelse(.pred_on_time > 0.60, "on_time", "late"))
  ) |>
  count(arr_delay, .pred_class) |>
  pivot_wider(
    names_from = .pred_class,
    values_from = n
  ) |>
  rename(Real = arr_delay)
# A higher threshold leads to less false positives and more false negatives.
  
# d) According to the confusion matrix the fitted model as a sensitivity of 0.120
# and a specificity of 0.990. Using ggplot, place these values in a ROC curve.
# Use geom_path(), geom_abline(), coord_equal() and theme_bw().
summary(flights_aug |> conf_mat(truth=arr_delay, .pred_class))

flights_aug |> 
  roc_curve(truth = arr_delay, .pred_late) |>
  ggplot(aes(x = 1 - specificity, y = sensitivity)) + 
  geom_path() +
  geom_abline(lty = 3) + 
  geom_hline(yintercept = 0.120, lty = 2) +
  geom_vline(xintercept=1 - 0.990, lty = 2) +
  geom_point(aes(x=1 - 0.990, y=0.120), shape=21, size=2) +
  coord_equal() +
  theme_bw()

# e) Find which terms are more important to the fitted model and plot them. 
# Use the statistics of each term as a proxy for their importance. Try also
# using the exp-transformed estimates of the coefficients. Are the results
# similar? Note that term "(Intercept)" is meaningless and should be discarded.
flights_fit |> 
  tidy() |>
  filter(!term %in% c("(Intercept)")) |>
  mutate(
    importance = abs(statistic),
    term = fct_reorder(term, importance)
  ) |>
  arrange(desc(importance)) |>
  slice_head(n = 20) |>
  ggplot(aes(x = importance, y = term)) +
  geom_col()

flights_fit |> 
  tidy(exponentiate=TRUE) |>
  filter(!term %in% c("(Intercept)")) |>
  mutate(
    importance = estimate,
    term = fct_reorder(term, importance)
  ) |>
  arrange(desc(importance)) |>
  slice_head(n = 20) |>
  ggplot(aes(x = importance, y = term)) +
  geom_col()
# Results are very different. While 'statistics' focus on the significance of
# the terms, 'estimate' focus on the absolute value of the term.

}
# 3. BUILD VARIOUS MODELS USING WORKFLOW
{
library("nnet")
library("NeuralNetTools")
library("vip")
library("tidymodels")
library("tidyverse")

## 3.1. LOOK AT DATA [no exercises]

## 3.2. SPLIT DATA [no exercises]

## 3.3. CREATE WORKFLOW AND FIT MODEL 1

# a) Considering the following split data set and tidymodel recipe, create the 
# respective workflow and fit a simple glm model. Use a ROC curve and the AUC to 
# evaluate your model.
#hotels <- 
#  read_csv("https://tidymodels.org/start/case-study/hotels.csv") |>
#  mutate(across(where(is.character), as.factor))
#set.seed(123)
#splits      <- initial_split(hotels, strata = children, prop = 0.75)
#hotel_other <- training(splits)
#hotel_test  <- testing(splits)
#set.seed(234)
#val_set <- validation_split(hotel_other, strata = children, prop = 0.80)
#holidays <- c("AllSouls", "AshWednesday", "ChristmasEve", "Easter", 
#              "ChristmasDay", "GoodFriday", "NewYearsDay", "PalmSunday")
#lr_recipe <- 
#  recipe(children ~ ., data = hotel_other) |> 
#  step_date(arrival_date) |> 
#  step_holiday(arrival_date, holidays = holidays) |> 
#  step_rm(arrival_date) |> 
#  step_dummy(all_nominal_predictors()) |> 
#  step_zv(all_predictors()) |> 
#  step_normalize(all_predictors())
hotels <- 
  read_csv("https://tidymodels.org/start/case-study/hotels.csv") |>
  mutate(across(where(is.character), as.factor))

set.seed(123)
splits      <- initial_split(hotels, strata = children, prop = 0.75)
hotel_other <- training(splits)
hotel_test  <- testing(splits)

holidays <- c("AllSouls", "AshWednesday", "ChristmasEve", "Easter", 
              "ChristmasDay", "GoodFriday", "NewYearsDay", "PalmSunday")

lr_recipe <- 
  recipe(children ~ ., data = hotel_other) |> 
  step_date(arrival_date) |> 
  step_holiday(arrival_date, holidays = holidays) |> 
  step_rm(arrival_date) |> 
  step_dummy(all_nominal_predictors()) |> 
  step_zv(all_predictors()) |> 
  step_normalize(all_predictors())

lr_mod <- 
  logistic_reg() |> 
  set_engine("glm")
  
lr_workflow <- 
  workflow() |> 
  add_model(lr_mod) |> 
  add_recipe(lr_recipe)

t1 <- Sys.time()
lr_res <- 
  lr_workflow |> 
  fit(data = hotel_other)
Sys.time() - t1
#Time difference of 30.82385 secs

lr_aug <- augment(lr_res, new_data = hotel_test)

lr_aug |> roc_curve(children, .pred_children) |> autoplot()

lr_aug |> roc_auc(truth = children, .pred_children)

## 3.4. CREATE WORKFLOW AND FIT MODEL 2

# a) Replace the glm model used above with a neural network model. Consider 
# function mlp() with engine "nnet" and mode "classification". For the hyper
# parameters, set hidden_units = 1 and penalty = tune(). Use the same recipe
# as before. To tune the model set a grid with 10 points and use the following
# data split. Choose the best model and evaluated with a ROC curve and the AUC.
# How confident are you about the tuning?
#set.seed(234)
#val_set <- validation_split(hotel_other, strata = children, prop = 0.80)
set.seed(234)
val_set <- validation_split(hotel_other, strata = children, prop = 0.80)

cores <- parallel::detectCores()
nn_mod <- 
  mlp(
    hidden_units = 1, #hidden units of the neural network
    penalty = tune(), #amount of regularization
  ) |> 
  set_engine("nnet", num.threads = cores) |> 
  set_mode("classification")

nn_workflow <- 
  workflow() |> 
  add_model(nn_mod) |> 
  add_recipe(lr_recipe)
  
t1 <- Sys.time()
set.seed(345)
nn_res <- 
  nn_workflow |> 
  tune_grid(
    val_set,
    grid = 10,
    control = control_grid(save_pred = TRUE),
    metrics = metric_set(roc_auc)
  )
Sys.time() - t1
#Time difference of 2.210645 mins

autoplot(nn_res)

nn_res |> 
  show_best(metric = "roc_auc", n = 5)
# The best values of the parameter seem to be rather random, providing little
# confidence about the tuning.

nn_best <- 
  nn_res |> 
  select_best(metric = "roc_auc")

nn_pred <-
  nn_res |> 
  collect_predictions(parameters = nn_best)

nn_pred |> 
  roc_curve(children, .pred_children) |> 
  autoplot()

nn_pred |> roc_auc(children, .pred_children)

## 3.5. EVALUATE LAST MODEL

# a) Choose the best neural network model and evaluated it using the test data
# created before. For the evaluation, consider the ROC curve and the AUC. Finish
# the analysis by plotting the most important features (i.e. predictors) of the 
# final model. Set the importance parameter of the engine to "impurity".

last_nn_mod <- 
  mlp(hidden_units = 1, penalty = nn_best$penalty) |> 
  set_engine("nnet", num.threads = cores, importance = "impurity") |> 
  set_mode("classification")

last_nn_workflow <- 
  nn_workflow |> 
  update_model(last_nn_mod)

t1 <- Sys.time()
set.seed(345)
last_nn_fit <- 
  last_nn_workflow |> 
  last_fit(split = splits)
Sys.time() - t1
#Time difference of 29.8 secs

last_nn_fit |>
  collect_predictions() |>
  roc_curve(children, .pred_children) |>
  autoplot()

last_nn_fit |> 
  collect_metrics()

last_nn_fit |> 
  extract_fit_parsnip() |> 
  vip(num_features = 20)
  
}
