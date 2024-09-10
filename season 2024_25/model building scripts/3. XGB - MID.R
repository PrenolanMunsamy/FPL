library(tidyverse)
library(here)
library(ggplot2)
library(GGally)
library(tidymodels)
library(vip)
library(DALEXtra)
library(finetune)

# Load Helper Functions ----
source(here("season 2024_25","99. Constants.R"))
source(here("season 2024_25/utils","xgb_reg_recipe.R"))
source(here("season 2024_25/utils","load_forward_data.R"))
source(here("season 2024_25/utils","load_defender_data.R"))
source(here("season 2024_25/utils","load_midfielder_data.R"))
source(here("season 2024_25/utils","load_goalkeeper_data.R"))
source(here("season 2024_25/utils","train_validation_test_split.R"))

# Import Data ----
input_df <- 
  load_midfielder_data(here(paste0("season ",curr_season,"/data/model"),
                            "model_data.csv"))

input_df %>% 
  colnames() %>% 
  View()

input_df <- 
  input_df %>%
  group_by(season_x,team,name,position) %>% 
  mutate(Q1 = quantile(total_points, 0.25),
         Q3 = quantile(total_points, 0.75),
         IQR = Q3 - Q1,
         lower_bound = Q1 - 1.5 * IQR,
         upper_bound = Q3 + 1.5 * IQR,
         is_outlier = ifelse(total_points < lower_bound | total_points > upper_bound, 
                             1, 
                             0)) %>% 
  ungroup() %>% 
  select(-c(Q1,Q3,IQR,lower_bound,upper_bound))

input_df %>% 
  count(is_outlier)

# # Filter to get only the outliers
input_df <-
  input_df %>%
  filter(is_outlier==0)

input_df %>% 
  count(season_x,GoalWeek) %>% 
  View()

input_df %>% 
  select(where(is.numeric)) %>% 
  corrr::correlate() %>% 
  View()

input_df <-
  input_df %>% 
  arrange(YEAR,MONTH,DAY)

input_df <-
  input_df %>% 
  filter(player_appearance_order>1)

input_df <-
  input_df %>%
  mutate(total_points = pmin(total_points,8))

# Train Test Split ----
set.seed(1234)

train_validation_test <- train_validation_test_split(df = input_df, 
                                                     test_season = prev_season,
                                                     validation_season_match_number = 19)

train_df <- train_validation_test$train
validation_df <- train_validation_test$validation
test_df <- train_validation_test$test

# train_df <- input_df %>% filter(season_x=="2022_23") %>% filter(season_match_number<=30)
# validation_df <- input_df %>% filter(season_x=="2022_23") %>% filter(season_match_number>=31 & season_match_number<=34)
# test_df <- input_df %>% filter(season_x=="2022_23") %>% filter(season_match_number>=35 & season_match_number<=38)

# 5 Fold Crossvalidation ----
set.seed(123)
folds <- vfold_cv(train_df, strata = NULL, v = 3)
folds

# Recipe ----
mod_recipe <- xgb_reg_recipe(train_df)
mod_recipe

mod_recipe_prepped <- prep(mod_recipe)

baked_df <- mod_recipe_prepped %>%
  bake(new_data = NULL)

head(baked_df) %>% 
  View()

# Model ----
# mod <-
#   boost_tree(tree_depth = tune(),
#              trees = 1000,
#              min_n = tune(),
#              mtry = tune(),
#              learn_rate = tune(),
#              sample_size = 0.9) %>%
#   set_mode("regression") %>%
#   set_engine("xgboost")

mod <-
  boost_tree(
    tree_depth = 20,
    trees = 2000,
    min_n = 200,
    mtry = NULL,
    learn_rate = 0.005,
    sample_size = 0.8
  ) %>%
  set_mode("regression") %>%
  set_engine("xgboost")

# xgb_grid <-
#   grid_latin_hypercube(
#     tree_depth(),
#     min_n(),
#     finalize(mtry(), input_df),
#     learn_rate(),
#   size = 10
# )

# xgb_grid %>% 
#   View()

# Workflow ----
wf <-
  workflow() %>%
  add_model(mod) %>% 
  add_recipe(mod_recipe)

wf

# Fit on cross validation folds ----
# doParallel::registerDoParallel()
# 
# set.seed(234)
# 
# wf_cval <-
#   tune_grid(
#     wf,
#     resamples = folds,
#     grid = xgb_grid,
#     control = control_grid(save_pred = TRUE)
#     )
# 
# wf_cval
# 
# collect_metrics(wf_cval) %>%
#   View()
# 
# wf_cval %>%
#   collect_metrics() %>%
#   filter(.metric == "rmse") %>%
#   select(mean, mtry:learn_rate) %>%
#   pivot_longer(mtry:learn_rate,
#                values_to = "value",
#                names_to = "parameter"
#   ) %>%
#   ggplot(aes(value, mean, color = parameter)) +
#   geom_point(alpha = 0.8, show.legend = FALSE) +
#   facet_wrap(~parameter, scales = "free_x") +
#   labs(x = NULL, y = "RMSE")
# 
# show_best(wf_cval, "rmse")
# 
# best_rmse <- select_best(wf_cval, "rmse")
# best_rmse
# 
# # Finalize Workflow ----
# wf_final <-
#   finalize_workflow(
#     wf,
#     best_rmse)

wf_final <- wf

wf_final

# Fit final workflow on train data ----
wf_fitted <- 
  wf_final %>% 
  fit(data = train_df)

# Variable Importance ----
wf_fitted %>% 
  extract_fit_parsnip() %>% 
  vip(30)

# Evaluate model on train data ----
train_rmse <-
  bind_cols(train_df %>% select(total_points),
            predict(wf_fitted,train_df)) %>% 
  rmse(truth=total_points,estimate=.pred)

bind_cols(train_df %>% select(total_points),
          predict(wf_fitted,train_df)) %>%
  ggplot(aes(x=total_points,y=.pred))+
  geom_point()

# Evaluate model on validation data ----
validation_rmse <-
  bind_cols(validation_df %>% select(total_points),
            predict(wf_fitted,validation_df)) %>% 
  rmse(truth=total_points,estimate=.pred)

bind_cols(validation_df %>% select(total_points),
          predict(wf_fitted,validation_df)) %>%
  ggplot(aes(x=total_points,y=.pred))+
  geom_point()

# Evaluate model on test data ----
test_rmse <-
  bind_cols(test_df %>% select(total_points),
            predict(wf_fitted,test_df)) %>% 
  rmse(truth=total_points,estimate=.pred)

bind_cols(test_df %>% select(total_points),
          predict(wf_fitted,test_df)) %>%
  ggplot(aes(x=total_points,y=.pred))+
  geom_point()

# Model Metrics ----
model_metrics <-
  bind_rows(train_rmse %>% mutate(data_set = "train"),
            validation_rmse %>% mutate(data_set = "validation"),
            test_rmse %>% mutate(data_set = "test"))

# Create Vetiver Model ----
v <- vetiver::vetiver_model(wf_fitted,
                            "xgb_mid_reg",
                            metadata = list(metrics = model_metrics))
v

model_board <-
  pins::board_folder(here(paste0("season ",curr_season,"/models")),
                     versioned = TRUE)
# model_board

model_board %>% 
  vetiver::vetiver_pin_write(v)

# model_board %>% 
#   pins::pin_versions("xgb_def_reg")
# 
# model_board %>% 
#   pins::pin_meta("xgb_def_reg") %>% 
#   pluck("user", "metrics") %>% 
#   as_tibble()
# 
# v %>% 
#   pluck("metadata","user","metrics")


# model_metrics %>% 
#   write_csv(here(paste0("season ",curr_season,"/models"),
#                  "XGBMod_DEF_metrics.csv"))

# # Save Model ----
# wf_fitted %>%
#   readr::write_rds(here(paste0("season ",curr_season,"/models"),
#                         "XGBMod_DEF_Reg.rds"))