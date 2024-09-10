library(tidyverse)
library(here)
library(ggplot2)
library(GGally)
library(tidymodels)
library(vip)
library(DALEXtra)
library(finetune)

# Load Helper Functions ----
source(here("scripts/season 2024_25","99. Constants.R"))
# source(here("utils","xgb_reg_recipe.R"))
source(here("utils","xgb_classif_recipe.R"))
source(here("utils","load_forward_data.R"))
source(here("utils","load_defender_data.R"))
source(here("utils","load_midfielder_data.R"))
source(here("utils","train_validation_test_split.R"))

# Import Data ----
input_df <- load_forward_data(here(paste0("data/model/season ",curr_season),
                                   "model_data.csv"))

input_df <-
  input_df %>% 
  mutate(scored_more_than_2_points = as.factor(ifelse(total_points>=3,1,0)))

input_df <-
  input_df %>% 
  filter(player_appearance_order>1)

# Train Test Split ----
set.seed(1234)

train_validation_test <- train_validation_test_split(df = input_df, 
                                                     test_season = prev_season,
                                                     validation_season_match_number = 19)

train_df <- train_validation_test$train
validation_df <- train_validation_test$validation
test_df <- train_validation_test$test

# 5 Fold Crossvalidation ----
set.seed(123)
folds <- vfold_cv(train_df, strata = NULL, v = 3)
folds

# Recipe ----
mod_recipe <- xgb_classif_recipe(train_df)
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
#   set_mode("classification") %>%
#   set_engine("xgboost")

mod <-
  boost_tree(
    tree_depth = 6,
    trees = 1000,
    min_n = 100,
    mtry = NULL,
    learn_rate = 0.3,
    sample_size = 0.9
  ) %>%
  set_mode("classification") %>%
  set_engine("xgboost",scale_pos_weight=0.13)

# xgb_grid <-
#   grid_latin_hypercube(
#     tree_depth(),
#     min_n(),
#     finalize(mtry(), input_df),
#     learn_rate(),
#   size = 10
# )
# 
# xgb_grid %>%
#   View()

# Workflow ----
wf <-
  workflow() %>%
  add_model(mod) %>% 
  add_recipe(mod_recipe)

wf

# Fit on cross validation folds ----
doParallel::registerDoParallel()

set.seed(234)

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

# show_best(wf_cval)

# best_roc_auc <- select_best(wf_cval)
# best_roc_auc

# Finalize Workflow ----
# wf_final <-
#   finalize_workflow(
#     wf,
#     best_roc_auc)

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
augment(wf_fitted,train_df) %>% 
  select(.pred_class,.pred_0,.pred_1,scored_more_than_2_points) %>%
  View()

train_roc_auc <-
  augment(wf_fitted,train_df) %>% 
  roc_auc(scored_more_than_2_points,.pred_0)
train_roc_auc

conf_mat(augment(wf_fitted,train_df),
         scored_more_than_2_points,
         .pred_class)

# Evaluate model on validation data ----
validation_roc_auc <-
  augment(wf_fitted,validation_df) %>% 
  roc_auc(scored_more_than_2_points,.pred_0)
validation_roc_auc

conf_mat(augment(wf_fitted,validation_df),
         scored_more_than_2_points,
         .pred_class)

# Evaluate model on test data ----
test_roc_auc <-
  augment(wf_fitted,test_df) %>% 
  roc_auc(scored_more_than_2_points,.pred_0)
test_roc_auc

augment(wf_fitted,test_df) %>% 
  select(name,.pred_class,.pred_0,.pred_1,scored_more_than_2_points,total_points,contains("lag_1"),everything()) %>%
  filter(name=="ollie watkins") %>% 
  # count(name) %>% 
  View()

augment(wf_fitted,test_df) %>%
  filter(name=="gabriel jesus") %>% 
  View()

conf_mat(augment(wf_fitted,test_df),
         scored_more_than_2_points,
         .pred_class)


bind_rows(
  augment(wf_fitted,train_df),
  augment(wf_fitted,validation_df),
  augment(wf_fitted,test_df)
  ) %>% 
  select(name,YEAR,MONTH,DAY,.pred_class,scored_more_than_2_points,total_points,
         minutes,minutes_lag_1,
         contains("_lag_1")) %>% 
  arrange(name,YEAR,MONTH,DAY) %>% 
  filter(name == "ollie watkins") %>% 
  View()

# Install DALEXtra if not already installed
# install.packages("DALEXtra")

# Load DALEXtra
library(DALEXtra)

# Prepare model for DALEXtra
# Extract xgboost model from the workflow
xgb_model <- 
  extract_fit_parsnip(wf_fitted)

# Prepare explainer for the model
explainer <- 
  explain(
  xgb_model,
  data = baked_df %>% select(-scored_more_than_2_points), # Use your baked data without the target
  y = as.numeric(baked_df$scored_more_than_2_points),
  label = "XGBoost Model")

# Specify the data points you want SHAP values for
# For example, get SHAP values for player 'Ollie Watkins'
data_points <- 
  mod_recipe_prepped %>% 
  bake(new_data = 
         input_df %>%
         filter(name == "ollie watkins") %>%
         filter(YEAR==2022) %>% 
         filter(MONTH==2) %>% 
         filter(DAY==13) %>% 
         select(-scored_more_than_2_points)) # Apply the same preprocessing as for training data

# Ensure the data points are in the same format as the training data
data_points <- data_points %>% as.data.frame()

# Calculate SHAP values for these specific data points
shap_values <- predict_parts(explainer, 
                             new_observation = data_points,
                             type = "shap")

library(forcats)
shap_values %>%
  group_by(variable) %>%
  mutate(mean_val = mean(contribution)) %>%
  ungroup() %>%
  mutate(variable = fct_reorder(variable, abs(mean_val))) %>%
  ggplot(aes(contribution, variable, fill = mean_val > 0)) +
  geom_col(data = ~distinct(., variable, mean_val), 
           aes(mean_val, variable), 
           alpha = 0.5) +
  geom_boxplot(width = 0.5) +
  theme(legend.position = "none") +
  scale_fill_viridis_d() +
  labs(y = NULL)

