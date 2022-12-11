# Decision Tree
decision_spec <- decision_tree() %>%
  set_engine("rpart")
decision_tree_spec <- decision_spec %>%
  set_mode("classification")
decision_tree_wf <- workflow() %>%
  add_model(decision_tree_spec %>% set_args(cost_complexity = tune())) %>%
  add_recipe(adapt_recipe)
save(decision_tree_wf, file = "decision_tree_wf.rda")

set.seed(1029)
adapt_dt_grid <- grid_regular(cost_complexity(range = c(-3, -1)), levels = 10)
dt_tune_res <- tune_grid(
  decision_tree_wf, 
  resamples = adapt_fold, 
  grid = adapt_dt_grid, 
  metrics = metric_set(roc_auc)
) 
save(dt_tune_res, file = "dt_tune_res.rda")

# Random Forest Tree
rf_spec <- rand_forest(mtry = tune(), trees = tune(), min_n = tune()) %>%
  set_engine("ranger", importance = "impurity") %>%
  set_mode("classification")
rf_wf <- workflow() %>%
  add_model(rf_spec) %>%
  add_recipe(adapt_recipe)
save(rf_wf, file = "rf_wf.rda")

set.seed(1029)
adapt_rf_grid <- grid_regular(mtry(range = c(1, 13)), trees(range = c(1, 10)), 
                              min_n(range = c(1, 15)), levels = 10)
rf_tune_res <- tune_grid(
  rf_wf, 
  resamples = adapt_fold, 
  grid = adapt_rf_grid, 
  metrics = metric_set(roc_auc)
)
save(rf_tune_res, file = "rf_tune_res.rda")

# Boosted Tree 
boost_spec <- boost_tree(mtry = tune(), trees = tune(), min_n = tune(), tree_depth = 4) %>%
  set_engine("xgboost") %>%
  set_mode("classification")
boost_wf <- workflow() %>%
  add_model(boost_spec) %>%
  add_recipe(adapt_recipe)
save(boost_wf, file = "boost_wf.rda")

set.seed(1029)
adapt_boost_grid <- grid_regular(mtry(range = c(1, 13)), trees(range = c(10, 2000)), 
                              min_n(range = c(1, 20)), levels = 10)
boost_tune_res <- tune_grid(
  boost_wf, 
  resamples = adapt_fold, 
  grid = adapt_boost_grid, 
  metrics = metric_set(roc_auc)
)
save(boost_tune_res, file = "boost_tune_res.rda")

# K-Nearest Neighbors (KNN)
knn_spec <- nearest_neighbor(neighbors = tune()) %>%
  set_engine("kknn") %>%
  set_mode("classification")
knn_wf <- workflow() %>%
  add_model(knn_spec) %>%
  add_recipe(adapt_recipe)
save(knn_wf, file = "knn_wf.rda")

set.seed(1029)
adapt_knn_grid <- grid_regular(neighbors(range = c(3, 15)), levels = 10)
knn_tune_res <- tune_grid(
  knn_wf, 
  resamples = adapt_fold, 
  grid = adapt_knn_grid, 
  metrics = metric_set(roc_auc)
)
save(knn_tune_res, file = "knn_tune_res.rda")
