
cores <- parallel::detectCores(logical = F)
doParallel::registerDoParallel(cores = cores-1)

load("data/data.RData")
colnames(d)

library(tidyverse)
library(tidymodels)

set.seed(666)
tt_split <- initial_split(d, 
                          prop = .9, 
                          strata = choice_of_party)
train_set <- training(tt_split)
test_set <- testing(tt_split)

model_recipe <- recipe(choice_of_party ~ ., data = train_set) %>%
  update_role(id, new_role = "ID") %>%
  step_naomit(everything(), skip = T) %>%
  step_novel(all_nominal(), -all_outcomes()) %>%
  step_normalize(all_numeric(), -all_outcomes()) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>%
  step_zv(all_numeric(), -all_outcomes()) %>%
  step_corr(all_predictors(), threshold = 0.7, method = "spearman")

summary(model_recipe)

prepped_data <- model_recipe %>% prep() %>% juice()

set.seed(666)
cv_folds <- vfold_cv(train_set, v = 10, strata = choice_of_party)

knn_spec <- 
  nearest_neighbor(neighbors = 8) %>%
  set_engine("kknn") %>% 
  set_mode("classification")

knn_wflow <-
  workflow() %>%
  add_recipe(model_recipe) %>% 
  add_model(knn_spec)

knn_res <- 
  knn_wflow %>% 
  fit_resamples(
    resamples = cv_folds, 
    metrics = metric_set(recall, precision, f_meas, 
                         accuracy, kap,
                         roc_auc, sens, spec),
    control = control_resamples(save_pred = T)
  ) 
knn_res %>% collect_metrics(summarize = T)

knn_metrics <- 
  knn_res %>% 
  collect_metrics(summarise = T) %>%
  mutate(model = "KNN")

rf_spec <- 
  rand_forest() %>% 
  set_engine("ranger",
             num.trees = 1000,
             importance = "impurity",
             seed = 666) %>% 
  set_mode("classification")

rf_wflow <-
  workflow() %>%
  add_recipe(model_recipe) %>% 
  add_model(rf_spec)

rf_res <-
  rf_wflow %>% 
  fit_resamples(
    resamples = cv_folds, 
    metrics = metric_set(recall, precision, f_meas, 
                         accuracy, kap,
                         roc_auc, sens, spec),
    control = control_resamples(save_pred = T)
  )
rf_res %>%  collect_metrics(summarize = T)

rf_metrics <- 
  rf_res %>% 
  collect_metrics(summarise = T) %>%
  mutate(model = "Random Forest")

xgb_spec <- 
  boost_tree(trees = 1000,
             learn_rate = 0.01) %>% 
  set_engine("xgboost") %>% 
  set_mode("classification")

xgb_wflow <-
  workflow() %>%
  add_recipe(model_recipe) %>% 
  add_model(xgb_spec)

xgb_res <- 
  xgb_wflow %>% 
  fit_resamples(
    resamples = cv_folds, 
    metrics = metric_set(recall, precision, f_meas, 
                         accuracy, kap,
                         roc_auc, sens, spec),
    control = control_resamples(save_pred = T)
  )
xgb_res %>% collect_metrics(summarize = T)

xgb_metrics <- 
  xgb_res %>% 
  collect_metrics(summarise = T) %>%
  mutate(model = "XGBoost")

last_fit(rf_wflow, 
         split = tt_split,
         metrics = metric_set(recall, precision, f_meas, 
                              accuracy, kap,
                              roc_auc, sens, spec)) %>% collect_metrics()

ML_op_acc <- bind_rows(knn_metrics, rf_metrics, xgb_metrics) %>%
  select(model, .metric, mean, std_err) %>% 
  pivot_wider(names_from = .metric, values_from = c(mean, std_err)) %>%
  arrange(mean_accuracy) %>% 
  mutate(model = fct_reorder(model, mean_accuracy)) %>%
  ggplot(aes(model, mean_accuracy, fill=model)) +
  geom_col() +
  coord_flip() +
  xlab("Mean Accuracy -- MultiClass") +
  ylab("Model") + 
  labs(title="Model Metrics Evaluation",
       subtitle = "without the latent class",
       caption = "Best Model is RF with .43 in Train Set & .48 in Test Set") +
  theme_bw()

ML_op_auc <- bind_rows(knn_metrics, rf_metrics, xgb_metrics) %>%
  select(model, .metric, mean, std_err) %>% 
  pivot_wider(names_from = .metric, values_from = c(mean, std_err)) %>%
  arrange(mean_roc_auc) %>% 
  mutate(model = fct_reorder(model, mean_roc_auc)) %>%
  ggplot(aes(model, mean_roc_auc, fill = model)) +
  geom_col() +
  coord_flip() +
  xlab("Mean ROC AUC -- MultiClass") +
  ylab("Model") + 
  labs(subtitle = "Raw Prepped Data", 
       caption = "Best Model is RF with .74 in Train Set & .77 in Test Set") +
  theme_bw() + theme(legend.position = "none")

rf_pred_roc_auc <- rf_res %>% collect_predictions() %>% 
  group_by(id) %>%
  roc_curve(choice_of_party, `.pred_CDU / CSU`:`.pred_UNDECIDED`) %>% 
  autoplot() + labs(subtitle = "ROC AUC")

gridExtra::grid.arrange(ML_op_auc, rf_pred_roc_auc, ncol = 2,
                        top = "Model Evaluation Metrics")
