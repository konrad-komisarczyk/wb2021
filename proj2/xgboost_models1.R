
d_exp <- read.csv("data/expired_dataextra.csv") %>% 
  select(-X)

d_icu <- read.csv("data/icu_dataextra.csv") %>% 
  select(-X)


d_exp %>% pull(death) %>% mean()
d_exp %>% filter(Procalcitonin > 2) %>% pull(death) %>% mean()


# models
n <- nrow(d_exp)
set.seed(123)

train_indices <- sample.int(n, size = 0.75 * n)

exp_train <- d_exp[train_indices, ]
exp_test <- d_exp[-train_indices, ]

exp_train_x <- exp_train %>% 
  select(-death)
exp_train_y <- exp_train %>% 
  pull(death)
exp_test_x <- exp_test %>% 
  select(-death)
exp_test_y <- exp_test %>% 
  pull(death)


xgb_1 <- xgboost(data = as.matrix(exp_train_x), label = exp_train_y, 
                 params = list(max_depth = 6), nrounds = 200)

unified_1 <- xgboost.unify(xgb_1, exp_train_x)
shaps_1 <- treeshap(unified_1, exp_train_x)
plot_contribution(shaps_1, obs = 2)
plot_feature_importance(shaps_1)
plot_feature_dependence(shaps_1, "Procalcitonin")


preds <- predict(xgb_1, as.matrix(exp_test_x))

roc_curve <- pROC::roc(response = factor(exp_test_y), predictor = preds, levels = c(0, 1))

plot(roc_curve)
pROC::auc(roc_curve)


# threshold <- roc_curve$thresholds[which.max(roc_curve$sensitivities + roc_curve$specificities)]
threshold <- 0.15

confm <- caret::confusionMatrix(factor(ifelse(preds > threshold, 1, 0)), 
                                factor(exp_test_y))
confm


