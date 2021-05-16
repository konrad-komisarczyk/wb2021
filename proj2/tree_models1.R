set.seed(123)

d_exp <- read.csv("proj2/data/expired_dataextra.csv") %>% 
  select(-X)

d_icu <- read.csv("proj2/data/icu_dataextra.csv") %>% 
  select(-X)


# EXPIRED
n <- nrow(d_exp)

train_indices <- sample.int(n, size = 0.75 * n)

exp_train <- d_exp[train_indices, ]
exp_test <- d_exp[-train_indices, ]

# write.csv(exp_train, "proj2/data/splitted/expired_train.csv")
# write.csv(exp_test, "proj2/data/splitted/expired_test.csv")

exp_train_x <- exp_train %>% 
  select(-death)
exp_train_y <- exp_train %>% 
  pull(death)
exp_test_x <- exp_test %>% 
  select(-death)
exp_test_y <- exp_test %>% 
  pull(death)


# ICU

n <- nrow(d_icu)

train_indices <- sample.int(n, size = 0.75 * n)

icu_train <- d_icu[train_indices, ]
icu_test <- d_icu[-train_indices, ]

# write.csv(icu_train, "proj2/data/splitted/icu_train.csv")
# write.csv(icu_test, "proj2/data/splitted/icu_test.csv")


icu_train_x <- icu_train %>% 
  select(-ICU.or.not)
icu_train_y <- icu_train %>% 
  pull(ICU.or.not)
icu_test_x <- icu_test %>% 
  select(-ICU.or.not)
icu_test_y <- icu_test %>% 
  pull(ICU.or.not)




# TUNING
xgboost_tuning <- function(nrounds = c(1, 2, 3, 4, 5, 6, 8, 10, runif(10, 11, 300)), 
                                           max_depth = c(2, 3, 4, 5, 6, 7), 
                                           train_x, train_y, test_x, test_y) {
  aucs <- data_frame(nrounds = numeric(0), max_depth = numeric(0), auc = numeric(0))
  for (i in nrounds) {
    for (j in max_depth) {
      xgb <- xgboost(data = as.matrix(train_x), label = train_y, 
                     params = list(max_depth = j), nrounds = i, verbose = 0)
      
      preds <- predict(xgb, as.matrix(test_x))
      roc_curve <- pROC::roc(response = factor(test_y), predictor = preds, levels = c(0, 1))
      auc <- pROC::auc(roc_curve)
      aucs <- aucs %>% 
        rbind(data_frame(nrounds = i, max_depth =j, auc = auc))
    }
  }
  return(aucs)
}

ranger_tuning_death <- function(nrounds = c(1, 3, 5, runif(11, 6, 500)), 
                                           max_depth = c(2, 3, 4, 5, NULL)) {
  aucs <- data_frame(nrounds = numeric(0), max_depth = numeric(0), auc = numeric(0), try = numeric(0))
  for (i in nrounds) {
    for (j in max_depth) {
      for (try in 1:5) {
        ranger_2 <- ranger(formula = death ~ ., data = exp_train, num.trees = i, max.depth = j)
        preds <- predict(ranger_2, as.matrix(exp_test_x))$predictions
        roc_curve <- pROC::roc(response = factor(exp_test_y), predictor = preds, levels = c(0, 1))
        auc <- pROC::auc(roc_curve)
        aucs <- aucs %>% 
          rbind(data_frame(nrounds = i, max_depth =j, auc = auc, try = try))
        
      }
    }
  }
  return(aucs)
}

ranger_tuning_icu <- function(nrounds = c(1, 3, 5, runif(11, 6, 500)), 
                                max_depth = c(2, 3, 4, 5, NULL)) {
  aucs <- data_frame(nrounds = numeric(0), max_depth = numeric(0), auc = numeric(0), try = numeric(0))
  for (i in nrounds) {
    for (j in max_depth) {
      for (try in 1:5) {
        ranger_5 <- ranger(formula = ICU.or.not ~ ., data = icu_train, num.trees = 150, max.depth = NULL)
        preds <- predict(ranger_5, as.matrix(icu_test_x))$predictions
        roc_curve <- pROC::roc(response = factor(icu_test_y), predictor = preds, levels = c(0, 1))
        auc <- pROC::auc(roc_curve)
        aucs <- aucs %>% 
          rbind(data_frame(nrounds = i, max_depth =j, auc = auc, try = try))
      }
    }
  }
  return(aucs)
}

xgb_tunes_death <- xgboost_tuning(train_x = exp_train_x, 
                                  train_y = exp_train_y, 
                                  test_x = exp_test_x, 
                                  test_y = exp_test_y)

xgb_tunes_icu <- xgboost_tuning(train_x = icu_train_x, 
                                train_y = icu_train_y, 
                                test_x = icu_test_x, 
                                test_y = icu_test_y)

rf_tunes_death <- ranger_tuning_death()
rf_tunes_icu <- ranger_tuning_icu()


xgb_tunes_death %>% 
  arrange(desc(auc))

xgb_tunes_icu %>% 
  arrange(desc(auc))

rf_tunes_death %>% 
  group_by(nrounds, max_depth) %>% 
  summarise(mean_auc = mean(auc), min_auc = min(auc), max_auc = max(auc)) %>% 
  arrange(desc(mean_auc))

rf_tunes_icu %>% 
  group_by(nrounds, max_depth) %>% 
  summarise(mean_auc = mean(auc), min_auc = min(auc), max_auc = max(auc)) %>% 
  arrange(desc(mean_auc))


# XGBOOST EXPIRED

xgb_1 <- xgboost(data = as.matrix(exp_train_x), label = exp_train_y, 
                 params = list(max_depth = 4), nrounds = 4)




unified_1 <- xgboost.unify(xgb_1, exp_train_x)
shaps_1 <- treeshap(unified_1, exp_train_x)
plot_contribution(shaps_1, obs = 2)
plot_feature_importance(shaps_1)
# plot_feature_dependence(shaps_1, "Procalcitonin")


preds <- predict(xgb_1, as.matrix(exp_test_x))


roc_curve <- pROC::roc(response = factor(exp_test_y), predictor = preds, levels = c(0, 1))

plot(roc_curve)
pROC::auc(roc_curve)


threshold <- 0.2

confm <- caret::confusionMatrix(factor(ifelse(preds > threshold, 1, 0)), 
                                factor(exp_test_y))
confm




# RANDOM FOREST EXPIRED
ranger_2 <- ranger(formula = death ~ ., data = exp_train, num.trees = 135, max.depth = 5)

unified_2 <- ranger.unify(ranger_2, exp_train_x)
shaps_2 <- treeshap(unified_2, exp_train_x)
# plot_contribution(shaps_2, obs = 1)
plot_feature_importance(shaps_2)

preds <- predict(ranger_2, as.matrix(exp_test_x))$predictions

roc_curve <- pROC::roc(response = factor(exp_test_y), predictor = preds, levels = c(0, 1))

plot(roc_curve)
pROC::auc(roc_curve)

threshold <- 0.11

confm <- caret::confusionMatrix(factor(ifelse(preds > threshold, 1, 0)), 
                                factor(exp_test_y))
confm


# RPART EXPIRED

rpart_3 <- rpart(death ~ ., data = exp_train)

preds <- predict(rpart_3, exp_test_x)
roc_curve <- pROC::roc(response = factor(exp_test_y), predictor = preds, levels = c(0, 1))
plot(roc_curve)
pROC::auc(roc_curve)

threshold <- 0.2
confm <- caret::confusionMatrix(factor(ifelse(preds > threshold, 1, 0)), 
                                factor(exp_test_y))
confm





# XGBOOST ICU

xgb_4 <- xgboost(data = as.matrix(icu_train_x), label = icu_train_y, 
                 params = list(max_depth = 4), nrounds = 6)



unified_4 <- xgboost.unify(xgb_4, icu_train_x)
shaps_4 <- treeshap(unified_4, icu_train_x)
plot_feature_importance(shaps_4)


preds <- predict(xgb_4, as.matrix(icu_test_x))


roc_curve <- pROC::roc(response = factor(icu_test_y), predictor = preds, levels = c(0, 1))

plot(roc_curve)
pROC::auc(roc_curve)


threshold <- 0.2

confm <- caret::confusionMatrix(factor(ifelse(preds > threshold, 1, 0)), 
                                factor(icu_test_y))
confm


# RANDOM FOREST ICU
ranger_5 <- ranger(formula = ICU.or.not ~ ., data = icu_train, num.trees = 389, max.depth = 4)

unified_5 <- ranger.unify(ranger_5, icu_train_x)
shaps_5 <- treeshap(unified_5, icu_train_x)
plot_feature_importance(shaps_5)

preds <- predict(ranger_5, as.matrix(icu_test_x))$predictions

roc_curve <- pROC::roc(response = factor(icu_test_y), predictor = preds, levels = c(0, 1))

plot(roc_curve)
pROC::auc(roc_curve)

threshold <- 0.2

confm <- caret::confusionMatrix(factor(ifelse(preds > threshold, 1, 0)), 
                                factor(icu_test_y))
confm

