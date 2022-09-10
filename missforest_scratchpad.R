library(tidyverse)
library(missForest)
library(woeBinning)
library(skimr)
library(naniar)
library(randomForest)
library(PRROC)


# https://rpubs.com/lmorgan95/MissForest
woeBinning::germancredit %>% glimpse()

# train/test:
set.seed(111)
train_index <- sample(nrow(germancredit), 700) 

train <- germancredit[train_index, ]
train %>% skim()
train_X <- prodNA(select(train, -creditability), 0.2)
train_X %>% skim()
train_X %>% glimpse()

test <- germancredit[-train_index, ]
test %>% skim()
test_X <- prodNA(select(test, -creditability), 0.2)
test_X %>% skim()

vis_miss(rbind(train_X, test_X), show_perc = F) + 
        coord_flip()


# 1) impute train
imp_train_X <- missForest(train_X)$ximp
imp_train_X 
imp_train_X %>% glimpse()
imp_train_X %>% skim()

# 2) build model
rf <- randomForest(x = imp_train_X, y = train$creditability)
rf

# 3) & 4) combine & impute test
train_test_X <- rbind(test_X, imp_train_X)
imp_test_X <- missForest(train_test_X)$ximp[1:nrow(test_X), ]

# 5) predict for test
pred_test <- predict(rf, imp_test_X, type = "prob")

# 6) test ROC & AUC
test_scores <- data.frame(event_prob = pred_test[ ,2], labels = test$creditability)

test_roc_v1 <- roc.curve(scores.class0 = test_scores[test_scores$labels == "good", ]$event_prob, # scores for the POSITIVE class
                         scores.class1 = test_scores[test_scores$labels == "bad", ]$event_prob, # scores for the NEGATIVE class
                         curve=T)


