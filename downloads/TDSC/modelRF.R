# assumes group1_1 and group2_1 are loaded into workspace
# builds the FINAL MODELS, which are randomforests

library(randomForest)
library(C50)
library(gmodels)
library(caret)

group1_1$purchased_eggs <- as.factor(group1_1$purchased_eggs)
group2_1$purchased_eggs <- as.factor(group2_1$purchased_eggs)

set.seed(1234)
group1_1_rand <- group1_1[order(runif(nrow(group1_1))),]
group2_1_rand <- group2_1[order(runif(nrow(group2_1))),]

train1_1 <- group1_1_rand[1:270,]
test1_1 <- group1_1_rand[271:338,]

train2_1 <- group2_1_rand[1:503,]
test2_1 <- group2_1_rand[504:629,]

# this trains a decision tree. I ultimately decided on random forests
tree1_1 <- C5.0(train1_1[,-22], train1_1$purchased_eggs)
tree1_1_pred <- predict(tree1_1,test1_1[-22])
CrossTable(test1_1$purchased_eggs,tree1_1_pred,prop.chisq = FALSE,prop.c = FALSE, prop.r = FALSE, dnn = c("actual", "predicted"))

# training randomforests
set.seed(567)
ctrl <- trainControl(method="cv",number=5)
grid_rf <- expand.grid(.mtry = c(16))
rf1_1 <- train(purchased_eggs ~ ., data = group1_1_rand, method = "rf",
              metric = "Kappa", trControl = ctrl, tuneGrid = grid_rf)
set.seed(89)
rf2_1 <- train(purchased_eggs ~ ., data = group2_1_rand, method = "rf",
               metric = "Kappa", trControl = ctrl, tuneGrid = grid_rf)
