Daft <- read.csv(file="daft_datav03.csv", head=TRUE, sep=",")

summary(Daft)
str(Daft)

## Install Necessary packages
install.packages("C50")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("rattle")

library(C50)
library(rpart)
library(rpart.plot)
library(rattle)

## Pre-processing

# Removing character data so that we can focus on numerical & factors
Daft2 <- Daft[,c(4:9)]
Daft2$house_type <- as.factor(Daft2$house_type)

str(Daft2)

# Removing NAs
df <- na.omit(Daft2)
df = df[df$listing_size.m2.>=1,]
summary(df)

shapiro.test(x = Daft2$price)

# Checking median of housing price
median(df$price) ## 230000

## while loop...
x = 1
while (x <= 155) {
  if (df$price[x] < 230000) {      # While loop to allocate 1 (yes) or 0 (no) to show whether house has below median price or not
    df$price[x] = 0
    x = x + 1
  } else {
    df$price[x] = 1
    x = x + 1
  }
}

df$price <- as.factor(df$price)  ## Making the column into a factor
df$bathrooms <- as.numeric(df$bathrooms)
df$bedrooms <- as.numeric(df$bedrooms)
df$X.energy_rating.Kwh. <- as.numeric(df$X.energy_rating.Kwh.)
df$listing_size.m2. <- as.numeric(df$listing_size.m2.)
str(df)

# Training & test samples
set.seed(12)
index <- sample(1:nrow(df), size = nrow(df)*0.8, replace = FALSE)
df_train <- df[index,]
df_test <- df[-index,]


## Data-mining Algorithm
Daft_treeModel1 <- rpart(price~., data=df_train, method="class",
                         control=rpart.control(minsplit = 20, minbucket = 8, maxdepth = 10, usesurrogate = 2, xval = 10)) 

Daft_treeModel2 <- rpart(price~., data=df_test, method="class",
                         control=rpart.control(minsplit = 5, minbucket = 2, maxdepth = 10, usesurrogate = 2)) 

summary(Daft_treeModel1)

plot(Daft_treeModel1)
text(Daft_treeModel1)


## Beautifying the tree
library(rpart.plot)
library(RColorBrewer)
library(rattle)

prp(Daft_treeModel1, faclen = 3, cex = 0.8, extra ="auto")
prp(Daft_treeModel2, faclen = 3, cex = 0.8, extra ="auto")


## Improve tree by pruning
printcp(Daft_treeModel1)

?printcp

bestcp <- Daft_treeModel1$cptable[which.min(Daft_treeModel1$cptable[, "xerror"]), "CP"]
bestcp_test <- Daft_treeModel2$cptable[which.min(Daft_treeModel2$cptable[, "xerror"]), "CP"]

# Creating pruned tree
PrunedTree <- prune(Daft_treeModel1, cp = bestcp)
PrunedTree_test <- prune(Daft_treeModel2, cp = bestcp_test)

## PLotting Pruned Tree
prp(PrunedTree, faclen = 3, cex = 0.8, extra ="auto")
prp(PrunedTree_test, faclen = 3, cex = 0.8, extra ="auto")


## Evaluating the results
## OR ##
library(caret)
confusionMatrix(predict(Daft_treeModel1, df_train, type = "class"), df_train$price)
confusionMatrix(predict(Daft_treeModel2, df_test, type = "class"), df_test$price)
confusionMatrix(predict(PrunedTree_test, df_test, type = "class"), df_test$price)

## ROC Curve
install.packages("ROCR")
library(ROCR)

val1 <- predict(Daft_treeModel2, df_test, type="prob")
val1

## storing model performance scores
pred_val <- prediction(val1[,2], df_test$price)

## Calculating area under the curve
perf_val <- performance(pred_val, "auc")  # auc = area under curve

## Plot the performance
plot(performance(pred_val, measure = "lift", x.measure = "rpp"), colorize=TRUE)

## Finding true positive v false positive rate
perf_val <- performance(pred_val, "tpr", "fpr")

plot(perf_val, color= "green", lwd = 1.5)

### Improving model performance using Random Forest
install.packages("randomForest")
library(randomForest)

# creating random forest model
rn_model <- randomForest(price~., data = df, subset = index)  # full data, prior to split into train and test, then index for train & test split
rn_model

# create prediction model for random forest
pred.rf <- predict(rn_model, newdata = df_test, type = "class")

# Show model on table
table(df$price[-index], pred.rf)

confusionMatrix(pred.rf, df_test$price)

plot(rn_model)