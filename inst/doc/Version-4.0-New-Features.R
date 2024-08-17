## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  echo = TRUE,
  warning = FALSE,
  message = FALSE
)
set.seed(42L)

## -----------------------------------------------------------------------------
model_list <- caretEnsemble::caretList(
  x = iris[, 1L:4L],
  y = iris[, 5L],
  methodList = c("rpart", "rf")
)
print(summary(model_list))

## -----------------------------------------------------------------------------
ens <- caretEnsemble::caretEnsemble(model_list)
print(summary(ens))

## -----------------------------------------------------------------------------
print(ens)
print(summary(ens))

## ----fig.alt="A dot and whisker plot of ROC for glmnet, rpart, and an ensemble. The ensemble has the highest ROC and is slighly better than the glmnet. The rpart model is bad."----
plot(ens)

## ----fig.alt="A 4-panel plot for glmnet, rpart, and an ensemble. The ensemble has the highest ROC and is slighly better than the glmnet. The rpart model is bad. The glmnet has the highest weight, and the residuals look biased."----
ggplot2::autoplot(ens)

## -----------------------------------------------------------------------------
class_control <- caretEnsemble::defaultControl(iris$Species)
print(ls(class_control))

## -----------------------------------------------------------------------------
reg_control <- caretEnsemble::defaultControl(iris$Sepal.Length)
print(ls(reg_control))

## -----------------------------------------------------------------------------
y <- iris[, 1L]
x <- iris[, 2L:3L]
flex_list <- caretEnsemble::caretList(
  x = x,
  y = y,
  methodList = c("rpart", "rf"),
  trControl = caretEnsemble::defaultControl(y, number = 3L)
)

flex_list$glm_boot <- caret::train(
  x = x,
  y = y,
  method = "glm",
  trControl = caretEnsemble::defaultControl(y, method = "boot", number = 25L)
)

flex_ens <- caretEnsemble::caretEnsemble(flex_list)
print(flex_ens)

## -----------------------------------------------------------------------------
X <- iris[, 1L:4L]

target_class <- iris[, 5L]
target_reg <- as.integer(iris[, 5L] == "virginica")

ctrl_class <- caretEnsemble::defaultControl(target_class)
ctrl_reg <- caretEnsemble::defaultControl(target_reg)

model_class <- caret::train(iris[, 1L:4L], target_class, method = "rf", trControl = ctrl_class)
model_reg <- caret::train(iris[, 1L:4L], target_reg, method = "rf", trControl = ctrl_reg)
mixed_list <- caretEnsemble::as.caretList(list(class = model_class, reg = model_reg))
mixed_ens <- caretEnsemble::caretEnsemble(mixed_list)
print(mixed_ens)

## -----------------------------------------------------------------------------
train_idx <- sample.int(nrow(iris), 100L)
train_data <- iris[train_idx, ]
new_data <- iris[-train_idx, ]

model_list <- caretEnsemble::caretList(
  x = train_data[, 1L:4L],
  y = train_data[, 5L],
  methodList = c("rpart", "rf")
)

transfer_ens <- caretEnsemble::caretEnsemble(
  model_list,
  new_X = new_data[, 1L:4L],
  new_y = new_data[, 5L]
)

print(transfer_ens)

## -----------------------------------------------------------------------------
preds <- predict(transfer_ens, newdata = head(new_data))
knitr::kable(preds, format = "markdown")

## -----------------------------------------------------------------------------
importance <- caret::varImp(transfer_ens)
print(round(importance, 2L))

