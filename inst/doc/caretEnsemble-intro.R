## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  echo = TRUE,
  warning = FALSE,
  message = FALSE
)

## -----------------------------------------------------------------------------
data(Sonar, package = "mlbench")
set.seed(107L)
inTrain <- caret::createDataPartition(y = Sonar$Class, p = 0.75, list = FALSE)
training <- Sonar[inTrain, ]
testing <- Sonar[-inTrain, ]

## -----------------------------------------------------------------------------
model_list <- caretEnsemble::caretList(
  Class ~ .,
  data = training,
  methodList = c("glmnet", "rpart")
)
print(summary(model_list))

## -----------------------------------------------------------------------------
p <- predict(model_list, newdata = head(testing))
knitr::kable(p, format = "markdown")

## -----------------------------------------------------------------------------
model_list_big <- caretEnsemble::caretList(
  Class ~ .,
  data = training,
  methodList = c("glmnet", "rpart"),
  tuneList = list(
    rf1 = caretEnsemble::caretModelSpec(method = "rf", tuneGrid = data.frame(.mtry = 2L)),
    rf2 = caretEnsemble::caretModelSpec(method = "rf", tuneGrid = data.frame(.mtry = 10L), preProcess = "pca"),
    nn = caretEnsemble::caretModelSpec(method = "nnet", tuneLength = 2L, trace = FALSE)
  )
)
print(summary(model_list_big))

## ----fig.alt="X/Y scatter plot of rpart vs glmnet AUCs on the Sonar dataset. The glmnet model is better for 4 out of 5 resamples."----
lattice::xyplot(caret::resamples(model_list))

## -----------------------------------------------------------------------------
caret::modelCor(caret::resamples(model_list))

## -----------------------------------------------------------------------------
greedy_ensemble <- caretEnsemble::caretEnsemble(model_list)
print(summary(greedy_ensemble))

## -----------------------------------------------------------------------------
model_preds <- predict(model_list, newdata = testing, excluded_class_id = 2L)
ens_preds <- predict(greedy_ensemble, newdata = testing, excluded_class_id = 2L)
model_preds$ensemble <- ens_preds
auc <- caTools::colAUC(model_preds, testing$Class)
print(auc)

## -----------------------------------------------------------------------------
p <- predict(greedy_ensemble, newdata = head(testing), excluded_class_id = 0L)
knitr::kable(p, format = "markdown")

## -----------------------------------------------------------------------------
round(caret::varImp(greedy_ensemble), 4L)

## -----------------------------------------------------------------------------
glm_ensemble <- caretEnsemble::caretStack(model_list, method = "glm")
model_preds2 <- model_preds
model_preds2$ensemble <- predict(glm_ensemble, newdata = testing, excluded_class_id = 2L)
print(caTools::colAUC(model_preds2, testing$Class))
CF <- coef(glm_ensemble$ens_model$finalModel)[-1L]
print(CF / sum(CF))

## -----------------------------------------------------------------------------
gbm_ensemble <- caretEnsemble::caretStack(
  model_list,
  method = "gbm",
  verbose = FALSE,
  tuneLength = 5L
)
model_preds3 <- model_preds
model_preds3$ensemble <- predict(gbm_ensemble, newdata = testing, excluded_class_id = 2L)
caTools::colAUC(model_preds3, testing$Class)

