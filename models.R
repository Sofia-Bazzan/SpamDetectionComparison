# Naive Bayes
{r, echo=TRUE}
nb_model <- naiveBayes(class ~ ., data = train_new)
probabilities_nb <- predict(nb_model, newdata = validation, type = "raw")
# Make predictions on the validation data (with the 'default' threshold of 0.5)
predictions_nb_05 <- predict(nb_model, newdata = validation)

cm_nb_05 <- table(predictions_nb_05, validation$class)
accuracy_nb_05 <- sum(diag(cm_nb_05)) / sum(cm_nb_05)
print(paste("Accuracy with 0.5:", accuracy_nb_05))
precision_nb_05=(cm_nb_05[1,1]/(cm_nb_05[1,1]+cm_nb_05[1,2]))
print(paste("Precision with 0.5",precision_nb_05))
{r, echo=TRUE}
pred_nb_06<- ifelse(probabilities_nb[, "1"] > 0.6, 1, 0)
cm_nb_06=table(validation$class, pred_nb_06)
accuracy_nb_06 = (cm_nb_06[1,1]+cm_nb_06[2,2])/nrow(test)
accuracy_nb_06 <- sum(diag(cm_nb_06)) / sum(cm_nb_06)
print(paste("Accuracy with 0.6:", accuracy_nb_06))
precision_nb_06=(cm_nb_06[1,1]/(cm_nb_06[1,1]+cm_nb_06[1,2]))
print(paste("Precision with 0.6:",precision_nb_06))
{r, echo=TRUE}
pred_nb_07<- ifelse(probabilities_nb[, "1"] > 0.7, 1, 0)
cm_nb_07=table(validation$class, pred_nb_07)
accuracy_nb_07 = (cm_nb_07[1,1]+cm_nb_07[2,2])/nrow(test)
accuracy_nb_07 <- sum(diag(cm_nb_07)) / sum(cm_nb_07)
print(paste("Accuracy with 0.7:", accuracy_nb_07))
precision_nb_07=(cm_nb_07[1,1]/(cm_nb_07[1,1]+cm_nb_07[1,2]))
print(paste("Precision with 0.7:",precision_nb_07))
{r, echo=TRUE}
probabilities_nb <- predict(nb_model, newdata = test, type = "raw")
pred_nb<- ifelse(probabilities_nb[, "1"] > 0.7, 1, 0)
cm_nb=table(test$class, pred_nb)
accuracy_nb = (cm_nb[1,1]+cm_nb[2,2])/nrow(test)
accuracy_nb
precision_nb=(cm_nb[1,1]/(cm_nb[1,1]+cm_nb[1,2]))
precision_nb
tpr_nb=(cm_nb[1,1])/(cm_nb[1,1]+cm_nb[2,1])
tpr_nb
tnr_nb=(cm_nb[2,2])/(cm_nb[2,2]+cm_nb[1,2])
tnr_nb
f1_nb = 2 * (precision_nb * tpr_nb) / (precision_nb + tpr_nb)
f1_nb
{r, echo=FALSE}
library(pROC)
roc_nb <- roc(test$class, probabilities_nb[, "1"])
plot(roc_nb, col = "blue", main = "ROC Curve", lwd = 2,print.auc=TRUE)
auc_nb <- auc(roc_nb)
coords <- coords(roc_nb, "best", best.method = "closest.topleft")
opt_thr_nb <- coords$threshold
plot(roc_nb, main = "ROC curve for naive bayes", print.auc = TRUE, auc.polygon = TRUE,
     max.auc.polygon = TRUE, grid = TRUE, legacy.axes = TRUE,
     xlab = "False positive rate", ylab = "True positive rate")
abline(v = 1-opt_thr_nb, col = "red", lty = 2)
print(paste("The optimal threshold is:", opt_thr_nb))
{r, echo=TRUE}
#RESIDUALS
res_nb <- test$class - probabilities_nb[,2]
mse_nb <- mean((res_nb)^2)
mse_nb
mae_nb <- mean(abs(res_nb))
mae_nb
#kNN
{r, echo=TRUE}
accuracy_all_knn=vector()
precision_all_knn=vector()
for (k in 1:100) {
  knn_=knn(train_new[,-ncol(train_new)], validation[,-ncol(validation)], cl = train_new$class, k = k)
  c=table(knn_,validation$class)
  accuracy_iter=(c[1,1]+c[2,2])/nrow(validation)
  precision_iter=c[2,2]/(c[2,2]+c[2,1])
  accuracy_all_knn=c(accuracy_all_knn,accuracy_iter)
  precision_all_knn=c(precision_all_knn,precision_iter)
  
}
print("ACCURACY FOR BEST K:")
print(max(accuracy_all_knn))
print("K=")
print(which.max(accuracy_all_knn))
print("###########################")
print("PRECISION FOR BEST K:")
print(max(precision_all_knn))
print("K=")
print(which.max(precision_all_knn))
{r, echo=TRUE}
accuracy_k=data.frame(K=1:100,ACCURACY=accuracy_all_knn)
ggplot(accuracy_k, aes(x = K, y = ACCURACY)) +
  geom_line(colour="blue") +
  geom_point(colour="blue") + ggtitle("Accuracy of K-NN given K")

precision_k <- data.frame(K = 1:100, PRECISION = precision_all_knn)
#sensitivity_k=data.frame(K=1:100, SENSITIVITY=sensitivity_all_knn)
ggplot(precision_k, aes(x = K, y = PRECISION)) +
  geom_line(colour="blue") +
  geom_point(colour="blue") + ggtitle("Precision of K-NN given K")
{r, echo=TRUE}
sixtytwo_nn=knn(train_new[,-ncol(train_new)], test[,-ncol(test)], cl = train_new$class, k = 62)
cm_knn=table(test$class, sixtytwo_nn)
accuracy_knn = (cm_nb[1,1]+cm_nb[2,2])/nrow(test)
accuracy_knn
precision_knn=(cm_nb[1,1]/(cm_nb[1,1]+cm_nb[1,2]))
precision_knn
tpr_knn=(cm_knn[1,1])/(cm_knn[1,1]+cm_knn[2,1])
tpr_knn
tnr_knn=(cm_nb[2,2])/(cm_nb[2,2]+cm_nb[1,2])
tnr_knn
f1_knn = 2 * (precision_knn * tpr_knn) / (precision_knn + tpr_knn)
f1_knn
{r, echo=FALSE}
roc_knn <- roc(test$class, as.numeric(sixtytwo_nn))
auc_knn <- auc(roc_knn)
coords <- coords(roc_knn, "best", best.method = "closest.topleft")
plot(roc_knn, main = "ROC curve for knn", print.auc = TRUE, auc.polygon = TRUE,
     max.auc.polygon = TRUE, grid = TRUE, legacy.axes = TRUE,
     xlab = "False positive rate", ylab = "True positive rate")
#glm
{r, echo=FALSE}
complete_model <- glm(class ~ . , data = train_new , family = binomial(link = "logit"))
summary(complete_model)
probabilities_glm_compl <- predict(complete_model, newdata = validation[, -which(names(validation) == "class")], type = "response")
pred_glm_compl_07<- ifelse(probabilities_glm_compl > 0.7, 1, 0)

probabilities_glm_compl <- predict(complete_model, newdata = test[, -which(names(test) == "class")], type = "response")
pred_glm_compl<- ifelse(probabilities_glm_compl > 0.7, 1, 0)
cm_glm_compl=table(test$class, pred_glm_compl)
accuracy_glm_compl = (cm_glm_compl[1,1]+cm_glm_compl[2,2])/nrow(test)
accuracy_glm_compl
precision_glm_compl=(cm_glm_compl[1,1]/(cm_glm_compl[1,1]+cm_glm_compl[1,2]))
precision_glm_compl
tpr_glm_compl=(cm_glm_compl[1,1])/(cm_glm_compl[1,1]+cm_glm_compl[2,1])
tpr_glm_compl
tnr_glm_compl=(cm_glm_compl[2,2])/(cm_glm_compl[2,2]+cm_glm_compl[1,2])
tnr_glm_compl
f1_glm_compl = 2 * (precision_glm_compl * tpr_glm_compl) / (precision_glm_compl + tpr_glm_compl)
f1_glm_compl

roc_glm_complete <- roc(test$class, probabilities_glm_compl)
plot(roc_glm_complete, col = "blue", main = "ROC Curve", lwd = 2,print.auc=TRUE)
auc_glm_complete <- auc(roc_glm_complete)
auc_glm_complete
coords <- coords(roc_glm_complete, "best", best.method = "closest.topleft")
opt_thr_glm_complete <- coords$threshold
plot(roc_glm_complete, main = "ROC curve for lasso regression", print.auc = TRUE, auc.polygon = TRUE,
     max.auc.polygon = TRUE, grid = TRUE, legacy.axes = TRUE,
     xlab = "False positive rate", ylab = "True positive rate")
abline(v = 1-opt_thr_glm_complete, col = "red", lty = 2)
print(paste("The new optimal threshold is:", opt_thr_glm_complete))
#it change a bit to the best one on the validation set
#COMPUTE THE RESIDUALS
res_glm_compl <- test$class - pred_glm_compl
mse_glm_compl <- mean((res_glm_compl)^2)
mse_glm_compl
mae_glm_compl <- mean(abs(res_glm_compl))
mae_glm_compl
model2 <- update(complete_model, . ~ .- word_freq_make - word_freq_address -word_freq_3d
                 -word_freq_over -word_freq_order -word_freq_mail 
                 -word_freq_report -word_freq_business
                 -word_freq_you -word_freq_credit
                 -word_freq_415 -word_freq_technology
                 -word_freq_pm -word_freq_direct
                 -word_freq_original -word_freq_table -char_freq_.3B 
                 -char_freq_.28)

predictors <- c("word_freq_make", "word_freq_address" , "word_freq_3d",
                "word_freq_over", "word_freq_order" ,"word_freq_mail", 
                "word_freq_report", "word_freq_business",
                "word_freq_you" , "word_freq_credit",
                "word_freq_415" ,"word_freq_technology",
                "word_freq_pm" , "word_freq_direct",
                "word_freq_original" , "word_freq_table" , "char_freq_.3B", 
                "char_freq_.28")
aic_values <- vector("numeric", length = length(predictors))
initial_model <- model2
for (i in seq_along(predictors)) {
  current_model <- update(initial_model, formula = as.formula(paste(". ~ . +", predictors[i])))
  aic_values[i] <- AIC(current_model)
  print(paste("Added predictor:", predictors[i], "AIC:", aic_values[i]))
}
model3_forward<- update(model2, . ~ .+word_freq_credit)
initial_model <- model3_forward

# Get the AIC value of the initial model (only intercept)
initial_model_aic <- AIC(initial_model)
print(paste("AIC of the initial model (only intercept):", initial_model_aic))
predictors <- c("word_freq_make", "word_freq_address" , "word_freq_3d",
                "word_freq_over", "word_freq_order" ,"word_freq_mail", 
                "word_freq_report", "word_freq_business",
                "word_freq_you" ,
                "word_freq_415" ,"word_freq_technology",
                "word_freq_pm" , "word_freq_direct",
                "word_freq_original" , "word_freq_table" , "char_freq_.3B", 
                "char_freq_.28")
aic_values <- vector("numeric", length = length(predictors))

for (i in seq_along(predictors)) {
  current_model <- update(initial_model, formula = as.formula(paste(". ~ . +", predictors[i])))
  aic_values[i] <- AIC(current_model)
  print(paste("Added predictor:", predictors[i], "AIC:", aic_values[i]))
}
model4_forward<- update(model3_forward, . ~ .+word_freq_over)
initial_model <- model4_forward

# Get the AIC value of the initial model (only intercept)
initial_model_aic <- AIC(initial_model)
print(paste("AIC of the initial model (only intercept):", initial_model_aic))
predictors <- c("word_freq_make", "word_freq_address" , "word_freq_3d",
                 "word_freq_order" ,"word_freq_mail", 
                "word_freq_report", "word_freq_business",
                "word_freq_you" ,
                "word_freq_415" ,"word_freq_technology",
                "word_freq_pm" , "word_freq_direct",
                "word_freq_original" , "word_freq_table" , "char_freq_.3B", 
                "char_freq_.28")
aic_values <- vector("numeric", length = length(predictors))

for (i in seq_along(predictors)) {
  current_model <- update(initial_model, formula = as.formula(paste(". ~ . +", predictors[i])))
  aic_values[i] <- AIC(current_model)
  print(paste("Added predictor:", predictors[i], "AIC:", aic_values[i]))
}

model5_forward<- update(model4_forward, . ~ .+word_freq_business)
initial_model <- model5_forward

# Get the AIC value of the initial model (only intercept)
initial_model_aic <- AIC(initial_model)
print(paste("AIC of the initial model (only intercept):", initial_model_aic))
predictors <- c("word_freq_make", "word_freq_address" , "word_freq_3d",
                "word_freq_order" ,"word_freq_mail", 
                "word_freq_report",
                "word_freq_you" ,
                "word_freq_415" ,"word_freq_technology",
                "word_freq_pm" , "word_freq_direct",
                "word_freq_original" , "word_freq_table" , "char_freq_.3B", 
                "char_freq_.28")
aic_values <- vector("numeric", length = length(predictors))

for (i in seq_along(predictors)) {
  current_model <- update(initial_model, formula = as.formula(paste(". ~ . +", predictors[i])))
  aic_values[i] <- AIC(current_model)
  print(paste("Added predictor:", predictors[i], "AIC:", aic_values[i]))
}

#the aic increase by adding each of the possible predictors so we kept this as the best model
best_glm<-model5_forward
probabilities_best_glm<- predict(best_glm, validation, type = "response")

probabilities_glm2<- predict(model2, test, type = "response")
pred_glm2<- ifelse(probabilities_glm2 > 0.7, 1, 0)
cm_glm2=table(test$class, pred_glm2)
accuracy_glm2 = (cm_glm2[1,1]+cm_glm2[2,2])/nrow(test)
accuracy_glm2
precision_glm2=(cm_glm2[1,1]/(cm_glm2[1,1]+cm_glm2[1,2]))
precision_glm2
tpr_glm2=(cm_glm2[1,1])/(cm_glm2[1,1]+cm_glm2[2,1])
tpr_glm2
tnr_glm2=(cm_glm2[2,2])/(cm_glm2[2,2]+cm_glm2[1,2])
tnr_glm2
f1_glm2 = 2 * (precision_glm2 * tpr_glm2) / (precision_glm2 + tpr_glm2)

#ROC CURVE COMPUTED ON THE TEST SET
library(pROC)
roc_glm2 <- roc(test$class, probabilities_glm2)
plot(roc_glm2, col = "blue", main = "ROC Curve", lwd = 2,print.auc=TRUE)
auc_glm2 <- auc(roc_glm2)
coords <- coords(roc_glm2, "best", best.method = "closest.topleft")
opt_thr_glm2 <- coords$threshold
plot(roc_glm2, main = "ROC curve for logistic regression", print.auc = TRUE, auc.polygon = TRUE,
     max.auc.polygon = TRUE, grid = TRUE, legacy.axes = TRUE,
     xlab = "False positive rate", ylab = "True positive rate")
abline(v = 1-opt_thr_glm2, col = "red", lty = 2)
print(paste("The new optimal threshold is:", opt_thr_glm2))
#RESIDUALS
res_glm2 <- test$class - pred_glm2
res_glm2
mse_glm2 <- mean((res_glm2)^2)
mse_glm2
mae_glm2 <- mean(abs(res_glm2))
mae_glm2


#ROC CURVE OF THE BEST MODEL
library(pROC)
roc_best_glm <- roc(validation$class, probabilities_best_glm)
plot(roc_best_glm, col = "blue", main = "ROC Curve", lwd = 2,print.auc=TRUE)
auc_best_glm <- auc(roc_best_glm)
coords <- coords(roc_best_glm, "best", best.method = "closest.topleft")
opt_thr_best_glm <- coords$threshold
plot(roc_best_glm, main = "ROC curve for logistic regression", print.auc = TRUE, auc.polygon = TRUE,
     max.auc.polygon = TRUE, grid = TRUE, legacy.axes = TRUE,
     xlab = "False positive rate", ylab = "True positive rate")
abline(v = 1-opt_thr_best_glm, col = "red", lty = 2)
print(paste("The optimal threshold is:", opt_thr_best_glm))

#DIFFERENT SCORES WITH THE BEST THRESHOLD FOR THE BEST MODEL
#ON UNSEEN DATA
probabilities_best_glm<- predict(best_glm, test, type = "response")
pred_glm_best<- ifelse(probabilities_best_glm > 0.7, 1, 0)
cm_glm_best=table(test$class, pred_glm_best)
accuracy_glm_best = (cm_glm_best[1,1]+cm_glm_best[2,2])/nrow(test)
accuracy_glm_best
precision_glm_best=(cm_glm_best[1,1]/(cm_glm_best[1,1]+cm_glm_best[1,2]))
precision_glm_best
tpr_glm_best=(cm_glm_best[1,1])/(cm_glm_best[1,1]+cm_glm_best[2,1])
tpr_glm_best
tnr_glm_best=(cm_glm_best[2,2])/(cm_glm_best[2,2]+cm_glm_best[1,2])
tnr_glm_best
f1_glm_best = 2 * (precision_glm_best * tpr_glm_best) / (precision_glm_best + tpr_glm_best)

#RESIDUALS
res_glm_best <- test$class - pred_glm_best
mse_glm_best <- mean((res_glm_best)^2)
mse_glm_best
mae_glm_best <- mean(abs(res_glm_best))
mae_glm_best


lda <- lda(class ~ word_freq_all+word_freq_our+word_freq_remove+word_freq_internet+
             word_freq_receive+
             word_freq_will+word_freq_people+word_freq_addresses+word_freq_free+
             word_freq_email+word_freq_your+word_freq_font+word_freq_000+
             word_freq_money+word_freq_hpl+word_freq_george+
             word_freq_data+word_freq_85+word_freq_1999+word_freq_parts+
             word_freq_meeting+word_freq_project+word_freq_re+word_freq_edu+
             word_freq_conference+char_freq_.5B+char_freq_.21+char_freq_.24+
             char_freq_.23+capital_run_length_total+word_freq_credit
             +word_freq_over+word_freq_business
           , data = train_new, family="binomial")
lda
lda.pred <- predict(lda, test[,-ncol(test)])
#lda post da la posterior probability che un dato appartenga ad una classe
#posso assegnare le classi prendendo quella più probabile tra le due o con una threshold
lda.post<-lda.pred$posterior
#lda.class assegna la classe con la posterior probability più alta
lda.class <- lda.pred$class
probabilities_lda <- lda.post
prob_class_1_lda <- probabilities_lda[, 2]

pred_lda<- ifelse(prob_class_1_lda > 0.7, 1, 0)
cm_lda=table(test$class, pred_lda)
accuracy_lda = (cm_lda[1,1]+cm_lda[2,2])/nrow(test)
accuracy_lda
precision_lda=(cm_lda[1,1]/(cm_lda[1,1]+cm_lda[1,2]))
precision_lda
tpr_lda=(cm_lda[1,1])/(cm_lda[1,1]+cm_lda[2,1])
tpr_lda
tnr_lda=(cm_lda[2,2])/(cm_lda[2,2]+cm_lda[1,2])
tnr_lda
f1_lda = 2 * (precision_lda * tpr_lda) / (precision_lda + tpr_lda)
f1_lda


roc_lda <- roc(test$class, prob_class_1_lda)
plot(roc_lda, col = "blue", main = "ROC Curve", lwd = 2,print.auc=TRUE)
auc_lda <- auc(roc_lda)
coords <- coords(roc_lda, "best", best.method = "closest.topleft")
opt_thr_lda <- coords$threshold
plot(roc_lda, main = "ROC curve for lda", print.auc = TRUE, auc.polygon = TRUE,
     max.auc.polygon = TRUE, grid = TRUE, legacy.axes = TRUE,
     xlab = "False positive rate", ylab = "True positive rate")
abline(v = 1-opt_thr_lda, col = "red", lty = 2)
print(paste("The new optimal threshold is:", opt_thr_lda))


#RESIDUALS
res_lda <- test$class - prob_class_1_lda
mse_lda <- mean((res_lda)^2)
mse_lda
mae_lda <- mean(abs(res_lda))
mae_lda

qda <- qda(class ~ word_freq_all+word_freq_our+word_freq_remove+word_freq_internet+
             word_freq_receive+
             word_freq_will+word_freq_people+word_freq_addresses+word_freq_free+
             word_freq_email+word_freq_your+word_freq_font+word_freq_000+
             word_freq_money+word_freq_hpl+word_freq_george+
             word_freq_data+word_freq_85+word_freq_1999+word_freq_parts+
             word_freq_meeting+word_freq_project+word_freq_re+word_freq_edu+
             word_freq_conference+char_freq_.5B+char_freq_.21+char_freq_.24+
             char_freq_.23+capital_run_length_total+word_freq_credit
           +word_freq_over+word_freq_business
           , data = train_new, family="binomial")

qda
qda.pred <- predict(qda, validation[,-ncol(validation)])
qda.post=qda.pred$posterior
qda.class <- qda.pred$class
probabilities_qda <- qda.post
prob_class_1_qda <- probabilities_qda[, 2]

qda.pred <- predict(qda, test[,-ncol(test)])
qda.post=qda.pred$posterior
qda.class <- qda.pred$class
probabilities_qda <- qda.post
prob_class_1_qda <- probabilities_qda[, 2]
pred_qda<- ifelse(prob_class_1_qda > 0.7, 1, 0)
cm_qda=table(test$class, pred_qda)
accuracy_qda = (cm_qda[1,1]+cm_qda[2,2])/nrow(test)
accuracy_qda
precision_qda=(cm_qda[1,1]/(cm_qda[1,1]+cm_qda[1,2]))
precision_qda
tpr_qda=(cm_qda[1,1])/(cm_qda[1,1]+cm_qda[2,1])
tpr_qda
tnr_qda=(cm_qda[2,2])/(cm_qda[2,2]+cm_qda[1,2])
tnr_qda
f1_qda = 2 * (precision_qda * tpr_qda) / (precision_qda + tpr_qda)
f1_qda

roc_qda <- roc(test$class, prob_class_1_qda)
plot(roc_qda, col = "blue", main = "ROC Curve", lwd = 2,print.auc=TRUE)
auc_qda <- auc(roc_qda)
coords <- coords(roc_qda, "best", best.method = "closest.topleft")
opt_thr_qda <- coords$threshold
plot(roc_qda, main = "ROC curve for qda", print.auc = TRUE, auc.polygon = TRUE,
     max.auc.polygon = TRUE, grid = TRUE, legacy.axes = TRUE,
     xlab = "False positive rate", ylab = "True positive rate")
abline(v = 1-opt_thr_qda, col = "red", lty = 2)
print(paste("The new optimal threshold is:", opt_thr_qda))

#RESIDUALS
res_qda <- test$class - prob_class_1_qda
mse_qda <- mean((res_qda)^2)
mse_qda
mae_qda <- mean(abs(res_qda))
mae_qda


predictor_names <- c("word_freq_all","word_freq_our","word_freq_remove","word_freq_internet",
                       "word_freq_receive",
                       "word_freq_will","word_freq_people","word_freq_addresses",
                       "word_freq_free",
                       "word_freq_email","word_freq_your","word_freq_font","word_freq_000",
                       "word_freq_money","word_freq_hpl","word_freq_george",
                       "word_freq_data","word_freq_85","word_freq_1999","word_freq_parts",
                       "word_freq_meeting","word_freq_project","word_freq_re","word_freq_edu",
                       "word_freq_conference","char_freq_.5B","char_freq_.21","char_freq_.24",
                       "char_freq_.23","capital_run_length_total","word_freq_credit",
                      "word_freq_over","word_freq_business")
predictors_matrix_train <- as.matrix(train_new[, predictor_names])
response_train <- train_new$class
predictors_matrix_validation <- as.matrix(validation[, predictor_names])
predictors_matrix_test<-as.matrix(test[, predictor_names])

#CROSS VALIDATION
lambda_grid <- 10^seq(-4, 4, length = 100)

# Perform k-fold cross-validation to select the best lambda
ridge_cv <- cv.glmnet(predictors_matrix_train, response_train, alpha = 0, lambda = lambda_grid, nfolds = 5)

# Print the cross-validated results
plot(ridge_cv)

# Identify the best lambda value
best_lambda <- ridge_cv$lambda.min
cat("Best Lambda:", best_lambda, "\n")

# Train the final Ridge model using the best lambda value
final_ridge_model <- glmnet(predictors_matrix_train, response_train, alpha = 0, lambda = best_lambda)
predictions <- predict(final_ridge_model, newx = predictors_matrix_validation)

predictions <- predict(final_ridge_model, newx = predictors_matrix_test)
pred_ridge<- ifelse(predictions> 0.7, 1, 0)
cm_ridge=table(test$class, pred_ridge)
accuracy_ridge = (cm_ridge[1,1]+cm_ridge[2,2])/nrow(test)
accuracy_ridge
precision_ridge=(cm_ridge[1,1]/(cm_ridge[1,1]+cm_ridge[1,2]))
precision_ridge
tpr_ridge=(cm_ridge[1,1])/(cm_ridge[1,1]+cm_ridge[2,1])
tpr_ridge
tnr_ridge=(cm_ridge[2,2])/(cm_ridge[2,2]+cm_ridge[1,2])
tnr_ridge
f1_ridge = 2 * (precision_ridge * tpr_ridge) / (precision_ridge + tpr_ridge)
f1_ridge
#NEW ROC CURVE
library(pROC)
roc_ridge <- roc(test$class, as.numeric(predictions))
plot(roc_ridge, col = "blue", main = "ROC Curve", lwd = 2,print.auc=TRUE)
auc_ridge <- auc(roc_ridge)
coords <- coords(roc_ridge, "best", best.method = "closest.topleft")
opt_thr_ridge <- coords$threshold
plot(roc_ridge, main = "ROC curve for ridge regression", print.auc = TRUE, auc.polygon = TRUE,
     max.auc.polygon = TRUE, grid = TRUE, legacy.axes = TRUE,
     xlab = "False positive rate", ylab = "True positive rate")
abline(v = 1-opt_thr_ridge, col = "red", lty = 2)
print(paste("The  new optimal threshold is:", opt_thr_ridge))

res_ridge <- test$class - predictions
res_ridge
mse_ridge <- mean((res_ridge)^2)
mse_ridge
mae_ridge <- mean(abs(res_ridge))
mae_ridge

lambda_grid <- 10^seq(-4, 4, length = 100)
lasso_cv <- cv.glmnet(predictors_matrix_train, response_train, alpha = 1, lambda = lambda_grid, nfolds = 5)

# Print the cross-validated results
plot(lasso_cv)


# Identify the best lambda value
best_lambda_lasso <- lasso_cv$lambda.min
cat("Best Lambda:", best_lambda, "\n")

# Train the final Lasso model using the best lambda value
final_lasso_model <- glmnet(predictors_matrix_train, response_train, alpha = 1, lambda = best_lambda_lasso)

predictions_lasso <- predict(final_lasso_model, s = best_lambda_lasso, newx = predictors_matrix_validation)
predictions_lasso <- predict(final_lasso_model, s = best_lambda_lasso, newx = predictors_matrix_test)
pred_lasso<- ifelse(predictions_lasso> 0.7, 1, 0)
cm_lasso=table(test$class, pred_lasso)
accuracy_lasso = (cm_lasso[1,1]+cm_lasso[2,2])/nrow(test)
accuracy_lasso
precision_lasso=(cm_lasso[1,1]/(cm_lasso[1,1]+cm_lasso[1,2]))
precision_lasso
tpr_lasso=(cm_lasso[1,1])/(cm_lasso[1,1]+cm_lasso[2,1])
tpr_ridge
tnr_lasso=(cm_lasso[2,2])/(cm_lasso[2,2]+cm_lasso[1,2])
tnr_lasso
f1_lasso = 2 * (precision_lasso * tpr_lasso) / (precision_lasso + tpr_lasso)
f1_lasso
roc_lasso <- roc(test$class, as.numeric(predictions_lasso))
plot(roc_lasso, col = "blue", main = "ROC Curve", lwd = 2,print.auc=TRUE)
auc_lasso <- auc(roc_lasso)
coords <- coords(roc_lasso, "best", best.method = "closest.topleft")
opt_thr_lasso <- coords$threshold
plot(roc_lasso, main = "ROC curve for lasso regression", print.auc = TRUE, auc.polygon = TRUE,
     max.auc.polygon = TRUE, grid = TRUE, legacy.axes = TRUE,
     xlab = "False positive rate", ylab = "True positive rate")
abline(v = 1-opt_thr_lasso, col = "red", lty = 2)
print(paste("The new optimal threshold is:", opt_thr_lasso))
#RESIDUALS
res_lasso <- test$class - predictions
res_lasso
mse_lasso <- mean((res_lasso)^2)
mse_lasso
mae_lasso <- mean(abs(res_lasso))
mae_lasso
