# In this section were compared the results obtained with the different models.
# Comparison of accuracy, precision tpr, tnr and F1-score
{r, echo=FALSE}
scores <- data.frame(
  Model = c("Best logistic", "Logistic without high p-value","complete logistic", "Ridge", "Lasso", "LDA", "QDA", "NB", "KNN"),
  Accuracy = c(accuracy_glm_best, accuracy_glm2,accuracy_glm_compl,accuracy_ridge,accuracy_lasso,accuracy_lda,accuracy_qda,accuracy_nb,accuracy_knn),
  Precision = c(precision_glm_best, precision_glm2,precision_glm_compl,precision_ridge,precision_lasso,precision_lda,precision_qda,precision_nb,precision_knn),
  Recall = c(tpr_glm_best, tpr_glm2,tpr_glm_compl,tpr_ridge,tpr_lasso,tpr_lda,tpr_qda,tpr_nb,tpr_knn),
  F1_score = c(f1_glm_best, f1_glm2,f1_glm_compl,f1_ridge,f1_lasso,f1_lda,f1_qda,f1_nb,f1_knn),
  AUC = c(auc_best_glm,auc_glm2,auc_glm_complete,auc_ridge,auc_lasso,auc_lda,auc_qda,auc_nb,auc_knn)
)
kable(scores, digits = 4, format.args = list(scientific = FALSE), 
      booktabs = TRUE, longtable = FALSE)

names <- c("Best logistic","Logistic without high p-value", "Complete logistic", "Ridge", "Lasso", "LDA", "QDA", "NB", "KNN")
ACC <- c(accuracy_glm_best, accuracy_glm2,accuracy_glm_compl,accuracy_ridge,accuracy_lasso,accuracy_lda,accuracy_qda,accuracy_nb,accuracy_knn)
index_max <- which.max(ACC)
colors <- rep('red', 8)
colors[index_max] <- 'green'
barplot(ACC, col=colors, names.arg=names, main='Accuracy', ylim=c(0, 1), las=2)

PREC <- c(precision_glm_best, precision_glm2,precision_glm_compl,precision_ridge,precision_lasso,precision_lda,precision_qda,precision_nb,precision_knn)
index_max <- which.max(PREC)
colors <- rep('red', 8)
colors[index_max] <- 'green'
barplot(PREC, col=colors, names.arg=names, main='Precision', ylim=c(0, 1), las=2 )

REC <- c(tpr_glm_best, tpr_glm2,tpr_glm_compl,tpr_ridge,tpr_lasso,tpr_lda,tpr_qda,tpr_nb,tpr_knn)
index_max <- which.max(REC)
colors <- rep('red', 8)
colors[index_max] <- 'green'
barplot(REC, col=colors, names.arg=names, main='Recall', ylim=c(0, 1), las=2)

F1 <- c(f1_glm_best, f1_glm2,f1_glm_compl,f1_ridge,f1_lasso,f1_lda,f1_qda,f1_nb,f1_knn)
index_max <- which.max(F1)
colors <- rep('red', 8)
colors[index_max] <- 'green'
barplot(F1, col=colors, names.arg=names, main='F1 scores', ylim=c(0, 1), las=2)

#Residual analysis
{r, echo=FALSE}
MSE<-c(mse_glm_best,mse_glm2,mse_glm_compl,mse_ridge,mse_lasso,mse_lda,mse_qda,mse_nb)
index_min <- which.min(MSE)
colors <- rep('red', 8)
colors[index_min] <- 'green'
barplot(MSE, col=colors, names.arg=names[1:(length(names) - 1)], main='Mean square error', ylim=c(0, 1), las=2)

MAE<-c(mae_glm_best,mae_glm2,mae_glm_compl,mae_ridge,mae_lasso,mae_lda,mae_qda,mae_nb)
index_min <- which.min(MAE)
colors <- rep('red', 8)
colors[index_min] <- 'green'
barplot(MAE, col=colors, names.arg=names[1:(length(names) - 1)], main='Mean absolute error', ylim=c(0, 1), las=2)
# ROC Curve
{r, echo=FALSE}

list_roc <- list(roc_best_glm,roc_glm2,roc_glm_complete, roc_ridge, roc_lasso, roc_lda, roc_qda, roc_nb, roc_knn)
plot(roc_best_glm, col = "blue", auc.polygon = TRUE, max.auc.polygon = TRUE, grid = TRUE, legacy.axes = TRUE, xlab = "False positive rate", ylab = "True positive rate")
lines(roc_glm2, col = "purple")
lines(roc_glm_complete, col = "pink")
lines(roc_ridge, col = "red")
lines(roc_lasso, col = "green")
lines(roc_lda, col = "black")
lines(roc_qda, col = "yellow")
lines(roc_nb, col = 'brown')
lines(roc_knn, col = 'lightblue')
legend("bottomright", legend = c("Best logistic", "Logistic without high p-value","complete logistic", "Ridge", "Lasso", "LDA", "QDA", "NB", "KNN"), col = c("blue", "purple","pink", "red", "green", "black", "yellow", "brown", "lightblue"), lty = 1, lwd = 2, cex = 0.7)
{r, echo=FALSE}
AUC <- c(auc_best_glm,auc_glm2,auc_glm_complete,auc_ridge,auc_lasso,auc_lda,auc_qda,auc_nb,auc_knn)
index_max <- which.max(AUC)
colors <- rep('red', 8)
colors[index_max] <- 'green'
barplot(AUC, col=colors, names.arg=names, main='Area under the curve', ylim=c(0, 1), las=2)
