#################################################################
#                         SVM                                   #
#################################################################

x <- svm(f, data=datos, method="C-classification", kernel="radial",
         cost=10, gamma=0.1)
pred <- predict(x, test2, decision.values = TRUE)
tab <- table(pred = pred, true = datos[,51])
tab
porcentaje_accierto(tab)
