library(bestglm)
library(glmnet)
library(glm2)


out<-glm(class~., data=datos, family=binomial)
step(out, k=log(nrow(SAheart)))
#but BICq with q=0.25
bestglm(datos[,c(96:122)], IC="BICq", t=0.25, family=binomial)
#

library(caret)
result <- train(dat[,1:39], dat[,40], family = "binomial", method = "glm")
result$finalModel
datos<-Data_train


datos[1:2000,c(92:94,96:122)]
alpha.value = 0.05
alpha.value.penalizado = alpha.value / nrow(datos) 
smot<-cbind(datos[,c(1:92)],scale(datos[,c(92:94)]),datos[,95],scale(datos[,,96:121]),datos[,122])
is.Cerioli.outlier = cerioli2010.irmcd.test(datos[,c(92:94,96:121)], signif.alpha=alpha.value)$outliers
is.Cerioli.outlier = cerioli2010.fsrmcd.test(datos[,c(1:2,10:16,31:40)], signif.alpha=alpha.value)$outliers
is.Cerioli.outlier = cerioli2010.irmcd.test(datos[,c(1:2,10:16,31:40)], signif.alpha=alpha.value.penalizado)$outliers

sum(is.Cerioli.outlier)
outlier.scores <- lofactor(datos, k=5)

ElementosNumericos<-sapply(1:dim(datos)[2],function(x)class(datos[,x]))=="numeric"
datos2<-datos[!is.Cerioli.outlier,]
summary(datos2$class)
summary(datos$class)
glm.fit <- glm(class~., data=datos2, family=binomial) 
glm2.fit <- glm2(class~.,family=binomial,data = datos2)

names(test2)
pred=predict(glm.fit, test, type="response")
gbm.roc.area(obs, pred)

mean(pred)
glm.pred <- ifelse(pred>0.5,1,0) 
pred<-glm.pred
tab <- table(pred = pred, true = test[,dim(test)[2]])
tab
porcentaje_accierto(tab)

hist(pred)
range(pred)

datos[,2]<-scale(Datos[,2])


binomial(link = "logit")
quasibinomial(link = "logit")

library(gbm)
what <- "GBM"
datos[,51]<-as.factor(datos[,51])
datos[,51]<-as.numeric(datos[,51])-1
GBM_model = gbm(class~.,data=datos,n.trees = 150,shrinkage = 0.005)# ,n.trees=50,shrinkage=0.005 ,cv.folds=5
best.iter <- gbm.perf(GBM_model,method="cv")

pred <- predict.gbm(GBM_model,test,n.trees = 30,type="response")
pred
display_results()