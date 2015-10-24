# Script de muestra para cargar y trabajar con el conjunto de ejemplos
# elegido para la practica del curso

library(utils)

# Antes de seguir, en el menu de RStudio hago lo siguiente:
# Menu->Session->Set Working Directory->Choose Directory...
# y selecciono el directorio donde esta el archivo UGR2014_tra.csv"
datos<-Data_ipf
datos = read.csv("../Datos pruebas/Pr/Datos_train_IPF.csv",header=T, sep=",", dec=".",
                 na.strings=c(".","NA","","?"))
# Transformo la salida de la base de ejemplos en 0s y 1s
v = ifelse(datos[,dim(datos)[2]]=="positive",1,0)

# Y la sustituyo por la columna antigua de clase
datos = cbind(datos[1:dim(datos)[2]-1],class=v)
datos$class = as.factor(datos$class)


# Cargo el conjunto de ejemplos de test que supongo que esta en el mismo
# directorio que el de entrenamiento
test = read.csv("../Data/UGR2014_tst.csv",header=T, sep=",", dec=".",
                na.strings=c(".","NA","","?"))

# Aqui vendria la parte de las operaciones de preprocesamiento y
# seleccion del algoritmo de clasificacion

# Como ejemplo aprendere con un simple arbol (J48) sobre todo el
# conjunto de entrenamiento

library(RWeka)
modelC4.5 = J48(f, na.action=na.omit, data=datos)
bag = randomForest(f, data=datos, importance=T ,ntree = 1000)
test2<-test2[,]
preds = predict(modelC4.5, test2)


library(e1071)
library(caret)

model <- svm(class~., data=datos, method="C-classification", kernel="radial",
             cost=10, gamma=0.1)

pred <- predict(model, test, decision.values = TRUE)


# Transformo el resultado para adaptarlo al fichero pedido en la
# competicion en Kaggle

resul = as.numeric(levels(pred))[pred]
resul=cbind(id=1:nrow(test),class=pred)
#resul<-pred2

# Salvo los datos a un fichero
write.csv(resul,"UGR2014_Sol1.csv",quote=F,row.names=F)

pred= predict (bag ,newdata =test2)#0.68587
pred=predict(glm.fit, test2, type="response") 
glm.pred <- ifelse(pred>0.5,1,0) 
pred<-glm.pred
resul<- ifelse(pred=="positive",1,0)

pred<-predict.boosting(bag ,newdata =test2,type="prob")

pred=predict(bag,test2,type="prob")[,2]
pred<-as.data.frame(pred)

resul=cbind(id=1:nrow(test2),class=pred[,2])

str(datos)
str(test2)
test2<-cbind(test2,class<-rep(c(0,1),20000))
test2[,51]<-as.factor(test2[,51])
