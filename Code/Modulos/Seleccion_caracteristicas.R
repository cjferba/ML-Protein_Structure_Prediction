###########################################################################
# Seleccion de caracteristicas
###########################################################################
if(SeleccionCaracteristicas==T){
  if(Caracteristica=="Caret_impotance"){
    ############################__Caret importance__###########################
    # prepara el esquema de entrenamiento
    control <- trainControl(method="repeatedcv", number=10, repeats=3)
    # aprende el modelo
    model <- train(class~., data=datos, method="lvq", 
                   preProcess="scale", trControl=control)
    # estima la importancia de las variables
    importance <- varImp(model, scale=F)
    # muestra los datos del analisis
    importance <- importance$importance[order(decreasing = T,importance$importance[,1]),]
    Nombres_Importancia<-row.names(importance[1:100,])
    
    f <- as.simple.formula(Nombres_Importancia,"class")
   
    
  } 
  else if(Caracteristica=="Fselector_ramdomForest"){
    ############################__Fselector ramdomForest__######################
    # se calculan los pesos
    weights <- random.forest.importance(class~.,(Data_train), importance.type=1)
    # se muestran los resultados
    print(weights)
    subset <- cutoff.k(weights,numeroImport)
    f <- as.simple.formula(subset,"class")
    print(f)
    
    write.csv(weights, "../Data/Importancia_Fselector_randomForest.csv", row.names = T)
    
  }
  else{}
}else{
  f<-class~.
}




library(psych)
fit <- principal(datos)
fit <- princomp(datos, cor=TRUE)
library(FactoMineR)
result <- PCA(datos[,110:121]) # graphs generated automatically
fit # print results
