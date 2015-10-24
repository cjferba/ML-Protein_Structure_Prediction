library(caret)
require(party)
library(e1071)
library(adabag)
library(RWeka)
library(partykit)
library(ggplot2)
require("robCompositions")
require(mice)
library(rpart)
library(FSelector)
library(class)
library(mlbench)
library(kml)
require(DMwR)
require(outliers)
library(gbm)
library (randomForest)
library(CerioliOutlierDetection)


Funcion2<-function(particiones,formula,indice,dataset,posicionClase){
  # se aprende el modelo
  fallosInternos <- c(rep(0,nrow(dataset)))
  #arbol<- bagging(formula, particiones[[indice]], mfinal = 50 ,control = Weka_control())
  #  arbol <- boosting(formula,data = particiones[[indice]],mfinal =150,coeflearn = "Zhu",
  #                   control=rpart.control(cp=0),)
  #arbol <-ada(formula,data=particiones[[indice]],iter=20,nu=1,type="gentle")
  
  arbol <- J48(formula,data=particiones[[indice]])
  cat("Aprendido modelo numero: ",indice,"\n") 
  # ahora hemos de clasificar las instancias (todas) usando
  # este arbol y vamos anotando si quedan mal o bien clasificadas
  pred <- predict(arbol, newdata=dataset)
  #pred <- predict.bagging(arbol,newdata=dataset, newmfinal=50)  
  #pred <- as.integer(pred$class)
  lista <- dataset[,posicionClase] != pred
  fallosInternos=fallosInternos+lista
  fallosInternos
}


# Algoritmo de deteccion de ruido mediante particionado
# itearativo

# Argumentos: conjunto de datos, numero de particiones
# a realizar sobre el y porcentaje limite para parada
ipf <- function(dataset, k, limite, may=2){
  seguir <- TRUE
  
  # obtiene la posicion de la variable clase: se asume que es
  # la ultima
  posicionClase <- length(names(dataset))
  
  # ahora se define el nivel de mayoria
  mayoria <- floor(k/may)
  
  # se crea un contador de iteraciones
  iteraciones <- 0
  
  # bucle principal
  while(seguir){
    # se incrementa el contador de iteraciones
    iteraciones <- iteraciones+1
    
    # se crea un vector de fallos, con una posicion para
    # cada instancia
    fallos <- c(rep(0,nrow(dataset)))
    
    # en primer lugar, se hace el particionado del conjunto de datos
    particiones <- obtenerParticiones(dataset,k)
    
    # idetificamos la variable clase como la ultima
    variableClase <- names(dataset)[posicionClase]
    
    # componemos una formula con el nombre de la variable clase
    formula <- as.formula(paste(variableClase,"~.",sep=""))
    
    # ahora hemos de construir un arbol para cada una de ellas
    fallosMatrix<-sapply(1:length(particiones), function(x) Funcion2(particiones,formula,x,dataset,posicionClase))
    fallos<-apply(X = fallosMatrix,MARGIN = 1,FUN = sum)
    
    # aquellas instancias cuyo numero de aciertos sea menor que mayoria
    # son descartadas
    indicesFallos <- (fallos > mayoria)
    
    # se determina que porcentaje de fallos hay
    numeroFallos <- length(indicesFallos[indicesFallos==TRUE])
    porcentajeFallos <- numeroFallos*100/nrow(dataset)
    
    cat("Iteracion: ", iteraciones, " porcentaje de fallos: ",porcentajeFallos,"\n")
    
    # se descartan los fallos del conjunto de datos
    dataset <- dataset[!indicesFallos, ,drop=FALSE]
    cat("Numero de instancias en nuevo conjunto de datos: ",nrow(dataset),"\n")
    
    if (porcentajeFallos < limite){
      # condicion de parada
      seguir <- FALSE
    }
  }
  
  # se devuelve el conjunto final de instancias
  return(dataset)
}

Data_train<-read.table(
  file = "IterativePartitioningFilter-Fs0.svmimpute-mv1tra.dat",sep = ",", comment.char="@")
test<-read.csv("../Data/UGR2014_tst.csv",header=T, sep=",", dec=".",
                      na.strings=c(".","NA","","?"))
names(Data_train)<-c(names(test),"class")

Data_train[,dim(Data_train)[2]]<-as.character(Data_train[,dim(Data_train)[2]])
v = ifelse(Data_train[,dim(Data_train)[2]]==Data_train[1,51],0,1)

# Y la sustituyo por la columna antigua de clase
Data_train = cbind(Data_train[1:dim(Data_train)[2]-1],class=v)
Data_train$class = as.factor(Data_train$class)


Data_train[,3]<-as.integer(Data_train[,3])
Data_train[,18]<-as.factor(Data_train[,18])
Data_train[,19]<-as.factor(Data_train[,19])
Data_train[,20]<-as.factor(Data_train[,20])
Data_train[,21]<-as.factor(Data_train[,21])
Data_train[,24]<-as.factor(Data_train[,24])
Data_train[,25]<-as.factor(Data_train[,25])
Data_train[,26]<-as.factor(Data_train[,26])
Data_train[,27]<-as.factor(Data_train[,27])
Data_train[,28]<-as.factor(Data_train[,28])
Data_train[,29]<-as.factor(Data_train[,29])
Data_train[,41]<-as.integer(Data_train[,41])
Data_train[,42]<-as.integer(Data_train[,42])
Data_train[,43]<-as.integer(Data_train[,43])
Data_train[,44]<-as.integer(Data_train[,44])
Data_train[,45]<-as.integer(Data_train[,45])
Data_train[,46]<-as.integer(Data_train[,46])
Data_train[,47]<-as.integer(Data_train[,47])
Data_train[,48]<-as.integer(Data_train[,48])
Data_train[,49]<-as.integer(Data_train[,49])
Data_train[,50]<-as.integer(Data_train[,50])

test[,3]<-as.integer(test[,3])
test[,18]<-as.factor(test[,18])
test[,19]<-as.factor(test[,19])
test[,20]<-as.factor(test[,20])
test[,21]<-as.factor(test[,21])
test[,24]<-as.factor(test[,24])
test[,25]<-as.factor(test[,25])
test[,26]<-as.factor(test[,26])
test[,27]<-as.factor(test[,27])
test[,28]<-as.factor(test[,28])
test[,29]<-as.factor(test[,29])
test[,41]<-as.integer(test[,41])
test[,42]<-as.integer(test[,42])
test[,43]<-as.integer(test[,43])
test[,44]<-as.integer(test[,44])
test[,45]<-as.integer(test[,45])
test[,46]<-as.integer(test[,46])
test[,47]<-as.integer(test[,47])
test[,48]<-as.integer(test[,48])
test[,49]<-as.integer(test[,49])
test[,50]<-as.integer(test[,50])

nombres<- as.vector(read.csv(file = "Nombres.csv"))
nombres<- as.vector(as.matrix(nombres))
Data_train <- Data_train[,c(nombres,"class")]

Data_ipf<-ipf(dataset = Data_train,k = 8,limite = 5 ,may = 1.2)

summary(Data_train$class)
summary(Data_ipf$class)


Data_train <- SMOTE(class~., Data_ipf, perc.over = 600,perc.under=100)
datos<-Data_train

is.Cerioli.outlier = cerioli2010.irmcd.test(Data_train[,c(1:2,10:16,31:40)], signif.alpha=alpha.value.penalizado)$outliers

sum(is.Cerioli.outlier)
Data_train<-Data_train[!is.Cerioli.outlier,]


glm.fit <- glm(class~., data=Data_train, family=binomial) 
pred=predict(glm.fit, test, type="response")

resul=cbind(id=1:nrow(test),class=pred)
write.csv(resul,"UGR2014_Sol1.csv",quote=F,row.names=F)



