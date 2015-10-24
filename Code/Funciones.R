library(caret)
require(party)
library(e1071)
library(adabag)
library(RWeka)
library(partykit)
library(ggplot2)
library(e1071)
library(cat)#nose si la utilizo
require("robCompositions", lib.loc="C:/Program Files/R/R-3.1.1/library")
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

# funcion para lectura de archivo. En este script tambien se consideran 
# opciones de analisis exploratorio preliminar de los datos
#' @param path       camino dentro del sistema de archivos donde se encuentra este.
#' @param file    nombre del archivo
#' @return                dataset contenido en el archivo
#' @export
#' @examples
#'  iris<- lecturaDatos("/Data/","iris")
lecturaDatos <- function(path,file){
  # se compone el path completo
  pathCompleto <- paste(path,file,sep="")
  
  # se leen los datos
  dataset <- read.csv(pathCompleto,na.strings=c(".","NA","","?"))
  
  # se devuelve el conjunto de datos
  return(dataset)
}

escrituraDatos <- function(path,file, dataset){
  # se compone el path completo
  pathCompleto <- paste(path,file,sep="")
  
  # se escribe el archivo
  write.csv(dataset, pathCompleto, row.names = FALSE)
}

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

Buscar_NA<- function(dato,i){
  sum(is.na(dato[,i]))
}

Devolver_NA<-function(datos,Bool_NA){
  if(Bool_NA==T){
    na.omit( datos )
  }else {
    datos[!complete.cases(datos), ]
  }
  
}

acierto <- function(bag.datos){
  return (sum (sapply(1:length(bag.datos$y), function(x){
    if (is.na(bag.datos$predicted[x])){
      0
    } 
    else if (as.numeric(bag.datos$y[x])==as.numeric(bag.datos$predicted[x])){
      1
    }
    else{
      0
    }
  }))/length(bag.datos$y))
}

k_foldsDiv<-function(data,clase,k){
  folds <- createFolds(data[[clase]],k)
  split_up <- lapply(folds, function(ind, dat) dat[ind,], dat = data)
#  unlist(lapply(split_up, nrow))
}


Funcion2<-function(particiones,formula,indice,dataset,posicionClase){
  # se aprende el modelo
  fallosInternos <- c(rep(0,nrow(dataset)))
  arbol<- bagging(formula, particiones[[indice]], mfinal = 500 ,control = Weka_control())
#  arbol <- boosting(formula,data = particiones[[indice]],mfinal =150,coeflearn = "Zhu",
 #                   control=rpart.control(cp=0),)
  #arbol <-ada(formula,data=particiones[[indice]],iter=20,nu=1,type="gentle")
  
  #arbol <- J48(formula,data=particiones[[indice]])
  cat("Aprendido modelo numero: ",indice,"\n") 
  # ahora hemos de clasificar las instancias (todas) usando
  # este arbol y vamos anotando si quedan mal o bien clasificadas
  #pred <- predict(arbol, newdata=dataset)
  pred <- predict.bagging(arbol,newdata=dataset, newmfinal=50)  
  pred <- as.integer(pred$class)
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
# funcion para hacer el particionado del conjunto de
# datos en un cierto numero de particiones
# primer argumento: conjunto de datos a particionar
# segundo argumento: numero de particiones a realizar
obtenerParticiones <- function(dataset,k){
  # se asignan numeros aleatorios a cada muestra, usando una
  # distribucion uniforme
  randoms <- runif(nrow(dataset))
  
  # se generan las diferentes particiones aplicando la
  # funcion de particion para cada particion a obtener
  particiones <- lapply(1:k, obtenerParticion, k=k, dataset=dataset, randoms=randoms)
  
  # se devuelven las particiones obtenidas
  return(particiones)
}

# funcion auxiliar para realizar el particionado. Recibe como
# argumento el indice de particion a obtener, el numero total
# de particiones a generar, el conjunto de datos y el conjunto
# de numeros aleatorios
obtenerParticion <- function(indice, k, dataset, randoms){  
  # se seleccionan los indices de la secuencia de numeros 
  # aleatorios que se corresponde con la fraccion de probabilidad
  # que se asigna a la particion de indice pasado como primer
  # argumento
  indices <- (randoms >= (indice-1)/k & randoms < (indice/k))
  
  # ahora se seleccionan las muestras con estos indices
  particion <- dataset[indices, ,drop=FALSE]
  
  # se devuelve la particion
  return(particion)
}


Filtrado_NA<-function(porcentaje,datos){

  # se obtiene el porcentaje de valores perdidos de cada fila
  res <- apply(datos, 1, function(x) sum(is.na(x))) / ncol(datos) * 100

  # se marcan aquellas filas con valor de porcentaje mayor de 5. Como
  # hay 51 variables, supone que hay 3 variables sin valor (redondeando)
  mal <- (res > porcentaje)

  # datos filtrados: se quitan las filas con muchos valores perdidos
  filtrados <- datos[!mal,]

  # se muestra el numero de filas del conjunto inicial y del resultante
  cat("Instancias archivo original: ",nrow(datos)," instancias en filtrado: ",nrow(filtrados),"\n")
  
  filtrados
}

porcentaje_accierto<-function(table){
  salida<-(table[1,1]/(table[1,2]+table[1,1]))/2+(table[2,2]/(table[2,2]+table[2,1]))/2
  salida
}



####################################################################
# Seleccion de caracteristicas
####################################################################


Exhaustive_Search_evaluator <- function(subset, k=5){  
  # genera valores aleatorios (uniforme) para cada muestra del
  # conjunto de datos
  splits <- runif(nrow(Data_train))
  
  # tratamiento de cada una de las particiones. Para cada valor de
  # particion se aplica la funcion que se define a continuacion
  results <- sapply(1:k, function(i) {
    # se determina el indice de las muestras para test (aproximadamente
    # una fraccion 1/k de las muestras del conjunto de datos)
    test.idx <- (splits >= ((i-1)/k) & (splits < (i/k)))
    
    # todas las demas muestras seran para training
    train.idx <- !test.idx
    
    # se seleccionan las muestras en si
    test <- iris[test.idx, ,drop=FALSE]
    train <- iris[train.idx, , drop=FALSE]
    
    # aprende el modelo sobre el conjunto de entrenamiento
    tree <- rpart(as.simple.formula(subset,"class"),train)
    
    # calcula la tasa de error
    error.rate <- sum(test$class != predict(tree,test,type="c"))/nrow(test)
    
    # devuelve la tasa de aciertos
    return(1-error.rate)
  })
  
  # se muestra el subconjunto y la media de resultados y se devuelve
  # la media de los resultados (un resultado por particion)
  print(subset)
  print(mean(results))
  return(mean(results))
}





