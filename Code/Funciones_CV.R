
CV<-function(dat,f,tipo,...){
  k<-length(dat)
  if(tipo=="J48"){
    sapply(1:k,function(x)CV_J48(Union(dat[x]),Union(dat[-x]),f))
  }else if(tipo=="RandomForest"){
    sapply(1:k,function(x)CV_RandomForest(Union(dat[x]),Union(dat[-x]),f,ntrees,importancia))
  }else{
    0
  }
}
Union<-function(datos){
  do.call(rbind, datos)
}



CV_J48<-function(train ,test , funcion){
  x<- J48(funcion, data=train)
  pred = predict(x,test[,1:50])
  tab <- table(pred = pred, true = test[["class"]])
  porcentaje_accierto(tab)
}


CV_RandomForest<-function(train ,test , funcion, n, impor){
  x = randomForest(f, data=train, ntree = n, importance=impor)
  pred= predict (x ,newdata =test)
  tab <- table(pred = pred, true = test[["class"]])
  porcentaje_accierto(tab)
}
