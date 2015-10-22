###########################################################################
# Cargar datos
###########################################################################
Data_train<-lecturaDatos("../Data/","UGR2014_tra.csv")
Data_test<-lecturaDatos("../Data/","UGR2014_tst.csv")
Data_full<-data.frame(rbind(Data_train[,-51],Data_test))
dim(Data_train)

ListaDatosImputados<-list()
ListaDatosImputados

if(filtro==3){
  path<-"../Data/Imputacion de datos/Filtrado 3%/UGR2014_tra_keel-filtered_3.csv"
  Data_train<-lecturaDatos(path,)
}
else if(filtro==5){
  path<-"../Data/Imputacion de datos/Filtrado 5%/"
}
else if(filtro==7){
  path<-"../Data/Imputacion de datos/Filtrado 7%/"
}
else{
  path<-"../Data/Imputacion de datos/Filtrado 0%/"
}

names(Data_train)<-c(names(test),"class")
names(Data_ipf)<-c(names(test),"class")

Data_train_IFKM<-read.table(file = "../Data/Imputacion de datos/Filtrado 3%/FKMeans-MV.ugr2014_tra_keel-filtered_3(k=3)/FKMeans-MVs0.ugr2014_tra_keel-filtered_31tra.dat",sep = ",", comment.char="@")

Data_trainX<-read.table(file = "../Data/Imputacion de datos/Filtrado 3%/UGR2014_tra_keel-filtered_3.csv",sep = ",", comment.char="@")


Data_train_IFKM<-read.table(file = "../Data/FKMeans-MV.ugr2014_tra/FKMeans-MVs0.ugr2014_tra1tra.dat",sep = ",", comment.char="@")

D<-read.table(file = "../Data/FKMeans-MV.ugr2014_tra/FKMeans-MVs0.ugr2014_tra1tst.dat",sep = ",", comment.char="@")
Data_train_IFKM<-data.frame(rbind(Data_train_IFKM,D))
 
D<- read.table(file = "../Data/Imputacion de datos/Filtrado 0%/svmImpute-MV/svmImpute-MV.ugr2014_tra1tra.dat",
               sep = ",", comment.char="@")
Data_train_SVM<- read.table(file = "../Data/Imputacion de datos/Filtrado 0%/svmImpute-MV/svmImpute-MV.ugr2014_tra1tst.dat",
               sep = ",", comment.char="@")
Data_train_SVM<-data.frame(rbind(Data_train_SVM,D))


Nombre<-""

