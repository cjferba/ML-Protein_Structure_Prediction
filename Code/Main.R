

source("Funciones.R")
source("FuncionesGraficas.R")
source("Funciones_CV.R")

###########################################################################
# Lectura de datos
###########################################################################
source("Lectura_Datos.R")
Dat<-read.csv(
  file = "../Data/Datos_train_IPF_full.csv",sep = ",", comment.char="@")
Data_train<-read.table(
  file = "../Data/Imputacion de datos/Filtrado 5%/SVM_SIGMA/svmImpute-MV.ugr2014_tra_keel-filtered_51tra.dat",sep = ",", comment.char="@")
Data_train<-rbind(Data_train,read.table(
  file = "../Data/Imputacion de datos/Filtrado 0%/svmImpute-MV/svmImpute-MV.ugr2014_tra1tst.dat",
  sep = ",", comment.char="@"))
############################################
#Test
############################################
datos2<-datos
test<-read.arff("../../TESTFORPROTEIN.arf")
test<-read.arff("F:././././4-Kaggle/TestSet.arff/TEST partes 20000/TestSet-1.arff")
aux<-read.csv("F:././././4-Kaggle/TestSet.arff/TEST partes 20000/TestSet-2.arff")
names(aux)=names(test)
test<-rbind(test,aux)
aux<-read.csv("F:././././4-Kaggle/TestSet.arff/TEST partes 20000/TestSet-3.arff")
names(aux)=names(test)
test<-rbind(test,aux)
aux<-read.csv("F:././././4-Kaggle/TestSet.arff/TEST partes 20000/TestSet-4.arff")
names(aux)=names(test)
test<-rbind(test,aux)
aux<-read.csv("F:././././4-Kaggle/TestSet.arff/TEST partes 20000/TestSet-5.arff")
names(aux)=names(test)
test<-rbind(test,aux)
aux<-read.csv("F:././././4-Kaggle/TestSet.arff/TEST partes 20000/TestSet-6.arff")
names(aux)=names(test)
test<-rbind(test,aux)

datos<-Data_train
names(Data_train)<-c(names(Dat))

nombresbuenos<-names(test) %in% names(Data_train)
test<-test[,nombresbuenos]
#MAl echos
test<-test[test$PredSS_r1_1!="X",]
test$PredSS_r1_1<-as.character(test$PredSS_r1_1)
test$PredSS_r1_1<-as.factor(test$PredSS_r1_1)


test<-test[test$PredSS_r2_.1!="X",]
test$PredSS_r2_.1<-as.character(test$PredSS_r2_.1)
test$PredSS_r2_.1<-as.factor(test$PredSS_r2_.1)


test<-test[test$PredRCH_r1_1!="X",]
test$PredRCH_r1_1<-as.character(test$PredRCH_r1_1)
test$PredRCH_r1_1<-as.factor(test$PredRCH_r1_1)


test<-test[test$PredRCH_r2_.1!="X",]
test$PredRCH_r2_.1<-as.character(test$PredRCH_r2_.1)
test$PredRCH_r2_.1<-as.factor(test$PredRCH_r2_.1)


test<-test[test$PredCN_r1_1!="X",]
test$PredCN_r1_1<-as.character(test$PredCN_r1_1)
test$PredCN_r1_1<-as.factor(test$PredCN_r1_1)


test<-test[test$PredSA_r2_.1!="X",]
test$PredSA_r2_.1<-as.character(test$PredSA_r2_.1)
test$PredSA_r2_.1<-as.factor(test$PredSA_r2_.1)

str(test)
###################################################
#Imposicion de nombres y factorizacion de la salida
###################################################
names(Data_train)<-c(names(test[,-122]),"class")
names(Data_train)<-c(names(Dat))
# Transformo la salida de la base de ejemplos en 0s y 1s
v = ifelse(Data_train[,dim(Data_train)[2]]=="positive",1,0)

# Y la sustituyo por la columna antigua de clase
Data_train = cbind(Data_train[1:dim(Data_train)[2]-1],class=v)
Data_train$class = as.factor(Data_train$class)


##################################################
# Normalizar las variables 
##################################################

Data_train<-datos
Data_train<-test
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


test<-Data_train
datos<-Data_train

ElementosNumericos<-sapply(1:dim(Data_train)[2],function(x)class(Data_train[,x]))=="numeric"
str(Data_train[,ElementosNumericos])
Data_train[,ElementosNumericos]<-  as.data.frame(scale(Data_train[,ElementosNumericos]))


Data_train<-Data_train[,c(nombresImportantes,"class")]

ListaFactores<-c("PredSS_r1_.1","PredSS_r1","PredSS_r1_1","PredSS_r2_.1",
                 "PredSS_r2","PredRCH_r1_.1","PredRCH_r1",
                 "PredRCH_r1_1","PredRCH_r2_.1",  "PredRCH_r2","PredRCH_r2_1","PredCN_r1_.1",
                 "PredCN_r1",   "PredCN_r1_1",  "PredCN_r2",  
                 "PredSA_r1",   "PredSA_r2_.1",    "PredSA_r2","PredSA_r2_1" )         
##################################################
# Binarizacion de las variables
##################################################
Data_train<-cbind((model.matrix(~PredSS_r1_.1-1,data=Data_train)),Data_train[,names(Data_train)!="PredSS_r1_.1"])
Data_train<-cbind((model.matrix(~PredSS_r1-1,data=Data_train)),Data_train[,names(Data_train)!="PredSS_r1"])
Data_train<-cbind((model.matrix(~PredSS_r1_1-1,data=Data_train)),Data_train[,names(Data_train)!="PredSS_r1_1"])
Data_train<-cbind((model.matrix(~PredSS_r2_.1-1,data=Data_train)),Data_train[,names(Data_train)!="PredSS_r2_.1"])
Data_train<-cbind((model.matrix(~PredSS_r2-1,data=Data_train)),Data_train[,names(Data_train)!="PredSS_r2"])
Data_train<-cbind((model.matrix(~PredRCH_r1_.1-1,data=Data_train)),Data_train[,names(Data_train)!="PredRCH_r1_.1"])
Data_train<-cbind((model.matrix(~PredRCH_r1-1,data=Data_train)),Data_train[,names(Data_train)!="PredRCH_r1"])
Data_train<-cbind((model.matrix(~PredRCH_r1_1-1,data=Data_train)),Data_train[,names(Data_train)!="PredRCH_r1_1"])
Data_train<-cbind((model.matrix(~PredRCH_r2_.1-1,data=Data_train)),Data_train[,names(Data_train)!="PredRCH_r2_.1"])
Data_train<-cbind((model.matrix(~PredRCH_r2-1,data=Data_train)),Data_train[,names(Data_train)!="PredRCH_r2"])
Data_train<-cbind((model.matrix(~PredRCH_r2_1-1,data=Data_train)),Data_train[,names(Data_train)!="PredRCH_r2_1"])
Data_train<-cbind((model.matrix(~PredCN_r1_.1-1,data=Data_train)),Data_train[,names(Data_train)!="PredCN_r1_.1"])
Data_train<-cbind(model.matrix(~PredCN_r1-1,data=Data_train),Data_train[,names(Data_train)!="PredCN_r1"])
Data_train<-cbind(model.matrix(~PredCN_r2-1,data=Data_train),Data_train[,names(Data_train)!="PredCN_r2"])
Data_train<-cbind(model.matrix(~PredCN_r1_1-1,data=Data_train),Data_train[,names(Data_train)!="PredCN_r1_1"])
Data_train<-cbind((model.matrix(~PredSA_r1-1,data=Data_train)),Data_train[,names(Data_train)!="PredSA_r1"])
Data_train<-cbind((model.matrix(~PredSA_r2_.1-1,data=Data_train)),Data_train[,names(Data_train)!="PredSA_r2_.1"])
Data_train<-cbind((model.matrix(~PredSA_r2-1,data=Data_train)),Data_train[,names(Data_train)!="PredSA_r2"])
Data_train<-cbind((model.matrix(~PredSA_r2_1-1,data=Data_train)),Data_train[,names(Data_train)!="PredSA_r2_1"])
test2<-Data_train

nombres<-names(Data_train)
names(Data_train[,1:dim(Data_train)[2]])
Data_train<-cbind( sapply(1:90,function(x) as.factor(Data_train[,x])),Data_train[,91:dim(Data_train)[2]])
names(Data_train)<-nombres#[,-122]
test2<-Data_train
summary(Data_train$class)
##################################################
#Discretizacion
##################################################
library(discretization)
lista<-c("PredSS_freq_global_H",  "PredCN_freq_global_0" , "PredRCH_freq_global_0" ,
"PredRCH_freq_global_4" ,"PredSA_freq_global_0" , "AA_freq_central_A"  , 
"AA_freq_central_D"   ,  "AA_freq_central_E"  ,   "AA_freq_central_I" ,     
"AA_freq_central_F","PredSS_freq_central_H" , "PredSS_freq_central_E"  ,"PredSS_freq_central_C" ,
"PredCN_freq_central_0" , "PredRCH_freq_central_0", "PredRCH_freq_central_1",
"PredSA_freq_central_0" , "PredSA_freq_central_4" ,"separation"  ,"propensity")  
# discretizacion mediante metodo CAIM
cm <- disc.Topdown(datos[,lista[1:20]], method=1)

# discretizacion mediante CACC
cm <- disc.Topdown(datos[,lista[2:4]], method=2)

# discretizacion mediante AMEVA
cm <- disc.Topdown(iris, method=3)

y<-sapply(1:19,function(x) cut(datos[,lista[x]], breaks =c(cm$cutp[[x]]) ))
table(y)
###########################################################################
# Eliminamos los datos con mas de 5% de NAs 
###########################################################################
filtro<-5
Data_train_filtrado<-Filtrado_NA(datos =Data_train,porcentaje = filtro )

escrituraDatos("../Data/","UGR2014Utimos1-1-2015.csv",datos)
###########################################################################
# Extracion de Clase negativa y positiva( con y sin NAs )
###########################################################################

source("Division_Dataset.R")

###########################################################################
# Seleccion de caracteristicas
###########################################################################
SeleccionCaracteristicas=T
Caracteristica="Fselector_ramdomForest"
numeroImport<-42
source("Modulos/Seleccion_caracteristicas.R")
###########################################################################
# Imposicion de los NAs
###########################################################################


imputados <- mice(Data_ipf)#,maxit = 5,m = 5,method = "rf"
newData <- complete(imputados)
Data_completo<-cbind(Data_full[1:20000,],class=Data_train[,51])
escrituraDatos("../Data/","UGR_train_Full_Mice.csv",Data_completo)
  
Data_train <- impKNNa(as.matrix(Data_train_filtrado) ,metric = "Aichison" ,agg = "median" )

kml(object,nbClusters=2:6,nbRedrawing=20,toPlot="none",parAlgo=parALGO())

###########################################################################
# Ejecucion de IPF , con 5% y 4 particiones
###########################################################################
Data_train<-datos
datos<-Data_train
neg<-datos[datos$class==0,]
pos<-datos[datos$class==1,]
datos1<-data.frame(rbind(neg[0:dim(pos)[1],]
                         ,pos[0:dim(pos)[1],]))
datos2<-data.frame(rbind(neg[dim(pos)[1]:(dim(pos)[1]+dim(pos)[1]),]
                         ,pos[0:dim(pos)[1],]))
datos3<-data.frame(rbind(neg[(dim(pos)[1]+dim(pos)[1]):dim(neg)[1],]
                         ,pos[0:(dim(neg)[1]-((dim(pos)[1]+dim(pos)[1]))),]))

summary(datos$class)
summary(datos1$class)
summary(datos2$class)
summary(datos3$class)
#Data_train<-datos
#Data_train<-Data_train_SVM
set.seed(12)
Data_ipf<-ipf(dataset = datos,k = 4,limite = 25 ,may = 1.2)

Data_ipf1<-ipf(dataset = datos1,k = 8,limite = 5 ,may = 2)
Data_ipf2<-ipf(dataset = datos2,k = 8,limite = 5 ,may = 2)
Data_ipf3<-ipf(dataset = datos3,k = 8,limite = 5 ,may = 2)

summary(Data_train[,122])
summary(datos[,dim(datos)[2]])
summary(Data_ipf[,dim(datos)[2]])
datos<-data.frame(rbind(Data_ipf1,Data_ipf2,Data_ipf3))
datos<-data.frame(rbind(datos,neg[dim(pos)[1]:dim(neg)[1],]))
#datos<-rbind(Data_ipf2,Data_ipf3)
escrituraDatos(dataset = datos,file = "Datos_train_21_01_09:54.csv",path = "..\\Data\\")

neg<-Data_ipf[Data_ipf$class==0,]
pos<-Data_ipf[Data_ipf$class==1,]
datos<-data.frame(rbind(neg[0:dim(pos)[1],],pos[0:dim(pos)[1],]))
summary(datos$class)

###########################################################################
# Ejecucion de Smote
###########################################################################

smot<-SMOTE(class~.,data = datos2)
,learner = )
escrituraDatos("../Data/","Datos_train_SMOTE_F2Mean500.csv",smot)
Data_train<-smot

x<-row.names((CV_lista[1]))<-c()

CV_lista<-obtenerParticiones(dataset = datos,k = 10) 
CV_lista2<-obtenerParticiones(dataset = newData,k = 10) 
CV(f = f,tipo = "J48",dat = CV_lista2)


names(Data_train)<-c(names(test),"class")
names(datos)<-c(names(test),"class")

f<-V51~.
newData <- SMOTE(class~., datos, perc.over = 600,perc.under=100,learner='rpartXse',se=0.5)
rpartXse(class ~ .,datos,se=0.5)

summary(datos[,dim(datos)[2]])
summary(newData[,dim(newData)[2]])
datos2<-newData
escrituraDatos("../Data/","Datos_train_SMOTE_F2Mean500.csv",smot)
###########################################################################
# Aprendizaje del modelo
###########################################################################
Data_train<-test
neg<-Data_train[Data_train$class=="1",]
pos<-Data_train[Data_train$class=="0",]
neg<-Data_train[Data_train$class=="negative",]
pos<-Data_train[Data_train$class=="positive",]
Dat<-data.frame(rbind(neg[1:5000,],pos[1:5000,]))
Dat<-data.frame(Data_train)
summary(Dat$class)
datos<-Dat
x<- J48(class~., na.action=na.omit, data=Data_train)
x<- J48(f, na.action=na.omit, data=Data_train)
pred = predict(x,test)
tab <- table(pred = pred, true = Data_ipf[,51])

porcentaje_accierto(tab)


f<-class~.
datos2<-datos
datos<-Dat
datos<-newData
sum(names(datos)==names(test))

x<- J48(f, na.action=na.omit, data=datos)
pred = predict(x,test)
tab <- table(pred = pred, true = test[,51])
cv_resul = evaluate_Weka_classifier(x,numFolds=10,data=datos,seed = 12)
cv_resul$confusionMatrix
porcentaje_accierto(cv_resul$confusionMatrix)


CV(dat = datos,f ,tipo = "RandomForest")

model.Ripper = JRip(f, datos)

model.Bagging<-Bagging(f, datos, control = Weka_control())

model.Ripper.pred = predict(model.Ripper, newdata = te)

# Acierto
(sum(model.Ripper.pred == iris.test[,5])/nrow(iris.test)*100)

 
cv_data<-obtenerParticiones(dataset = Data_ipf,10)



# Random Forest

############################__Fselector ramdomForest__######################
# se calculan los pesos
str(Data_train)
weights<-read.csv(  file = "../Data/Importancia_Fselector_randomForest.csv",sep = ",",comment.char="@")
row.names(weights)<-weights[,1]
weights <- random.forest.importance(class~.,datos, importance.type=1)
# se muestran los resultados
print(weights)
subset <- cutoff.k(weights,122)
subset<-nombresImportantes
f <- as.simple.formula(subset,"class")
str(datos)


neg<-datos2[datos2$class==0,]
pos<-datos2[datos2$class==1,]
datos<-Data_train
bag = randomForest(class~. , data=datos2 ,ntree = 1000, importance=T,keep.forest=TRUE)
,mtry = 12,nodesize = 1)
names(datos2)<-names(Data_train)

pred = predict (bag ,newdata =test)
tab <- table(pred = pred, true = test[,dim(test)[2]])
tab
porcentaje_accierto(tab)
##0.52722  ->0,706
##0.52     ->0,716
                       
str(test$PredSS_r1_1)
str(datos$)
test[,]


library(party)
set.seed(415)
bag <- cforest(f,data = datos, controls=cforest_unbiased(ntree=2000, mtry=3))
Prediction <- predict(fit, test, OOB=TRUE, type = "response")


#####################################################
# Random Forest
#####################################################
library(randomForest)
what <- "Random Forest"
FOREST_model <- randomForest(theFormula, data=trainset, ntree=50)

train_pred <- predict(FOREST_model, trainset, type="prob")[,2]
test_pred <- predict(FOREST_model, testset, type="prob")[,2]

display_results()
