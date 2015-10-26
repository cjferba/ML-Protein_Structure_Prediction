#division de los conjuntos sehgun clase y segun NAs
Elementos_clase_neg<-Data_train[Data_train$class=="negative",]
neg_Sin_NA<-Devolver_NA(datos =neg ,Bool_NA = T)
neg_Con_NA<-Devolver_NA(datos =neg ,Bool_NA = F)

Elementos_clase_pos<-Data_train[Data_train$class=="positive",]
pos_Sin_NA<-Devolver_NA(datos =pos ,Bool_NA = T)
pos_Con_NA<-Devolver_NA(datos =pos ,Bool_NA = F)

dim(neg_Sin_NA)
dim(neg_Con_NA)
dim(pos_Sin_NA)
dim(pos_Con_NA)

###########################################################################
# Variables con NAs y sin NAs
###########################################################################

Variables_Sin_NA<-sapply(1:dim(Data_train)[2],Buscar_NA,dato=Data_train)==0
ConjuntoSinNA<-Data_train[,Variables_Sin_NA]

Variables_Con_NA<-sapply(1:dim(Data_train)[2],Buscar_NA,dato=Data_train)!=0
ConjuntoConNA<-Data_train[,Variables_Con_NA]


###########################################################################
# Creacion de conjuntos divididos en 10 conjuntos
###########################################################################

#CV_fold_neg<- k_foldsDiv(neg_Sin_NA,"class",k=10)
#CV_fold_pos<-k_foldsDiv(pos_Sin_NA,"class",k=10)
