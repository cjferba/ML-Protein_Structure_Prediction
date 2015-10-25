##################################################################
#                     Boosting                                   #
#################################################################
library(ada)

modelo <- boosting.cv(f,datos,mfinal=10,control=rpart.control(cp=0))
modelo$class
modelo$error
modelo$confusion
f<-class~.
bag <- boosting(formula = f,data = datos,mfinal =5,coeflearn = "Zhu",control=rpart.control(cp=0),)

modelo <-ada(class~.,data=datos,iter=20,nu=1,type="gentle")
modelo$class
modelo$error
modelo$confusion
pred=predict.boosting (bag ,newdata =datos)


set.seed (12)
boost =gbm(f,data=datos2, 
           distribution="multinomial",n.trees =500,
           interaction.depth =4)

pred=predict (boost ,newdata =test,type ="response",n.trees=500 )



yhat.boost.y = sapply(1:nrow(Data_ipf), function(x){
  which.max(yhat.boost[x,])
})

(sum(yhat.boost.y == as.numeric(iris.test[,5]))/nrow(iris.test))*100
