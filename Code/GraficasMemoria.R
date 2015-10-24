pm <- ggpairs(datos[,91:122])
pm <- ggpairs(tips, 1:3, columnLabels = c("Total Bill", "Tip", "Sex"))
# pm
pm <- ggpairs(tips, upper = "blank")
# pm

## ------------------------- ##
## An Example of ##
## Automating Plot Output ##
## ------------------------- ##
mypath <- file.path("C:./../IMG/","Graficas",paste("myplot_", "nombre", ".jpg", sep = ""))


names = LETTERS[1:26] ## Gives a sequence of the letters of the alphabet

beta1 = rnorm(26, 5, 2) ## A vector of slopes (one for each letter)
beta0 = 10 ## A common intercept

for(i in 1:50){
  mypath <- file.path("C:./../IMG/","Graficas",paste("myplot_", names(datos)[i], ".jpg", sep = ""))
  jpeg(file=mypath)
  mytitle = paste("",names(datos)[i])
  plot(datos[,i],datos[,51], main = mytitle,xl)
  dev.off()
}



