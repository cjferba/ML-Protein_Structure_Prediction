# Gr?ficos
#install.packages("ggplot2")
library(ggplot2)
#install.packages("devtools")
library(devtools)
#install_github("ggbiplot", "vqv")
library(reshape)   # melt
#library(ggbiplot)


#install.packages("rgl")     #plot3D
#library(rgl)                 
#install.packages("GGally")  #ggpairs
#library(GGally)


# 1-variate
#install.packages("outliers")  # Grubb
library(outliers)
#install.packages("EnvStats")  # Rosner
library(EnvStats)

# Multi-variate -Mahalanobis-
#install.packages("mvoutlier")  #MCD ChiC
library(mvoutlier)       
#install.packages("CerioliOutlierDetection")  #MCD Hardin Rocke
library(CerioliOutlierDetection)
#install.packages("robustbase")
library(robustbase)
#install.packages("mvnormtest")   # Test Normalidad multivariante
library(mvnormtest)   

# Multivariate Unsupervised
#install.packages("DMwR")  #lof
library(DMwR)
#install.packages("cluster")
library(cluster)    # PAM



# 
#  ggbiplot.r
#  
#  Copyright 2011 Vincent Q. Vu.
# 
#  This program is free software; you can redistribute it and/or
#  modify it under the terms of the GNU General Public License
#  as published by the Free Software Foundation; either version 2
#  of the License, or (at your option) any later version.
#  
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#  
#  You should have received a copy of the GNU General Public License
#  along with this program; if not, write to the Free Software
#  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
# 

#' Biplot for Principal Components using ggplot2
#'
#' @param pcobj           an object returned by prcomp() or princomp()
#' @param choices         which PCs to plot
#' @param scale           covariance biplot (scale = 1), form biplot (scale = 0). When scale = 1, the inner product between the variables approximates the covariance and the distance between the points approximates the Mahalanobis distance.
#' @param obs.scale       scale factor to apply to observations
#' @param var.scale       scale factor to apply to variables
#' @param pc.biplot       for compatibility with biplot.princomp()
#' @param groups          optional factor variable indicating the groups that the observations belong to. If provided the points will be colored according to groups
#' @param ellipse         draw a normal data ellipse for each group?
#' @param ellipse.prob    size of the ellipse in Normal probability
#' @param labels          optional vector of labels for the observations
#' @param labels.size     size of the text used for the labels
#' @param alpha           alpha transparency value for the points (0 = TRUEransparent, 1 = opaque)
#' @param circle          draw a correlation circle? (only applies when prcomp was called with scale = TRUE and when var.scale = 1)
#' @param var.axes        draw arrows for the variables?
#' @param varname.size    size of the text for variable names
#' @param varname.adjust  adjustment factor the placement of the variable names, >= 1 means farther from the arrow

#' @param varname.abbrev  whether or not to abbreviate the variable names
#'
#' @return                a ggplot2 plot
#' @export
#' @examples
#'   data(wine)
#'   wine.pca <- prcomp(wine, scale. = TRUE)
#'   print(ggbiplot(wine.pca, obs.scale = 1, var.scale = 1, groups = wine.class, ellipse = TRUE, circle = TRUE))
#'
ggbiplot <- function(pcobj, choices = 1:2, scale = 1, pc.biplot = TRUE, 
                     obs.scale = 1 - scale, var.scale = scale, 
                     groups = NULL, ellipse = FALSE, ellipse.prob = 0.68, 
                     labels = NULL, labels.size = 3, alpha = 1, 
                     var.axes = TRUE, 
                     circle = FALSE, circle.prob = 0.69, 
                     varname.size = 3, varname.adjust = 1.5, 
                     varname.abbrev = FALSE, ...)
{
  library(ggplot2)
  library(plyr)
  library(scales)
  library(grid)
  
  stopifnot(length(choices) == 2)
  
  # Recover the SVD
  if(inherits(pcobj, 'prcomp')){
    nobs.factor <- sqrt(nrow(pcobj$x) - 1)
    d <- pcobj$sdev
    u <- sweep(pcobj$x, 2, 1 / (d * nobs.factor), FUN = '*')
    v <- pcobj$rotation
  } else if(inherits(pcobj, 'princomp')) {
    nobs.factor <- sqrt(pcobj$n.obs)
    d <- pcobj$sdev
    u <- sweep(pcobj$scores, 2, 1 / (d * nobs.factor), FUN = '*')
    v <- pcobj$loadings
  } else if(inherits(pcobj, 'PCA')) {
    nobs.factor <- sqrt(nrow(pcobj$call$X))
    d <- unlist(sqrt(pcobj$eig)[1])
    u <- sweep(pcobj$ind$coord, 2, 1 / (d * nobs.factor), FUN = '*')
    v <- sweep(pcobj$var$coord,2,sqrt(pcobj$eig[1:ncol(pcobj$var$coord),1]),FUN="/")
  } else if(inherits(pcobj, "lda")) {
    nobs.factor <- sqrt(pcobj$N)
    d <- pcobj$svd
    u <- predict(pcobj)$x/nobs.factor
    v <- pcobj$scaling
    d.total <- sum(d^2)
  } else {
    stop('Expected a object of class prcomp, princomp, PCA, or lda')
  }
  
  # Scores
  choices <- pmin(choices, ncol(u))
  df.u <- as.data.frame(sweep(u[,choices], 2, d[choices]^obs.scale, FUN='*'))
  
  # Directions
  v <- sweep(v, 2, d^var.scale, FUN='*')
  df.v <- as.data.frame(v[, choices])
  
  names(df.u) <- c('xvar', 'yvar')
  names(df.v) <- names(df.u)
  
  if(pc.biplot) {
    df.u <- df.u * nobs.factor
  }
  
  # Scale the radius of the correlation circle so that it corresponds to 
  # a data ellipse for the standardized PC scores
  r <- sqrt(qchisq(circle.prob, df = 2)) * prod(colMeans(df.u^2))^(1/4)
  
  # Scale directions
  v.scale <- rowSums(v^2)
  df.v <- r * df.v / sqrt(max(v.scale))
  
  # Change the labels for the axes
  if(obs.scale == 0) {
    u.axis.labs <- paste('standardized PC', choices, sep='')
  } else {
    u.axis.labs <- paste('PC', choices, sep='')
  }
  
  # Append the proportion of explained variance to the axis labels
  u.axis.labs <- paste(u.axis.labs, 
                       sprintf('(%0.1f%% explained var.)', 
                               100 * pcobj$sdev[choices]^2/sum(pcobj$sdev^2)))
  
  # Score Labels
  if(!is.null(labels)) {
    df.u$labels <- labels
  }
  
  # Grouping variable
  if(!is.null(groups)) {
    df.u$groups <- groups
  }
  
  # Variable Names
  if(varname.abbrev) {
    df.v$varname <- abbreviate(rownames(v))
  } else {
    df.v$varname <- rownames(v)
  }
  
  # Variables for text label placement
  df.v$angle <- with(df.v, (180/pi) * atan(yvar / xvar))
  df.v$hjust = with(df.v, (1 - varname.adjust * sign(xvar)) / 2)
  
  # Base plot
  g <- ggplot(data = df.u, aes(x = xvar, y = yvar)) + 
    xlab(u.axis.labs[1]) + ylab(u.axis.labs[2]) + coord_equal()
  
  if(var.axes) {
    # Draw circle
    if(circle) 
    {
      theta <- c(seq(-pi, pi, length = 50), seq(pi, -pi, length = 50))
      circle <- data.frame(xvar = r * cos(theta), yvar = r * sin(theta))
      g <- g + geom_path(data = circle, color = muted('white'), 
                         size = 1/2, alpha = 1/3)
    }
    
    # Draw directions
    g <- g +
      geom_segment(data = df.v,
                   aes(x = 0, y = 0, xend = xvar, yend = yvar),
                   arrow = arrow(length = unit(1/2, 'picas')), 
                   color = muted('red'))
  }
  
  # Draw either labels or points
  if(!is.null(df.u$labels)) {
    if(!is.null(df.u$groups)) {
      g <- g + geom_text(aes(label = labels, color = groups), 
                         size = labels.size)
    } else {
      g <- g + geom_text(aes(label = labels), size = labels.size)      
    }
  } else {
    if(!is.null(df.u$groups)) {
      g <- g + geom_point(aes(color = groups), alpha = alpha)
    } else {
      g <- g + geom_point(alpha = alpha)      
    }
  }
  
  # Overlay a concentration ellipse if there are groups
  if(!is.null(df.u$groups) && ellipse) {
    theta <- c(seq(-pi, pi, length = 50), seq(pi, -pi, length = 50))
    circle <- cbind(cos(theta), sin(theta))
    
    ell <- ddply(df.u, 'groups', function(x) {
      if(nrow(x) <= 2) {
        return(NULL)
      }
      sigma <- var(cbind(x$xvar, x$yvar))
      mu <- c(mean(x$xvar), mean(x$yvar))
      ed <- sqrt(qchisq(ellipse.prob, df = 2))
      data.frame(sweep(circle %*% chol(sigma) * ed, 2, mu, FUN = '+'), 
                 groups = x$groups[1])
    })
    names(ell)[1:2] <- c('xvar', 'yvar')
    g <- g + geom_path(data = ell, aes(color = groups, group = groups))
  }
  
  # Label the variable axes
  if(var.axes) {
    g <- g + 
      geom_text(data = df.v, 
                aes(label = varname, x = xvar, y = yvar, 
                    angle = angle, hjust = hjust), 
                color = 'darkred', size = varname.size)
  }
  # Change the name of the legend for groups
  # if(!is.null(groups)) {
  #   g <- g + scale_color_brewer(name = deparse(substitute(groups)), 
  #                               palette = 'Dark2')
  # }
  
  # TODO: Add a second set of axes
  
  return(g)
}


###########################################################################
# Calcula los outliers IQR (devuelve un vector de T/F)

vector.es.outlier.IQR = function (datos, indice, coef = 1.5){
  columna.datos = datos[,indice]
  cuartil.primero = quantile(columna.datos)[2]  #quantile[1] es el m?nimo y quantile[5] el m?ximo.
  cuartil.tercero = quantile(columna.datos)[4] 
  iqr = cuartil.tercero - cuartil.primero
  extremo.superior.outlier = (iqr * coef) + cuartil.tercero
  extremo.inferior.outlier = cuartil.primero - (iqr * coef)
  es.outlier  = columna.datos > extremo.superior.outlier |
    columna.datos < extremo.inferior.outlier
  return (es.outlier)
}

###########################################################################
# Calcula y devuelve las claves de los outliers IQR (devuelve un vector de T/F)

vector.claves.outliers.IQR = function(datos, indice, coef = 1.5){
  columna.datos = datos[,indice]
  vector.de.outliers = vector.es.outlier.IQR(datos, indice, coef)
  which(vector.de.outliers  == TRUE)
}


#######################################################################
# Muestra un plot b?sico con los outliers en rojo

MiPlot_Univariate_Outliers = function (datos, indices_de_Outliers, titulo){
  numero.de.datos = nrow(as.matrix(datos))
  print(numero.de.datos)
  vectorTFoutliers =  rep(FALSE, numero.de.datos)
  vectorTFoutliers[indices_de_Outliers] = TRUE
  print(vectorTFoutliers)
  vector.colores.outlier = rep("black", numero.de.datos)
  vector.colores.outlier [vectorTFoutliers] = "red"
  windows()
  plot(datos, col=vector.colores.outlier, main = titulo)
}




#######################################################################
# Devuelve los nombres de aquellas filas de datos especificadas 
# en el segundo par?metro (vector de T/F)

SeleccionNombres = function (datos, vector_TF_datos_a_incluir) {
  if (is.null(row.names(datos)))
    row.names(datos) = rep(1:numero.de.filas)
  
  numero.de.filas = nrow(datos)
  nombres.de.filas = rep("", numero.de.filas)
  
  nombres.de.filas[vector_TF_datos_a_incluir==TRUE] = row.names(datos)[vector_TF_datos_a_incluir==TRUE]
  
  nombres.de.filas
}


#######################################################################
# Calcular los outliers IQR y muestra sus etiquetas en un BoxPlot

MiBoxPlot_IQR_Univariate_Outliers = function (datos, indice.de.columna, coef = 1.5){
  # Importante: Para que aes busque los par?metros en el ?mbito local, debe incluirse  environment = environment()
  
  datos = as.data.frame(datos)
  
  vector.TF.outliers.IQR = vector.es.outlier.IQR(datos, indice.de.columna, coef)
  nombres.de.filas = SeleccionNombres(datos, vector.TF.outliers.IQR)
  nombre.de.columna = colnames(datos,indice.de.columna)
  
  ggboxplot = ggplot(data = datos, aes(x=factor(""), y=datos[,indice.de.columna]) , environment = environment()) + 
    xlab(nombre.de.columna) + ylab("") +
    geom_boxplot(outlier.colour = "red") + 
    geom_text(aes(label = nombres.de.filas)) #, position = position_jitter(width = 0.1))   
  
  windows()
  ggboxplot
}

#######################################################################
# Muestra de forma conjunta todos los BoxPlots de las columnas de datos
# Para ello, normaliza los datos.
# Tambi?n muestra con un punto en rojo los outliers de cada columna
# Para hacerlo con ggplot, lamentablemente hay que construir antes una tabla 
# que contenga en cada fila el valor que a cada tupla le da cada variable -> paquete reshape->melt

MiBoxPlot_juntos  = function (datos, vector_TF_datos_a_incluir = c()){  
  # Requiere reshape
  # Importante: Para que aes busque los par?metros en el ?mbito local, debe incluirse  environment = environment()
  
  nombres.de.filas = SeleccionNombres(datos, vector_TF_datos_a_incluir)
  
  datos = scale(datos)
  datos.melted = melt(datos)
  colnames(datos.melted)[2]="Variables"
  colnames(datos.melted)[3]="zscore"
  factor.melted = colnames(datos.melted)[1]
  columna.factor = as.factor(datos.melted[,factor.melted])
  levels(columna.factor)[!levels(columna.factor) %in% nombres.de.filas] = ""  
  
  ggplot(data = datos.melted, aes(x=Variables, y=zscore), environment = environment()) + 
    geom_boxplot(outlier.colour = "red") + 
    geom_text(aes(label = columna.factor), size = 3) 
}


#######################################################################
# Muestra de forma conjunta todos los BoxPlots de las columnas de datos
# Para ello, normaliza los datos
# Tambi?n muestra las etiquetas de los outliers de cada columna


MiBoxPlot_juntos_con_etiquetas = function (datos, coef = 1.5){
  matriz.datos.TF.outliers = sapply(1:ncol(datos), function(x) vector.es.outlier.IQR(datos, x, coef))  # Aplicamos outlier IQR a cada columna
  vector.datos.TF.outliers = apply(matriz.datos.TF.outliers, 1, sum)   
  vector.datos.TF.outliers[vector.datos.TF.outliers > 1] = 1            # Si un registro es outlier en alguna columna lo incluimos
  
  MiBoxPlot_juntos(datos, vector.datos.TF.outliers)
}


MiBiPlot_Multivariate_Outliers = function (datos, vectorTFoutliers, titulo){
  identificadores_de_datos = rownames(datos)
  identificadores_de_datos[!vectorTFoutliers] = ''
  print(identificadores_de_datos)
  
  PCA.model = princomp(scale(datos))
  outlier.shapes = c(".","x") #c(21,8)
  biplot = ggbiplot(PCA.model, obs.scale = 1, var.scale=1 , varname.size = 5,groups =  vectorTFoutliers, alpha = 1/2) #alpha = 1/10, 
  biplot = biplot + labs(color = "Outliers")
  biplot = biplot + scale_color_manual(values = c("black","red"))
  biplot = biplot + geom_text(label = identificadores_de_datos, stat = "identity", size = 3, hjust=0, vjust=0)
  biplot = biplot + ggtitle(titulo)
  
  windows()
  print(biplot)
}
