#librerías
require(MASS)
require(klaR)
library(ggplot2)
library(grid)
library(gridExtra)

# Carga de datos
data(iris)
datos <- iris
View(datos)

# Selección de una submuestra del 75%
set.seed(101)
Total <- nrow(datos)
Muestra <- round(150*0.75)
indices <- sample(1:Total , size=Muestra)
entreno <- datos[indices,]
prueba <- datos[-indices,]

# Análisis discriminante
lda <- lda(formula= Species~. , data=entreno)
modelo_lda <- lda(Species~Sepal.Width+Sepal.Length+Petal.Length+Petal.Width,data=entreno)
modelo_lda

# probabilidad de pertenencia a cada clase
lda.p <- predict(lda, newdata=prueba, interval='confidence')

# Asignación de colores a cada especie para dibujar.
color <- rep("green",nrow(entreno))
color[entreno$Species == "setosa"] <- "red"
color[entreno$Species == "virginica"] <- "blue"

# primeras componentes del AD. Abbrev abrevia los nombres
plot(lda, dimen=2, col=color, abbrev=3)

# pares de componentes.
pairs(lda, col=color, abbrev=1)

# Matriz de confusión, validación del 35% de datos restantes.
mc <- table(lda.p$class, prueba$Species)
mc

# Correctamente clasificados en %
100 * sum(diag(mc)) / sum(mc)


