

# -------  Bienvenido-------
# Raul Valerio - Statistics

# Carga el paquete específico del método Random Forest

library(randomForest)


# Porque random forest?

# ----->>> bootstrap aggregation ( Bagging )
       N,  m samples

# ----->>> random subset of the features. 10 features --> 

#For classification, the default value for m is sqrt(p) and the minimum
#node size is one.
#For regression, the default value for m is p/3 and the minimum
#node size is five.

# cuando usar random forest?   clasificacion supervisada solamente? 
  # -> majority vote
       
  # -> average


# Carga de datos inicial, tipos de flores con diferentes características 
       
data(iris)

datos <- iris

View(datos)

# Selección de una submuestra del 70% de los datos

set.seed(101)

tamano.total <- nrow(datos)

tamano.entreno <- round(tamano.total*0.7)  # 70%  entrenamiento, 30% prueba

datos.indices <- sample(1:tamano.total , size=tamano.entreno)

datos.entreno <- datos[datos.indices,]

datos.test <- datos[-datos.indices,]


# quien rivaliza con random forest?

  NN

# Ajustar modelo
help(randomForest)

modelo <- randomForest(Species~., data=datos.entreno)    # options: mxtest, ytest, maxnodes, ntree, mtry

# Resumen del ajuste del modelo

modelo
## 
## Call:
##  randomForest(formula = Species ~ ., data = datos.entreno) 
##                Type of random forest: classification
Confusion matrix:
  setosa versicolor virginica class.error
setosa         35          0         0  0.00000000
versicolor      0         36         2  0.05263158
virginica       0          3        29  0.09375000

# Importancia de las variables
modelo$importance
##              MeanDecreaseGini

MeanDecreaseGini
Sepal.Length         8.114463
Sepal.Width          1.324631
Petal.Length        32.069008
Petal.Width         27.565726
# Hacer predicciones

predicciones <- predict(modelo, datos.test[,-5])  #  5th column is real labels

# Matriz de confusión

(mc <- with(datos.test,table(predicciones, Species)))

#other way

mc<- table(predicciones,datos.test$Species)
mc

##             Species
## predicciones setosa versicolor virginica

predicciones setosa versicolor virginica
setosa         15          0         0
versicolor      0         11         3
virginica       0          1        15

# % correcto
  100 * sum(diag(mc)) / sum(mc)
  
  91%  tasa de acierto para random forest, datos Iris
  
