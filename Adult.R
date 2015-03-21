#########################################
# Predecir income > 50K de los adultos trabajadores en USA 
# Url dataset -> http://archive.ics.uci.edu/ml/datasets/Adult
#########################################

# Carga de librerias
library(stats)
library(modeest)
library(prettyR)
library(e1071)
library(nortest)

# Limpieza workspace
rm(list = ls(all = TRUE))

# Carga de datos
url.data <- 'http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data'
adults <- read.csv(url.data, header = FALSE)

# Renombramos columnas
columnas <- c('age', 'workclass', 'fnlwgt', 'education', 'education_num', 'marital_status',
              'occupation', 'relationship', 'race', 'sex', 'capital_gain', 'capital_loss',
              'hours_per_week', 'native_country', 'prediction_salary')
colnames(adults) <- columnas

len.sample <- nrow(adults)

########################################
#####   DECLARACION  FUNCIONES    ######
########################################

calculate.max.whisker <- function(variable){
  # Devuelve el max.whisker de la variable pasada como parametro
  quantiles <- summary(variable)
  RIC <- quantiles[['3rd Qu.']] - quantiles[['1st Qu.']]
  return(quantiles[['3rd Qu.']] + 1.5*RIC)
}

calculate.min.whisker <- function(variable){
  # Devuelve el min.whisker de la variable pasada como parametro
  quantiles <- summary(variable)
  RIC <- quantiles[['3rd Qu.']] - quantiles[['1st Qu.']]
  return(quantiles[['1st Qu.']] - 1.5*RIC)
}


#########################################
####    ANALISIS DE LAS VARIABLES   #####
#########################################

# Vamos a describir las variables del dataset:
  # age --> Variable discreta numerica 
  # education_num --> Variable discreta categorica
  # race --> Variable cualitativa  

########################## AGE ############################

# No hay que limpiar la variable ya que no hay valores NA
unique(adults$age)

# Descripción medidas descriptivas previo a eliminacion de outliers
# Media: 38.58165, Mediana: 37, Moda: 36 
mean(adults$age)
summary(adults$age)
mlv(adults$age)

# Observamos valores atípicos mayores a una edad en torno a 80 anios
# Vamos a calcular el bigote máximo para eliminar outliers
# y filtramos el dataset eliminando valores cuya age sea mayor que max.whisker
boxplot(adults$age)
adults <- subset(adults, age <= calculate.max.whisker(adults$age))

# Observamos que aun hay outliers con lo cual aplicamos el mismo metodo de nuevo
boxplot(adults$age)
adults <- subset(adults, age <= calculate.max.whisker(adults$age))

# Conseguimos eliminar outliers
boxplot(adults$age)

# Descripción medidas descriptivas despues de la eliminacion de outliers
# Media: 38.26522, Mediana: 37, Moda: 36
mean(adults$age)
summary(adults$age)
mlv(adults$age)

# Como vemos, los outliers no estaban teniendo un gran efecto en la distribucion
# de la variable ya que las medidas no han variado signficativamente

# Vamos a ver ahora como se distribuyen los datos con un histograma
hist(adults$age, breaks = 5)

# varianza y desviacion estandar
var(adults$age)
sd(adults$age)

# Plot funcion de densidad
plot(density(adults$age), col="red")

# LLevamos a cabo el test de Normalidad de Lilliefors (Kolmogorov-Smirnov) 
# H0 = "La distribución se aproxima a una normal" 
# ya que en la funcion de densidad apreciamos cierta normalidad
lillie.test(adults$age)

# Para un nivel de signifacion alpha = 0.05/0.01, 
# rechazamos H0 ya que p-value < 0.05/0.01

########################## EDUCATION_NUM ############################

# No hay que limpiar la variable ya que no hay valores NA
unique(adults$education_num)

# Descripción medidas descriptivas previo a eliminacion de outliers
# Media: 10.0866, Mediana: 10, Moda: 9
mean(adults$education_num)
summary(adults$education_num)
mlv(adults$education_num)

# Observamos valores atípicos menores a una education_num en torno a 5
# Vamos a calcular el bigote minimo para eliminar outliers
# y filtramos el dataset eliminando valores cuyo education_num sea menor que min.whisker
boxplot(adults$education_num)
adults <- subset(adults, education_num >= calculate.min.whisker(adults$education_num))

# Conseguimos eliminar outliers
boxplot(adults$education_num)

# Descripción medidas descriptivas despues de la eliminacion de outliers
# Media: 10.33848, Mediana: 10, Moda: 9
mean(adults$education_num)
summary(adults$education_num)
mlv(adults$education_num)

# Visualizacion de los valores una vez filtrados
hist(adults$education_num)

# No obtenemos mucha informacion, con lo cual veamos como se distribuyen
# en un sector circular de forma que 9, 10 y 13 son los mas frecuentes
pie(table(adults$education_num))

# Varianza y desviacion estandar
var(adults$education_num)
sd(adults$education_num)

########################## RACE ############################

# vemos que predomina "White" con respecto a los demás valores
pie(table(adults$race))

# En concreto, "White" agrupa un 81% del total de los datos
table(adults$race)/len.sample







