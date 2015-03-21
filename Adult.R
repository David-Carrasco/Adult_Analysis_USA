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
data <- read.csv(url.data, header = FALSE)

# Renombramos columnas
columnas <- c('age', 'workclass', 'fnlwgt', 'education', 'education_num', 'marital_status',
              'occupation', 'relationship', 'race', 'sex', 'capital_gain', 'capital_loss',
              'hours_per_week', 'native_country', 'prediction_salary')
colnames(data) <- columnas

len.sample <- nrow(data)


########################################
#####   DECLARACION  FUNCIONES    ######
########################################

calculate.max.whisker <- function(variable){
  # Devuelve el max.whisker de la variable pasada como parametro
  quantiles <- summary(variable)
  RIC <- quantiles[['3rd Qu.']] - quantiles[['1st Qu.']]
  return(quantiles[['3rd Qu.']] + 1.5*RIC)
}


#########################################
####    ANALISIS DE LAS VARIABLES   #####
#########################################

# Vamos a analizar las siguientes variables del dataset:
  # age --> Variable discreta numerica 
  # education_num --> Variable discreta categorica
  # race --> Variable cualitativa  

########################## AGE ############################

# No hay que limpiar la variable ya que no hay valores NA
unique(data$age)

# Descripción medidas descriptivas previo a eliminacion de outliers
# Media: 38.58165, Mediana: 37, Moda: 36 
mlv(data$age)
mean(data$age)
summary(data$age)

# Observamos valores atípicos mayores a una edad en torno a 80 anios
# Vamos a calcular el bigote máximo para eliminar outliers
# y filtramos el dataset eliminando valores cuya age sea mayor que max.whisker
boxplot(data$age)
data <- subset(data, age <= calculate.max.whisker(data$age))

# Observamos que aun hay outliers con lo cual aplicamos el mismo metodo de nuevo
boxplot(data$age)
data <- subset(data, age <= calculate.max.whisker(data$age))

# Conseguimos eliminar outliers
boxplot(data$age)

# Descripción medidas descriptivas despues de la eliminacion de outliers
# Media: 38.26522, Mediana: 37, Moda: 36
mlv(data$age)
mean(data$age)
summary(data$age)

# Como vemos, los outliers no estaban teniendo un gran efecto en la distribucion
# de la variable ya que las medidas no han variado signficativamente

# Vamos a ver ahora como se distribuyen los datos con un histograma
hist(data$age, breaks = 5)

# varianza y desviacion estandar
var(data$age)
sd(data$age)

# Plot funcion de densidad
plot(density(data$age), col="red")

# LLevamos a cabo el test de Normalidad de Lilliefors (Kolmogorov-Smirnov) 
# H0 = "La distribución se aproxima a una normal" 
# ya que en la funcion de densidad apreciamos cierta normalidad
lillie.test(data$age)

# Para un nivel de signifacion alpha = 0.05/0.01, 
# rechazamos H0 ya que p-value < 0.05/0.01


########################## EDUCATION_NUM ############################



