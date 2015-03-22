#########################################
# Predecir income > 50K de los adultos trabajadores en USA 
# Url dataset -> http://archive.ics.uci.edu/ml/datasets/Adult
# Descripción features -> http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.names
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

########################################
########   LIMPIEZA  DATOS    ##########
########################################

#Limpiamos dataset de NAs
#En este caso, no hay NAs como tal
sapply(adults, function(x){any(is.na(x))})

#Observando cada columna, vemos que los NAs se especifican como un level ' ?'
#con lo cual, vamos a eliminar dicho level en los factors y en las filas que lo tengan
sapply(adults, unique)

#Filtramos el dataframe y los levels en base a la condicion anterior
adults <- adults[apply(adults, MARGIN = 1, function(row) {all(row != ' ?')}),]
adults[, columnas] <- droplevels(adults[, columnas])

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
  # age --> Discreta numerica
  # workclass --> Cualitativa
  # fnlwgt (final weight) --> Discreta numerica
  # education --> Cualitativa
  # education_num --> Discreta categorica
  # marital-status --> Cualitativa
  # occupation --> Cualitativa
  # relationship --> Cualitativa
  # race --> Cualitativa
  # sex --> Cualitativa
  # capital-gain --> Discreta Numerica
  # capital-loss --> Discreta Numerica
  # hours-per-week --> Discreta Numerica
  # native-country --> Cualitativa

# Nos centraremos primero en las cuantitativas ya que eliminaremos outliers si es preciso
# de forma que no afecte a los analisis de las variables cualitativas ya con los outliers eliminados

#########################################
###########  CUANTITATIVAS   ############
#########################################

########################## AGE ############################

# Descripción medidas descriptivas previo a eliminacion de outliers
# Media: 38.4379, Mediana: 37, Moda: 36 
mean(adults$age)
summary(adults$age)
mlv(adults$age)

# Observamos valores atípicos mayores a una edad en torno a 80 anios
# Vamos a calcular el bigote máximo para eliminar outliers
# y filtramos el dataset eliminando valores cuya age sea mayor que max.whisker
boxplot(adults$age)
adults <- subset(adults, age <= calculate.max.whisker(adults$age))

# Conseguimos eliminar outliers
boxplot(adults$age)

# Descripción medidas descriptivas despues de la eliminacion de outliers
# Media: 38.19538, Mediana: 37, Moda: 36
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


########################## FNLWGT ############################

# Descripción medidas descriptivas previo a eliminacion de outliers
# Media: 189928.7, Mediana: 178600, Moda: 203488 
mean(adults$fnlwgt)
summary(adults$fnlwgt)
mlv(adults$fnlwgt)

# Observamos valores atípicos menores a una education_num en torno a 500000
# Vamos a calcular el bigote maximo para eliminar outliers
# y filtramos el dataset eliminando valores cuyo fnlwgt sea mayor que max.whisker
boxplot(adults$fnlwgt)
adults <- subset(adults, fnlwgt <= calculate.max.whisker(adults$fnlwgt))

# Observamos que aun hay outliers con lo cual aplicamos el mismo metodo de nuevo
boxplot(adults$fnlwgt)
adults <- subset(adults, fnlwgt <= calculate.max.whisker(adults$fnlwgt))

# Observamos que aun hay outliers con lo cual aplicamos el mismo metodo de nuevo
boxplot(adults$fnlwgt)
adults <- subset(adults, fnlwgt <= calculate.max.whisker(adults$fnlwgt))

# Conseguimos eliminar outliers
boxplot(adults$fnlwgt)

# Descripción medidas descriptivas tras eliminar outliers
# Media: 177119.9, Mediana: 175200, Moda: 203488 
mean(adults$fnlwgt)
summary(adults$fnlwgt)
mlv(adults$fnlwgt)

# Visualizacion de los valores una vez filtrados
hist(adults$fnlwgt)

# Varianza y desviacion estandar
var(adults$fnlwgt)
sd(adults$fnlwgt)

# Plot funcion de densidad
plot(density(adults$fnlwgt), col="red")

# Vamos a inferir si nuestra muestra puede modelarse como una distribución Normal en vista
# a su función de densidad con el test de Lilliefors (Kolmogorov-Smirnov)
lillie.test(adults$fnlwgt)   

# Para un nivel de signifacion alpha = 0.05/0.01, 
# rechazamos Hipotesis de Normalidad ya que p-value < 0.05/0.01

########################## EDUCATION_NUM ############################

# Descripción medidas descriptivas previo a eliminacion de outliers
# Media: 10.14634, Mediana: 10, Moda: 9
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
# Media: 10.19621, Mediana: 10, Moda: 9
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

########################## CAPITAL-GAIN ############################

# Descripción medidas descriptivas previo a eliminacion de outliers
# Media: 1093.744, Mediana: 0, Moda: 0
mean(adults$capital_gain)
summary(adults$capital_gain)
mlv(adults$capital_gain)

# Observamos valores atípicos mayores a un capital-gain de 0
# Vamos a calcular el bigote maximo para eliminar outliers
# y filtramos el dataset eliminando valores cuyo capital_gain sea mayor que max.whisker
boxplot(adults$capital_gain)
adults <- subset(adults, capital_gain <= calculate.max.whisker(adults$capital_gain))

# Conseguimos eliminar outliers
boxplot(adults$capital_gain)

# Descripción medidas descriptivas despues de eliminar outliers
# Media: 0, Mediana: 0, Moda: 0
mean(adults$capital_gain)
summary(adults$capital_gain)
mlv(adults$capital_gain)

# Todos los valores para capital-gain quedan como 0
table(adults$capital_gain)


########################## CAPITAL-LOSS ############################

# Descripción medidas descriptivas previo a eliminacion de outliers
# Media: 97.27649, Mediana: 0, Moda: 0
mean(adults$capital_loss)
summary(adults$capital_loss)
mlv(adults$capital_loss)

# Observamos valores atípicos mayores a un capital_loss de 0
# Vamos a calcular el bigote maximo para eliminar outliers
# y filtramos el dataset eliminando valores cuyo capital_loss sea mayor que max.whisker
boxplot(adults$capital_loss)
adults <- subset(adults, capital_loss <= calculate.max.whisker(adults$capital_loss))

# Conseguimos eliminar outliers
boxplot(adults$capital_loss)

# Descripción medidas descriptivas despues de eliminar outliers
# Media: 0, Mediana: 0, Moda: 0
mean(adults$capital_loss)
summary(adults$capital_loss)
mlv(adults$capital_loss)

# Todos los valores para capital-gain quedan como 0
table(adults$capital_loss)


########################## HOURS_PER_WEEK ############################

# Descripción medidas descriptivas previo a eliminacion de outliers
# Media: 40.60579, Mediana: 40, Moda: 40 
mean(adults$hours_per_week)
summary(adults$hours_per_week)
mlv(adults$hours_per_week)

# Observamos valores atípicos mayores a una edad en torno a 50 horas y menores en torno a 30 horas
# Vamos a calcular el bigote máximo y minimo para eliminar outliers
boxplot(adults$hours_per_week)

#Empezamos por el whisker superior
adults <- subset(adults, hours_per_week <= calculate.max.whisker(adults$hours_per_week))
boxplot(adults$hours_per_week)
adults <- subset(adults, hours_per_week <= calculate.max.whisker(adults$hours_per_week))
boxplot(adults$hours_per_week)

#Ahora el limite inferior
adults <- subset(adults, hours_per_week >= calculate.min.whisker(adults$hours_per_week))
boxplot(adults$hours_per_week)
adults <- subset(adults, hours_per_week >= calculate.min.whisker(adults$hours_per_week))

# Conseguimos eliminar outliers
boxplot(adults$hours_per_week)

# Descripción medidas descriptivas despues de la eliminacion de outliers
# Media: 40.06, Mediana: 40, Moda: 40 
mean(adults$hours_per_week)
summary(adults$hours_per_week)
mlv(adults$hours_per_week)

# varianza y desviacion estandar
var(adults$hours_per_week)
sd(adults$hours_per_week)


#########################################
###########  CUALITATIVAS   #############
#########################################

########################## EDUCATION ############################

#Vemos que lo mas predominante es gente con educacion bachillerato (High School),
#gente con carrera grado(Bachelors) y gente con algún titulo universitario (Some-College)
pie(table(adults$education))

#En concreto:
# High-School --> 37.8%
# Alguna carrera --> 21.31%
# Grados --> 14.9%
table(adults$education)/nrow(adults) * 100


########################## WORKCLASS ############################

#Vemos que el sector privado es con diferencia el mas abundante
pie(table(adults$workclass))

#En concreto, agrupa el 77% de los datos
#Los demas valores, tienen porcentajes similares entre 1.76% y 7.5%
#y uno muy reducido "without-pay" que no llega ni al 1%
table(adults$workclass)/nrow(adults) * 100

########################## MARITAL-STATUS ############################

# vemos que predomina Casado por lo civil y nunca casado, y despues divorciado
pie(table(adults$marital_status))

# En concreto:
#   Casado por lo civil --> agrupa un 44.8% del total de los datos
#   Nunca casado --> agrupa un 32% del total de los datos
#   Divorciado --> agrupa un 15.6% del total de los datos
table(adults$marital_status)/nrow(adults) * 100


######################### OCCUPATION #########################

# vemos que con el sector circular no lo distinguimos muy bien
pie(table(adults$occupation))

# Con un barplot obtenermos que los 2 mas representativos son:
#   Craft-repair (reparaciones)
#   Adm-Clerical (Administrativo eclesiastico)
barplot(sort(table(adults$occupation)), horiz = TRUE, cex.names=0.5, las=1)

# En concreto:
#   Craft-repair --> agrupa un 16.6% del total de los datos
#   Adm-Clerical --> agrupa un 15.3% del total de los datos
table(adults$occupation)/nrow(adults) * 100


########################## RELATIONSHIP ############################

# vemos que predomina Husband y not-in-Family con respecto a los demás valores
pie(table(adults$relationship))

# En concreto:
#   Husband agrupa un 39.8% del total de los datos
#   Not-in-Family agrupa un 26.3% del total de los datos
table(adults$relationship)/nrow(adults) * 100


########################## RACE ############################

# Vemos que predomina "White" con respecto a los demás valores
pie(table(adults$race))

# En concreto, "White" agrupa un 83% del total de los datos
table(adults$race)/nrow(adults) * 100


########################## SEX ############################

# Vemos que hay mas hombres que mujeres en la muestra
pie(table(adults$sex))

# En concreto, 66.4% frente a un 33.6%
table(adults$sex)/nrow(adults) * 100

########################## NATIVE-COUNTRY ############################

# Vemos que la mayoria es nativa de USA
pie(table(adults$native_country))

# En concreto, el 90.5% ocupa USA 
table(adults$native_country)/nrow(adults) * 100


#########################################
#######    MODELO DE REGRESION   ########
#########################################







