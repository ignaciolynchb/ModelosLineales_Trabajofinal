###########  Carga de librerias ########### 
library(corrplot)
library(ggplot2)
library(readxl)
library(tidyverse)
library(mixlm)
library(skedastic) #Para hacer el test de homoscedasticidad
library(gridExtra)
########### Carga de datos ########### 
datos <- read_excel('datos_edv.xlsx')
continente <- read_excel('Pais-Continente.xlsx')
datos <- left_join(datos, continente, by = join_by(country == pais))

###########  Transformaciones ########### 
datos$status <- as.factor(datos$status)
levels(datos$status)
datos$continente <- as.factor(datos$continente)
levels(datos$continente)

#Scatterplot de todos vs life expectancy
scatterplot_continente <- function(x, y) {
  ggplot(datos, aes_string(x = x, y = y)) +
    geom_point(aes(col = continente))
}
scatterplot <- function(x, y) {
  ggplot(datos, aes_string(x = x, y = y)) +
    geom_point()
}
# crea una matriz de gráficos de 4 filas y 3 columnas
grid.arrange(
  scatterplot_continente("adult_mortality", "life_expectancy"),
  scatterplot_continente("infant_mortality", "life_expectancy"),
  scatterplot_continente("bmi", "life_expectancy"),
  scatterplot_continente("total_expenditure", "life_expectancy"),
  scatterplot_continente("GDP", "life_expectancy"),
  scatterplot_continente("Population", "life_expectancy"),
  scatterplot_continente("HDI", "life_expectancy"),
  scatterplot_continente("Schooling", "life_expectancy"),
  nrow = 4, ncol = 2
)


########### Descripción ########### 
nrow(datos) #filas
ncol(datos) #columnas
summary(datos) #estadisticas basicas
apply(X = 100*(is.na(datos)/nrow(datos)), MARGIN = 2, FUN = sum) #proporcion de NA's
  #22% de los paises no tienen el dato de población, 15% el de GDP
nrow(na.omit(datos)) #Registros completos, sin NA en ninguna variable

#Correlación
corr <- data.frame(cor(na.omit(datos)[,(3:11)])) 
corr_matrix <- cor(as.matrix(na.omit(datos)[,(3:11)]))
corrplot(corr_matrix)
#Baja correlacion con population (variable con mayor proporción de NA's)
#Alta correlacion con HDI (razonable por la formula de cálculo: https://psicologiaymente.com/cultura/indice-desarrollo-humano)
  #Se excluye por potenciales problemas
  #Hay alguna correlacion alta entre los regresores -> evaluar

datos <- datos |> select(!c(country,HDI,Population))

#De momento borramos los NA de GDP y Schooling (Ver que hacer despues)
datos <- datos |> filter(!is.na(GDP))
datos <- datos |> filter(!is.na(Schooling))


########### Modelado ########### 

modelo0 <- lm(life_expectancy ~ ., datos)

#Modelo con todas las combinaciones, se podria ver de usar con LASSO.
#modelo0 <- lm(life_expectancy ~ .^2, datos)

#Buscando el modelo stepwise
modStepwise <- stepWise(modelo0, alpha.enter = 0.05, alpha.remove=0.06)
#Buscando el modelo backward
modBackward <- backward(modelo0, alpha = 0.05)
#Buscando el modelo forward
modForward <- forward(modelo0, alpha = 0.05)

# Hacemos a mano la selección del modelo.
modelo1 = lm(formula = life_expectancy ~ ., data = datos) 
summary(modelo1) #Resumen del modelo


#Comenzamos quitando del modelo a las variables menos significativas individualmente

#1. Population
modelo2 = lm(formula = life_expectancy ~ ., data = datos[,-8]) 
summary(modelo2) #Resumen del modelo

#2. BMI
modelo3 = lm(formula = life_expectancy ~ ., data = datos[,-c(8,5)]) 
summary(modelo3) #Resumen del modelo

#3. infant_mortality
modelo4 = lm(formula = life_expectancy ~ ., data = datos[,-c(8,5,4)]) 
summary(modelo4) #Resumen del modelo

#Va aumentando el R2 ajustado


########### Diagnostico ###########
modelo_final = modelo4


#Esperanza de vida vs Valores ajustados
plot(na.omit(datos[,-c(8,5,4)])$life_expectancy, 
     modelo_final$fitted.values, 
     ylab = "Valores ajustados",
     xlab = "life_expectancy",
     main = "Valores ajustados vs life_expectancy",
     pch = 16, 
     col = "blue")
     abline(0, 1 , col = "red")

#Residuos: scatter plot
plot(modelo_final$residuals, 
     ylab = "Residuos",
     xlab = "",
     main = "Residuos",
     pch = 16, 
     col = "blue")

#Residuos: Boxplot
boxplot(modelo_final$residuals,
        main = "Residuos: Boxplot")

#Residuos vs variable explicativa
datos$r_i <- rstudent(modelo_final) #Residuos estandarizados

grid.arrange(
  scatterplot("adult_mortality", "r_i"),
  scatterplot("total_expenditure", "r_i"),
  scatterplot("GDP", "r_i"),
  scatterplot("Schooling", "r_i"),
  nrow = 2, ncol = 2
)


#Test de breush pagan para testear homoscedasticidad
breusch_pagan(modelo_final)


ggplot(datos, aes(x = edad_meses, y = t_i)) + 
  geom_point() +
  xlab('Edad (meses)') +
  ylab('Residuos') +
  geom_abline(slope = 0, intercept = 0)

#Residuos: Densidad
eje <- seq(-4,4,0.01)
plot(eje, 
     dnorm(eje, mean=0, sd=1), 
     col = 'red', 
     main = 'Residuos: Densidad',
     ylab = 'Densidad'
     )
lines(density(modelo_final$residuals), col = 'blue')

#Residuos: QQ Plot
ggplot(modelo_final, aes(sample = rstudent(modelo_final))) +
  geom_qq() +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  xlab("Quantiles teoricos Norm") +
  ylab("Quantiles Observados") +
  ggtitle("QQ Plot") +
  theme_bw()


#Residuos: Test de normalidad
shapiro.test(modelo_final$residuals)
tseries::jarque.bera.test(modelo_final$residuals)
ks.test(modelo_final$residuals, 'pnorm')

#Atipicos/Influyentes

#Viendo cuales son atipicas
atipicas <- c()
for(i in 1:nrow(datos)){
  if((1 - pt(abs(datos[i,]$r_i), nrow(datos) - 8 - 1)) < 0.01/2){
    atipicas <- c(atipicas, i)
  }
}

#Hay muchisimas atipicas, 10% lo son.
atipicas

# medidas de influencia
h_i <- influence(modelo_final)$hat
D_i <- cooks.distance(modelo_final)
df <- data.frame(i = 1:nrow(datos),
                 h_i = h_i,
                 D_i = D_i)


# leverage
ggplot(df, aes(x = i, y = h_i)) +
  geom_point() +
  geom_segment(aes(x = i, xend = i, y = 0, yend = h_i)) +
  xlab('') +
  ylab(expression(h[i])) +
  geom_abline(slope = 0, intercept = 2*8/nrow(datos), col = 2, linetype = 'dashed')

# Distancia de Cook
ggplot(df, aes(x = i, y = D_i)) +
  geom_point() +
  geom_segment(aes(x = i, xend = i, y = 0, yend = D_i)) +
  xlab('') +
  ylab(expression(D[i])) +
  geom_abline(slope = 0, intercept = 4/nrow(datos), col = 2, linetype = 'dashed')

#Saco las atipicas ya que son influyentes
datos <- datos[-c(45, 55), ]
modelo_final = lm(formula = life_expectancy ~ ., data = datos[,-c(8,5,4)]) 
breusch_pagan(modelo_final)
