###########  Carga de librerias ########### 
library(corrplot)
library(ggplot2)
library(readxl)
library(tidyverse)

########### Carga de datos ########### 
setwd("C:/Users/Nacho/Desktop/Modelos Lineales/Trabajo final")
datos <- read_excel('datos_edv.xlsx')
continente <- read_excel('Pais-Continente.xlsx')
datos <- left_join(datos, continente, by = join_by(country == pais))

###########  Transformaciones ########### 
datos$status <- as.factor(datos$status)
levels(datos$status)
datos$continente <- as.factor(datos$continente)
levels(datos$continente)

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
View(corr)
dev.off()
corrplot(corr_matrix)
#Baja correlacion con population (variable con mayor proporción de NA's)
#Alta correlacion con HDI (razonable por la formula de cálculo: https://psicologiaymente.com/cultura/indice-desarrollo-humano)
  #Se excluye por potenciales problemas
  #Hay alguna correlacion alta entre los regresores -> evaluar
datos <- datos[,-10] #HDI
datos <- datos[,-1] #country

########### Modelado ########### 
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