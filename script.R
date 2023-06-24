########### Carga de librerias ###########
library(corrplot)
library(ggplot2)
library(readxl)
library(tidyverse)
library(mixlm) #Para hacer backward, forward y stepwise
library(skedastic) #Para hacer el test de homoscedasticidad
library(gridExtra)
library(car) #Para hacer el vif

########### Creción de funciones ###########
scatter_plot_continente <- function(x, y, df) {
  ggplot(df, aes_string(x = x, y = y)) +
    geom_point(aes(col = continente)) +
    theme_bw()
}

scatter_plot <- function(x, y) {
  ggplot(datos, aes_string(x = x, y = y)) +
    geom_point() +
    theme_bw()
}

########### Carga y limpieza de datos ########### 
datos <- read_excel('datos_edv.xlsx') #Carga de datos
continente <- read_excel('Pais-Continente.xlsx') #Carga de un dataset de paises por continentes
datos <- left_join(datos, continente, by = join_by(country == pais)) #Juntamos datos y continentes
datos$status <- as.factor(datos$status) #Pasamos el status a factor
levels(datos$status) #"Developed", "Developing"
datos$continente <- as.factor(datos$continente) #Pasamos el continente a factor
levels(datos$continente) #"Africa"   "Americas" "Asia"     "Europe"   "Oceania" 

########### Descripción y visualizacion ########### 
nrow(datos) #filas
ncol(datos) #columnas
summary(datos) #estadisticas basicas

#Estudiamos cantidad de NA
apply(X = 100*(is.na(datos)/nrow(datos)), MARGIN = 2, FUN = sum) #proporcion de NA's
nrow(na.omit(datos)) #Registros completos, sin NA en ninguna variable
#Conclusiones: 22% de los paises no tienen el dato de población, 15% el de GDP

grid.arrange(
  scatter_plot_continente("adult_mortality", "life_expectancy", datos),
  scatter_plot_continente("infant_mortality", "life_expectancy", datos),
  scatter_plot_continente("bmi", "life_expectancy", datos),
  scatter_plot_continente("total_expenditure", "life_expectancy", datos),
  scatter_plot_continente("HDI", "life_expectancy", datos),
  scatter_plot_continente("Population", "life_expectancy", datos),
  scatter_plot_continente("GDP", "life_expectancy", datos),
  scatter_plot_continente("Schooling", "life_expectancy", datos),
  nrow = 4, ncol = 2
)
#Conclusiones: 
# 1) Hay datos raros en adult_mortality, paises con muy baja mortalidad adulta tienen baja
# expectativa de vida. Comprobando con otras fuentes llegamos a la conclusion que estos 
# paises con baja mortalidad estaban mal medidos (estarian muertes cada 100 en vez de 
# muertes cada mil). Comprobamos con datos de world bank

#Selecciono la patita que estaba colgando y le multiplico por 10 la expectativa de vida
datos <- datos |> 
  mutate(adult_mortality = ifelse(adult_mortality < 50 & life_expectancy < 70, adult_mortality*10, adult_mortality))

datos <- datos |> 
  mutate(adult_mortality = ifelse(adult_mortality < 25, adult_mortality*10, adult_mortality))

#Correlación
corr <- data.frame(cor(na.omit(datos)[,(3:11)])) 
corr_matrix <- cor(as.matrix(na.omit(datos)[,(3:11)]))
corrplot(corr_matrix)
#Baja correlacion con population (variable con mayor proporción de NA's) -> Justificación para sacarla
#Alta correlacion con HDI (razonable por la formula de cálculo: https://psicologiaymente.com/cultura/indice-desarrollo-humano) -> Se excluye por potenciales problemas
#Alta correlacion con mortalidad adulta "arreglada"

#Por los motivos expuestos sacamos Population y HDI
datos <- datos[, -c(9,10)] #HDI, Population

#Borramos los registros que tienen NA en GDP y Schooling
datos <- datos |> filter(!is.na(GDP))
datos <- datos |> filter(!is.na(Schooling))

datos <- datos |> mutate(GDP = log(GDP))
datos <- datos |> mutate(infant_mortality = log(1+infant_mortality))

nombre_fila <- datos$country
datos <- datos[, -c(1)] #country
row.names(datos) <- nombre_fila #Para saber a qué país representa la fila

########### Modelado ###########

#Seleccionamos el modelo con las tecnicas vistas en el curso: stepwise, bacjward y forward.
#Hacemos estas tecnicas con las funciones del paquete mixlm (lo hace con pruebas de hipotesis)

modelo0 <- lm(life_expectancy ~ ., datos)

#Buscando el modelo stepwise
modStepwise <- stepWise(modelo0, alpha.enter = 0.05, alpha.remove=0.06)
#Buscando el modelo backward
modBackward <- backward(modelo0, alpha = 0.05)
#Buscando el modelo forward
modForward <- forward(modelo0, alpha = 0.05)

#Los tres métodos coinciden
modelo_final = modBackward
summary(modBackward)

########### Diagnostico ###########

#Linealidad
vif(modelo_final)
#Conclusion: GVIF < 5 

#Esperanza de vida vs Valores ajustados
plot(na.omit(datos[,c(2,3,8,9)])$life_expectancy, 
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

#Muchos valores atípicos

#Residuos vs variable explicativa
r_i <- rstudent(modelo_final) #Residuos estandarizados
grid.arrange(
  scatter_plot_continente("adult_mortality", "r_i", datos),
  scatter_plot_continente("Schooling", "r_i", datos),
  nrow = 1, ncol = 2
)

#Test de breush_pagan para testear homoscedasticidad
breusch_pagan(modelo_final)
#Problemas de heteroscedasticidad

#Normailidad
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
#Se rechaza normalidad con todos los test

#PROBLEMAS: heteroscedasticidad, no normalidad
#POSIBLE SOLUCIÓN A RAÍZ DE DIAGNOSTICO: atípicos

#Atipicos influyentes

#Atipicos
atipicas <- c()
for(i in 1:nrow(datos)){
  if((1 - pt(abs(r_i[i]), nrow(datos) - 9 - 1)) < 0.01/2){
    atipicas <- c(atipicas, i)
  }
}
atipicas

#Medidas de influencia
h_i <- influence(modelo_final)$hat
D_i <- cooks.distance(modelo_final)
df <- data.frame(i = 1:nrow(datos),
                 h_i = h_i,
                 D_i = D_i)
influyentes_leverage <- which(if_else(h_i > 2*(length(coefficients(modelo_final))-1)/nrow(datos), 1, 0) == 1)
row.names(datos)[influyentes_leverage] #Paises influyentes
influyentes_cook <- which(if_else(D_i > 4/nrow(datos), 1, 0) == 1)
row.names(datos)[influyentes_cook] #Paises influyentes

#Leverage
ggplot(df, aes(x = i, y = h_i)) +
  geom_point() +
  geom_segment(aes(x = i, xend = i, y = 0, yend = h_i)) +
  xlab('') +
  ylab(expression(h[i])) +
  geom_abline(slope = 0, intercept = 2*(length(coefficients(modelo_final))-1)/nrow(datos), col = 2, linetype = 'dashed') +
  theme_bw()

#Distancia de Cook
ggplot(df, aes(x = i, y = D_i)) +
  geom_point() +
  geom_segment(aes(x = i, xend = i, y = 0, yend = D_i)) +
  xlab('') +
  ylab(expression(D[i])) +
  geom_abline(slope = 0, intercept = 4/nrow(datos), col = 2, linetype = 'dashed') +
  theme_bw()

#Excluimos los influyentes de Cook
datos_sin_influyentes <- datos[-c(influyentes_cook),]
modelo_final_sin_influyentes = lm(life_expectancy ~ 
                                  adult_mortality +
                                  continente + 
                                  Schooling,
                                  data = datos_sin_influyentes)
summary(modelo_final_sin_influyentes)
#Mejora el R2

########### Diagnostico Modelo sin influyentes ###########

#Linealidad
vif(modelo_final_sin_influyentes)
#Conclusion: GVIF < 5 

#Esperanza de vida vs Valores ajustados
plot(na.omit(datos_sin_influyentes[,c(2,3,8,9)])$life_expectancy, 
     modelo_final_sin_influyentes$fitted.values, 
     ylab = "Valores ajustados",
     xlab = "life_expectancy",
     main = "Valores ajustados vs life_expectancy",
     pch = 16, 
     col = "blue")
abline(0, 1 , col = "red")

#Residuos: scatter plot
plot(modelo_final_sin_influyentes$residuals, 
     ylab = "Residuos",
     xlab = "",
     main = "Residuos",
     pch = 16, 
     col = "blue")

#Residuos: Boxplot
boxplot(modelo_final_sin_influyentes$residuals,
        main = "Residuos: Boxplot")
#Se reducen valores atípicos

#Residuos vs variable explicativa
r_i <- rstudent(modelo_final_sin_influyentes) #Residuos estandarizados
grid.arrange(
  scatter_plot_continente("adult_mortality", "r_i", datos_sin_influyentes),
  scatter_plot_continente("Schooling", "r_i", datos_sin_influyentes),
  nrow = 1, ncol = 2
)

#Test de breush_pagan para testear homoscedasticidad
breusch_pagan(modelo_final_sin_influyentes)
#No se rechaza homoscedasticidad

#Normalidad
#Residuos: Densidad
eje <- seq(-4,4,0.01)
plot(eje, 
     dnorm(eje, mean=0, sd=1), 
     col = 'red', 
     main = 'Residuos: Densidad',
     ylab = 'Densidad'
)
lines(density(modelo_final_sin_influyentes$residuals), col = 'blue')

#Residuos: QQ Plot
ggplot(modelo_final_sin_influyentes, aes(sample = rstudent(modelo_final_sin_influyentes))) +
  geom_qq() +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  xlab("Quantiles teoricos Norm") +
  ylab("Quantiles Observados") +
  ggtitle("QQ Plot") +
  theme_bw()

#Residuos: Test de normalidad
shapiro.test(modelo_final_sin_influyentes$residuals)
tseries::jarque.bera.test(modelo_final_sin_influyentes$residuals)
ks.test(modelo_final_sin_influyentes$residuals, 'pnorm')
#Se rechaza normalidad con 2 de los 3 test