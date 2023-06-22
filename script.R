###########  Carga de librerias ########### 
library(corrplot)
library(ggplot2)
library(readxl)
library(tidyverse)
library(mixlm)
library(skedastic) #Para hacer el test de homoscedasticidad
library(gridExtra)
library(fastDummies)

########### Carga de datos ########### 
datos <- read_excel('datos_edv.xlsx')
continente <- read_excel('Pais-Continente.xlsx')
datos <- left_join(datos, continente, by = join_by(country == pais))

###########  Transformaciones ########### 
datos$status <- as.factor(datos$status)
levels(datos$status)
datos$continente <- as.factor(datos$continente)
levels(datos$continente)


scatter_plot_continente <- function(x, y, df) {
  ggplot(df, aes_string(x = x, y = y)) +
  geom_point(aes(col = continente)) 
}
#scatter_plot de todos vs life expectancy
#con nombre de paises
#scatter_plot_continente <- function(x, y, df) {
 # ggplot(df, aes_string(x = x, y = y)) +
  #  geom_point(aes(col = continente)) +
   # geom_text(aes(label = country), nudge_x = 0.5, nudge_y = 0.2)
#}
scatter_plot <- function(x, y) {
  ggplot(datos, aes_string(x = x, y = y)) +
    geom_point()
}

# crea una matriz de gráficos de 4 filas y 3 columnas
grid.arrange(
  scatter_plot_continente("adult_mortality", "life_expectancy", datos),
  scatter_plot_continente("infant_mortality", "life_expectancy", datos),
  scatter_plot_continente("bmi", "life_expectancy", datos),
  scatter_plot_continente("total_expenditure", "life_expectancy", datos),
  scatter_plot_continente("GDP", "life_expectancy", datos),
  scatter_plot_continente("Population", "life_expectancy", datos),
  scatter_plot_continente("HDI", "life_expectancy", datos),
  scatter_plot_continente("Schooling", "life_expectancy", datos),
  nrow = 4, ncol = 2
)

datos <- datos |> 
  mutate(adult_mortality = ifelse(adult_mortality < 50 & life_expectancy < 70, adult_mortality*10, adult_mortality))

datos <- datos |> 
  mutate(adult_mortality = ifelse(adult_mortality < 25, adult_mortality*10, adult_mortality))



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
datos <- datos |> mutate(GDP = log(GDP))
modelo0 <- lm(life_expectancy ~ ., datos)

#Modelo con todas las combinaciones, se podria ver de usar con LASSO.
#modelo0 <- lm(life_expectancy ~ .^2, datos)

#Buscando el modelo stepwise
modStepwise <- stepWise(modelo0, alpha.enter = 0.05, alpha.remove=0.06)
#Buscando el modelo backward
modBackward <- backward(modelo0, alpha = 0.1)
#Buscando el modelo forward
modForward <- forward(modelo0, alpha = 0.05)

modBackward$coefficients




########### Diagnostico ###########
modelo_final = modBackward

#VIF para testear linealidad
vif(modBackward)




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
r_i <- rstudent(modelo_final) #Residuos estandarizados

grid.arrange(
  scatter_plot_continente("adult_mortality", "r_i", datos),
  scatter_plot_continente("total_expenditure", "r_i", datos),
  scatter_plot_continente("GDP", "r_i", datos),
  scatter_plot_continente("Schooling", "r_i", datos),
  nrow = 2, ncol = 2
)

#Test de breush pagan para testear homoscedasticidad
breusch_pagan(modelo_final)

##Normailidad

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
  if((1 - pt(abs(r_i[i]), nrow(datos) - 9 - 1)) < 0.01/2){
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

influyentes_leverage <- which(if_else(h_i > 2*9/nrow(datos), 1, 0) == 1)
influyentes_cook <- which(if_else(D_i > 4/nrow(datos), 1, 0) == 1)

# leverage
ggplot(df, aes(x = i, y = h_i)) +
  geom_point() +
  geom_segment(aes(x = i, xend = i, y = 0, yend = h_i)) +
  xlab('') +
  ylab(expression(h[i])) +
  geom_abline(slope = 0, intercept = 2*9/nrow(datos), col = 2, linetype = 'dashed')

# Distancia de Cook
ggplot(df, aes(x = i, y = D_i)) +
  geom_point() +
  geom_segment(aes(x = i, xend = i, y = 0, yend = D_i)) +
  xlab('') +
  ylab(expression(D[i])) +
  geom_abline(slope = 0, intercept = 4/nrow(datos), col = 2, linetype = 'dashed')

datos_sin_influyentes <- datos[-c(8,13),]

modelo_final = lm(life_expectancy ~ 
                    log(adult_mortality) +
                    GDP + 
                    continente + 
                    total_expenditure + 
                    Schooling +
                    status, 
                  data = datos_sin_influyentes)
