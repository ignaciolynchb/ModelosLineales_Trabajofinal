###########  Carga de librerias ########### 
library(corrplot)
library(ggplot2)
library(readxl)
library(tidyverse)
library(mixlm) #Para hacer backward, forward y stepwise
library(skedastic) #Para hacer el test de homoscedasticidad
library(gridExtra)
library(car) #Para hacer el vif

########### Carga de datos ########### 
datos <- read_excel('datos_edv.xlsx') #Leemos los datos
continente <- read_excel('Pais-Continente.xlsx') #Leemos los continentes
datos <- left_join(datos, continente, by = join_by(country == pais)) #Juntamos datos y continentes

datos$status <- as.factor(datos$status) #Pasamos el status a factor
levels(datos$status)
datos$continente <- as.factor(datos$continente) #Pasamos el continente a factor
levels(datos$continente)

################################ 


########### Descripción y visualizacion ########### 
nrow(datos) #filas
ncol(datos) #columnas
summary(datos) #estadisticas basicas

##Estudiamos cantidad de NA
apply(X = 100*(is.na(datos)/nrow(datos)), MARGIN = 2, FUN = sum) #proporcion de NA's
nrow(na.omit(datos)) #Registros completos, sin NA en ninguna variable
##Conclusiones: 22% de los paises no tienen el dato de población, 15% el de GDP. Poblacion no importa porque la sacamos

scatter_plot_continente <- function(x, y, df) {
  ggplot(df, aes_string(x = x, y = y)) +
    geom_point(aes(col = continente)) 
}

scatter_plot <- function(x, y) {
  ggplot(datos, aes_string(x = x, y = y)) +
    geom_point()
}

### Visualizacion
# crea una matriz de gráficos de 4 filas y 3 columnas
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
#Conclusion: Hay datos raros en adult_mortality, paises con muy baja mortalidad adulta tienen baja
# expectativa de vida. Comprobando con otras fuentes llegamos a la conclusion que estos paises con baja mortalidad
#Estaban mal medidos (Estarian muertes cada 100 en vez de muertes cada mil). Comprobamos con datos de world bank

#Selecciono la patita que estaba colgando y le multiplico por 10 la expectativa de vida
datos <- datos |> 
  mutate(adult_mortality = ifelse(adult_mortality < 50 & life_expectancy < 70, adult_mortality*10, adult_mortality))

datos <- datos |> 
  mutate(adult_mortality = ifelse(adult_mortality < 25, adult_mortality*10, adult_mortality))


#Correlación
corr <- data.frame(cor(na.omit(datos)[,(3:11)])) 
corr_matrix <- cor(as.matrix(na.omit(datos)[,(3:11)]))
corrplot(corr_matrix)
#Baja correlacion con population (variable con mayor proporción de NA's)
#Alta correlacion con HDI (razonable por la formula de cálculo: https://psicologiaymente.com/cultura/indice-desarrollo-humano)
#Alta correlacion con mortalidad adulta arreglada.
#Se excluye por potenciales problemas
#Hay alguna correlacion alta entre los regresores -> evaluar


#Por los motivos expuestoas sacamos poblacion y HDI
datos <- datos |> select(!c(country,HDI,Population))

#Borramos los NA de GDP y Schooling/
datos <- datos |> filter(!is.na(GDP))
datos <- datos |> filter(!is.na(Schooling))

datos <- datos |> mutate(GDP = log(GDP))
datos <- datos |> mutate(infant_mortality = log(1.5 + infant_mortality))


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


########### Diagnostico ###########
modelo_final = modBackward

modelo_alternativo = lm(life_expectancy ~ 
                    adult_mortality +
                    infant_mortality + 
                    continente + 
                    Schooling, 
                  data = datos)

#VIF para testear linealidad
vif(modelo_final)
#Conclusion, como todos los GVIF dicen < 5 

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
breusch_pagan(modelo_alternativo)

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
                    adult_mortality +
                    infant_mortality +
                    continente + 
                    Schooling,
                  data = datos_sin_influyentes)
