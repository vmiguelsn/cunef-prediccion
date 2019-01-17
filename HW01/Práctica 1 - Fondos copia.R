##Práctica 1 de Predicción - FONDOS

library(leaps)
library(car)


datosfondos <- read.csv('Fondos.csv', sep = ';', dec = ',')   #Cargamos los datos del enunciado. Se observa que hay mucho NA.
View(datosfondos)

datosfiltrados <- datosfondos[, c(-1, -2, -5, -6, -18)]       #Para trabajar correctamente, eliminamos variables que no nos aportan valor.
View(datosfiltrados)

Y <- datosfondos$rent_1                                        #Creamos la variable dependiente
mX <- cbind(1, datosfondos[,10:16])                            #Creamos una matriz sin las ultimas columnas (no me acuerdo porqué)
head(Y)                                                      # Muestro el vector Y
head.matrix(mX)


#Modelo de regresión hecho con todas las variables menos alguna que tiene muchos NAs.
#Como se observa en las siguientes regresiones, da igual calcularla con datosfondos que con datosfiltrados, pues se pueden elegir las variables que se usan o se puede poner ~. para coger todas.


#Vamos a probar a hacer los modelos lineales con todas las variables para ver su nivel de significación o "estrellitas"
#Hemos eliminado las variables que son de tipo string o incluyen un porcentaje grande de NA. Elegimos depende de que modelo que variables cogemos para ver como se ajustan.


regres01 <- lm(Y ~ rent_1_mes + rent_3_meses + rent_6_meses + rent_en_el_anio + Volatilidad_3, datosfondos)
summary(regres01)  #Tras ver este summary, eliminamos algunas variables que tienen una Pr muy alta (menos estrellas)

regres02 <- lm(Y ~ ., datosfiltrados)
summary(regres02) #Tras ver este summary, eliminamos algunas variables que tienen una Pr muy alta (menos estrellas)

regres03 <- lm(Y ~ rent_6_meses + rent_en_el_anio, datosfondos)
summary(regres03) #Tras ver este summary, eliminamos algunas variables que tienen una Pr muy alta (menos estrellas)


#Modelos de selección de variables
AIC(regres03, regres02)  
BIC(regres03, regres02)    


#Ahora realizamos las predicciones de todos los modelos de regresión lineal

prediccion1 <- predict(regres01, datosfondos)
sum(is.na(prediccion1))

prediccion2 <- predict(regres02, datosfiltrados)
sum(is.na(prediccion2))

prediccion3 <- predict(regres03, datosfondos)
sum(is.na(prediccion3))


#Comparaciones entre las rentabilidades y las predicciones anteriores. Como vemos, se ajustan, pero tienen error considerable.

comparacion1 <- cbind(datosfondos$rent_1, prediccion1)
View(comparacion1)
crPlots(regres01)

comparacion2 <- cbind(datosfondos$rent_1, prediccion2)
View(comparacion2)
crPlots(regres02)

comparacion3 <- cbind(datosfondos$rent_1, prediccion3)
View(comparacion3)
crPlots(regres03)


#Modelos de selección de variables
AIC(regres03, regres02)  
BIC(regres03, regres02)    


#Modelo de selección Best Subset
regfit.full = regsubsets(Y~.,datosfiltrados )
reg.summary = summary(regfit.full)
head(reg.summary)

reg.summary$rss

reg.summary$cp

reg.summary$aic

reg.summary$bic


#Visualización de los datos
qqPlot(regres01, labels=row.names(datos), id.method="identify",
       simulate=TRUE, main="Q-Q Plot")

qqPlot(regres02, labels=row.names(datos), id.method="identify",
       simulate=TRUE, main="Q-Q Plot")

qqPlot(regres03, labels=row.names(datos), id.method="identify",
       simulate=TRUE, main="Q-Q Plot")

