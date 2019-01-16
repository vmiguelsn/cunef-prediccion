# En esta práctica se hará la predicción de ventas de Apple usando ETS y Arima.
# Se realizarán las predicciones de manera agregada y desagregada.


library(magrittr)
library(dplyr)
require(forecast)
require(xts)
require(ggplot2)
library(ggfortify)

ruta<-"apple.csv"

ventas<-read.csv(ruta,sep=",")

ventas[is.na(ventas)] <- 0            #Cuando no hay ventas ponemos 0, asumiendo que los distintos aparatos aun no estan a la venta

for (i in 1:nrow(ventas)){             #Suma de todas las columnas
  ventas["Total"]<- ventas[,3]+ventas[,4]+ventas[,5]+ventas[,6]
}

VentasApple <- ventas
View(VentasApple)

##### En esta primera parte vamos a obtener las ventas totales por trimestre. #####

rawVentas <- VentasApple$Total
rawDate <- seq(as.Date("1998/10/01"), as.Date("2016/03/31"), by = "quarter")

xVentas <- xts(rawVentas, order.by=rawDate)

xVentas <- to.quarterly(xVentas)

TotalVentas = as.zoo(xVentas$xVentas.Close)

names(TotalVentas) <- 'Ingresos Totales'
View(TotalVentas)


##### En esta segunda parte parte vamos a obtener las ventas de cada item por trimestre. (Descartamos el iPod) #####

##### Ventas del iPhone #####

iPhoneVentas <- VentasApple$iPhone
rawDate2 <- seq(as.Date("1998/10/01"), as.Date("2016/03/31"), by = "quarter")

iPhoneVentas <- xts(iPhoneVentas, order.by=rawDate2)

iPhoneVentas <- to.quarterly(iPhoneVentas)

iPhoneVentas = as.zoo(iPhoneVentas$iPhoneVentas.Close)

names(iPhoneVentas) <- 'Ingresos iPhone'
View(iPhoneVentas)

##### Ventas del iPad #####

iPadVentas <- VentasApple$iPad
rawDate3 <- seq(as.Date("1998/10/01"), as.Date("2016/03/31"), by = "quarter")

iPadVentas <- xts(iPadVentas, order.by=rawDate3)

iPadVentas <- to.quarterly(iPadVentas)

iPadVentas = as.zoo(iPadVentas$iPadVentas.Close)

names(iPadVentas) <- 'Ingresos iPad'
View(iPadVentas)

##### Ventas del iPad #####

MacVentas <- VentasApple$Mac
rawDate4 <- seq(as.Date("1998/10/01"), as.Date("2016/03/31"), by = "quarter")

MacVentas <- xts(MacVentas, order.by=rawDate4)

MacVentas <- to.quarterly(MacVentas)

MacVentas = as.zoo(MacVentas$MacVentas.Close)

names(MacVentas) <- 'Ingresos Mac'
View(MacVentas)


##### Se procede a realizar el modelo ETS y el modelo ARIMA (Con la información agregada y con la información desagregada por productos) #####

##### Modelo ETS para todas las variables agregadas #####

##Plot Serie
autoplot(TotalVentas)+ggtitle("Ventas Trimestrales Apple")+xlab("Trimestres")+ylab("Ventas")
#Seasonal Plot
ggfreqplot(as.ts(TotalVentas),freq=4,nrow=1,facet.labeller=c("1T","2T","3T","4T"))+ggtitle("Ventas Trimestrales")

#Select number of observation to compare forecast
cOmit=4

#Data Size
nObs=length(TotalVentas)

#sub_sample
#oVentas=zVentas[1:(nObs-cOmit),]
oVentas <- window(TotalVentas,start=index(TotalVentas[1]),end=index(TotalVentas[nObs-cOmit]))

#Fit Simple Exponential Smoothing
fit1 <- ses(oVentas)

#Fit Holt
fit2 <- holt(oVentas)

#Fit Holt- exponential
fit3 <- holt(oVentas,exponential=TRUE)

#Fit Holt - damped
fit4 <- holt(oVentas,damped=TRUE)

#Fit Holt - (exponential+damped)
fit5 <- holt(oVentas,exponential=TRUE,damped=TRUE)

# Results for first model:
fit1$model

#Plot models fitted
plot(fit3, type="o", ylab="Ventas",  flwd=1, plot.conf=FALSE)
lines(window(zVentas),type="o")
lines(fit1$mean,col=2)
lines(fit2$mean,col=3)
lines(fit4$mean,col=5)
lines(fit5$mean,col=6)
legend("topleft", lty=1, pch=1, col=1:6,
       c("Data","SES","Holt's","Exponential",
         "Additive Damped","Multiplicative Damped"))


#seasonal model Holt-winters
fit6 <- hw(oVentas,seasonal="additive")
fit7 <- hw(oVentas,seasonal="multiplicative")

#Plot models
plot(fit7,ylab="Ventas",
     plot.conf=FALSE, type="o", fcol="white", xlab="Year")
lines(window(TotalVentas),type="o",col="blue")
lines(fitted(fit6), col="red", lty=2)
lines(fitted(fit7), col="green", lty=2)
lines(fit6$mean, type="o", col="red")
lines(fit7$mean, type="o", col="green")
legend("topleft",lty=1, pch=1, col=1:3, 
       c("data","Holt Winters' Additive","Holt Winters' Multiplicative"))


#Calculate Components
states <- cbind(fit6$model$states[,1:3],fit7$model$states[,1:3])
colnames(states) <- c("level","slope","seasonal","level","slope","seasonal")
plot(states, xlab="Year")
fit6$model$state[,1:3]
fitted(fit6)
fit6$mean


## Select automatic ETS
etsfit<-ets(oVentas)
#forecast model
fventas.ets=forecast(etsfit)
#Results
summary(fventas.ets)

#Plot
plot(fventas.ets)
lines(window(TotalVentas),type="o")

#Actual and Forecast
matrix(c(fventas.ets$mean[1:cOmit],TotalVentas[(nObs-cOmit+1):nObs]),ncol=2)


## Select automatic ETS
etsfit2<-ets(oVentas,damped=TRUE)
#forecast model
fventas.ets2=forecast(etsfit2)
#Results
summary(fventas.ets2)

#Plot
plot(fventas.ets2)
lines(window(TotalVentas),type="o")

#Actual and Forecast
matrix(c(fventas.ets2$mean[1:cOmit],fventas.ets$mean[1:cOmit],TotalVentas[(nObs-cOmit+1):nObs]),ncol=3)

#Plot all models
plot(fventas.ets2)
lines(window(TotalVentas),type="o")
lines(fventas.ets$mean,type="o",col="red")

#Prediccion en tiempo real (los datos mas recientes)
model2=ets(TotalVentas, damped=T)
f.model2=forecast(model2)
plot(f.model2)


##### Modelo ARIMA para todas las variables agregadas #####

df_new <- data.frame(value = as.vector(TotalVentas),
                     time = time(TotalVentas))

ggplot(df_new)+geom_point(aes(x=time,y=value))+geom_line(aes(x=time,y=value))+ylab("Ventas")+ggtitle("Ventas Trimestrales Apple")+xlab("Trimestres")


#Log transformation?
zlVentas=log(zVentas)
df_newl <- data.frame(value = as.vector(zlVentas),
                      time = time(zlVentas))
ggplot(df_newl)+geom_point(aes(x=time,y=value))+geom_line(aes(x=time,y=value))+ylab("Ventas")+ggtitle("Ventas Trimestrales LOG Apple")+xlab("Trimestres")


#Difference
ggtsdisplay(zlVentas)
ggtsdisplay(diff(zlVentas))
ggtsdisplay(diff(zlVentas,4))
ggtsdisplay(diff(diff(zlVentas,4),1))

#Select number of observation to compare forecast
cOmit=6

#Data Size
nObs=length(TotalVentas)

#sub_sample
oVentas <- window(TotalVentas,start=index(TotalVentas[1]),end=index(TotalVentas[nObs-cOmit]))

#out sample (real data to forecast performance)
pVentas <- window(TotalVentas,start=index(TotalVentas[nObs-cOmit+1]),end=index(TotalVentas[nObs]))


#ARIMA MODEL
fit1=auto.arima(oVentas,lambda=0)
summary(fit1)

#residual analysis
ggtsdisplay(fit1$residuals)

#box-Ljung Test
Box.test(fit1$residuals,lag=4, fitdf=3, type="Lj")
Box.test(fit1$residuals,lag=8, fitdf=3, type="Lj")
Box.test(fit1$residuals,lag=12, fitdf=3, type="Lj")

fventas.arima=forecast(fit1)

ggplot(df_new)+geom_point(aes(x=time,y=value))+geom_line(aes(x=time,y=value))+ geom_forecast(fventas.arima,alpha=0.4)+ggtitle("ARIMA: Predicción Apple")

fventas.arima

##### Modelos ETS y ARIMA para los productos de manera desagregada #####

##### iPhone #####

##### ETS #####

## Select automatic ETS
etsfit<-ets(iPhoneVentas)
#forecast model
fventas.ets=forecast(etsfit)
#Results
summary(fventas.ets)

#Plot
plot(fventas.ets)
lines(window(iPhoneVentas),type="o")

##### ARIMA #####

#ARIMA MODEL
fit1=auto.arima(iPhoneVentas,lambda=0)
summary(fit1)

fventas.arima=forecast(fit1)

ggplot(df_new)+geom_point(aes(x=time,y=value))+geom_line(aes(x=time,y=value))+ geom_forecast(fventas.arima,alpha=0.4)+ggtitle("ARIMA: Predicción iPhone")

##### iPad #####

##### ETS #####

## Select automatic ETS
etsfit<-ets(iPadVentas)
#forecast model
fventas.ets=forecast(etsfit)
#Results
summary(fventas.ets)

#Plot
plot(fventas.ets)
lines(window(iPadVentas),type="o")

##### ARIMA #####

#ARIMA MODEL
fit1=auto.arima(iPadVentas,lambda=0)
summary(fit1)

fventas.arima=forecast(fit1)

ggplot(df_new)+geom_point(aes(x=time,y=value))+geom_line(aes(x=time,y=value))+ geom_forecast(fventas.arima,alpha=0.4)+ggtitle("ARIMA: Predicción iPad")

##### Mac #####

##### ETS #####

## Select automatic ETS
etsfit<-ets(MacVentas)
#forecast model
fventas.ets=forecast(etsfit)
#Results
summary(fventas.ets)

#Plot
plot(fventas.ets)
lines(window(MacVentas),type="o")

##### ARIMA #####

#ARIMA MODEL
fit1=auto.arima(MacVentas,lambda=0)
summary(fit1)

fventas.arima=forecast(fit1)

ggplot(df_new)+geom_point(aes(x=time,y=value))+geom_line(aes(x=time,y=value))+ geom_forecast(fventas.arima,alpha=0.4)+ggtitle("ARIMA: Predicción Mac")

