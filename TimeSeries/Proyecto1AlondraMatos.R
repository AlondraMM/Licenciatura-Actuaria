#Premium

setwd("C:/Users/DELL/Desktop/SeriesTiempo/Unidad 4/scripts")

library(fpp2)
library(TSA) #eacf
library(readxl)
base<-as.data.frame(read_xlsx("Precios-Proyecto1.xlsx"))
base<-as.data.frame(Precios_Proyecto1)

# Gr?ficas para estacionariedad -----------------------------------------------------------------

#Gr?fica general
datos.ts<- ts(base[,3], start = c(2017,1), frequency = 365)

par(mfrow=c(1,1))
plot(datos.ts,col ="#AE017E",lwd=2,xlab="Tiempo", ylab="Precio promedio",main="Precio promedio diario de la Gasolina Premium")

#Gr?fica de la diferencia
plot(diff(datos.ts),xlab="Tiempo", col="#7A0177",ylab="Diff(Precio)",main="Serie diferenciada",lwd=1)
plot(diff(diff(datos.ts)),xlab="Tiempo", col="#7A0177",ylab="Diff(Precio)",main="Serie diferenciada",lwd=1)

#Gr?fica de la diferencia transformada
lambda_dif<-BoxCox.lambda(datos.ts, method = c( "guerrero"), lower = 0.1, upper = 10)
lambda_dif
plot((diff(BoxCox(datos.ts,lambda=lambda_dif))),col="#C994C7",xlab="Tiempo",ylab="Diff([Precio^Lambda-1]/Lambda)",main="Serie diferenciada de los datos transformados",lwd=1)

# Prueba de ra?ces unitarias ----------------------------------------------

library(urca)
df.test1<-ur.df(BoxCox(datos.ts,lambda=lambda_dif),type=c("trend"),selectlags = c("AIC"))
summary(df.test1)

df.test2<-ur.df(BoxCox(datos.ts,lambda=lambda_dif),type=c("drift"),selectlags = c("AIC"))
summary(df.test2)

df.test3<-ur.df(diff(BoxCox(datos.ts,lambda=lambda_dif)),type=c("trend"),selectlags = c("AIC"))
summary(df.test3)


#valores cr?ticos de la normal est?ndar
qnorm(c(0.1/2,0.05/2,0.01/2))

#valores cr?ticos de la t de Student
qt(0.01/2, df=(1605), lower.tail = FALSE)

#Grados de libertad: total de datos-menos el m?ximo orden de diferencia-# de coeficientes a estimar del AR
#length(datos.ts)-1-6
#Valor p: 2*pt(0.171,df=1605,lower.tail = F)

# Identificaci?n del modelo -----------------------------------------------

par(mfrow=c(1,2))
acf(as.vector(diff(BoxCox(datos.ts,lambda=lambda_dif))),main="Funci?n de autocorrelaci?n estimada")
pacf(as.vector(diff(BoxCox(datos.ts,lambda=lambda_dif))),main="Funci?n de autocorrelaci?n parcial estimada")
eacf(diff(BoxCox(datos.ts,lambda=lambda_dif))) 

modelo<-Arima(datos.ts,order=c(6,1,5),lambda=lambda_dif)
summary(modelo)

plot(modelo)

# Verificaci?n de supuestos -----------------------------------------------

residuos<-residuals(modelo)

par(mfrow=c(1,2))
acf(as.vector(residuos),main="Funci?n de autocorrelaci?n estimada")
pacf(as.vector(residuos),main="Funci?n de autocorrelaci?n parcial estimada")

#Ruido blanco
prueba<-Box.test(residuos,lag=20,type="Ljung-Box")
prueba

#normalidad
par(mfrow=c(1,2))
qqnorm((residuos-mean(residuos))/sqrt(var(residuos)),xlab="Cuantiles te?ricos", ylab="Cuantiles muestrales", main="Gr?fico de probabilidad normal",col="Green")
ks.test((residuos-mean(residuos))/sqrt(var(residuos)),"pnorm",0,1) #prueba de kolmogorov-smirnov


# Pron?sticos -------------------------------------------------------------
par(mfrow=c(1,1))

estimados<-fitted(modelo)
plot.ts(cbind(datos.ts,estimados),plot.type = "single",col=c("black","red"))

autoplot(datos.ts)+
  autolayer(modelo$fitted,series="Estimaci?n",lwd=1)+
  autolayer(datos.ts,series="Real",lwd=1)+
  ggtitle("Precio diario de la Gasolina Premium") + ylab("Precio diario")+
  xlab("Tiempo")+ theme(plot.title = element_text(hjust=0.5))

pronostico<-forecast(modelo,h = 5,level = 99)
pronostico


autoplot(datos.ts,series="Real",ts.colour="Black",lwd=1)+
  autolayer(pronostico,series="Pron?stico", PI=TRUE)+
  autolayer(modelo$fitted,series="Estimaci?n")+
  ggtitle("Precio diario de la Gasolina Premium") + ylab("Precio diario")+
  xlab("Tiempo")+ theme(plot.title = element_text(hjust=0.5))+
  guides(Colour=guide_legend(title="Series"))


df<-ts(base[1248:1612,3],start = c(2020,153), frequency = 365)
fit2<-ts(modelo$fitted[1248:1612],start = c(2020,153), frequency = 365)

autoplot(df)+
  autolayer(pronostico,series="Pron?stico", PI=TRUE)+
  autolayer(fit2,series="Estimaci?n")+
  ggtitle("Precio diario de la Gasolina Premium") + ylab("Precio diario")+
  xlab("Tiempo")+ theme(plot.title = element_text(hjust=0.5))+
  guides(Colour=guide_legend(title="Pron?sticos"))




