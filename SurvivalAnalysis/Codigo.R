#PROYECTO

setwd("C:/Users/Alondra Maus/Desktop/Analisis_Supervivencia/Unidad3/Scripts/Datos")
setwd("D:/Actuaría/7 semestre/Analisis_Supervivencia/Unidad3/Scripts/Datos")
# Librer?as ---------------------------------------------------------------
library(ggplot2) 
library(survival) 
library(coin) 
library(dplyr) 
library(ggfortify) 
library(survminer)
library(MASS)
library(graphics)
library(lattice)
library(foreign)
library(splines)
library(KMsurv)
library(stats)
library(mvtnorm)
library(modeltools)
library(stats4)
library(ggfortify)
library(survMisc)
library("flexsurv")

#Lecura de la base de datos
Telco<-read.csv("Telco-Customer-Churn.csv")
str(Telco)
base<-subset(Telco, select=-c(customerID))

### An?lisis previo de la muestra ------------------------------------------------
prop.table(table(base$Churn)) #Proporci?n de fuga

#Proporci?n seg?n el g?nero
prop.table(table(base[which(base$Churn=="Yes"),colnames(base)=="gender"]))*100
prop.table(table(base[which(base$Churn=="No"),colnames(base)=="gender"]))*100

#Proporci?n seg?n PhoneService
tab1<-table(base[which(base$Churn=="Yes"),colnames(base)=="PhoneService"])
tab1
proporcion_si<-as.numeric(prop.table(tab1)*100)
proporcion_si

tab2<-table(base[which(base$Churn=="No"),colnames(base)=="PhoneService"])
proporcion_no<-as.numeric(prop.table(tab2)*100)
proporcion_no

#Gr?ficas de pastel para PhoneService
par(mfrow=c(1,2))
proporcion<-as.numeric(prop.table(table(base[which(base$Churn=="Yes"),colnames(base)=="PhoneService"]))*100)
etiquetas<-c("No","S?")
etiquetas<-paste(etiquetas,round(proporcion,2),"%")
pie(proporcion,etiquetas,col=6:8,main="Distribuci?n por servicio telef?nico de los clientes \n que cancelaron el contrato")

proporcion<-as.numeric(prop.table(table(base[which(base$Churn=="No"),colnames(base)=="PhoneService"]))*100)
etiquetas<-c("No","S?")
etiquetas<-paste(etiquetas,round(proporcion,2),"%")
pie(proporcion,etiquetas,col=6:8,main="Distribuci?n por contrato de los clientes \n que mantuvieron el contrato")
par(mfrow=c(1,1))


#Gr?ficas de pastel para la variable Contract
par(mfrow=c(1,2))
proporcion<-as.numeric(prop.table(table(base[which(base$Churn=="Yes"),colnames(base)=="Contract"]))*100)
etiquetas<-c("Mes a mes","1 a?o","2 a?os")
etiquetas<-paste(etiquetas,round(proporcion,2),"%")
pie(proporcion,etiquetas,col=6:8,main="Distribuci?n por contrato de los clientes \n que cancelaron el contrato")

proporcion<-as.numeric(prop.table(table(base[which(base$Churn=="No"),colnames(base)=="Contract"]))*100)
etiquetas<-c("Mes a mes","1 a?o","2 a?os")
etiquetas<-paste(etiquetas,round(proporcion,2),"%")
pie(proporcion,etiquetas,col=6:8,main="Distribuci?n por contrato de los clientes \n que mantuvieron el contrato")
par(mfrow=c(1,1))

#Gr?ficas de pastel para la variable InternetService
ggplot(base)+
  geom_bar(aes(x=Churn,fill=InternetService),position="dodge")+
  labs(x="Cancelaci?n del contrato",y='Frecuencia')+
  ggtitle("Gr?fica de barras del servicio de internet \n por tipo de cliente")+
  scale_x_discrete(labels=c("No","S?"))+
  scale_fill_discrete( name="Servicio de internet",labels=c("DSL","Fiber optic","No"))+
  theme(plot.title=element_text(hjust=.5))

#Densidad del cargo mensual
library(scales) #Solo porque es dinero
ggplot(base)+geom_density(aes(x=MonthlyCharges),fill="pink",color="black")+
  facet_wrap( ~ Churn, ncol=2)+
  labs(x="Cargo mensual",y='Densidad')+
  ggtitle("Densidad del cargo mensual por tipo de cliente")+
  theme(plot.title=element_text(hjust=.5))+scale_x_continuous(label=dollar)


#Juntando las categor?as "No internet service" y "No" en simplemente "No"
for (j in 1:(ncol(base)-1)){
  if(sort(unique(base[,j]))[1]=="No" && sort(unique(base[,j]))[2]=="No internet service"  && length(unique(base[,j]))==3 ){
       for(i in 1:(nrow(base))){
            if(base[i,j]=="No internet service"){base[i,j]<-"No"}}}}


#Juntando las categor?as "No phone service" y "No" en simplemente "No"
for(i in 1:(nrow(base))){
  if(base[i,colnames(base)=="MultipleLines"]=="No phone service"){base[i,colnames(base)=="MultipleLines"]<-"No"}}


#Convirtiendo en factores los atributos con valores 
for (j in 1:(ncol(base)-1)){
  
  if(sort(unique(base[,j]))[1]=="No" && length(unique(base[,j]))==2 ){
    base[,j]<-factor(base[,j],levels=c("No","Yes"),labels=0:1)}
}
base$gender<-factor(base$gender,levels=c("Female","Male"),labels=0:1)
base$SeniorCitizen<-factor(base$SeniorCitizen,levels=c(0,1),labels=0:1)
base$InternetService<-factor(base$InternetService,levels=c("No", "DSL","Fiber optic"),labels=0:2)
base$Contract<-factor(base$Contract,levels=c("Month-to-month" , "One year", "Two year"),labels=0:2)
base$PaymentMethod<-factor(base$PaymentMethod,levels=c("Mailed check" , "Bank transfer (automatic)","Credit card (automatic)","Electronic check" ),labels=0:3)
base$Churn<-ifelse(base$Churn=="No",0,1)
base$MonthlyCharges<-base$MonthlyCharges-min(base$MonthlyCharges)
base$TotalCharges<-base$TotalCharges-min(base$TotalCharges[which(is.na(base$TotalCharges)==FALSE)])


base$InternetServiceDSL=ifelse(Telco$InternetService=="DSL",1,0)
base$InternetServiceFiberOptic=ifelse(Telco$InternetService=="Fiber optic",1,0)
base$Contract1Anio=ifelse(Telco$Contract=="One year",1,0)
base$Contract2Anio=ifelse(Telco$Contract=="Two year",1,0)
base$PaymentMethodBank=ifelse(Telco$PaymentMethod=="Bank transfer (automatic)",1,0)
base$PaymentMethodCredit=ifelse(Telco$PaymentMethod=="Credit card (automatic)",1,0)
base$PaymentMethodElect=ifelse(Telco$PaymentMethod=="Electronic check" ,1,0)

base$InternetServiceDSL<-factor(base$InternetServiceDSL,levels=c(0,1),labels=0:1)
base$InternetServiceFiberOptic<-factor(base$InternetServiceFiberOptic,levels=c(0,1),labels=0:1)
base$Contract1Anio<-factor(base$Contract1Anio,levels=c(0,1),labels=0:1)
base$Contract2Anio<-factor(base$Contract2Anio,levels=c(0,1),labels=0:1)
base$PaymentMethodBank<-factor(base$PaymentMethodBank,levels=c(0,1),labels=0:1)
base$PaymentMethodCredit<-factor(base$PaymentMethodCredit,levels=c(0,1),labels=0:1)
base$PaymentMethodElect<-factor(base$PaymentMethodElect,levels=c(0,1),labels=0:1)


### An?lisis estad?stico ----------------------------------------------------


## Creaci?n de las gr?ficas de la funci?n de supervivencia -----------------

##Estimaci?n de la funci?n de supervivencia por factor del atributo PhoneService
base.km.PhoneService<- survfit(Surv(tenure,Churn) ~PhoneService, data = base, type = "kaplan-meier")
summary(base.km.PhoneService)

plot(base.km.PhoneService,xlab="Tiempo en meses",ylab="Funci?n de Supervivencia", main="Funci?n de Supervivencia Kaplan Meier \n seg?n el servicio telef?nico",lty=1:2,col=2:3,cex=0.8)#cex determina tama?o de los pts
legend(locator(2),
       legend=c("No", "S?"),
       lty=1:2, col=2:3,cex=0.6)

## Estimaci?n de la funci?n de supervivencia por factor del atributo InternetService
base.km.InternetService<- survfit(Surv(tenure,Churn) ~InternetService, data = base, type = "kaplan-meier")
summary(base.km.InternetService)

plot(base.km.InternetService,xlab="Tiempo en meses",ylab="Funci?n de Supervivencia", main="Funci?n de Supervivencia Kaplan Meier \n seg?n el servicio de Internet ",lty=1:3,col=4:6,cex=0.8)#cex determina tama?o de los pts
legend(locator(2),
       legend=c("No", "DSL","Fiber optic"),
         lty=1:3, col=4:6,cex=0.8)

## Comparaci?n de funciones de supervivencia -----------------

#Prueba log-rank para PhoneService   
prueba<-survdiff(Surv(tenure,Churn) ~ PhoneService, data = base, rho = 0)
valorp<-pchisq(prueba$chisq, df= (length(prueba$n)-1), lower.tail = F)
valorp

#Prueba log-rank para InternetService  
survdiff(Surv(tenure,Churn) ~ InternetService, data = base, rho = 0)

#Media del tiempo de vida
print(base.km.InternetService, print.rmean = TRUE)#rmean:limite sup de la integral de la esperanza

#Comparaci?n de Funciones de supervivencia entre supoblaciones
pairwise_survdiff(Surv(tenure,Churn) ~ InternetService, data = base, p.adjust.method = "bonferroni",   rho = 0)

#Separando las subpoblaciones
base0<-subset(base,InternetService==0)#No
base1<-subset(base,InternetService==1)#DSL
base2<-subset(base,InternetService==2)#Fiber Optic
base0<-subset(base0,tenure!=0)
base1<-subset(base1,tenure!=0)
base2<-subset(base2,tenure!=0)

base0.km<-survfit(Surv(tenure,Churn) ~ 1, data = base0, type = "kaplan-meier")  
base1.km<-survfit(Surv(tenure,Churn) ~ 1, data = base1, type = "kaplan-meier")  
base2.km<-survfit(Surv(tenure,Churn) ~ 1, data = base2, type = "kaplan-meier") 

## Papeles de probabilidad para No

par(mfrow=c(2,2))
#Papel de probabilidad Weibull
y<- log(-log(base0.km$surv))
x<-log(base0.km$time)
plot(x,y, pch=16, xlab="Ln(t)", ylab=" Log(-Log(S(t)))",main="Papel de Probabilidad \n Weibull",col="red",cex.lab=1.2) #xlim=range(6:8)

#Papel de Probabilidad lognormal
yl<-log(base1.km$time)
qqnorm(yl, pch=16, xlab="Cuantiles te?ricos", ylab="Cuantiles muestrales", main="Papel de Probabilidad \n LogNormal",col="Orange",cex.lab=1.2)

#Papel de probabilidad Loglog?stico
ss<- -log(base0.km$surv/(1-base0.km$surv))
st<-log(base0.km$time)
plot(st,ss, pch=16, col=4,xlab="Ln(t)", ylab=" Log(-Log(S(t)/(1-S(t))))", main="Papel de Probabilidad \n LogLog?stico",cex.lab=1.2)


#Ajuste para las distribuciones Weibull y loglog?stica
ajusteflex_weibull_0 <- flexsurvreg(formula=Surv(tenure,Churn) ~ 1, data = base0, dist = "weibull")
ajusteflex_weibull_0
ajusteflex_weibull_0$AIC

ajusteflex_llogis_0 <- flexsurvreg(formula=Surv(tenure,Churn) ~ 1, data = base0 ,dist = "llogis")
ajusteflex_llogis_0 
ajusteflex_llogis_0$AIC

ajusteflex_llogis_0$AIC>ajusteflex_weibull_0$AIC


## Papeles de probabilidad para DSL
par(mfrow=c(1,1))
par(mfrow=c(2,2))
#Papel de probabilidad Weibull
y<- log(-log(base1.km$surv))
x<-log(base1.km$time)
plot(x,y, pch=16, xlab="Ln(t)", ylab=" Log(-Log(S(t)))",main="Papel de Probabilidad \n Weibull",col="red",cex.lab=1.2) #xlim=range(6:8)

#Papel de Probabilidad lognormal
yl<-log(base1.km$time)
qqnorm(yl, pch=16, xlab="Cuantiles te?ricos", ylab="Cuantiles muestrales", main="Papel de Probabilidad \n LogNormal",col="Orange",cex.lab=1.2)

#Papel de probabilidad Loglog?stico
ss<- -log(base1.km$surv/(1-base1.km$surv))
st<-log(base1.km$time)
plot(st,ss, pch=16, col=4,xlab="Ln(t)", ylab=" Log(-Log(S(t)/(1-S(t))))", main="Papel de Probabilidad \n LogLog?stico",cex.lab=1.2)
par(mfrow=c(1,1))

ajusteflex_weibull_1 <- flexsurvreg(formula=Surv(tenure,Churn) ~ 1, data = base1, dist = "weibull")
ajusteflex_weibull_1
ajusteflex_weibull_1$AIC

ajusteflex_llogis_1 <- flexsurvreg(formula=Surv(tenure,Churn) ~ 1, data = base1 ,dist = "llogis")
ajusteflex_llogis_1 
ajusteflex_llogis_1$AIC

ajusteflex_llogis_1$AIC>ajusteflex_weibull_1$AIC

## Papeles de probabilidad para Fiber optic
par(mfrow=c(2,2))
#Papel de probabilidad Weibull
y<- log(-log(base2.km$surv))
x<-log(base2.km$time)
plot(x,y, pch=16, xlab="Ln(t)", ylab=" Log(-Log(S(t)))",main="Papel de Probabilidad \n Weibull",col="red",cex.lab=1.2)

#Papel de Probabilidad lognormal
yl<-log(base2.km$time)
qqnorm(yl, pch=16, xlab="Cuantiles te?ricos", ylab="Cuantiles muestrales", main="Papel de Probabilidad \n LogNormal",col="Orange",cex.lab=1.2)

#Papel de probabilidad Loglog?stico
ss<- -log(base2.km$surv/(1-base2.km$surv))
st<-log(base2.km$time)
plot(st,ss, pch=16, col=4,xlab="Ln(t)", ylab=" Log(-Log(S(t)/(1-S(t))))", main="Papel de Probabilidad \n LogLog?stico",cex.lab=1.2)

par(mfrow=c(1,1))

ajusteflex_weibull_2 <- flexsurvreg(formula=Surv(tenure,Churn) ~ 1, data = base2, dist = "weibull")
ajusteflex_weibull_2
ajusteflex_weibull_2$AIC

ajusteflex_llogis_2 <- flexsurvreg(formula=Surv(tenure,Churn) ~ 1, data = base2 ,dist = "llogis")
ajusteflex_llogis_2 
ajusteflex_llogis_2$AIC

ajusteflex_llogis_2$AIC>ajusteflex_weibull_2$AIC

### Creando el modelo de Cox ------------------------------------------------


## Paso 1: Ajustando un modelo para cada una las covariables  ----------------------

gender.ph<-coxph(Surv(tenure,Churn)~gender,base,method="breslow",na.action=na.exclude)
summary(gender.ph)
#P-VALOR es 0.472>0.05, por tanto  no es significativa en el modelo

senior.ph<-coxph(Surv(tenure,Churn)~SeniorCitizen,base,method="breslow",na.action=na.exclude)
summary(senior.ph)
#P-VALOR es 2e-16<0.05, por tanto s? es significativa en el modelo

partner.ph<-coxph(Surv(tenure,Churn)~Partner,base,method="breslow",na.action=na.exclude)
summary(partner.ph)
#P-VALOR es 2e-16<0.05, por tanto s? es significativa en el modelo

dependents.ph<-coxph(Surv(tenure,Churn)~Dependents,base,method="breslow",na.action=na.exclude)
summary(dependents.ph)
#P-VALOR es 2e-16<0.05, por tanto s? es significativa en el modelo

phoneserv.ph<-coxph(Surv(tenure,Churn)~PhoneService,base,method="breslow",na.action=na.exclude)
summary(phoneserv.ph)
#P-VALOR es 0.515>0.05, por tanto no es significativa en el modelo

multlines.ph<-coxph(Surv(tenure,Churn)~MultipleLines,base,method="breslow",na.action=na.exclude)
summary(multlines.ph)
#P-VALOR es 0.0205<0.05, por tanto MultipleLines1 s? es significativa en el modelo

internetserv.ph<-coxph(Surv(tenure,Churn)~InternetService,base,method="breslow",na.action=na.exclude)
summary(internetserv.ph)

InternetServiceFiberOptic.ph<-coxph(Surv(tenure,Churn)~InternetServiceFiberOptic,base,method="breslow",na.action=na.exclude)
summary(InternetServiceFiberOptic.ph)

InternetServiceDSL.ph<-coxph(Surv(tenure,Churn)~InternetServiceDSL,base,method="breslow",na.action=na.exclude)
summary(InternetServiceDSL.ph)

#P-VALOR es 2e-16<0.05, por tanto InternetService1 s? es significativa en el modelo
#P-VALOR es 2e-16<0.05, por tanto InternetService2 s? es significativa en el modelo


onlinesec.ph<-coxph(Surv(tenure,Churn)~OnlineSecurity,base,method="breslow",na.action=na.exclude)
summary(onlinesec.ph)
#P-VALOR es 2e-16<0.05, por tanto OnlineSecurity1 s? es significativa en el modelo

onlinebackup.ph<-coxph(Surv(tenure,Churn)~OnlineBackup,base,method="breslow",na.action=na.exclude)
summary(onlinebackup.ph)
#P-VALOR es 2e-16<0.05, por tanto OnlineBackup1 s? es significativa en el modelo

deviceprotect.ph<-coxph(Surv(tenure,Churn)~DeviceProtection,base,method="breslow",na.action=na.exclude)
summary(deviceprotect.ph)
#P-VALOR es 2e-16<0.05, por tanto DeviceProtection1 s? es significativa en el modelo

techsupport.ph<-coxph(Surv(tenure,Churn)~TechSupport,base,method="breslow",na.action=na.exclude)
summary(techsupport.ph)
#P-VALOR es 2e-16<0.05, por tanto TechSupport1 s? es significativa en el modelo

streamingtv.ph<-coxph(Surv(tenure,Churn)~StreamingTV,base,method="breslow",na.action=na.exclude)
summary(streamingtv.ph)
#P-VALOR es0.0451<0.05, por tanto StreamingTV1 s? es significativa en el modelo

streamingmovies.ph<-coxph(Surv(tenure,Churn)~StreamingMovies,base,method="breslow",na.action=na.exclude)
summary(streamingmovies.ph)
#P-VALOR es 0.0222<0.05, por tanto StreamingMovies1 s? es significativa en el modelo

contract.ph<-coxph(Surv(tenure,Churn)~Contract,base,method="breslow",na.action=na.exclude)
summary(contract.ph)

contract2anio.ph<-coxph(Surv(tenure,Churn)~Contract2Anio,base,method="breslow",na.action=na.exclude)
summary(contract2anio.ph)

contract1a?o.ph<-coxph(Surv(tenure,Churn)~Contract1A?o,base,method="breslow",na.action=na.exclude)
summary(contract1a?o.ph)

#P-VALOR es 2e-16<0.05, por tanto Contract1 s? es significativa en el modelo
#P-VALOR es 2e-16<0.05, por tanto Contract2 s? es significativa en el modelo

paperlessbill.ph<-coxph(Surv(tenure,Churn)~PaperlessBilling,base,method="breslow",na.action=na.exclude)
summary(paperlessbill.ph)
#P-VALOR es 2e-16<0.05, por tanto s? es significativa en el modelo

paymentmethod.ph<-coxph(Surv(tenure,Churn)~PaymentMethod,base,method="breslow",na.action=na.exclude)
summary(paymentmethod.ph)

paymentmethodCredit.ph<-coxph(Surv(tenure,Churn)~PaymentMethodCredit,base,method="breslow",na.action=na.exclude)
summary(paymentmethodCredit.ph)

paymentmethodElect.ph<-coxph(Surv(tenure,Churn)~PaymentMethodElect,base,method="breslow",na.action=na.exclude)
summary(paymentmethodElect.ph)

paymentmethodBank.ph<-coxph(Surv(tenure,Churn)~PaymentMethodBank,base,method="breslow",na.action=na.exclude)
summary(paymentmethodBank.ph)

#P-VALOR es 2e-16<0.05, por tanto PaymentMethod1 s? es significativa en el modelo
#P-VALOR es 2e-16<0.05, por tanto PaymentMethod2 s? es significativa en el modelo
#P-VALOR es 2e-16<0.05, por tanto PaymentMethod3 s? es significativa en el modelo

monthlycharges.ph<-coxph(Surv(tenure,Churn)~MonthlyCharges,base,method="breslow",na.action=na.exclude)
summary(monthlycharges.ph)
#P-VALOR es 3.73e-15<0.05, por tanto s? es significativa en el modelo

totalcharges.ph<-coxph(Surv(tenure,Churn)~TotalCharges,base,method="breslow",na.action=na.exclude)
summary(totalcharges.ph)
#P-VALOR es 2e-16<0.05, por tanto s? es significativa en el modelo


## Paso 2: Se ajusta un modelo con las covariables significativas --------
modelo1.ph<-coxph(Surv(tenure,Churn)~.,subset(base[,1:20], select=-c(gender,PhoneService)),method="breslow",na.action=na.exclude)
summary(modelo1.ph)
#Variables no significativas:
#SeniorCitizen
#Dependents
#MultipleLines
#DeviceProtection
#StreamingTV
#StreamingMovies
#PaymentMethod3

#Modelo con Variables significativas:
modelo2.ph<-coxph(Surv(tenure,Churn)~Partner+InternetService+OnlineSecurity+OnlineBackup+TechSupport+
              Contract+PaperlessBilling+PaymentMethodBank+PaymentMethodCredit+MonthlyCharges+TotalCharges,subset(base, select=-c(gender,PhoneService)),method="breslow",na.action=na.exclude)
summary(modelo2.ph)

## Paso 3: Verificanco la significancia de las covariables que no se incluyeron en el paso 2, pero s? en el --------

modelo2_1.ph<-coxph(Surv(tenure,Churn)~gender+Partner+InternetService+OnlineSecurity+OnlineBackup+TechSupport+
                    Contract+PaperlessBilling+PaymentMethodBank+PaymentMethodCredit+MonthlyCharges+TotalCharges,base,method="breslow",na.action=na.exclude)
summary(modelo2_1.ph)

modelo2_2.ph<-coxph(Surv(tenure,Churn)~PhoneService+Partner+InternetService+OnlineSecurity+OnlineBackup+TechSupport+
                      Contract+PaperlessBilling+PaymentMethodBank+PaymentMethodCredit+MonthlyCharges+TotalCharges,base,method="breslow",na.action=na.exclude)
summary(modelo2_2.ph)


#Verificando interacci?n
modelo2_2_1.ph<-coxph(Surv(tenure,Churn)~PhoneService+Partner+InternetService+OnlineSecurity+OnlineBackup+TechSupport+
                      Contract+PaperlessBilling+PaymentMethodBank+PaymentMethodCredit+MonthlyCharges+TotalCharges+PaymentMethodCredit:MonthlyCharges+PaymentMethodBank:MonthlyCharges,base,method="breslow",na.action=na.exclude)
summary(modelo2_2_1.ph)

modelo2_2_2.ph<-coxph(Surv(tenure,Churn)~PhoneService+Partner+InternetService+OnlineSecurity+OnlineBackup+TechSupport+
                        Contract+PaperlessBilling+PaymentMethodBank+PaymentMethodCredit+MonthlyCharges+TotalCharges+Contract:MonthlyCharges,base,method="breslow",na.action=na.exclude)
summary(modelo2_2_2.ph)

modelo2_2_3.ph<-coxph(Surv(tenure,Churn)~PhoneService+Partner+InternetService+OnlineSecurity+OnlineBackup+TechSupport+
                        Contract+PaperlessBilling+PaymentMethodBank+PaymentMethodCredit+MonthlyCharges+TotalCharges+OnlineSecurity:TechSupport,base,method="breslow",na.action=na.exclude)
summary(modelo2_2_3.ph)

modelo2_2_4.ph<-coxph(Surv(tenure,Churn)~PhoneService+Partner+InternetService+OnlineSecurity+OnlineBackup+TechSupport+
                        Contract+PaperlessBilling+PaymentMethodBank+PaymentMethodCredit+MonthlyCharges+TotalCharges+OnlineSecurity:TechSupport+Contract:MonthlyCharges,base,method="breslow",na.action=na.exclude)
summary(modelo2_2_4.ph)

modelo2_2_5.ph<-coxph(Surv(tenure,Churn)~PhoneService+Partner+InternetService+OnlineSecurity+OnlineBackup+TechSupport+
                        Contract+PaperlessBilling+PaymentMethodBank+PaymentMethodCredit+MonthlyCharges+TotalCharges+OnlineSecurity:TechSupport+Contract:MonthlyCharges+PaymentMethodCredit,base,method="breslow",na.action=na.exclude)
summary(modelo2_2_5.ph)


modelofinal.ph<-modelo2_2_4.ph
summary(modelofinal.ph)

### Validacion del modelo ---------------------------------------------------

#checando riesgos proporcionales
cox.modelofinal.ph<-cox.zph(modelofinal.ph)
cox.modelofinal.ph
#los p valores mayores 0.05 no rechazan la hipotesis 
#h0=riesgos proporcionales vs h1=riesgos no proporcionales

par(mfrow=c(1,1))
#Residuales Schoenfeld
plot(cox.modelofinal.ph)

#checando linealidad
j=1
mar.modelo<-residuals(modelofinal.ph,type="martingale")
par(mfrow=c(2,1))# se adapta al n?m de variables
X<-as.matrix(base[,c("MonthlyCharges","TotalCharges")])
for (j in 1:2){ #gr?fica de residuales martingalas
  scatter.smooth((X[,j]),mar.modelo,type="p",pch=".",xlab =c("MonthlyCharges","TotalCharges")[j],ylab="Residuos martingalas",col=blues9)
}
#no hay evidencia en contra de linealidad
# debe mostrar una tendencia lineal

#checando datos influyentes
dfbeta.modelo<-residuals(modelofinal.ph,type="dfbeta")
dfbeta.modelo
par(mfrow=c(3,3))
for (j in 1:9){
  plot (dfbeta.modelo[,j],ylab=names(coef(modelofinal.ph))[j])
  abline(h=0,lty=2)
}

dfbeta.modelo<-residuals(modelofinal.ph,type="dfbeta")
dfbeta.modelo
par(mfrow=c(3,3))
for (j in 10:17){
  plot (dfbeta.modelo[,j],ylab=names(coef(modelofinal.ph))[j])
  abline(h=0,lty=2)
}

dfbeta.modelo<-residuals(modelofinal.ph,type="dfbeta")
dfbeta.modelo
par(mfrow=c(1,1))
for (j in 14:14){
  plot (dfbeta.modelo[,j],ylab=names(coef(modelofinal.ph))[j],col="red")
  abline(h=0,lty=2)
}

#Base riesgo
BRiesgo<-basehaz(modelofinal.ph,centered=T)
BRiesgo

XX<-as.matrix(BRiesgo)
plot(XX[,2],XX[,1],type="s",xlab="Tiempo",ylab="Riesgo",main="Funci?n Riesgo Base")


