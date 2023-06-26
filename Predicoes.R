library(dplyr)
library(httr)
library(readxl)
library(readr)
library(tsibble)
library(ggplot2)
library(tseries)
library(forecast)
library(feasts)
library(data.table)
library(fpp3)
library(seasonal)
library(stringr)
library(flexdashboard)
library(knitr)
library(DT)
library(yfR)
library(rugarch)
#setwd("~/Universidad/2023-I Brasil/SeriesTemp/Projeto/TrabalhoFinal")
source("Funcoes.R")
source("Predicoes.R")


#Esse código foi rodado antes da entrega para escolher o melhor modelo,
#não precisamos na validação cruzada na app do shiny
  #write.csv(cv_pospandemia,"cvPospandemia.csv",row.names = FALSE) #salvamos o arquivo
  #write.csv(cv_results,"cvPrepandemia.csv",row.names = FALSE) #salvamos o arquivo
cvPospandemia<- read_csv("cvPospandemia.csv")
cvPrepandemia <- read_csv("cvPrepandemia.csv")


#Vamos fazer as predicoes para o proximo dia dos melhores modelos para antes de pandemia e depois da pandemia
pred_prepandemia<-ugarchforecast(mod7,n.ahead=1)
pred_pospandemia<-predict(mod8,h=1,newxreg=1)

#Vamos fazer uma comparacao das predicoes feitas e o valor real. E salvamos a compração num arquivo csv
colnames=c("Data","Pred_sem pandemia", "s.e sem pandemia","Pred com pandemia","s.e com pandemia","Valor real")

prepred<-as.numeric(pred_prepandemia@forecast$seriesFor)
prese<-as.numeric(pred_prepandemia@forecast$sigmaFor)
pospred<-as.numeric(pred_pospandemia$pred)
posse<-as.numeric(pred_pospandemia$se)
results<-data.frame(t(c(data,prepred,prese,pospred,posse,valor_real)))
colnames(results)<-colnames
Predict <- read_csv("Predict.csv") #Este arquivo vai armazenar as predicoes diarias
Predict <-rbind(Predict,results) #Acrescentamos a predicao

write.csv(Predict,"Predict.csv",row.names = FALSE) #salvamos o arquivo
