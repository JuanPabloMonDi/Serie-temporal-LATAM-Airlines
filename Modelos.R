library(dplyr)
library(httr)
#library(readxl)
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
source("Funcoes.R")
nome_acao <- "LTM.SN"#"MSFT.MX"  #"SMSN.IL" "MAT" # Código no Yahoo Finance
data_ini  <- "2013-01-01" # Data de inicio
data_fim  <- Sys.Date() # Data de fim, hoje
cutoff.date = as.Date("2020-01-1")
precos <- yf_get(tickers = nome_acao, first_date = data_ini, last_date = data_fim)
#O ultimo valor não vai ser considerado, para poder comparar as predições com ele ao final
valor_real<-precos$ret_adjusted_prices[length(precos$ret_adjusted_prices)]
data<-as.character(precos$ref_date[nrow(precos)])
precos <- precos[1:nrow(precos)-1,]

precos2 <- yf_get(tickers = nome_acao, first_date = data_ini, last_date = cutoff.date)
head(precos)


precos$depois_da_pandemia= ifelse(precos$ref_date< cutoff.date, 0, 1)
retornos = precos$ret_adjusted_prices[-1]
retornos_antes_da_pandemia = ts(precos2$ret_adjusted_prices[-1])
retornos_c = scale(retornos, center = TRUE, scale = FALSE)
retornos_c_antes_pandemia = scale(retornos_antes_da_pandemia, center = TRUE, scale = FALSE)
precos1<-data.frame(precos[c("ref_date","ret_adjusted_prices","depois_da_pandemia")])[-1,]
precos1$retornos_c<-retornos_c
colnames(precos1)=c("ref_date","ret_adjusted_prices","depois_da_pandemia","retornos_c" )
precos1$ref_date<-as.Date(precos1$ref_date)
rownames(precos1)<-precos1$ref_date
a<-ts(precos1$ret_adjusted_prices, frequency = 260)
b<-decompose(a)
a2<-ts(precos2$ret_adjusted_prices, frequency = 260)
b2<-decompose(a2)




Box<-Box.test(retornos_c,"Ljung-Box",lag=10)
Box2<-Box.test(retornos_c_antes_pandemia,"Ljung-Box",lag=10)
retornos_c_antes_pandemia<-retornos_c_antes_pandemia[retornos_c_antes_pandemia<=0.15]


#Agora construimos os modelos,
#a seguir, código do cada um dos modelos feitos, mas só vão ficar os melhores modelos na app


#mod1<-arima(retornos_c_antes_pandemia,order=c(0,0,1))
#acf(mod1$residuals)
#pacf(mod1$residuals)
#plot(mod1)
#qqnorm(mod1$residuals/mod1$sigma2)
#qqline(mod1$residuals/mod1$sigma2)
#hist(mod1$residuals)
#Box.test(mod1$residuals) #nao temos que rejeitar a hipotese nula


#mod2<-arima(retornos_c_antes_pandemia,order=c(1,0,1))
#acf(mod2$residuals)
#pacf(mod2$residuals)
#plot(mod2)
#qqnorm(mod2$residuals/mod2$sigma2)
#qqline(mod2$residuals/mod2$sigma2)
#hist(mod2$residuals)
#Box.test(mod2$residuals) 

#mod3<-arima(retornos_c_antes_pandemia,order=c(2,0,1))
#acf(mod3$residuals)
#pacf(mod3$residuals)
#plot(mod3)
#qqnorm(mod3$residuals/mod3$sigma2)
#qqline(mod3$residuals/mod3$sigma2)
#hist(mod3$residuals)
#Box.test(mod3$residuals) 

#mod4<-arima(retornos_c_antes_pandemia,order=c(0,1,2))

#acf(mod4$residuals)
#pacf(mod4$residuals)
#plot(mod4)
#qqnorm(mod4$residuals/mod4$sigma2)
#qqline(mod4$residuals/mod4$sigma2)
#hist(mod4$residuals)
#Box.test(mod4$residuals) 

#mod5<-arima(retornos_c_antes_pandemia,order=c(1,1,1))
#acf(mod5$residuals)
#pacf(mod5$residuals)
#plot(mod5)
#qqnorm(mod5$residuals/mod5$sigma2)
#qqline(mod5$residuals/mod5$sigma2)
#hist(mod5$residuals)
#Box.test(mod5$residuals) 

#O melhor foi o modelo 1, mas todos os modelos apresentaram um aic proximo
#min(c(mod1$aic,mod2$aic,mod3$aic,mod4$aic,mod5$aic))

#variancia constante, normalidade
#Diagnostico

#mod6spec<- ugarchspec(mean.model=list(armaOrder=c(0,1),include.mean=FALSE),
#                      variance.model = list(model="sGARCH", garchOrder=c(1,1)),
#                      distribution.model = "std") 

#mod6<-ugarchfit(mod6spec,retornos_c_antes_pandemia,solver="hybrid")
#acf(mod6@fit$residuals)
#acf(mod6@fit$residuals^2)
#qqnorm(mod6@fit$residuals/mod6@fit$sigma)
#qqline(mod6@fit$residuals/mod6@fit$sigma)


mod7spec<- ugarchspec(mean.model=list(armaOrder=c(0,1),include.mean=FALSE),
                      variance.model = list(model="sGARCH", garchOrder=c(3,2)),
                      distribution.model = "sstd") 

mod7<-ugarchfit(mod7spec,retornos_c_antes_pandemia,solver="hybrid")


#Todos os modelos arima tiveram um comportamiento semelhante nos
#Diagnostico dos residuais feito para o modelo 7
#o modelo que teve melhor desempenho na validação cruzada
acf(mod7@fit$residuals)
acf(mod7@fit$residuals^2)
qqnorm(mod7@fit$residuals/mod7@fit$sigma)
qqline(mod7@fit$residuals/mod7@fit$sigma)
ggplot(data.table(res=as.numeric(mod7@fit$residuals)))+
  geom_histogram(aes(x=res),colour="white",fill="black")


#Agora, considerando a pandemia

mod8<-arima(precos1$retornos_c, 
            order = c(1,1,1), 
            xreg=precos1$depois_da_pandemia,
            include.mean = FALSE)

#mod9<-arima(precos1$retornos_c, 
#            order = c(7,1,7), 
#            xreg=precos1$depois_da_pandemia,
#            include.mean = FALSE)

#mod10<-arima(precos1$retornos_c, 
#             order = c(7,0,4), 
#             xreg=precos1$depois_da_pandemia,
#             include.mean = FALSE)
#mod11<-arima(precos1$retornos_c, 
#             order = c(7,0,4),
#             include.mean = FALSE)
#mod12spec<-ugarchspec(mean.model = list(armaOrder = c(1, 1), include.mean = FALSE),
#                  variance.model = list(model = 'sGARCH', garchOrder = c(1, 1),external.regressors=matrix(precos1$depois_da_pandemia)),
#                  distribution.model = 'sstd')

#mod12<-ugarchfit(mod12spec,precos1$retornos_c,solver="hybrid")
#qqnorm(mod12@fit$residuals/mod12@fit$sigma)
#qqline(mod12@fit$residuals/mod12@fit$sigma)



#mod13spec<-ugarchspec(mean.model = list(armaOrder = c(7, 4), include.mean = FALSE),
#                 variance.model = list(model = 'sGARCH', garchOrder = c(2, 2),external.regressors=matrix(precos1$depois_da_pandemia)),
#                 distribution.model = 'sstd')

#mod13<-ugarchfit(mod13spec,diff(precos1$retornos_c),solver="hybrid")
#qqnorm(mod13@fit$residuals/mod13@fit$sigma)
#qqline(mod13@fit$residuals/mod13@fit$sigma)

#Agora, vamos fazer a validação cruzada
#modelos<-c()
#modelos[[1]]<-mod1
#modelos[[2]]<-mod2
#modelos[[3]]<-mod3
#modelos[[4]]<-mod4
#modelos[[5]]<-mod5
#modelos[[6]]<-mod6
#modelos[[7]]<-mod7
#modelos[[8]]<-mod8
#modelos[[9]]<-mod9
#modelos[[10]]<-mod10
#modelos[[11]]<-mod11
#modelos[[12]]<-mod12
#modelos[[13]]<-mod13
#cv_results<-data.frame()

#Validação cruzada para os modelos antes da pandemia
#for (i in 1:7){
#  train_data<-retornos_c_antes_pandemia
#  mod<-modelos[[i]]
#  errores<-t(data.frame(cross_val(retornos_c_antes_pandemia,mod,window_size = 450))) 
#  rownames(errores)<-i
#  cv_results<-rbind(cv_results,errores)
#}


#Agora, a validação cruzada para os modelos apos pandemia
#train_data<-precos1$retornos_c
#cv_pospandemia<-data.frame()
#cv_pospandemia<-rbind(cv_pospandemia,t(data.frame(cross_val(precos1$retornos_c,mod8,window_size = 450,xreg=precos1$depois_da_pandemia))))
#cv_pospandemia<-rbind(cv_pospandemia,t(data.frame(cross_val(precos1$retornos_c,mod9,window_size = 450, xreg=precos1$depois_da_pandemia))))
#cv_pospandemia<-rbind(cv_pospandemia,t(data.frame(cross_val(precos1$retornos_c,mod10,window_size = 450,xreg=precos1$depois_da_pandemia))))
#cv_pospandemia<-rbind(cv_pospandemia,t(data.frame(cross_val(precos1$retornos_c,mod11,window_size = 450, xreg=precos1$depois_da_pandemia))))
#cv_pospandemia<-rbind(cv_pospandemia,t(data.frame(cross_val(precos1$retornos_c,mod12,window_size = 450, xreg=precos1$depois_da_pandemia))))
#cv_pospandemia<-rbind(cv_pospandemia,t(data.frame(cross_val(precos1$retornos_c,mod13,window_size = 450,xreg=precos1$depois_da_pandemia,differentation = 1))))
#rownames(cv_pospandemia)<-8:13

#Vamos salvar nossos resultados da validação cruzada num excel, esso é feito para que
#a app de shiny seja mais rapida e não faça a validação cruzada.


#Esse código foi rodado antes da entrega para escolher o melhor modelo,
#não precisamos na validação cruzada na app do shiny
  #write.csv(cv_pospandemia,"cvPospandemia.csv",row.names = FALSE) #salvamos o arquivo
  #write.csv(cv_results,"cvPrepandemia.csv",row.names = FALSE) #salvamos o arquivo



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
Predict <- read_csv("Predicoes/Predict.csv") #Este arquivo vai armazenar as predicoes diarias
Predict <-rbind(Predict,results) #Acrescentamos a predicao

write.csv(Predict,"Predict.csv",row.names = FALSE) #salvamos o arquivo
