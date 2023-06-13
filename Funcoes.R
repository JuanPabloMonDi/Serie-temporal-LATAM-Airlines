library(forecast)
library(rugarch)
library(tidyr)
library(tsibble)
library(feasts)
library(ggplot2)



#Validação cruzada por o método de expanding windows
cross_val<-function(dados,modelo,window_size=200,xreg=NULL,differentation=NULL){
  if (length(dados)%%window_size==0){
    intervals<-c(seq(window_size,(length(dados)),window_size))
  }else{intervals<-c(seq(window_size,(length(dados)),window_size),length(dados))}
  
  results<-data.frame()
  if (class(modelo)=="Arima"){
    b<-deparse(modelo[["call"]])
    c<-str_split(b,"=")
    d<-grep("order",c[[1]])
    d<-c[[1]][d+1]
    d<- substr(d,start = 1,11)
    order<-parse(text=as.vector(d))
    order<-eval(order)
    
    for (i in 1:(length(intervals)-1)){
      train_data<-c()
      train_data<-window(dados,start=1,end=intervals[i]) 
      test_data<-window(dados,start=intervals[i]+1,end=intervals[i+1])
      mod<-tryCatch(
        {mod<-arima(train_data,order,include.mean = FALSE,xreg = xreg[1:length(train_data)])
        pred<-forecast::forecast(mod,h=length(test_data))
    },
      error=function(e){
        mod<-arima(train_data,order,include.mean = FALSE)
        print("Error na convergência durante a validação cruzada, tirando a variavel de regressao")
        pred<-forecast::forecast(mod,h=length(test_data))
        return(mod)
      }
      )
      pred<-forecast::forecast(mod,h=length(test_data))
      error<-forecast::accuracy(pred,test_data)
      error<-data.frame(error)[c("RMSE","MAE")]["Test set",]
      rownames(error)<-i
      results<-rbind(results,error)
    }
    
  }
  if (class(modelo)=="uGARCHfit"){
    if (is.null(xreg)){external.regressors=NULL}else{external.regressors=matrix(xreg[1:length(dados)])}
    modspec<-ugarchspec(mean.model = list(armaOrder=c(modelo@model[["modelinc"]][["ar"]],
                                                      modelo@model[["modelinc"]][["ma"]],
                                                      include.mean=FALSE)),
                        variance.model = list(model="sGARCH", garchOrder=c(modelo@model[["modelinc"]][["alpha"]],
                                                                           modelo@model[["modelinc"]][["beta"]]), 
                                              external.regressors=external.regressors),
                        distribution.model = "sstd")
    for (i in 1:(length(intervals)-2)){
      train_data<-window(dados,start=1,end=intervals[i+1]) 
      test_data<-window(dados,start=intervals[i+1]+1,end=intervals[i+2])
      if (is.null(differentation)==TRUE){
        mod<-ugarchfit(modspec,train_data,solver="hybrid")
      }else{mod<-ugarchfit(modspec,diff(train_data,differentation),solver="hybrid")}
      pred<-ugarchforecast(mod,n.ahead=length(test_data))
      pred<-as.numeric(pred@forecast[["seriesFor"]])
      error<-forecast::accuracy(pred,test_data)
      error<-data.frame(error)[c("RMSE","MAE")]["Test set",]
      rownames(error)<-i
      results<-rbind(results,error)
      }
  }
 
  return(colSums(results)/nrow(results))
}


multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

