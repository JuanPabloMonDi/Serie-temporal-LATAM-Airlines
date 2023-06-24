#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#https://shiny.posit.co/r/getstarted/shiny-basics/lesson2/
library(dplyr)
library(rsconnect)
rsconnect::setAccountInfo(name='qglhmj-juan0pablo-montano0diaz',
                          token='3D3D75597C7A9A56D2A183B231D14A29',
                          secret='wDGqKDeK5VMo3M6bhOR+uGilAhbq2msrZuHFlLc4')
#library(devtools)
library(httr)
library(readxl)
library(readr)
library(tsibble)
library(ggplot2)
library(forecast)
library(feasts)
library(data.table)
library(fpp3)
library(seasonal)
library(stringr)
library(shinydashboard)
library(flexdashboard)
library(knitr)
library(DT)
library(yfR)
library(shiny)
library(shinythemes)
source("CodigoSemPredicoes.R")



ui <- fluidPage( #shinythemes::themeSelector(), #Para mudar de tema sem correr o código várias vezes
  theme = shinytheme("journal"), #Candidatos, Journal, Sanstone, Yeti e Cosmo
  #navlistPanel(
  navbarPage(
    "Voos LATAM",
             tabPanel("Introdução",
                      # Application title
                      titlePanel("Series Temporais- Trabalho final"),
                      
                      # Sidebar with a slider input for number of bins 
                      sidebarLayout( position = "left",
                                     sidebarPanel( h1("Fonte dos dados",  align = "left"),
                                                   h2("Pacote"),
                                                   ),
                                     
                      # Show a plot of the generated distribution
                      mainPanel(img(src = "LogoUnicamp.jpg", height=90,width=170, align="right"),
                                br(),
                                br(),
                                br(),
                                h1("Retornos das ações da Aerolinea Latam-Chile",  align = "center"),
                                h2("Analise descriptivo"),
                                h3("Third level title"),
                                h4("Fourth level title"),
                                h5("Fifth level title"),
                                h6("Sixth level title"),
                                p("p creates a paragraph of text."),
                                p("A new p() command starts a new paragraph. Supply a style attribute to change the format of the entire paragraph.", style = "font-family: 'times'; font-si16pt"),
                                strong("strong() makes bold text."),
                                em("em() creates italicized (i.e, emphasized) text."),
                  
                                br(),
                                code("code displays your text similar to computer code"),
                                div("div creates segments of text with a similar style. This division of text is all blue because I passed the argument 'style = color:blue' to div", style = "color:blue"),
                                br(),
                                p("span does the same thing as div, but it works with",
                                  span("groups of words", style = "color:blue"),
                                  "that appear inside a paragraph."),
                                tabsetPanel(
                                  tabPanel("Preço",
                                            plotOutput("Price")),
                                  tabPanel("Retorno",
                                           plotOutput("Retorno"))
                                ),
                                dateRangeInput('datasRango',
                                               label = paste('Ensire o intervalo de tempo que deseja graficar, a data minima é 1 de janeiro do 2013'),
                                               start = as.Date(data_ini), end = Sys.Date() -1,
                                               min = as.Date(data_ini), max = Sys.Date()-1,
                                               separator = " - ", format = "dd/mm/yy",
                                               startview = 'year', language = 'pt', weekstart = 1
                                ),
                      ),
                    
                                  )
                      ),
    tabPanel("Análise Descriptivo",
             # SidebarLayout para dividir a pagina em duas partes
             sidebarLayout( position = "left",
                            sidebarPanel( h2("Análise Descriptivo dos dados",  align = "left"),
                                          h3("Escribir ventajas y desventajas de incluir o no la pandemia"),
                                          radioButtons(inputId = "SemPandemia" , 
                                                       label = "Incluir a pandemia?",
                                                       choices = c("Sem pandemia", "Com pandemia")),
                                                       sliderInput(inputId = "Lags",
                                                                   label = "Numero de lags:",
                                                                   min = 1,
                                                                   max = 50,
                                                                   value = 30),
                                          fluid=TRUE,
                                          width=3,
                                          ),
                            mainPanel("Pagina",
                                      fluid=TRUE,
                                      width=9,
                                      tabsetPanel(
                                        tabPanel("Autocorrelação",
                                                 plotOutput("Analise")),
                                        tabPanel("Autocorrelação parcial",
                                                 plotOutput("par_autocorrelacao")),
                                        tabPanel("Decomposicao",
                                                 plotOutput("Decomposicao1")),
                                        tabPanel("Ljung-Box",
                                                verbatimTextOutput("Testes")),
                                        tabPanel("Estacionaridade",
                                                 verbatimTextOutput("Dickey")),
                                        
                                      ), #Fecha o tabsetPanel
                            ), #Fecha o main Panel
    ), #Fecha o sidelayout
    ), #Fechar a secção de análise descriptivo
    
             tabPanel("Modelos",
                      sidebarLayout( position = "left",
                                     sidebarPanel( h2("Análise Descriptivo dos dados",  align = "left"),
                                                   h3("Escribir ventajas y desventajas de incluir o no la pandemia"),
                                                   radioButtons(inputId = "Modelo" , 
                                                                label = "Seleccionar modelo",
                                                                choices = c("1.MA(1)"=1, 
                                                                            "2.ARMA(1,1)"=2,
                                                                            "3.ARMA(2,1)"=3,
                                                                            "4.IMA(1,2)"=4,
                                                                            "5.ARIMA(1,1,1)"=5,
                                                                            "6.MA(1)-GARCH(1,1)"=6,
                                                                            "7.MA(1)-GARCH(3,2)"=7,
                                                                            "8.ARIMAX(1,1,1)"=8,
                                                                            "9.ARIMAX(7,1,7)"=9,
                                                                            "10.ARMAX(7,4)"=10,
                                                                            "11.ARMA(7,4)"=11,
                                                                            "12.ARMAX(1,1)-GARCH(1,1)"=12,
                                                                            "13.ARMAX(7,1,4)-GARCH(2,2)"=13
                                                                            )),
                                                   fluid=TRUE,
                                                   width=3,
                                     ),
                                     
                      mainPanel(fluidPage(
                        wellPanel(fluidRow(
                          column(
                            width = 12,
                            align = "center",
                            h3("Control Panel"),
                            #verbatimTextOutput("ModeloAcf")
                            column(width=6,plotOutput("Modeloqqline")),
                            column(width = 5, plotOutput("Dist")),
                            #column(width = 3, offset = 1, sliderInput('sampleSize','Sample Size', min = 1, max = 100, value = min(1, 100), step = 500,round = 0)),
                            #column(width = 1, offset = 1, actionButton("readButton", "Read Data!"))
                          )
                        )),
                        hr(),
                        fluidRow(
                        column(
                          width = 7,
                          plotOutput('Modeloacf', height = 300),
                        ),
                        column(width = 5, plotOutput('otros', height = 400))
                      ),
                      fluidRow(
                        column(width=6,
                               verbatimTextOutput("TestModelo")),
                        column(width=6,
                               verbatimTextOutput("ModeloResumo"))
                      )
                      
                      ))),
                      ), #Fecha modelos
    tabPanel("Desempenho fora da amostra",
             DT::datatable(cvPrepandemia),
             DT::datatable(cvPospandemia)),
             
                      
             tabPanel("Predições",
                      p(),
                      DT::datatable(Predict)),
    navbarMenu("Sobre o site web",
                        tabPanel("O trabalho",
                                 p("mi nombre"),
                                 #DT::datatable(cv_results)
                        )
             )
)
)

server <- function(input, output) {
  

  
    output$Price<- renderPlot({
      data_ini  <- input$datasRango[1] # Data de inicio
      data_fim  <- input$datasRango[2] # Data de fim
      precosPlot <- yf_get(tickers = nome_acao, first_date = data_ini, last_date = data_fim)
      head(precos)
      
      ggplot(precosPlot) + 
        geom_line(aes(x =ref_date, y = price_adjusted), color = "green4") +
        xlab("Tempo") + ylab("Preço")
      
    })
    
    output$Retorno<- renderPlot({
      data_ini  <- input$datasRango[1] # Data de inicio
      data_fim  <- input$datasRango[2] # Data de fim
      precos <- yf_get(tickers = nome_acao, first_date = data_ini, last_date = data_fim)
      head(precos)
      
      ggplot(precos) + 
        geom_line(aes(x =ref_date, y = ret_adjusted_prices), color = "blue4") +
        xlab("Tempo") + ylab("Retorno")
      
    })
    
    #Grafico de autocorrelação
    output$Analise<- renderPlot({
      if(as.character(input$SemPandemia)=="Com pandemia"){
        p1<-ggAcf(retornos_c, lag.max = input$Lags)+ggtitle("Autocorrelação da série com pandemia")
        p2<-ggAcf(retornos_c^2, lag.max = input$Lags)+ggtitle("Autocorrelação dos cuadrados série com pandemia")
        multiplot(p1,p2,cols=2)} else{
          p1<-ggAcf(retornos_c_antes_pandemia, lag.max = input$Lags)+ggtitle("Autocorrelação sem pandemia")
          p2<-ggAcf(retornos_c_antes_pandemia^2, lag.max = input$Lags)+ggtitle("Autocorrelação dos cuadrados sem pandemia")
          multiplot(p1,p2,cols=2)} 
    })
    #Grafico de autocorrelações parciales
    output$par_autocorrelacao<- renderPlot({
      if(as.character(input$SemPandemia)=="Com pandemia"){
      p1<-ggPacf(retornos_c, lag.max = input$Lags)+ggtitle("Autocorrelação parcial com pandemia")
      p2<-ggPacf(retornos_c^2, lag.max = input$Lags)+ggtitle("Autocorrelação parcial dos cuadrados com pandemia")
      multiplot(p1,p2,cols=2)}else{
        p1<-ggPacf(retornos_c_antes_pandemia, lag.max = input$Lags)+ggtitle("Autocorrelação parcial da série sem pandemia")
        p2<-ggPacf(retornos_c_antes_pandemia^2, lag.max = input$Lags)+ggtitle("Autocorrelação parcial dos cuadrados da série sem pandemia")
        multiplot(p1,p2,cols=2)
        }
    })

    
    #Grafico da descomposição da série
    output$Decomposicao1<- renderPlot({
      if(as.character(input$SemPandemia)=="Com pandemia"){
      p1<-drop_na(data.frame(b$trend))|> ggplot()+geom_line(aes(x=seq_len(nrow(drop_na(data.frame(b$trend)))),y=b.trend))+ylab("Tendência")+xlab("")
      p2<-drop_na(data.frame(b$seasonal))|> ggplot()+geom_line(aes(x=seq_len(nrow(drop_na(data.frame(b$seasonal)))),y=b.seasonal))+ylab("Sazonalidade")+xlab("")
      p3<-drop_na(data.frame(b$random))|> ggplot()+geom_line(aes(x=seq_len(nrow(drop_na(data.frame(b$random)))),y=b.random))+ylab("Aleatorio")+xlab("Tempo")
      multiplot(p1,p2,p3,cols=1)}else{
        p1<-drop_na(data.frame(b2$trend))|> ggplot()+geom_line(aes(x=seq_len(nrow(drop_na(data.frame(b2$trend)))),y=b2.trend))+ylab("Tendência")+xlab("")
        p2<-drop_na(data.frame(b2$seasonal))|> ggplot()+geom_line(aes(x=seq_len(nrow(drop_na(data.frame(b2$seasonal)))),y=b2.seasonal))+ylab("Sazonalidade")+xlab("")
        p3<-drop_na(data.frame(b2$random))|> ggplot()+geom_line(aes(x=seq_len(nrow(drop_na(data.frame(b2$random)))),y=b2.random))+ylab("Aleatorio")+xlab("Tempo")
        multiplot(p1,p2,p3,cols=1)
      }
    })
    
    #Testes de Jung- Box e Box- Pierce
    output$Testes<- renderPrint({
      if(as.character(input$SemPandemia)=="Com pandemia"){
      Box<-Box.test(retornos_c,"Ljung-Box",lag=input$Lags)
      Box2<-Box.test(retornos_c,"Box-Pierce",lag=input$Lags)
      print(Box)
      print(Box2)}else{
        Box<-Box.test(retornos_c_antes_pandemia,"Ljung-Box",lag=input$Lags)
        Box2<-Box.test(retornos_c_antes_pandemia,"Box-Pierce",lag=input$Lags)
        print(Box)
        print(Box2)
      }}
      )

    #Test de Dickey Fuller 
    output$Dickey<- renderPrint({
      if(as.character(input$SemPandemia)=="Com pandemia"){
      test_estacionaridade<-adf.test(retornos_c,alternative="stationary",k=input$Lags)
      print(test_estacionaridade)}else{
        test_estacionaridade<-adf.test(retornos_c_antes_pandemia,alternative="stationary",k=input$Lags)
        print(test_estacionaridade)
      }
    })
    #Resultados do modelo
    output$ModeloResumo<-renderPrint({
      mod<-modelos[[as.numeric(input$Modelo)]]
      print(mod)
    })
    
    #acf dos residuais do modelo
    output$Modeloacf<-renderPlot({
      mod<-modelos[[as.numeric(input$Modelo)]]
      if (class(mod)=="uGARCHfit"){
        par(mfrow=c(1,2))
        plot(mod,which=10)
        plot(mod,which=11)
        par(mfrow=c(1,1))
      }
      if (class(mod)=="Arima"){
        p1<-ggAcf(mod$residuals)+ggtitle("Autocorrelação dos residuos do modelo")
        p2<-ggAcf(mod$residuals^2)+ggtitle("Autocorrelação dos quadrados dos residuos do modelo")
        multiplot(p1,p2,cols = 2)
      }
      })
    
    
    #Qqline dos residuais dos modelo
      output$Modeloqqline<-renderPlot({
        mod<-modelos[[as.numeric(input$Modelo)]]
        if (class(mod)=="uGARCHfit"){
          plot(mod,which=9)
        }
        if (class(mod)=="Arima"){
          qqnorm(mod$residuals,col="lightblue4")
          qqline(mod$residuals)
        }
})
      
      #Histograma dos residuais
      output$Dist<- renderPlot({
        mod<-modelos[[as.numeric(input$Modelo)]]
        if (class(mod)=="uGARCHfit"){
          plot(mod,which=8)
        }
        if (class(mod)=="Arima"){
          hist(mod$residuals, freq = FALSE)
          lines(density(rnorm(100000,0,sqrt(mod$sigma2))),lw=3)
        }
      })
      #Test de JungBox dos residuais
      output$TestModelo<- renderPrint({
        mod<-modelos[[as.numeric(input$Modelo)]]
        if (class(mod)=="uGARCHfit"){
          Box<-Box.test(mod@fit$residuals/mod@fit$sigma,type="Ljung-Box")
          Box2<-Box.test(mod@fit$residuals/mod@fit$sigma,type="Box-Pierce")
          print(Box)
          print(Box2)
        }
        if (class(mod)=="Arima"){
          Box<-Box.test(mod$residuals,type="Ljung-Box")
          Box2<-Box.test(mod$residuals, type="Box-Pierce")
          print(Box)
          print(Box2)
        }
      })
      output$otros<- renderPlot({
        mod<-modelos[[as.numeric(input$Modelo)]]
        if (class(mod)=="uGARCHfit"){
          plot(mod,which=2)
        }
        if (class(mod)=="Arima"){
          plot(mod)
        }
      })
      
      
     
}
# Run the application 
shinyApp(ui = ui, server = server)


#GitHUB access
#rsconnect::deployApp("Universidad/2023-I Brasil/SeriesTemp/Projeto/TrabalhoFinal")
#options(rsconnect.max.bundle.size = 3145728000)
#runGitHub( "SeriesTemporaisUnicamp", "JuanPabloMonDi")

