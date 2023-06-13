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
library(httr)
library(jsonlite)
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
library(flexdashboard)
library(knitr)
library(DT)
library(yfR)
library(shiny)
library(shinythemes)
#source("Codigo R.R")

ui <- fluidPage( #shinythemes::themeSelector(), #Para mudar de tema sem correr o código várias vezes
  theme = shinytheme("journal"), #Candidatos, Journal, Sanstone, Yeti e Cosmo
  #navlistPanel(
  navbarPage(
    "MENU!",
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
             tabPanel("Analise Descriptivo dos Dados",
                      helpText("AYUDA"),
                      h1("Analise descriptivo dos dados",align="center"),
                      h2("Considerando a pandemia na serie"),
                      p("Texto introductorio donde hablo de que la serie puede ser muy voltail y dar modelos malos considerando la pandemia"),
                     
                       tabsetPanel(
                        tabPanel("Autocorrelação",
                                 sliderInput(inputId = "Lags",
                                             label = "Numero de lags:",
                                             min = 1,
                                             max = 50,
                                             value = 30),
                                 plotOutput("autocorrelacao")),
                        tabPanel("Autocorrelação parcial",
                                 sliderInput(inputId = "LagsP",
                                             label = "Numero de lags:",
                                             min = 1,
                                             max = 50,
                                             value = 30),
                                 plotOutput("par_autocorrelacao")),
                        tabPanel("Decomposicao",
                                 plotOutput("Decomposicao1")),
                        tabPanel("Ljung-Box",
                                 sliderInput(inputId = 'BoxLags',label="Lags para o teste", min=10, max=100, value=15),
                                 verbatimTextOutput("Testes")),
                        tabPanel("Estacionaridade",
                                 sliderInput(inputId = 'DickeyLags',label="Lags para o teste", min=10, max=100, value=15),
                                 verbatimTextOutput("Dickey")),
                        
                        ),
                      h2("Sem considerar a pandemia na serie"),
                      p("Aqui escribo de las ventas de no considerar la pandemia en el modeleja de la serie"),
                      tabsetPanel(
                        tabPanel("Autocorrelação",
                                 sliderInput(inputId = "Lags",
                                             label = "Numero de lags:",
                                             min = 1,
                                             max = 50,
                                             value = 30),
                                 plotOutput("autocorrelacaoPande")),
                        tabPanel("Autocorrelação parcial",
                                 sliderInput(inputId = "LagsP",
                                             label = "Numero de lags:",
                                             min = 1,
                                             max = 50,
                                             value = 30),
                                 plotOutput("par_autocorrelacaoPande")),
                        tabPanel("Decomposicao",
                                 plotOutput("Decomposicao1Pande")),
                        tabPanel("Ljung-Box",
                                 sliderInput(inputId = 'BoxLags',label="Lags para o teste", min=10, max=100, value=15),
                                 verbatimTextOutput("TestesPande")),
                        tabPanel("Estacionaridade",
                                 sliderInput(inputId = 'DickeyLags',label="Lags para o teste", min=10, max=100, value=15),
                                 verbatimTextOutput("DickeyPande")),
                        
                      ),
                      sidebarPanel(position="right",
                        p("Aqui van los deslizadores"),
                        p("Aqui van los deslizadores"),
                         p("Aqui van los deslizadores"),
                        p("Aqui van los deslizadores"),
                        p("Aqui van los deslizadores"),
                        sliderInput(inputId = '1',label="Lags para o teste", min=10, max=100, value=15),
                        sliderInput(inputId = '2',label="Lags para o teste", min=10, max=100, value=15),
                        
                        ),
                      ),# esse daqui fecha a janela de analise descritivo
             tabPanel("Modelos",
                      p()),
             navbarMenu("Availação dos modelos",
                        tabPanel("A partir dos residuais",
                                 p("texto de prueba"),
                                 #splitLayout(
                                #   DT::datatable(cv_pospandemia),
                                #   DT::datatable(cv_results))
                                # ), 
                                 ),
                        tabPanel("Desempenho fora da amostra",
                                 DT::datatable(cv_pospandemia),
                                 DT::datatable(cv_results))
                      ),
             tabPanel("Predições",
                      p()),
             navbarMenu("Sobre o site web",
                        tabPanel("O trabalho",
                                 p("mi nombre"),
                                 DT::datatable(cv_results)
                        )
             )
)
)
# Define server logic required to draw a histogram
Box.test(retornos^2, type = "Ljung-Box", lag = 10)
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
    
    output$autocorrelacao<- renderPlot({
      p1<-ggAcf(retornos_c, lag.max = input$Lags)+ggtitle("Autocorrelação da série")
      p2<-ggAcf(retornos_c^2, lag.max = input$Lags)+ggtitle("Autocorrelação dos cuadrados série")
      multiplot(p1,p2,cols=2)
    })
    output$autocorrelacaoPande<- renderPlot({
      p1<-ggAcf(retornos_c_antes_pandemia, lag.max = input$Lags)+ggtitle("Autocorrelação sem pandemia")
      p2<-ggAcf(retornos_c_antes_pandemia^2, lag.max = input$Lags)+ggtitle("Autocorrelação dos cuadrados sem pandemia")
      multiplot(p1,p2,cols=2)
    })
    output$par_autocorrelacao<- renderPlot({
      p1<-ggPacf(retornos_c, lag.max = input$LagsP)+ggtitle("Autocorrelação parcial sem pandemia")
      p2<-ggPacf(retornos_c^2, lag.max = input$LagsP)+ggtitle("Autocorrelação parcial dos cuadrados sem pandemia")
      multiplot(p1,p2,cols=2)
    })
    output$par_autocorrelacaoPande<- renderPlot({
      p1<-ggPacf(retornos_c_antes_pandemia, lag.max = input$LagsP)+ggtitle("Autocorrelação parcial da série sem pandemia")
      p2<-ggPacf(retornos_c_antes_pandemia^2, lag.max = input$LagsP)+ggtitle("Autocorrelação parcial dos cuadrados da série sem pandemia")
      multiplot(p1,p2,cols=2)
    })
    output$Decomposicao1<- renderPlot({
      p1<-drop_na(data.frame(b$trend))|> ggplot()+geom_line(aes(x=seq_len(nrow(drop_na(data.frame(b$trend)))),y=b.trend))+ylab("Tendência")+xlab("")
      p2<-drop_na(data.frame(b$seasonal))|> ggplot()+geom_line(aes(x=seq_len(nrow(drop_na(data.frame(b$seasonal)))),y=b.seasonal))+ylab("Sazonalidade")+xlab("")
      p3<-drop_na(data.frame(b$random))|> ggplot()+geom_line(aes(x=seq_len(nrow(drop_na(data.frame(b$random)))),y=b.random))+ylab("Aleatorio")+xlab("Tempo")
      
      multiplot(p1,p2,p3,cols=1)
    })
    output$Decomposicao1Pande<- renderPlot({
      p1<-drop_na(data.frame(b2$trend))|> ggplot()+geom_line(aes(x=seq_len(nrow(drop_na(data.frame(b2$trend)))),y=b2.trend))+ylab("Tendência")+xlab("")
      p2<-drop_na(data.frame(b2$seasonal))|> ggplot()+geom_line(aes(x=seq_len(nrow(drop_na(data.frame(b2$seasonal)))),y=b2.seasonal))+ylab("Sazonalidade")+xlab("")
      p3<-drop_na(data.frame(b2$random))|> ggplot()+geom_line(aes(x=seq_len(nrow(drop_na(data.frame(b2$random)))),y=b2.random))+ylab("Aleatorio")+xlab("Tempo")
      
      multiplot(p1,p2,p3,cols=1)
    })
    output$Testes<- renderPrint({
      Box<-Box.test(retornos_c,"Ljung-Box",lag=input$BoxLags)
      Box2<-Box.test(retornos_c,"Box-Pierce",lag=input$BoxLags)
      print(Box)
      print(Box2)})
    
    output$TestesPande<- renderPrint({
      Box<-Box.test(retornos_c_antes_pandemia,"Ljung-Box",lag=input$BoxLags)
      Box2<-Box.test(retornos_c_antes_pandemia,"Box-Pierce",lag=input$BoxLags)
      print(Box)
      print(Box2)})
    
    output$Dickey<- renderPrint({
      test_estacionaridade<-adf.test(retornos_c,alternative="stationary",k=input$DickeyLags)
      print(test_estacionaridade)
    })
    output$DickeyPande<- renderPrint({
      test_estacionaridade<-adf.test(retornos_c_antes_pandemia,alternative="stationary",k=input$DickeyLags)
      print(test_estacionaridade)
    })
  
}

# Run the application 
shinyApp(ui = ui, server = server)


#GitHUB access
