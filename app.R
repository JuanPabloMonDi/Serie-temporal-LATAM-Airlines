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
source("Modelos.R")
library(shinyjs)

#library(shinyWidgets)

ui <- fluidPage(title="Trabalho Juan Pablo Montaño",
                #shinythemes::themeSelector(), #Para mudar de tema sem correr o código várias vezes
  theme = shinytheme("journal"), #Candidatos, Journal, Sanstone, Yeti e Cosmo
  #navlistPanel(
  navbarPage(
    #"Voos LATAM",
    img(src = "UNICAMP2.png", height=38,width=50, align="right"),
    #img(src = "latam-airlines.png", height=30,width=80, align="left"),
    
             tabPanel("Introdução",
                      # Application title
                      #titlePanel("Series Temporais- Trabalho final"),
                      
                      # Sidebar with a slider input for number of bins 
                      sidebarLayout( position = "right",
                                     sidebarPanel( width=4,
                                                   h3("Trabalho Series Temporais",align='left'),
                                                   p("Feito por ",strong("Juan Pablo Montaño,RA:241656"),
                                                     style = "font-family: 'Bahnschrift'; font-si20pt"),
                                                   hr(),
                                                   p("Neste trabalho, foi realizada uma análise dos efeitos da pandemia sobre
                                                     as ações da companhia aérea LATAM na bolsa chilena. 
                                                     A análise foi conduzida de forma estatística, comparando análises
                                                     descritivas e modelos desenvolvidos para a série, tanto excluindo
                                                     como incluindo o período pós-pandemia",
                                                   style = "font-family: 'Bahnschrift'; font-si20pt"),
                                                   h3("Fonte dos dados",  align = "left"),
                                                   p("Os dados foram obtidos através de site web",
                                                     tags$a(href="https://finance.yahoo.com/quote/LTM.SN?p=LTM.SN&.tsrc=fin-srch", "Yahoo Finance"),
                                                     "usando o pacote ",code("yfR"),"de R.",
                                                     style = "font-family: 'Bahnschrift'; font-si20pt"),
                                                   img(src = "UNICAMP_logo.png", height=70,width=80, align="right"),
                                                   br(),
                                                   br(),
                                                   br(),
                                                   br(),
                                                   ),
                                     
                      # Show a plot of the generated distribution
                      mainPanel(width=8,
                                br(),
                                h1("Análise dos retornos da companhia aérea Latam-Chile",  align = "center"),
                                h2("Motivação"),
                                p("O mercado das companhias aéreas foi um dos mais afetados pela pandemia de COVID-19,
                                levando muitas companhias aéreas a enfrentarem crises econômicas e até mesmo falências.
                                Nesse contexto, o objetivo deste estudo é desenvolver um modelo para analisar os retornos
                                obtidos na bolsa chilena pela companhia aérea LATAM. Será observado como esses retornos 
                                foram afetados pela COVID-19, buscando modelar os retornos considerando o período da 
                                pandemia e também supondo um cenário em que a pandemia não tivesse ocorrido.
                                  Por fim, será avaliado se esses modelos são válidos e eficientes em suas previsões",
                                  style = "font-family: 'Bahnschrift'; font-si20pt"),
                  
                                br(),
                                h3("Preços e retornos de LATAM-Chile"),
                                fluidRow(column(width=3, align="left",
                                                hr(),
                                dateRangeInput('datasRango',
                                               label = paste('Ensire o intervalo de tempo que deseja graficar, a data minima é 1 de janeiro do 2013'),
                                               start = as.Date(data_ini), end = Sys.Date() -1,
                                               min = as.Date(data_ini), max = Sys.Date()-1,
                                               separator = " - ", format = "dd/mm/yy",
                                               startview = 'year', language = 'pt', weekstart = 1
                                ),
                                hr(),
                                br(),
                                img(src = "latam-airlines.png", height=60,width=140, align="left"),
                                
                                ),
                                column(width=9,
                                tabsetPanel(
                                  tabPanel("Preço",
                                            plotOutput("Price",height =300)),
                                  tabPanel("Retorno",
                                           plotOutput("Retorno",height =300))
                                ))),
                                
                      ),
                    
                                  )
                      ),
    tabPanel("Análise descritivo",
             # SidebarLayout para dividir a pagina em duas partes
             sidebarLayout( position = "left",
                            sidebarPanel( h2("Análise descritivo dos dados",  align = "left"),
                                         p("É possível evidenciar a volatilidade da série causada pela COVID-19
                                           por meio da análise descritiva da série antes e depois da pandemia.",
                                           style = "font-family: 'Bahnschrift'; font-si15pt; color: 'black';"),
                                         
                                           radioButtons(inputId = "SemPandemia" , 
                                                       label = "Incluir a pandemia?",
                                                       choices = c("Não"="Sem pandemia", "Sim"="Com pandemia")),
                                                       sliderInput(inputId = "Lags",
                                                                   label = "Numero de lags:",
                                                                   min = 1,
                                                                   max = 50,
                                                                   value = 30),
                                          fluid=TRUE,
                                          width=3,
                                          ),
                            mainPanel(
                                      fluid=TRUE,
                                      width=9,
                                      tabsetPanel(
                                        tabPanel("Autocorrelação",
                                                 h3("ACF dos retornos e dos quadrados dos retornos"),
                                                 plotOutput("Analise"),
                                                 h4("Observações"),
                                                 div("A função", code("ggplot2::ggAcf"), "mostra as autocorrelações sem considerar o valor", code("k=0"),".",
                                                   style = "font-family: 'Bahnschrift'; font-si20pt"),
                                                 p("Ao olhar os valores de autocorrelação (ACF) antes e depois da pandemia, 
                                                 torna-se evidente como o comportamento autorregressivo da série foi afetado.
                                                 Antes da pandemia, a série apresentava apenas autocorrelação no primeiro intervalo de defasagem.
                                                 No entanto, após a pandemia, observa-se uma volatilidade significativa 
                                                   nesse padrão de correlação ao longo dos diferentes lags. ",
                                                   style = "font-family: 'Bahnschrift'; font-si20pt; color: 'black';"),
                                        p("Em relação aos quadrados dos retornos, é possível observar uma correlação
                                        entre eles, o que indica a possibilidade de a série apresentar heterocedasticidade.
                                        Nesse caso,se poderia considerar um modelo GARCH para abordar essa característica da série. ",
                                          style = "font-family: 'Bahnschrift'; font-si20pt; color: 'black'"),
                                      ),
                                        tabPanel("Autocorrelação parcial",
                                                 h3("PACF dos retornos e dos quadrados dos retornos"),
                                                 plotOutput("par_autocorrelacao"),
                                                 h4("Observações"),
                                                 p("Os valores das autocorrelações parciais também evidenciam 
                                                 a volatilidade causada pela pandemia. Anteriormente, esses valores eram não significativos. No entanto, após a pandemia, observam-se valores significativos,
                                                   inclusive em defasagens distantes, como o lag 28.",
                                                 style = "font-family: 'Bahnschrift'; font-si20pt;color:'black'"),
                                                 p("Entretanto, o quadrado dos retornos exibe valores significativos de autocorrelação parcial em ambos os casos.",
                                                   style = "font-family: 'Bahnschrift'; font-si20pt;color:'black'"),),
                                        tabPanel("Decomposição",
                                                 h3("Decomposição da série"),
                                                 plotOutput("Decomposicao1"),
                                                 h4("Observações"),
                                                 p("Embora o gráfico indique a presença de sazonalidade na série, 
                                                   ao analisar a amplitude dos valores sazonais, temos que esta muito próxima
                                                   a 0. Logo, é possivel que a sazonalidade identificada pode ser gerada
                                                   artificialmente pelo código e por tanto, pode ser descartada de nosso modelo.",
                                                   style = "font-family: 'Bahnschrift'; font-si20pt;color:'black'"),),
                                        tabPanel("Ljung-Box",
                                                h3("Testes de Ljung-Box e Box-Pierces"),
                                                verbatimTextOutput("Testes"),
                                                h4("Observações"),
                                                p("Os resultados dos tests de Ljung-Box e Box-Pierce confirmam o que foi visto nas 
                                                  autocorrelações e autocorrelações parciais. Assim, é necesario modelar a série como um modelo
                                                  ARIMA(p,d,q) antes de modelar a variância",
                                                  style = "font-family: 'Bahnschrift'; font-si20pt;color:'black'"),),
                                        tabPanel("Estacionaridade",
                                                 h3("Teste de Dickey-Fuller"),
                                                 verbatimTextOutput("Dickey"),
                                                 h4("Observações"),
                                                 p("O teste de Dickey-Fuller pode ajudar a identificar se a 
                                                 série precisa ser diferenciada ou não. Os p-valores obtidos para
                                                 os dois casos indicam que a série é estacionária. 
                                                 No entanto, o comportamento observado na decomposição da tendência mostra o contrário.
                                                   Portanto, consideraremos ambos os casos durante a construção do modelo.",
                                                   style = "font-family: 'Bahnschrift'; font-si20pt;color:'black'"),),
                                      
                                        
                                      ), #Fecha o tabsetPanel
                            ), #Fecha o main Panel
    ), #Fecha o sidelayout
    ), #Fechar a secção de análise descritivo
    
             tabPanel("Modelos",
                      sidebarLayout( position = "left",
                                     sidebarPanel( h2("Modelos",  align = "left"),
                                                   h5("Os modelos de 1 a 7 não consideram a pandemia,
                                                   enquanto os modelos de 8 a 13 levam em conta a presença da pandemia."),
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
                          column(width = 12, align="left",
                            p("A partir da análise exploratória, foram construidos
                            vários modelos para modelar os retornos da empresa considerando
                            e não considerando a pandemia. A seguir, apresentam-se os graficos e testes de avaliação
                              dos residuos obtidos nos 13 modelos construidos. Para algums modelos pos-pandemia, foi adicionada uma variável dummy X_t, que representa
                              se a data do dado da série é anterior o posterior à pandemia (considerando que a pandemia começou no dia 1 de janeiro de 2020).",
                              style = "font-family: 'Bahnschrift'; font-si20pt;color:'black'"))),
                            br(),
                          fluidRow(
                            column(
                              width = 12,
                              align="left",
                            selectInput(inputId="ListaModelo",
                                        label="Selecione o gráfico dos residuos que deseja visualizar",
                                        choices=c("ACF","PACF","Histograma","Q-Q plot","Raizes do polinômio")),
                            ))),
                        hr(),
                        fluidRow( 
                          column(width = 12,
                                 plotOutput("Plots",height = 300)),
                        ),
                        hr(),
                        wellPanel(
                        fluidRow(column(
                          width = 12,
                                 p("A seguir, estão os resultados dos testes de Ljung-Box,
                                   Box-Pierce e os critérios de avaliação, como verossimilhança e AIC.",
                                   style = "font-family: 'Bahnschrift'; font-si20pt;color:'black'")),
                          fluidRow(column(width = 12,
                                          selectInput(inputId="Lista2Modelo",
                                                      label="Seleccione o test dos residuos que deseja realizar",
                                                      choices=c("Test Ljung-Box","Test Box-Pierce","Resumo do modelo")))),
                          )),
                        fluidRow(column(width = 12,
                                        verbatimTextOutput('Plots2')
                          ),
                          ),
                      ))),
                      ), #Fecha modelos
    tabPanel("Desempenho fora da amostra",
             fluidRow(
               column(width = 4,
             h2("Validação cruzada"),
             p("Além da avaliação dos resíduos, o desempenho dos modelos fora da amostra
               também foi avaliado por meio de validação cruzada, usando o método de ",em("Expanding Windows."),
               " Os resultados de erro quadrático médio e erro absoluto médio são apresentados na seguinte tabela.",
               style = "font-family: 'Bahnschrift'; font-si20pt;color:'black'"),
             h5("Mostrar os resultados dos modelos considerando ou não considerando a pandemia?"),
             radioButtons(inputId = "cv" , 
                          label = NULL,
                          choices = c("Não incluir pandemia"="Sem pandemia", "Incluir pandemia"="Com pandemia")),
             p("Por fim, foram selecionados os modelos que apresentaram melhor desempenho na avaliação dos resíduos
             e na validação cruzada (em termos de RMSE). Esses modelos são
               o MA(1)-GARCH(3,2) para o caso sem pandemia e o modelo ARIMAX(1,1,1) 
               considerando a pandemia.",
               style = "font-family: 'Bahnschrift'; font-si20pt;color:'black'"),
               ),
             column(width = 8,dataTableOutput("crossv"))),
             #DT::datatable(cvPrepandemia),
             #DT::datatable(cvPospandemia)),
             
    ),
             tabPanel("Predições",
                      p("Uma vez foram escolhidos os modelos que vão ser utilizados para fazer as predições,
                        colocamos o modelo em produção no site web de Github. O modelo vai gerar uma nova predição
                        diariamente de segundas a sextas. (O modelo pode repetir predições nos dias feriados de Chile).",
                        style = "font-family: 'Bahnschrift'; font-si20pt;color:'black'"),
                      br(),
                      p("No caso que as predições não estem atualizadas, podem ser consultadas ",
                        tags$a(href="https://github.com/JuanPabloMonDi/SeriesTemporaisUnicamp/blob/main/Predict.csv", "aqui"),".",
                        style = "font-family: 'Bahnschrift'; font-si20pt"),
                      
                      DT::datatable(Predict),
                      h3("Conclusões do trabalho"),
                      p("Como conclusão da análise, pode-se afirmar que o modelo selecionado para o
                        período pré-pandemia atende (ou está muito próximo de atender) aos
                        critérios de avaliação, tornando-se assim um modelo válido, 
                        cujas previsões refletem o comportamento dos retornos em um contexto em que a pandemia nunca ocorreu.",
                        style = "font-family: 'Bahnschrift'; font-si20pt;color:'black'"),
                      p("Com relação ao modelo escolhido para o período pós-pandemia, embora seja o melhor modelo para esse período, ele não 
                      satisfaz os critérios para ser considerado válido, pois não captura adequadamente o comportamento 
                      dos retornos. Isso não é surpreendente, uma vez que a pandemia foi um evento totalmente inesperado
                      e teve um impacto muito alto na série. No entanto, dependendo do contexto, pode ser importante
                      incluí-la ou não na série, pois pode ajudar a avaliar os efeitos da pandemia nos valores futuros,
                        além de estabelecer novos padrões nos dados e modelar os dados em um contexto atual, nesse sentido, é importante desenvolver outros modelos usando metodologias diferentes para tentar ter melhores resultados com os dados pos-pandemia",
                        style = "font-family: 'Bahnschrift'; font-si20pt;color:'black'"),),
    
    tabPanel("Sobre o site web",
             fluidRow(column(width = 6,
                             h2("Sobre o trabalho"),
                             p("Esse site web trata-se do trabalho final da disciplina ME607- Series Temporais da
                               Universidade Estadual de Campinas (UNICAMP) no primer semestre do ano 2023. O aluno responsável desse trabalho
                               é Juan Pablo Montaño, com RA:241656.",
                               style = "font-family: 'Bahnschrift'; font-si20pt;color:'black'")),
                               p("É importante ressaltar que todas as informações e análises aqui apresentadas são estritamente para fins acadêmicos
                               e não devem ser interpretadas como recomendações de investimento ou análise financeira. Da mesma forma, os códigos e
                               modelos apresentados não buscam comercializar nem perjudicar à companhia aerea LATAM.",
                                 style = "font-family: 'Bahnschrift'; font-si20pt;color:'black'"))),
                      column(width = 6,
                             img(src = "UNICAMP_logo.png", height=140,width=160, align="right"),
                             ))
             ,
                                 
                        
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
   
    
    output$Plots<- renderPlot({
      mod<-modelos[[as.numeric(input$Modelo)]]
      if (input$ListaModelo=="ACF"){
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
      }
      
      if (input$ListaModelo=="PACF"){
        if (class(mod)=="uGARCHfit"){
          p1<- ggPacf(mod@fit$residuals/mod@fit$sigma)+ggtitle("Autocorrelação parcial dos residuos do modelo")
          p2<- ggPacf((mod@fit$residuals/mod@fit$sigma)^2)+ggtitle("Autocorrelação parcial dos quadrados dos residuos do modelo")
          multiplot(p1,p2,cols = 2)
        }
        if (class(mod)=="Arima"){
          p1<-ggPacf(mod$residuals)+ggtitle("Autocorrelação parcial dos residuos do modelo")
          p2<-ggPacf(mod$residuals^2)+ggtitle("Autocorrelação parcial dos quadrados dos residuos do modelo")
          multiplot(p1,p2,cols = 2)
        }
      }
      
      if (input$ListaModelo=="Histograma"){
        if (class(mod)=="uGARCHfit"){
          plot(mod,which=8)
        }
        if (class(mod)=="Arima"){
          hist(mod$residuals, freq = FALSE,xlab="Residuo", ylab="Probabilidade",main="Histograma dos residuais do modelo")
          lines(density(rnorm(100000,0,sqrt(mod$sigma2))),lw=3)
        }
      }
      
      if (as.character(input$ListaModelo)=="Q-Q plot"){
        if (class(mod)=="uGARCHfit"){
          plot(mod,which=9)
        }
        if (class(mod)=="Arima"){
          qqnorm(mod$residuals,col="lightblue4")
          qqline(mod$residuals)
        }
      }
      if (as.character(input$ListaModelo)=="Raizes do polinômio"){
        if (class(mod)=="Arima"){
          plot(mod)
        }
        if ((as.numeric(input$Modelo) %in% c(6,7))==TRUE){
          plot(modelos[[1]])
        }
        if (as.numeric(input$Modelo)== 13){
          plot(modelos[[10]])
        }
        if (as.numeric(input$Modelo)==12){
          plot(modelos[[8]])
        }
      }
      
      output$crossv<-renderDataTable({
        if ((as.character(input$cv))=="Com pandemia"){
          dados=cvPospandemia}else{dados=cvPrepandemia}
      })
    })
    
      
      
      #Test de JungBox dos residuais
      output$Plots2<- renderPrint({
        mod<-modelos[[as.numeric(input$Modelo)]]
        if (input$Lista2Modelo=="Test Ljung-Box"){
          if (class(mod)=="uGARCHfit"){
            Box<-Box.test(mod@fit$residuals/mod@fit$sigma,type="Ljung-Box",lag=20)
            
            print(Box)
            
          }
          if (class(mod)=="Arima"){
            Box<-Box.test(mod$residuals,type="Ljung-Box",lag=20)
            
            print(Box)
            
          }
        }
        if (input$Lista2Modelo=="Test Box-Pierce"){
          if (class(mod)=="uGARCHfit"){
            Box2<-Box.test(mod@fit$residuals/mod@fit$sigma,type="Box-Pierce",lag=20)
            
            print(Box2)
          }
          if (class(mod)=="Arima"){
            
            Box2<-Box.test(mod$residuals, type="Box-Pierce",lag=20)
            
            print(Box2)
          }
        }
        if (input$Lista2Modelo=="Resumo do modelo"){
          print(mod)
        }
      })
      
   
}
# Run the application 
shinyApp(ui = ui, server = server)


#GitHUB access
#rsconnect::deployApp("Universidad/2023-I Brasil/SeriesTemp/Projeto/TrabalhoFinal")
#options(rsconnect.max.bundle.size = 3145728000)
#runGitHub( "SeriesTemporaisUnicamp", "JuanPabloMonDi")

