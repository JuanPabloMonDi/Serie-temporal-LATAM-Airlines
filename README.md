# Trabalho Final Series Temporais Unicamp 2023-I
Nesse repositório ficam todos os arquivos usados no trabalho final da disciplina ME607-Series Temporais 2023-1 na UNICAMP

O repositório tem os seguintes arquivos

* Cross-Validation contem os resultados da validação cruzada dos 13 modelos construidos. 
* Funcoes.R é uma pequeno código de R que define uma função para mostrar várias graficas e a função construida para realizar a validação cruzada
* Modelos.R basicamente é o esqueleto do trabalho, o código contem a análise descritiva da série, a construção dos modelos, a validação cruzada (mas esta marcada com #) e o código para fazer as previsçoes
* Predict.csv são as previsões feitas diariamente por github actions
* App.R é a app do shiny
* Plots.pdf são algunas graficas feitas pelo código Modelos.R


## Sobre as predições.

De tercas a sabados, o código vai ser executado uma vez por dia às 23:30 (Hora Brasilia). O modelo gera uma predição sem contar a ultima data da base de dados. A ideia é comparar esse valor com nossa predição.

Por exemplo, o dia 23 de junho o modelo preverá o valor para o mesmo dia, utilizando os dados até o día 22 de junho.

### Nota: 
Nas predições algumas datas vão se repetir, a razão disso é pelos feriados do pais da empresa escolhida, nesse caso foram escolhidas as ações da aerolinea LATAM em Chile. 

Por exemplo, os dias 21 e 26 de junho foram feriados em Chile, portanto, o modelo não gerou predições para esses dias, mas forneceu uma nova predição para o ultimo dia que foi gerado um novo dato.
