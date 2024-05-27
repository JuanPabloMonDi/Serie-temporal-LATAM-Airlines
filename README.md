# Análise dos retornos da companhia aérea Latam-Chile - Trabalho Final Series Temporais Unicamp 2023-I
Nesse repositório ficam todos os arquivos usados no trabalho final da disciplina ME607-Series Temporais 2023-1 na UNICAMP. No trabalho foi realizada uma análise dos efeitos da pandemia sobre as ações da companhia aérea LATAM na bolsa chilena. A análise foi conduzida de forma estatística, comparando análises descritivas e modelos desenvolvidos para a série, tanto excluindo como incluindo o período pós-pandemia.

O repositório tem os seguintes arquivos:

* Cross-Validation contem os resultados da validação cruzada dos 13 modelos construidos. 
* Funcoes.R é uma pequeno código de R que define uma função para mostrar várias graficas e a função construida para realizar a validação cruzada
* Modelos.R basicamente é o esqueleto do trabalho, o código contem a análise descritiva da série, a construção dos modelos, a validação cruzada (mas esta marcada com #) e
* Predicoes.R  o código para fazer as previsões
* Predict.csv são as previsões feitas diariamente por github actions
* App.R é a app do shiny

O Aplicativo Shiny pode ser executado no software R usando a função runGitHub() do pacote Shiny. O comando a executar é runGitHub("SeriesTemporaisUnicamp","JuanPabloMonDi").



> [!IMPORTANT]
> É importante ressaltar que todas as informações e análises aqui apresentadas são estritamente para fins acadêmicos e não devem ser interpretadas como recomendações de investimento ou análise financeira. Da mesma forma, os códigos e modelos apresentados não buscam comercializar nem perjudicar à companhia aerea LATAM.



## Sobre as predições.

De segundas a sextas, o código vai ser executado uma vez por dia às 23:30 (Hora Brasilia). O modelo gera uma predição sem levar em conta a ultima data da base de dados. A ideia é comparar esse valor com nossa predição.

Por exemplo, o dia 23 de junho o modelo preverá o valor para o mesmo dia, utilizando os dados até o día 22 de junho.

> [!NOTE]
> Nas predições algumas datas vão se repetir, a razão disso é pelos feriados do pais da empresa escolhida, nesse caso foram escolhidas as ações da aerolinea LATAM em Chile. 

>Por exemplo, os dias 21 e 26 de junho foram feriados em Chile, portanto, o modelo não gerou predições para esses dias, mas forneceu uma nova predição para o ultimo dia que foi gerado um novo dato.
