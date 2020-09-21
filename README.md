# Projeto pardas do Tietê #

Projeto do Centro Nacional de Predadores, em parceria com a o Instituto pró-carnivoros e financiado
pela AES tietê.

Este projeto visa identificar áreas adequadas para preservação da espécie na região da bacia do Rio Tietê. 

## Estrutura do projeto ##


O projeto é organizado em experimentos. Experimentos são sequenciais, i.e. o último experimento representa a ultima analise e é o resultado da ultima versão do código. Experimentos anteriores são mantidos porque o processo de recriar os dados é bastante custoso computacionalmente, e são muito pesados. Todos os experimentos tem uma subpasta para receber dados intermediarios, e.g. '/experiment 001'. 

Cada experimento tem duas subpastas '/mapsderived' para representar transformações nos mapas de dados
e '/data derived' que representa transformações nos dados de localização dos animais. './mapsderived por sua vez tem as subpastas '/observedstack' pra representar as variaveis ambientais ao redor das localizações dos animais. '/studyarea' representa as variaveis ambientais dentro da área de estudo. Os dois mapas não são semelhantes porque muitos animais saem para fora da área de estudo. 

O projeto também tem uma pasta '/raw' que contem uma subpasta para cada um dos grupos de dados coletados para cada animal identificados pela data de acquisição e.g. '/data 17.12.19'. Também temos uma pasta para armazenar os mapas conforme ele foram baixados '/maps'.

## Descrição dos experimentos ##

- experimento 1: Baseado nos dados de dezembro de 2019. Não tem predições espaciais, ou os multiplos modelos.
- experimento 2: Baseado nos dados de julho de 2019. Prove predições espaciais em ressolução menor (5000m).
- experimento 3: Ainda baseado nos dados de dezembro de 2019, mas utiliza resolução melhor (30m).

## Descrição dos arquivos de código ##

- main.r: contem a função principal que chama todos as outras.
- data importer.r: contem a função 'data.importer()' que recebe arquivos xlsx de diferentes individuos, os filtra, e soma todas as informações uma geodatabase (gpkg)
- envpreparator.r: contem a função 'envpreparator()', que calcula as variaveis ambientais para uma determinada região. Chamada por 'data.importer()', e por main.r.
- HMMfitter.r: calcula o que é dispersante e residente para nossas analises usando uma Hidden Markov Chain.
- lightsff.r: tem a função 'SSFer()' que rodas os modelos de step selection function contidos em modelslist.r. Também contêm os testes de AUC de cada modelo, e seleciona os principais modelos.
- predictor.r: tem a função 'predictor()' que converte o modelo em projeção espaciais para a area de estudo, usando um mapa de variaveis ambientais calculados por main.r.
- report figures.r: Prepara figuras para relatórios trimestrais.
- acessory functions.r: contem varias pequenas funções, a maior parte chamada por 'SSFer()'.




