# Projeto pardas do Tietê #

Projeto do Centro Nacional de Predadores, em parceria com a o Instituto pró-carnivoros e financiado
pela AES tietê.

Este projeto visa identificar áreas adequadas para preservação da espécie na região da bacia do Rio Tietê. 

## Estrutura do projeto ##


O projeto é organizado em experimentos. Experimentos são sequenciais, i.e. o último experimento representa a ultima analise e é o resultado da ultima versão do código. Experimentos anteriores são mantidos porque o processo de recriar os dados é bastante custoso computacionalmente, e são muito pesados. Todos os experimentos tem uma subpasta para receber dados intermediarios, e.g. '/experiment 001'. 

Cada experimento tem duas subpastas '/maps derived' para representar transformações nos mapas de dados
e '/data derived' que representa transformações nos dados de localização dos animais. './maps derived por sua vez tem as subpastas '/observedstack' pra representar as variaveis ambientais ao redor das localizações dos animais. '/studyarea' representa as variaveis ambientais dentro da área de estudo. Os dois mapas não são semelhantes porque muitos animais saem para fora da área de estudo. 

O projeto também tem uma pasta '/raw' que contem uma subpasta para cada um dos grupos de dados coletados para cada animal identificados pela data de acquisição e.g. '/data 17.12.19'. Também temos uma pasta para armazenar os mapas conforme ele foram baixados '/maps'.

## Descrição dos experimentos ##

- experimento 1: Baseado nos dados de julho de 2019. Não predições espaciais, ou os multiplos modelos.
- experimento 2: Baseado nos dados de julho de 2019. Prove predições espaciais em ressolução menor (5000m).
- experimento 3: Ainda baseado nos dados de julho de 2019, mas utiliza resolução melhor (30m).




