#################### Carregando pacotes ####################

if(!require(dplyr))
  install.packages("dplyr")
library(dplyr)

if(!require(psych))
  install.packages("psych")
library(psych)


############### Carregando o banco de dados ###############


dados <- read.csv('enem.csv', sep = ';', dec = ',', stringsAsFactors = T)

dados <- read.csv2('enem.csv', stringsAsFactors = T)

############### Visualizando o banco de dados ###############

View(dados)
glimpse(dados)


###### GERANDO HISTOGRAMA ######
hist(dados$NOTA_ENEN, breaks = 14, col = "green", xlab = "M�dia no ENEM 2019", ylab = "Frequ�ncia", main = "Histograma com a frequ�ncia das notas - ENEM 2019")

