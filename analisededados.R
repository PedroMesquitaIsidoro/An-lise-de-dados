#Trabalho Probabilidade e Estatística Lívia Soares e Pedro Henrique Isidoro
#Lendo o arquivo em formato .csv

ENEM <- read.csv2('ENEM2.csv' , sep=',' , dec=',')

#Chamar alguns pacotes para serem usados
library(psych)
library(gridExtra)
library(formattable)
library(dplyr)

#QUESTÃO 01
#Criar um boxplot com as NOTA_ENEM
boxplot(ENEM$NOTA_ENEN,col = "pink", ylab = "Média no ENEM 2019", main = "Boxplot NOTAS ENEM 2019")

# Tabela de frequências:

#Criação da tabela frequência relativa
FreqRel <- table(cut(ENEM$NOTA_ENEN, seq(300, 800, l = 6)))
FreqRel
#Criação da tabela frequência absoluta
FreqAbs <-prop.table(FreqRel)
FreqAbs
FreqAbs <-percent(c(FreqAbs))
FreqAbs

#Criar uma tabela com as frequências
Tabela_frequencias <- data.frame(
  Frequencia_Relativa = c(FreqRel),
  Frequencia_Absoluta =  c(FreqAbs))
#Exibir a tabela
Tabela_frequencias
#Plotar a tabela
formattable(Tabela_frequencias)

#Criar um histograma com as NOTA_ENEM
#800 - 300 = 500 
#500 / 10 = 50

hist(ENEM$NOTA_ENEN, breaks = 50, 
     col = "green",xlab = "Média geral", 
     ylab = "Frequência", 
     main = "Histograma com a frequência das notas - ENEM 2019")


#Gerar um gráfico de barras com as NOTA_ENEN agrupado pelos quartis e sexo e interpretar os valores.

#Gera os quartis da das notas
Quartil_Nota <- c(quantile(ENEM$NOTA_ENEN))
Quartil_Nota

#Transformar Feminino em 1 e masculino em 0 para uma melhor manipulação dos dados
GENERO <- ENEM$NOTA_ENEN
for(i in 1:67192){
  if(ENEM$TP_SEXO[i] == 'Feminino'){
    GENERO[i] = 1
  }else{
    GENERO[i] = 0
  }
}
#Criar variáveis para armazenar a quantidade feminina(m) e masculina (n) em cada quartil
m1 = 0
m2 = 0
m3 = 0
m4 = 0
n1 = 0
n2 = 0
n3 = 0 
n4 = 0
#Calcular a quantidade de cada um 
for(i in 1:67192){
  if(i < 16798){
    if(GENERO[i] == 1){m1 = m1 + 1}
    else{n1 = n1 + 1}
    }
  else{if(i < 33596){
      if(GENERO[i] == 1){m2 = m2 +1}
      else{n2 = n2 + 1}
    }
    else{if(i < 50394){
        if(GENERO[i] == 1){m3 = m3 + 1}
        else{n3 = n3 + 1}
      }
      else{if(GENERO[i] == 1){m4 = m4 + 1}
        else{n4 = n4 + 1}
      }
    }
  }
}
Percentil_MF<-c(m1,n1,m2,n2,m3,n3,m4,n4)
Per_Gen <-matrix(data = Percentil_MF, ncol = 4, byrow = TRUE,
            dimnames = (list(c("Feminino","Masculino"),
                             c("0% - 25%","25% - 50%","50% - 75%","75% - 100%"))))
#Gerar o gráfico com os respectivos quartis
barplot(Per_Gen,
        main = "Mulheres e homens por percentil",
        xlab = "Percentis",
        col = c("purple","green"),
        beside = TRUE
)
legend("topright",
      fill = c("purple","green"),
      c("Mulheres","Homens")
       
)

#Escolher duas variáveis (colunas) e gerar os gráficos mais adequados para tais colunas.

#gráfico de pontos contagem  x nota redação x idade
#carrega os pacotes                            
library(ggplot2)
library(tidyverse)

#código
ENEM %>%
  group_by(NU_NOTA_REDACAO, NU_IDADE) %>%
  summarise(
    contagem = n()
  ) %>%
  ggplot(aes(x = NU_NOTA_REDACAO, y = contagem, fill = NU_IDADE, label = contagem)) + 
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Nota de redação por município",
       subtitle = "Conjunto de dados ENEM 2019",
       x = "Nota redação", y = "Contagem")
#Escolher duas variáveis (colunas) e gerar os gráficos mais adequados para tais colunas.

#gráfico de pontos contagem  x nota redação x Município
#carrega os pacotes                            
library(ggplot2)
library(tidyverse)

#código
ENEM %>%
  group_by(NU_NOTA_REDACAO, NO_MUNICIPIO_PROVA) %>%
  summarise(
    contagem = n()
  ) %>%
  ggplot(aes(x = NU_NOTA_REDACAO, y = contagem, fill = NO_MUNICIPIO_PROVA, label = contagem)) + 
  geom_bar(stat = "identity") +
  labs(title = "Nota de redação por município",
       subtitle = "Conjunto de dados ENEM 2019",
       x = "Nota redação", y = "Contagem")


################################################################################  
#gráfico de pontos nota de redação  x nota geral
#carrega os pacotes                            
library(ggplot2)
library(tidyverse)

#acha o arquivo no diretório
dados = read.csv2(file.choose())
dados
#CÓDIGO
dados %>%
  ggplot(aes(x = NU_NOTA_REDACAO, y = NOTA_ENEN, color = TP_COR_RACA)) + geom_point()+
  geom_smooth(method = "lm") + labs(title = "Comparação das notas de redação com a nota geral",
                                    subtitle = "Conjunto de dados ENEM 2019",
                                    x = "Nota de redação", y = "Nota geral")

################################################################################
#gráfico de pontos contagem  x idioma x sexo
#carrega os pacotes                            
library(ggplot2)
library(tidyverse)
#acha o arquivo no diretório
dados = read.csv2(file.choose())
dados

#código
dados %>%
  group_by(TP_LINGUA, TP_SEXO) %>%
  summarise(
    contagem = n()
  ) %>%
  ggplot(aes(x = TP_LINGUA, y = contagem, fill = TP_SEXO, label = contagem)) + 
  geom_bar(stat = "identity", position = "dodge") +
  geom_label(position = position_dodge(width = 1), alpha = 0.5) +
  labs(title = "Distribuição dos idiomas por sexo",
       subtitle = "Conjunto de dados ENEM 2019",
       x = "Idiomas", y = "Contagem")


################################################################################
#gráfico de pontos nota de matemática  x ciências humanas
#carrega os pacotes                            
library(ggplot2)
library(tidyverse)

#acha o arquivo no diretório
dados = read.csv2(file.choose())
dados
#CÓDIGO
dados %>%
  ggplot(aes(x = NU_NOTA_CH, y = NU_NOTA_MT, color = NU_NOTA_CH)) + geom_point(color = "green", size = 0.5, alpha = 0.5)+
  geom_smooth() + 
  labs(title = "Comparação entre as notas de humanas x matemática",
       subtitle = "Conjunto de dados ENEM 2019",
       x = "Nota de ciências humanas", y = "Nota de matemática")

#QUESTÃO 02
#Primeiro foi separado igaci do resto do municípios de prova
igaci <- filter(ENEM, ENEM$NO_MUNICIPIO_PROVA == "Igaci")
igaci %>%
  group_by(TP_COR_RACA, TP_LINGUA) %>%
  summarise(
    contagem = n()
  ) %>%
  ggplot(aes(x = TP_COR_RACA, y = contagem, fill = TP_LINGUA, label = contagem)) + 
  
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Idioma vesus raça - Igaci",
       subtitle = "Conjunto de dados ENEM 2019",
       x = "Raça declarada", y = "Contagem")

igaci %>%
  group_by(NU_NOTA_LC, TP_LINGUA) %>%
  ggplot(aes(x = NU_NOTA_LC, y = NU_NOTA_MT, color = TP_SEXO)) + 
  geom_smooth() +
  geom_point()+
  labs(title = "Nota em linguagens x Nota matemática - Igaci",
       subtitle = "Conjunto de dados ENEM 2019",
       x = "Notas linguagens", y = "Notas matemática")

hist(igaci$NU_IDADE, breaks = 20, 
     col = "red",xlab = "Idade", 
     ylab = "Pessoas", 
     main = "Histograma com a quantidade de pessoas por idade em Igaci")
