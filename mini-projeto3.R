#******************************************************************************
#
#               Microsoft Power BI Para Data Science, Versão 2.0
#
#                               Data Science Academy
#
#                                   Mini-Projeto 3
#       
#         Prevendo a Inadimplência de Clientes com Machine Learning e Power BI
#
#******************************************************************************

#Definindo a pasta de trabalho

setwd("C:/DSA/Cap15")

#Definição do problema
#Leia o manual em pdf no Cap15 do curso com a definição do problema

#Instalando os pacotes para o projeto
#Obs.: Os pacotes precisam ser instalados apenas uma vez
install.packages("Amelia")
install.packages("caret")
install.packages("ddplot2")
install.packages("dplyr")
install.packages("reshape")
install.packages("randomForest")
install.packages("e1071")

#Carregando os pacotes

library(Amelia)
library(ggplot2)
library(caret)
library(reshape)
library(randomForest)
library(dplyr)
library(e1071)

#Carregando o dataset
#Fonte: https://archive.ics.uci.edu/ml/datasets/default+of+credit+card+clients
dados_clientes <- read.csv("dados/dataset.csv")

# Visualizando os dados e sua estrutura
View(dados_clientes)
str(dados_clientes)
summary(dados_clientes)

#### Análise Exploratória, Limpeza e Transformação ####

# Removendo a primeira coluna ID

dados_clientes$ID <- NULL

# Renomeando a colna de classe
colnames(dados_clientes)
colnames(dados_clientes)[24] <- "Inadimplente"
colnames(dados_clientes)
View(dados_clientes)

# Verificando valores ausentes e removendo do dataset
sapply(dados_clientes, function(x) sum (is.na(x)))
?missmap
missmap(dados_clientes, main = "Valores Missing Observados")
dados_clientes <- na.omit(dados_clientes) #Exemplo de alteração caso houvesse valores ausentes

# Convertendo atributos genero, escolaridade, estado civil e idade
# para fatores (categorias)

# Renomenado colunas categoricas
colnames(dados_clientes)
colnames(dados_clientes)[2] <- "Genero"
colnames(dados_clientes)[3] <- "Escolaridade"
colnames(dados_clientes)[4] <- "Estado_Civil"
colnames(dados_clientes)[5] <- "Idade"
colnames(dados_clientes)
View(dados_clientes)

# Genero
View(dados_clientes$Genero)
str(dados_clientes$Genero)
summary(dados_clientes$Genero)
?cut
dados_clientes$Genero <- cut(dados_clientes$Genero,
                             c(0,1,2),
                             labels = c("Masculino","Feminino"))
View(dados_clientes$Genero)
str(dados_clientes$Genero)
summary(dados_clientes$Genero)

# Escolaridade
View(dados_clientes)
str(dados_clientes$Escolaridade)
summary(dados_clientes$Escolaridade)
?cut
dados_clientes$Escolaridade <- cut(dados_clientes$Escolaridade,
                             c(0,1,2,3,4),
                             labels = c("Pos Graduado",
                                        "Graduado",
                                        "Ensino Medio",
                                        "Outros"))
# Valores distintos não tratados para aprendizado

# Estado Civil
dados_clientes$Estado_Civil <- cut(dados_clientes$Estado_Civil,
                                   c(-1,0,1,2,3),
                                   labels = c("Desconhecido", 
                                              "Casado",
                                              "Solteiro",
                                              "Outros"))

# Convertendo a variável para o tipo fator com faixa etaria
dados_clientes$Idade <- cut(dados_clientes$Idade,
                            c(0,30,50,100),
                            labels = c("Jovem",
                                      "Adulto",
                                      "Idoso"))
                          
                                   
                                   
