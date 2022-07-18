#Definindo a pasta de trabalho

setwd("C:/DSA/Cap15")

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

# Renomeando a coluna de classe
colnames(dados_clientes)[24] <- "Inadimplente"

# Verificando valores ausentes e removendo do dataset
sapply(dados_clientes, function(x) sum (is.na(x)))
?missmap
missmap(dados_clientes, main = "Valores Missing Observados")

# Convertendo atributos genero, escolaridade, estado civil e idade para fatores (categorias)

# Renomeanado colunas categoricas
colnames(dados_clientes)[2] <- "Genero"
colnames(dados_clientes)[3] <- "Escolaridade"
colnames(dados_clientes)[4] <- "Estado_Civil"
colnames(dados_clientes)[5] <- "Idade"

# Genero
?cut
dados_clientes$Genero <- cut(dados_clientes$Genero,
                             c(0,1,2),
                             labels = c("Masculino","Feminino"))

# Escolaridade
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
summary(dados_clientes$Idade)

# Convertendo a variavel que indica pagamentos para o tipo fator
# Uso do 'as factor' pois há alteração apenas da variavel, sem alteração de valores

dados_clientes$PAY_0 <- as.factor(dados_clientes$PAY_0)
dados_clientes$PAY_2 <- as.factor(dados_clientes$PAY_2)
dados_clientes$PAY_3 <- as.factor(dados_clientes$PAY_3)
dados_clientes$PAY_4 <- as.factor(dados_clientes$PAY_4)
dados_clientes$PAY_5 <- as.factor(dados_clientes$PAY_5)
dados_clientes$PAY_6 <- as.factor(dados_clientes$PAY_6)
                          
# Dataset após as conversões
str(dados_clientes)
sapply(dados_clientes, function(x) sum(is.na(x)))
missmap(dados_clientes, main = "Valores Missing Observados")
dados_clientes <- na.omit(dados_clientes) #Remove valores ausentes
missmap(dados_clientes, main = "Valores Missing Observados")
dim(dados_clientes)

# Alterando a variável dependente para o tipo fator
str(dados_clientes$Inadimplente)
colnames(dados_clientes)
dados_clientes$Inadimplente <- as.factor(dados_clientes$Inadimplente)
str(dados_clientes$Inadimplente)
                                      
# Total de inadimplentes versus não-inadimplentes
table(dados_clientes$Inadimplente)

# Vejamos as procentagens entre as classes
prop.table(table(dados_clientes$Inadimplente))

# Plot da distribuição usando ggplot2
qplot(Inadimplente, data = dados_clientes, geom = "bar") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# set seed
set.seed(12345)

# Amonstragem estratificada
# Seleciona as linhas de acordo com a variável inadimplente como strata
indice <- createDataPartition(dados_clientes$Inadimplente, p = 0.75, list = FALSE)
dim(indice)

# Definimos os dados de treinamento como subconjunto do conjunt de dados original
# com números de indice de linha (conforme identificado acima) e todas as colunas
dados_treino <- dados_clientes[indice,]
table(dados_treino$Inadimplente)

# Porcentagem entre classes
prop.table(table(dados_treino$Inadimplente))

# Número de registros no dataset de treinamento
dim(dados_treinamento)

# Comparamos as porcentagens entre classes de treinamento e dados originais
compara_dados <- cbind(prop.table(table(dados_treino$Inadimplente)),
                      prop.table(table(dados_clientes$Inadimplente)))
colnames(compara_dados) <- c("Treinamento", "Original")
compara_dados

# Melt Data - Converte colunas em linhas
?reshape2::melt
melt_compara_dados <- melt(compara_dados)
melt_compara_dados

# Plot para ver a distribuição do treinamento vs original
ggplot(melt_compara_dados, aes(x = X1, y = value)) +
  geom_bar( aes(fill = X2), stat = "identity", position = "dodge") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Tudo o que não está no dataset de treinamento está no dataset de teste. Observe o sinal - (menos)
dados_teste <- dados_clientes[-indice,]
dim(dados_teste)
dim(dados_treino)

############# Machine Learning #############

# Construindo a primeira versão do modelo
?randomForest
modelo_v1 <- randomForest(Inadimplente ~ . , data = dados_treino)
modelo_v1

# Avaliando o modelo
plot(modelo_v1)

# Previsões com dados de teste
previsoes_v1 <- predict(modelo_v1, dados_teste)

# Confusion matrix
cm_v1 <- caret::confusionMatrix(previsoes_v1, dados_teste$Inadimplente, positive = "1")
cm_v1

# Calculando presicion, recall e F1-score, métricas de avaliação do modelo preditivo
y <- dados_teste$Inadimplente
y_pred_v1 <- previsoes_v1

precision <- posPredValue(y_pred_v1, y)
precision

recall <- sensitivity(y_pred_v1, y)
recall

F1 <- (2 * precision * recall) / (precision + recall)
F1

# Balanceamento de classe
install.packages("performanceEstimation")
library(performanceEstimation)

# Usado 'performanceEstimation' pois não permite o uso do DMwR na versão atual do R

# Aplicando o SMOTE - SMOTE: Synthetic Minority Over-Sampling Technique
table(dados_treino$Inadimplente)
prop.table(table(dados_treino$Inadimplente))
set.seed(9560)
dados_treino_bal <- smote(Inadimplente ~ ., dados_treino, perc.over = 7, perc.under = 2)
table(dados_treino_bal$Inadimplente)
prop.table(table(dados_treino_bal$Inadimplente))

# Construindo a segunda versão do modelo
modelo_v2 <- randomForest(Inadimplente ~ ., data = dados_treino_bal)
modelo_v2

# Avaliando o modelo
plot(modelo_v2)

# Previsões com dados de teste
previsoes_v2 <- predict(modelo_v2, dados_teste)

# Confusion matrix
cm_v2 <- caret::confusionMatrix(previsoes_v2, dados_teste$Inadimplente, positive = "1")
cm_v2

# Calculando presicion, recall e F1-score, métricas de avaliação do modelo preditivo
y <- dados_teste$Inadimplente
y_pred_v2 <- previsoes_v2

precision <- posPredValue(y_pred_v2, y)
precision

recall <- sensitivity(y_pred_v2, y)
recall

F1 <- (2 * precision * recall) / (precision + recall)
F1

# Conferência das importancias.
# Importância das variáveis preditoras para as previsões
View(dados_treino_bal)
varImpPlot(modelo_v2)


# Construindo a 3ª versão do modelo com variaveis mais importantes
colnames(dados_treino_bal)
modelo_v3 <- randomForest(Inadimplente ~ PAY_0 + LIMIT_BAL + PAY_AMT6 + PAY_AMT2 + 
                          PAY_AMT1 + PAY_AMT5 + BILL_AMT1,
                          data = dados_treino_bal)
modelo_v3

# Avaliando o modelo
plot(modelo_v3)

# Previsões com dados de teste
previsoes_v3 <- predict(modelo_v3, dados_teste)

# Confusion matrix
cm_v3 <- caret::confusionMatrix(previsoes_v3, dados_teste$Inadimplente, positive = "1")
cm_v3

# Calculando presicion, recall e F1-score, métricas de avaliação do modelo preditivo
y <- dados_teste$Inadimplente
y_pred_v3 <- previsoes_v3

precision <- posPredValue(y_pred_v3, y)
precision

recall <- sensitivity(y_pred_v3, y)
recall

F1 <- (2 * precision * recall) / (precision + recall)
F1

# Salvando o modelo em disco
saveRDS(modelo_v3, file = "modelo/modelo_v3.rds")

# Carregando o modelo
modelo_final <- readRDS("modelo/modelo_v3.rds")

# Previsões com novos dados de 3 clientes

# Dados dos clientes
PAY_0 <- c(0,0,0)
LIMIT_BAL <- c(2000,5000,6000)
PAY_AMT6 <- c(500,700,800)
PAY_AMT2 <- c(450,300,800)
PAY_AMT1 <- c(405,350,950)
PAY_AMT5 <- c(300,5000,1800)
BILL_AMT1 <- c(1000,2300,5000)

#Concatena em um df
novos_clientes <- data.frame(PAY_0, LIMIT_BAL, PAY_AMT6, PAY_AMT2, PAY_AMT1, PAY_AMT5, BILL_AMT1)
View(novos_clientes)

#Previsoes
previsoes_novos_clientes <- predict(modelo_final, novos_clientes)

# Checando os tipos de dados
str(dados_treino_bal)
str(novos_clientes)

# Convertendo os tipos de dados
novos_clientes$PAY_0 <- factor(novos_clientes$PAY_0, levels = levels(dados_treino_bal$PAY_0))
str(novos_clientes)

#Previsoes
previsoes_novos_clientes <- predict(modelo_final, novos_clientes)
View(previsoes_novos_clientes)      
