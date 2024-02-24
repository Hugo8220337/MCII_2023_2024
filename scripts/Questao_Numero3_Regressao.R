library(corrplot)
library(car) # para os testes de Durbin Wattson

#Snipets:
  # Contar NA's por variável: supply(dados, function(x)(sum(is.na(x))))
  # Eliminar NA's dados_1 <- dados[which (!is.na(dados$varQueQueroEliminar))]

# Pressupostos:
  # ter removido a primeira linha do excel

# constantes:
  # nível de significância de 5%

data <- read.csv2("23_24_Recolhas_TP_MCII_ESII.csv", sep=";", stringsAsFactors=TRUE)
data$X = NULL

# Remover valores ausentes do conjunto de dados
data <- na.omit(data)


# ANTES REGRESSÃO: MATRIZ DAS CORRELAÇÔES, para ver se faz sentido  fazer a regressão
correlationMatrix <- cor(data, method = "spearman") # procurar vars mais próximas de 1, para serem candidatas a variáveis independentes
corrplot(correlationMatrix) # vermelho é o que mais interessa





# Dados que vão ser usados para a regressão:
# M9 - numero de testes falhados (Variável dependente)
#
# M3 - Número de User Stories abertas 
# M4 - Número de User Stories fechadas 
# M5 - Número de pedidos de alterações abertos
# M22 - Número de correções necessárias por tempo de ciclo


# Criação do modelo de regressão linear 
regression_model <- lm(data$M9 ~ data$M3 + data$M4 + data$M5 + data$M22, na.action = na.omit, data = data) # regressão linear
summary(regression_model)

# Realizar seleção stepwise
stepwise_model <- step(regression_model)


# modelo de regressão linear final
regression_model <- lm(data$M9 ~ data$M3)
summary(regression_model)



# Pressupostos da regressão Linear (testes):

# teste de normalidade:
shapiro.test(regression_model$residuals)

#  outra forma de verificar a normalidade dos resíduos
hist(regression_model$residuals, main="Histograma de resíduos")
  
# testes de resíduos
durbinWatsonTest(regression_model)



# Calcular o Variation factor para saber se existe multicolinearidade entre as variáveis independentes
#vif(regression_model) # só seria preciso se a regressão linear não fosse simples



