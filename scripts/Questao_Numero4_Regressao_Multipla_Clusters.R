# Que variáveis correlacionam com o class cohesion (M17)

library(corrplot)
library(car) # para os testes de Durbin Wattson
library(cluster)

data <- read.csv2("23_24_Recolhas_TP_MCII_ESII.csv", sep=";", stringsAsFactors=TRUE)
data$X = NULL

# Remover valores ausentes do conjunto de dados
data <- na.omit(data)

# ANTES REGRESSÃO: MATRIZ DAS CORRELAÇÔES, para ver se faz sentido  fazer a regressão
correlationMatrix <- cor(data, method = "spearman") # procurar vars mais próximas de 1, para serem candidatas a variáveis independentes
corrplot(correlationMatrix) # vermelho é o que mais interessa

# Dados que vão ser usados para a regressão:
# M17 - Cçass cohesion, número de items menores que 10 (Variável dependente)
#
# M06 - número de pedidos de alterações rejeitados
# M07 - número de pedidos de alterações aprovados
# M14 - Cyclomantic Complexity, número de items menores que 10
# M15 - Cognitive Complexity, número de items menores que 10
# M16 - Class Coupling

regression_model <- lm(data$M17 ~ data$M6 + data$M7 + data$M14 + data$M15 + data$M16, na.action = na.omit, data = data) # regressão linear
summary(regression_model)

stepwise_model <- step(regression_model)

regression_model <- lm(data$M17 ~ data$M14 + data$M16, na.action = na.omit, data = data) # regressão linear
summary(regression_model)


# Pressupostos da regressão Linear (testes):

# teste de normalidade:
shapiro.test(regression_model$residuals)

#  outra forma de verificar a normalidade dos resíduos
hist(regression_model$residuals, main="Histograma de resíduos")

# testes de resíduos
durbinWatsonTest(regression_model)

# Calcular o Variation factor para saber se existe multicolinearidade entre as variáveis independentes
vif(regression_model) # usado nas regressões lineares múltiplas


# CLUSTERS


#
# Para encontrar o número de clusters
#
data_used_in_regression <- data.frame(M14 = data$M14, M16 = data$M16)

# Determinar o número ideal de clusters, método de Elbow
wss <- sapply(1:10, function(k) kmeans(data_used_in_regression, centers = k)$tot.withinss)

# Gráfico do método de Elbow
plot(1:10, wss, type = "b", xlab = "Número de Clusters", ylab = "Soma dos Quadrados Intra-cluster", main = "Método de Elbow")

# Adicionar linhas de referência
abline(v = 3, col = "red", lty = 2)  # Número escolhido de clusters
abline(v = 4, col = "blue", lty = 2)  # Exemplo de outro número de clusters

# Legenda
legend("topright", legend = c("Número Escolhido", "Outro Número"), col = c("red", "blue"), lty = 2)
# Resposta: escolheu-se 3 clusters


#
# visualização dos clusters
#

# Visualizar a distribuição dos resíduos por cluster
numero_de_clusters <- 3
clusters_residuos <- kmeans(regression_model$residuals, centers = numero_de_clusters)

# Adicionar a informação de cluster aos dados originais
data$cluster <- as.factor(clusters_residuos$cluster)

boxplot(regression_model$residuals ~ data$cluster, main = "Distribuição dos Resíduos por Cluster", xlab = "Cluster", ylab = "Resíduos")

# Criar um vetor de cores com base nos clusters
cores <- c("red", "green", "blue")[data$cluster]

# Criar o gráfico de dispersão
plot(regression_model$residuals, col = cores, main = "Distribuição dos Resíduos por Cluster", xlab = "Observação", ylab = "Resíduos")

# Adicionar legenda
legend("bottomleft", legend = unique(data$cluster), fill = c("red", "green", "blue"), title = "Clusters")

# Aggregate para Média dos Resíduos
aggregate(regression_model$residuals, by = list(data$cluster), FUN = mean)
