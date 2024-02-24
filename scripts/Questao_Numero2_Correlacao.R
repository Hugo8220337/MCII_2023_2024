# houve um aumento das linhas de codigo entre as recohas
# o número de linhas de código totais (M18) está correlacionado com a taxa de cobertura dos testes (M24)

#Snipets:
# Contar NA's por variável: supply(dados, function(x)(sum(is.na(x))))
# Eliminar NA's dados_1 <- dados[which (!is.na(dados$varQueQueroEliminar))]

# Pressupostos:
# ter removido a primeira linha do excel

# constantes:
# nível de significância de 5%

recolhas <- read.csv2("23_24_Recolhas_TP_MCII_ESII.csv", sep=";", stringsAsFactors=TRUE)
recolhas$X = NULL

#
# houve um aumento das linhas de codigo da entre as recolhas
#

linhasCodigoSegundaSemana <- recolhas[recolhas$M1 == 2, "M18"]
linhasCodigoTerceiraSemana <- recolhas[recolhas$M1 == 3, "M18"]
linhasCodigoQuartaSemana <- recolhas[recolhas$M1 == 4, "M18"]


# Observar os dados
linhasCodigoSegundaSemana
linhasCodigoTerceiraSemana
linhasCodigoQuartaSemana
summary(linhasCodigoSegundaSemana)
summary(linhasCodigoTerceiraSemana)
summary(linhasCodigoQuartaSemana)
boxplot(linhasCodigoSegundaSemana, linhasCodigoTerceiraSemana, linhasCodigoQuartaSemana, main="Linhas de código totais da segunda, terceira e quarta semana", ylab="linhas de código")

# verificar normalidade
shapiro.test(linhasCodigoSegundaSemana)
shapiro.test(linhasCodigoTerceiraSemana)
shapiro.test(linhasCodigoQuartaSemana) # este segue distribuição normal

# Como apenas uma das amostras segue uma distribuição normal, não se vai poder usar um teste de ANOVA, vai ter de se usar um teste não paramétrico
# como as variáveis são independentes, então vai-se usar um teste de kruskal, se fossem emparelhadas era um teste de friedman
# Teste de Kruskal-Wallis
kruskal.test(list(linhasCodigoSegundaSemana, linhasCodigoTerceiraSemana, linhasCodigoQuartaSemana), na.action = na.omit) # precisa do list, porque as amostras têm tamanhos diferentes




#
# o número de linhas de código totais (M18) está correlacionado com a taxa de cobertura dos testes (M24)
#

# Selecionar apenas as colunas relevantes (M18 e M24), que não seja do primeiro sprint (M1)
numeroLinhasCodigo <- recolhas[recolhas$M1 != 1, "M18"]
taxaCoberturaTestes <- recolhas[recolhas$M1 != 1, "M24"]

# Teste de normalidade
shapiro.test(numeroLinhasCodigo)
shapiro.test(taxaCoberturaTestes)



# Como não seguem distribuições normais, vai-se usar um teste de correlação de Spearman, que é o teste não paramétrico do teste de correlação de pearson
correlation_result <- cor.test(numeroLinhasCodigo, taxaCoberturaTestes, method = "spearman", conf.level = 0.95, na.action = na.omit, exact = FALSE)
correlation_result
cor.test(numeroLinhasCodigo, taxaCoberturaTestes, method = "spearman", conf.level = 0.95, na.action = na.omit, exact = FALSE)
# na.ommit vai ignorar os NA, não os podia remover os NA no começo porque não dá para fazer cor.test de variáveis com tamanhos diferentes


# Scatterplot
plot(numeroLinhasCodigo, taxaCoberturaTestes, 
     main = "Número de Linhas Código Totais vs Taxa Cobertura dos Testes",
     xlab = "Número Linhas Código Totais", ylab = "Taxa Cobertura Testes")
# Adição da linha no gráfico
abline(lm(taxaCoberturaTestes ~ numeroLinhasCodigo), col = "red", lwd = 3)
