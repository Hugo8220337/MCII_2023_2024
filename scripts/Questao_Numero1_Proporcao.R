# saber a evolução da taxa de cobertura dos testes e saber se os
  #grupos cumpriram o critério definido no Quality Gate dos trabalhos de Engenharia de Software II para a taxa
  #de cobertura admissível dos mesmos.

recolhas <- read.csv2("23_24_Recolhas_TP_MCII_ESII.csv", stringsAsFactors=TRUE)
recolhas$X = NULL

# Selecionar apenas as colunas relevantes (M1 e M24) para as semanas 2 e 4
semana2_taxaCobertura <- recolhas[recolhas$M1 == 2, "M24"]
semana4_taxaCobertura <- recolhas[recolhas$M1 == 4, "M24"]


# Excluir linhas com NAs
semana2_taxaCobertura <- na.omit(semana2_taxaCobertura)
semana4_taxaCobertura <- na.omit(semana4_taxaCobertura)


# Visualização dos dados
boxplot(semana2_taxaCobertura, semana4_taxaCobertura, main ="Taxas de cobertura na semana 2 e 4", ylab = "Taxa de Cobertura")

summary(semana2_taxaCobertura)
summary(semana4_taxaCobertura)

# Teste de igualdade de variâncias
var_test_result <- var.test(semana2_taxaCobertura, semana4_taxaCobertura, alternative = "two.sided")
var_test_result

# Teste de normalidade 
shapiro.test(semana2_taxaCobertura)
shapiro.test(semana4_taxaCobertura)


# como uma das vars não segue distribuição normal, e como as variâncias são diferentes,
# vai ter de se usar um teste não paramétrico, o de Wilcox

# Teste de Wilcoxon-Mann-Whitney (teste de Wilcoxon rank-sum)
wilcox_test_result <- wilcox.test(semana2_taxaCobertura, semana4_taxaCobertura, alternative = "two.sided",  exact = FALSE)
wilcox_test_result

wilcox.test(semana2_taxaCobertura, semana4_taxaCobertura, alternative = "two.sided",  exact = FALSE)

# Se a diferença entre as médias fosse diferente de 0, faria um teste unilateral para saber qual das médias era maior e qual a menor.
#wilcox_test_result_unilateral <- wilcox.test(semana2_taxaCobertura, semana4_taxaCobertura, alternative = "greater", exact = FALSE)


# Realizar o teste de proporções
# maior que 80% devido ao ESII

sucess2 <-sum(semana2_taxaCobertura > 0.80, na.rm = TRUE)
sucess2

sucess4 <-sum(semana4_taxaCobertura > 0.80, na.rm = TRUE)
sucess4

## teste de proporção
resultado_teste <- prop.test(c(sucess2, sucess4), c(length(semana2_taxaCobertura), length(semana4_taxaCobertura)))
resultado_teste
length(semana2_taxaCobertura)
length(semana4_taxaCobertura)

# saber se as pessoas cumpriram as métricas do quality gate (80% de cobertura)
wilcox.test(semana4_taxaCobertura, alteranitve="less", mu = 0.8, conf.level = 0.95, exact = FALSE)

