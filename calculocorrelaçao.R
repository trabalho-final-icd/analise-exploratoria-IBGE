library(tidyverse)
library(vcd)

# Cálculo das correlações

dados <- read.csv("BaseDadosIBGE_MG_Limpa.csv", stringsAsFactors = FALSE)

# Conversão para dados numéricos, exceto muniípio:
dados <- dados %>%
  mutate(across(-municipio, as.numeric))

# Para calcular as correlações, é necessário executar os seguintes pacotes:
library(tidyverse)
library(vcd)

# PIB x Escolarização
cor(dados$pib_per_capita, dados$escolarizacao, method = "pearson")
cor(dados$pib_per_capita, dados$escolarizacao, method = "spearman")
cor(dados$pib_per_capita, dados$escolarizacao, method = "kendall")

# PIB x Mortalidade
cor(dados$pib_per_capita, dados$mortalidade_infantil, method = "pearson", use = "complete.obs")
cor(dados$pib_per_capita, dados$mortalidade_infantil, method = "spearman", use = "complete.obs")
cor(dados$pib_per_capita, dados$mortalidade_infantil, method = "kendall", use = "complete.obs")