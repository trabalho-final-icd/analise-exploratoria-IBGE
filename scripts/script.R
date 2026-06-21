##BIBLIOTECAS
library(dplyr)
library(ggplot2)
library(MetBrewer)
library(paletteer)
library(purrr)
library(readxl)
library(scales)
library(tidyr)

dim(dados)
names(dados)
str(dados)
summary(dados)

# --------------- LIVIA 
dados <- dados %>%
  rename(
    municipio = `Município [-]`,
    area_territorial = `Área Territorial - km² [2025]`,
    populacao = `População no último censo - pessoas [2022]`,
    densidade_demografica = `Densidade demográfica - hab/km² [2022]`,
    populacao_estimada = `População estimada - pessoas [2025]`,
    escolarizacao = `Escolarização <span>6 a 14 anos</span> - % [2022]`,
    idhm = `IDHM <span>Índice de desenvolvimento humano municipal</span> [2010]`,
    mortalidade_infantil = `Mortalidade infantil - óbitos por mil nascidos vivos [2023]`,
    receitas_brutas = `Total de receitas brutas realizadas - R$ [2024]`,
    despesas_brutas = `Total de despesas brutas empenhadas - R$ [2024]`,
    pib_per_capita = `PIB per capita - R$ [2023]`
  )

names(dados)



colSums(is.na(dados))

faltantes <- data.frame(
  variavel = names(dados),
  n_na = colSums(is.na(dados)),
  perc_na = round(
    colSums(is.na(dados)) / nrow(dados) * 100,
    2
  )
)

faltantes



sum(dados$mortalidade_infantil == "-")

dados <- dados %>%
  mutate(
    mortalidade_infantil = na_if(
      mortalidade_infantil,
      "-"
    )
  )

dados <- dados %>%
  mutate(
    mortalidade_infantil =
      as.numeric(mortalidade_infantil))

str(dados)



colSums(is.na(dados))

faltantes_finais <- data.frame(
  variavel = names(dados),
  n_na = colSums(is.na(dados)),
  perc_na = round(
    colSums(is.na(dados)) / nrow(dados) * 100,
    2))

faltantes_finais



sum(duplicated(dados$municipio))



range(dados$idhm, na.rm = TRUE)

range(dados$escolarizacao, na.rm = TRUE)

min(dados$mortalidade_infantil, na.rm = TRUE)

min(dados$area_territorial)

min(dados$populacao)

min(dados$populacao_estimada)

min(dados$pib_per_capita)

min(dados$receitas_brutas)

min(dados$despesas_brutas)



str(dados)

summary(dados)
str 






cores <- paletteer::paletteer_d("MetBrewer::Pissaro")

tema_trabalho <- theme_minimal() +
  theme(
    plot.title = element_text(
      face = "bold",
      hjust = 0.5,
      size = 14
    ),
    axis.title = element_text(face = "bold")
  )

ggplot(
  dados,
  aes(
    x = pib_per_capita,
    y = escolarizacao
  )
) +
  geom_point(
    color = cores[1],
    alpha = 0.7,
    size = 2
  ) +
  scale_x_log10(
    labels = label_number(big.mark = ".")
  ) +
  labs(
    title = "PIB per capita e Escolarização",
    x = "PIB per capita (R$)",
    y = "Escolarização (%)"
  ) +
  tema_trabalho


ggplot(
  dados,
  aes(
    x = pib_per_capita,
    y = mortalidade_infantil
  )
) +
  geom_point(
    color = cores[2],
    alpha = 0.7,
    size = 2
  ) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    color = cores[7]
  ) +
  scale_x_log10(
    labels = label_number(big.mark = ".")
  ) +
  labs(
    title = "PIB per capita e Mortalidade Infantil",
    x = "PIB per capita (R$)",
    y = "Mortalidade Infantil"
  ) +
  tema_trabalho


ggplot(
  dados,
  aes(
    x = idhm,
    y = escolarizacao
  )
) +
  geom_point(
    color = cores[3],
    alpha = 0.7,
    size = 2
  ) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    color = cores[8]
  ) +
  labs(
    title = "IDHM e Escolarização",
    x = "IDHM",
    y = "Escolarização (%)"
  ) +
  tema_trabalho



dados$faixa_idhm <- cut(
  dados$idhm,
  breaks = quantile(
    dados$idhm,
    probs = seq(0, 1, 0.25),
    na.rm = TRUE
  ),
  include.lowest = TRUE
)


ggplot(
  dados,
  aes(
    x = faixa_idhm,
    y = mortalidade_infantil,
    fill = faixa_idhm
  )
) +
  geom_boxplot() +
  scale_fill_paletteer_d("MetBrewer::Pissaro") +
  labs(
    title = "Mortalidade Infantil por Faixa de IDHM",
    x = "Faixa de IDHM",
    y = "Mortalidade Infantil"
  ) +
  tema_trabalho +
  theme(
    legend.position = "none"
  )


ggplot(
  dados,
  aes(
    x = receitas_brutas,
    y = despesas_brutas
  )
) +
  geom_point(
    color = cores[5],
    alpha = 0.7,
    size = 2
  ) +
  scale_x_log10(
    labels = label_number(big.mark = ".")
  ) +
  scale_y_log10(
    labels = label_number(big.mark = ".")
  ) +
  labs(
    title = "Receitas e Despesas Municipais",
    x = "Receitas Brutas (R$)",
    y = "Despesas Brutas (R$)"
  ) +
  tema_trabalho


dados <- dados %>%
  mutate(
    z_pib = as.numeric(scale(pib_per_capita)),
    z_idhm = as.numeric(scale(idhm)),
    z_escolarizacao = as.numeric(scale(escolarizacao)),
    z_mortalidade = -as.numeric(scale(mortalidade_infantil))
  )

dados <- dados %>%
  mutate(
    indice_desenvolvimento =
      (z_pib +
         z_idhm +
         z_escolarizacao +
         z_mortalidade) / 4
  )

dados_rank <- dados %>%
  filter(!is.na(indice_desenvolvimento))

ranking <- dados_rank %>%
  arrange(desc(indice_desenvolvimento))

top10 <- ranking %>%
  select(municipio, indice_desenvolvimento) %>%
  slice(1:10)

dados_rank <- dados_rank %>%
  mutate(
    quartil_desenvolvimento =
      ntile(indice_desenvolvimento, 4)
  )

dados_rank <- dados_rank %>%
  mutate(
    categoria_desenvolvimento =
      case_when(
        quartil_desenvolvimento == 1 ~ "Baixo",
        quartil_desenvolvimento == 2 ~ "Médio-Baixo",
        quartil_desenvolvimento == 3 ~ "Médio-Alto",
        quartil_desenvolvimento == 4 ~ "Alto"
      )
  )


ggplot(top10,
       aes(x = reorder(municipio,
                       indice_desenvolvimento),
           y = indice_desenvolvimento,
           fill = indice_desenvolvimento)) +
  geom_col() +
  coord_flip() +
  scale_fill_gradientn(colors = cores) +
  labs(
    title = "Top 10 Municípios Mais Desenvolvidos de Minas Gerais",
    x = "Município",
    y = "Índice de Desenvolvimento"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none"
  )

#---------------  HÁGATA - medidas de resumo para variáveis numéricas
numericas <- dados %>% select(where(is.numeric))
#selecionando numéricas

estatisticas <- data.frame(
  Variavel = names(numericas),
  Media = round(sapply(numericas, mean, na.rm = TRUE), 2),
  Mediana = round(sapply(numericas, median, na.rm = TRUE), 2),
  Minimo = round(sapply(numericas, min, na.rm = TRUE), 2),
  Maximo = round(sapply(numericas, max, na.rm = TRUE), 2),
  DP = round(sapply(numericas, sd, na.rm = TRUE), 2),
  CV = round(sapply(numericas, function(x) (sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE)) * 100), 2))
#calculando estatísticas
print(estatisticas)
write.csv(estatisticas, "estatisticas_descritivas.csv", row.names = FALSE)
#exibir e salvar
tabela_resumida <- data.frame(
  Variável = c(
    "Área Territorial (km²)",
    "População (Censo 2022)",
    "Densidade Demográfica (hab/km²)",
    "População Estimada (2025)",
    "Escolarização 6-14 anos (%)",
    "IDHM",
    "Mortalidade Infantil (óbitos/1000)",
    "Receitas Brutas (R$)",
    "Despesas Brutas (R$)",
    "PIB per Capita (R$)"
  ),
  
  Média = round(sapply(numericas, mean, na.rm = TRUE), 2),
  Mediana = round(sapply(numericas, median, na.rm = TRUE), 2),
  Mínimo = round(sapply(numericas, min, na.rm = TRUE), 2),
  Máximo = round(sapply(numericas, max, na.rm = TRUE), 2),
  CV = round(sapply(numericas, function(x) (sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE)) * 100), 2)
)


tabela_resumida$Média <- format(tabela_resumida$Média, big.mark = ".", decimal.mark = ",")
tabela_resumida$Mediana <- format(tabela_resumida$Mediana, big.mark = ".", decimal.mark = ",")
tabela_resumida$Mínimo <- format(tabela_resumida$Mínimo, big.mark = ".", decimal.mark = ",")
tabela_resumida$Máximo <- format(tabela_resumida$Máximo, big.mark = ".", decimal.mark = ",")

print(tabela_resumida)
#criando tabela resumida

encontrar_municipio <- function(df, var, tipo = "max") {
  df_clean <- df[!is.na(df[[var]]), ]
  
  if (nrow(df_clean) == 0) {
    return(NA)
  }
  
  if (tipo == "max") {
    valor <- max(df_clean[[var]], na.rm = TRUE)
  } else {
    valor <- min(df_clean[[var]], na.rm = TRUE)
  }
  
  municipios <- df_clean$municipio[df_clean[[var]] == valor]
  
  return(municipios[1])
}

tabela_extremos <- data.frame(
     Variável = c(
           "Área Territorial (km²)",
           "População (Censo 2022)",
           "Densidade Demográfica (hab/km²)",
           "População Estimada (2025)",
           "Escolarização 6-14 anos (%)",
           "IDHM",
           "Mortalidade Infantil (óbitos/1000)",
           "Receitas Brutas (R$)",
           "Despesas Brutas (R$)",
           "PIB per Capita (R$)"
        ),
       
        `Mínimo` = c(
             min(dados$area_territorial, na.rm = TRUE),
             min(dados$populacao, na.rm = TRUE),
             min(dados$densidade_demografica, na.rm = TRUE),
             min(dados$populacao_estimada, na.rm = TRUE),
              min(dados$escolarizacao, na.rm = TRUE),
              min(dados$idhm, na.rm = TRUE),
              min(dados$mortalidade_infantil, na.rm = TRUE),
             min(dados$receitas_brutas, na.rm = TRUE),
             min(dados$despesas_brutas, na.rm = TRUE),
              min(dados$pib_per_capita, na.rm = TRUE)
         ),
     
       `Município (Mín)` = c(
             encontrar_municipio(dados, "area_territorial", "min"),
             encontrar_municipio(dados, "populacao", "min"),
             encontrar_municipio(dados, "densidade_demografica", "min"),
             encontrar_municipio(dados, "populacao_estimada", "min"),
             encontrar_municipio(dados, "escolarizacao", "min"),
            encontrar_municipio(dados, "idhm", "min"),
             encontrar_municipio(dados, "mortalidade_infantil", "min"),
             encontrar_municipio(dados, "receitas_brutas", "min"),
             encontrar_municipio(dados, "despesas_brutas", "min"),
              encontrar_municipio(dados, "pib_per_capita", "min")
          ),
      
        `Máximo` = c(
             max(dados$area_territorial, na.rm = TRUE),
               max(dados$populacao, na.rm = TRUE),
           max(dados$densidade_demografica, na.rm = TRUE),
             max(dados$populacao_estimada, na.rm = TRUE),
               max(dados$escolarizacao, na.rm = TRUE),
               max(dados$idhm, na.rm = TRUE),
               max(dados$mortalidade_infantil, na.rm = TRUE),
               max(dados$receitas_brutas, na.rm = TRUE),
               max(dados$despesas_brutas, na.rm = TRUE),
               max(dados$pib_per_capita, na.rm = TRUE)
           ),
      
         `Município (Máx)` = c(
               encontrar_municipio(dados, "area_territorial", "max"),
               encontrar_municipio(dados, "populacao", "max"),
               encontrar_municipio(dados, "densidade_demografica", "max"),
               encontrar_municipio(dados, "populacao_estimada", "max"),
               encontrar_municipio(dados, "escolarizacao", "max"),
               encontrar_municipio(dados, "idhm", "max"),
               encontrar_municipio(dados, "mortalidade_infantil", "max"),
               encontrar_municipio(dados, "receitas_brutas", "max"),
               encontrar_municipio(dados, "despesas_brutas", "max"),
               encontrar_municipio(dados, "pib_per_capita", "max")
           )
   )

   
   tabela_extremos$Mínimo <- format(round(tabela_extremos$Mínimo, 2), 
    big.mark = ".", decimal.mark = ",")
 tabela_extremos$Máximo <- format(round(tabela_extremos$Máximo, 2), 
     big.mark = ".", decimal.mark = ",")
 
   #exibir 
   print(tabela_extremos)
   
# --------------- geovanna linda --------------- <3

   
dadosnum <- names(dados)[sapply(dados, is.numeric)]
 numlg <- dados %>%
     select(all_of(dadosnum)) %>%
     pivot_longer(cols = everything(), names_to = "Variavel", values_to = "Valor")

#Gráficos de todas variáveis 
#HISTOGRAMA
  ggplot(numlg, aes(x = Valor)) +
    geom_histogram(fill = cores[1], color = "black", bins = 30, na.rm = TRUE) +
    facet_wrap(~Variavel, scales = "free") +
    labs( title = "Histogramas das Variáveis", 
       x = "Valores", 
       y = "Frequência") + tema_trabalho
  
#BOXPLOT
  ggplot(numlg, aes(y = Valor)) +
    geom_boxplot(fill = cores[4], color = "black", na.rm = TRUE) +
    facet_wrap(~Variavel, scales = "free") +
    labs(title = "Boxplots das Variáveis", 
      y = "Valores",
      x = "") +
    tema_trabalho +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank())
#10 municipios  
#FILTRAR
  maiores_areas <- dados %>%
    arrange(desc(area_territorial)) %>% 
    slice_head(n = 10)                 
  
#GRÁFICO PIB PER CAPITA
  ggplot(maiores_areas, aes(x = reorder(municipio, -pib_per_capita), y = pib_per_capita)) +
    geom_col(fill = cores[2], color = "black", alpha = 0.8) +
    scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
    labs( title = "PIB per Capita dos 10 Maiores Municípios em Área de MG",
      x = "Município",
      y = "PIB per Capita (R$)") + tema_trabalho +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
  
##graficos isolados (boxplot)
#ÁREA TERRITORIAL
  ggplot(dados, aes(y = area_territorial)) +
    geom_boxplot(fill = cores[4], color = "black", na.rm = TRUE) +
    scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
    labs(title = "Boxplot área Territorial (km²)", y = "Área Territorial", x = "") +
    tema_trabalho + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  
#POPULAÇÃO
  ggplot(dados, aes(y = populacao)) +
    geom_boxplot(fill = cores[4], color = "black", na.rm = TRUE) +
    scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
    labs(title = "População", y = "População (pessoas)", x = "") +
    tema_trabalho + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  
#DENSIDADE DEMOGRÁFICA
  ggplot(dados, aes(y = densidade_demografica)) +
    geom_boxplot(fill = cores[4], color = "black", na.rm = TRUE) +
    scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
    labs(title = "Boxplot Densidade Demográfica (hab/km²)", y = "Densidade Demográfica", x = "") +
    tema_trabalho + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  
#POPULAÇÃO ESTIMADA
  ggplot(dados, aes(y = populacao_estimada)) +
    geom_boxplot(fill = cores[4], color = "black", na.rm = TRUE) +
    scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
    labs(title = "Boxplot População estimada", y = "População Estimada (Pessoas)", x = "") +
    tema_trabalho + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  
#ESCOLARIZAÇÃO
  ggplot(dados, aes(y = escolarizacao)) +
    geom_boxplot(fill = cores[4], color = "black", na.rm = TRUE) +
    scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
    labs(title = "boxplot Taxa de Escolarização (6 a 14 anos)", y = "Escolarização (%)", x = "") +
    tema_trabalho + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  
#IDHM
  ggplot(dados, aes(y = idhm)) +
    geom_boxplot(fill = cores[4], color = "black", na.rm = TRUE) +
    scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
    labs(title = "Boxplot IDHM", y = "Índice de Desenvolvimento Humano Municipal", x = "") +
    tema_trabalho + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  
#MORTALIDADE INFANTIL
  ggplot(dados, aes(y = mortalidade_infantil)) +
    geom_boxplot(fill = cores[4], color = "black", na.rm = TRUE) +
    scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
    labs(title = "Boxplot mortalidade infantil", y = "Óbitos por mil nascidos vivos", x = "") +
    tema_trabalho + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  
#RECEITAS BRUTAS
  ggplot(dados, aes(y = receitas_brutas)) +
    geom_boxplot(fill = cores[4], color = "black", na.rm = TRUE) +
    scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
    labs(title = "Boxplo Receitas Brutas", y = "Receitas (R$)", x = "") +
    tema_trabalho + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  
#DESPESAS BRUTAS
  ggplot(dados, aes(y = despesas_brutas)) +
    geom_boxplot(fill = cores[4], color = "black", na.rm = TRUE) +
    scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
    labs(title = "Boxplot Despesas Brutas", y = "Despesas (R$)", x = "") +
    tema_trabalho + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  
#PIB PER CAPITA
  ggplot(dados, aes(y = pib_per_capita)) +
    geom_boxplot(fill = cores[4], color = "black", na.rm = TRUE) +
    scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
    labs(title = "Boxplot PIB per Capita", y = "PIB per Capita (R$)", x = "") +
    tema_trabalho + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  
##graficos isolados (boxplot)
#ÁREA TERRITORIAL
  ggplot(dados, aes(x = area_territorial)) +
    geom_histogram(fill = cores[1], color = "black", bins = 30, na.rm = TRUE) +
    scale_x_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
    labs(title = "Histograma Área Territorial (km²)", x = "Área Territorial", y = "Frequência") +
    tema_trabalho
  
#POPULAÇÃO
  ggplot(dados, aes(x = populacao)) +
    geom_histogram(fill = cores[1], color = "black", bins = 30, na.rm = TRUE) +
    scale_x_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
    labs(title = "Histograma População", x = "População (Pessoas)", y = "Frequência") +
    tema_trabalho
  
#DENSIDADE DEMOGRÁFICA
  ggplot(dados, aes(x = densidade_demografica)) +
    geom_histogram(fill = cores[1], color = "black", bins = 30, na.rm = TRUE) +
    scale_x_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
    labs(title = "Histograma Densidade Demográfica (hab/km²)", x = "Densidade Demográfica", y = "Frequência") +
    tema_trabalho
  
#POPULAÇAO ESTIMADA
  ggplot(dados, aes(x = populacao_estimada)) +
    geom_histogram(fill = cores[1], color = "black", bins = 30, na.rm = TRUE) +
    scale_x_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
    labs(title = "Histograma População Extimada", x = "População Estimada (Pessoas)", y = "Frequência") +
    tema_trabalho
  
#ESCOLARIZAÇÃO
  ggplot(dados, aes(x = escolarizacao)) +
    geom_histogram(fill = cores[1], color = "black", bins = 30, na.rm = TRUE) +
    scale_x_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
    labs(title = "Histograma Taxa de Escolarização (6 a 14 anos)", x = "Escolarização (%)", y = "Frequência") +
    tema_trabalho
  
#IDHM 
  ggplot(dados, aes(x = idhm)) +
    geom_histogram(fill = cores[1], color = "black", bins = 30, na.rm = TRUE) +
    scale_x_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
    labs(title = "Histograma IDHM", x = "Índice de Desenvolvimento Humano Municipal", y = "Frequência") +
    tema_trabalho
  
#MORTALIDADE INFANTIL
  ggplot(dados, aes(x = mortalidade_infantil)) +
    geom_histogram(fill = cores[1], color = "black", bins = 30, na.rm = TRUE) +
    scale_x_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
    labs(title = "Histograma Mortalidade Infantil", x = "Óbitos por mil nascidos vivos", y = "Frequência") +
    tema_trabalho
  
#RECEITA
  ggplot(dados, aes(x = receitas_brutas)) +
    geom_histogram(fill = cores[1], color = "black", bins = 30, na.rm = TRUE) +
    scale_x_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
    labs(title = "Histograma Receitas Brutas", x = "Receitas (R$)", y = "Frequência") +
    tema_trabalho
  
#DESPESAS BRUTAS
  ggplot(dados, aes(x = despesas_brutas)) +
    geom_histogram(fill = cores[1], color = "black", bins = 30, na.rm = TRUE) +
    scale_x_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
    labs(title = "Histograma Despesas Brutas ", x = "Despesas (R$)", y = "Frequência") +
    tema_trabalho
  
#PIB PR CAPITA 
  ggplot(dados, aes(x = pib_per_capita)) +
    geom_histogram(fill = cores[1], color = "black", bins = 30, na.rm = TRUE) +
    scale_x_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
    labs(title = "Histograma PIB per Capita", x = "PIB per Capita (R$)", y = "Frequência") +
    tema_trabalho



   
