library(dplyr)
library(tidyr)
library(ggplot2)
library(data.table)
library(scales)
library(readxl)

# Função para processar um conjunto de dados de um ano específico
df_por_estado <- function(df_mortalidade_geral) {
  resultado <- df_mortalidade_geral %>%
    filter(ACIDTRAB == 1) %>%
    group_by(NATURAL, ESC2010) %>%
    summarise(Qtd_Pessoas = n()) %>%
    pivot_wider(names_from = ESC2010, values_from = Qtd_Pessoas, values_fill = 0)
}

# Mapeamento de códigos para nomes dos estados
mapeamento_estados <- c("815" = "Pará", "821" = "Maranhão", "823" = "Ceará", "829" = "Bahia", "831" = "Minas Gerais", "835" = "São Paulo", "841" = "Paraná", "842" = "Santa Catarina", "843" = "Rio Grande do Sul", "852" = "Goiás")

df_por_estado_esc_name <- function(df_mortalidade_geral) {
  return <- df_mortalidade_geral %>%
    # Ordena as colunas da forma solicitada
    select("NATURAL", "0", "1", "2", "3", "4", "5", "9", "NA") %>%
    select(-"NA") %>%
    # Função serve para selecionar uma única coluna, nesse caso usamos para somar a quantidade de pessoas.
    rowwise() %>%
    mutate(Total_Pessoas = sum(c_across(where(is.numeric)))) %>%
    # Renomeia as colunas
    setNames(c("NATURAL", "Sem_escolaridade", "Fundamental_I", "Fundamental_II", "Médio", "Superior_incompleto", "Superior_completo", "Ignorado", "Total")) %>%
    mutate(
      NATURAL = mapeamento_estados[NATURAL]
    )
}

df_por_estado_2023 <- df_por_estado_esc_name(df_por_estado(DO23OPEN))
df_por_estado_2022 <- df_por_estado_esc_name(df_por_estado(DO22OPEN))
df_por_estado_2021 <- df_por_estado_esc_name(df_por_estado(Mortalidade_Geral_2021))
df_por_estado_2020 <- df_por_estado_esc_name(df_por_estado(Mortalidade_Geral_2020))
df_por_estado_2019 <- df_por_estado_esc_name(df_por_estado(Mortalidade_Geral_2019))
df_por_estado_2018 <- df_por_estado_esc_name(df_por_estado(Mortalidade_Geral_2018))
df_por_estado_2017 <- df_por_estado_esc_name(df_por_estado(Mortalidade_Geral_2017))
df_por_estado_2016 <- df_por_estado_esc_name(df_por_estado(Mortalidade_Geral_2016))
df_por_estado_2015 <- df_por_estado_esc_name(df_por_estado(Mortalidade_Geral_2015))

# Classificar o dataframe com base em 'Total' em ordem decrescente e filtrar os 10 primeiros.
df_por_estado_2023 <- head(df_por_estado_2023[order(df_por_estado_2023$Total, decreasing = TRUE), ], 10)
df_por_estado_2022 <- head(df_por_estado_2022[order(df_por_estado_2022$Total, decreasing = TRUE), ], 10)
df_por_estado_2021 <- head(df_por_estado_2021[order(df_por_estado_2021$Total, decreasing = TRUE), ], 10)
df_por_estado_2020 <- head(df_por_estado_2020[order(df_por_estado_2020$Total, decreasing = TRUE), ], 10)
df_por_estado_2019 <- head(df_por_estado_2019[order(df_por_estado_2019$Total, decreasing = TRUE), ], 10)
df_por_estado_2018 <- head(df_por_estado_2018[order(df_por_estado_2018$Total, decreasing = TRUE), ], 10)
df_por_estado_2017 <- head(df_por_estado_2017[order(df_por_estado_2017$Total, decreasing = TRUE), ], 10)
df_por_estado_2016 <- head(df_por_estado_2016[order(df_por_estado_2016$Total, decreasing = TRUE), ], 10)
df_por_estado_2015 <- head(df_por_estado_2015[order(df_por_estado_2015$Total, decreasing = TRUE), ], 10)

df_final <- bind_rows(
  df_por_estado_2023,
  df_por_estado_2022,
  df_por_estado_2021,
  df_por_estado_2020,
  df_por_estado_2019,
  df_por_estado_2018,
  df_por_estado_2017,
  df_por_estado_2016,
  df_por_estado_2015
)

df_acid_esc_por_estado <- df_final %>%
  group_by(NATURAL)%>%
  summarise_all(sum)

df_acid_esc_por_estado <- df_acid_esc_por_estado %>%
  arrange(desc(Total))

write.csv(df_acid_esc_por_estado, file = "df_acid_esc_por_estado.csv", row.names = FALSE)

#Cria paleta de cores
cores <- rainbow(length(unique(df_acid_esc_por_estado$NATURAL)))

ggplot(df_acid_esc_por_estado, aes(x = NATURAL, y = Total, fill = NATURAL)) +
  geom_bar(stat = "identity") +
  labs(title = "10 Maiores Óbitos por Estados",
       x = " ",
       y = "Total de Óbitos",
       fill = "Estados") +
  scale_fill_manual(values = cores) +
  theme(legend.position = "right", axis.text.x = element_blank())


