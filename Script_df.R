# Instalando bibliotecas
install.packages("tidyr")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("data.table")
install.packages("readxl")

# Carregando as bibliotecas
library(dplyr)
library(tidyr)
library(ggplot2)
library(data.table)
library(scales)
library(readxl)

#tally()
# Use a função group_by() para agrupar os valores com base na coluna de grupo
tabela_ensino_19 <- Mortalidade_Geral_2019 %>%
  filter(ACIDTRAB == 1) %>%
  group_by(OCUP, ESC2010) %>%
  summarise(Qtd_Pessoas = n()) %>%
  # Vira a tabela para agrupar por 'ESC2010'
  pivot_wider(names_from = ESC2010, values_from = Qtd_Pessoas, values_fill = 0)

tabela_ensino_19 <- tabela_ensino_19 %>%
  # Ordena as colunas da forma solicitada
  select("OCUP", "0", "1", "2", "3", "4", "5", "9", "NA") %>%
  select(-"NA") %>%
  # Função serve para selecionar uma única coluna, nesse caso usamos para somar a quantidade de pessoas.
  rowwise() %>%
  mutate(Total_Pessoas = sum(c_across(where(is.numeric)))) %>%
  # Renomeia as colunas
  setNames(c("OCUP", "Sem_escolaridade", "Fundamental_I", "Fundamental_II", "Médio", "Superior_incompleto", "Superior_completo", "Ignorado", "Total"))



tabela_ensino_20 <- Mortalidade_Geral_2020 %>%
  filter(ACIDTRAB == 1) %>%
  group_by(OCUP, ESC2010) %>%
  summarise(Qtd_Pessoas = n()) %>%
  # Vira a tabela para agrupar por 'ESC2010'
  pivot_wider(names_from = ESC2010, values_from = Qtd_Pessoas, values_fill = 0)

tabela_ensino_20 <- tabela_ensino_20 %>%
  # Ordena as colunas da forma solicitada
  select("OCUP", "0", "1", "2", "3", "4", "5", "9", "NA") %>%
  select(-"NA") %>%
  # Função serve para selecionar uma única coluna, nesse caso usamos para somar a quantidade de pessoas.
  rowwise() %>%
  mutate(Total_Pessoas = sum(c_across(where(is.numeric)))) %>%
  # Renomeia as colunas
  setNames(c("OCUP", "Sem_escolaridade", "Fundamental_I", "Fundamental_II", "Médio", "Superior_incompleto", "Superior_completo", "Ignorado", "Total"))


tabela_ensino_21 <- Mortalidade_Geral_2021 %>%
  filter(ACIDTRAB == 1) %>%
  group_by(OCUP, ESC2010) %>%
  summarise(Qtd_Pessoas = n()) %>%
  # Vira a tabela para agrupar por 'ESC2010'
  pivot_wider(names_from = ESC2010, values_from = Qtd_Pessoas, values_fill = 0)

tabela_ensino_21 <- tabela_ensino_21 %>%
  # Ordena as colunas da forma solicitada
  select("OCUP", "0", "1", "2", "3", "4", "5", "9", "NA") %>%
  select(-"NA") %>%
  # Função serve para selecionar uma única coluna, nesse caso usamos para somar a quantidade de pessoas.
  rowwise() %>%
  mutate(Total_Pessoas = sum(c_across(where(is.numeric)))) %>%
  # Renomeia as colunas
  setNames(c("OCUP", "Sem_escolaridade", "Fundamental_I", "Fundamental_II", "Médio", "Superior_incompleto", "Superior_completo", "Ignorado", "Total"))

tabela_ensino_combinada <- bind_rows(tabela_ensino_19, tabela_ensino_20, tabela_ensino_21)%>%
  group_by(OCUP)%>%
  summarise_all(sum)

## Código refatorado ##
#----------------------------------------------------------------------------------#-----------------------------------------------------------------##
##Contagem óbitos por ano##
mortalidade_geral <- data.frame(
  ano = c(2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023),
  quantidade = c(nrow(Mortalidade_Geral_2015), nrow(Mortalidade_Geral_2016), nrow(Mortalidade_Geral_2017), 
            nrow(Mortalidade_Geral_2018), nrow(Mortalidade_Geral_2019), nrow(Mortalidade_Geral_2020),
            nrow(Mortalidade_Geral_2021), nrow(DO22OPEN), nrow(DO23OPEN))
)

# Grafico de barras do total de óbitos
ggplot(mortalidade_geral, aes(x = as.factor(ano), y = quantidade)) +
  geom_bar(stat = "identity", fill = "#B22222") +
  geom_text(aes(label = comma(quantidade)), vjust = -0.5, size = 4, fontface="italic") +
  labs(x = "Ano", y = "Óbitos", title = "Registro de óbitos por ano")+
  theme(axis.title.x = element_text(face = "italic", family = "Arial", size = 14, color = "black"),
        axis.title.y = element_text(face = "italic", family = "Arial", size = 15, color = "black"))
  theme_minimal()
#----------------------------------------------------------------------------------#-----------------------------------------------------------------##
# Função para processar um conjunto de dados de um ano específico
df_mortalidade_ACIDTRAB <- function(df_mortalidade_geral) {
  resultado <- df_mortalidade_geral %>%
    filter(ACIDTRAB == 1)
}

# Filtra os dataframes de cada ano com o número de óbitos em acidente de trabalho
df_2015_acidtrab <- df_mortalidade_ACIDTRAB(Mortalidade_Geral_2015)
df_2016_acidtrab <- df_mortalidade_ACIDTRAB(Mortalidade_Geral_2016)
df_2017_acidtrab <- df_mortalidade_ACIDTRAB(Mortalidade_Geral_2017)
df_2018_acidtrab <- df_mortalidade_ACIDTRAB(Mortalidade_Geral_2018)
df_2019_acidtrab <- df_mortalidade_ACIDTRAB(Mortalidade_Geral_2019)
df_2020_acidtrab <- df_mortalidade_ACIDTRAB(Mortalidade_Geral_2020)
df_2021_acidtrab <- df_mortalidade_ACIDTRAB(Mortalidade_Geral_2021)
df_2022_acidtrab <- df_mortalidade_ACIDTRAB(DO22OPEN)
df_2023_acidtrab <- df_mortalidade_ACIDTRAB(DO23OPEN)

mortalidade_acidtrab <- data.frame(
  ano = c(2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023),
  quantidade = c(nrow(df_2015_acidtrab), nrow(df_2016_acidtrab), nrow(df_2017_acidtrab), 
                 nrow(df_2018_acidtrab), nrow(df_2019_acidtrab), nrow(df_2020_acidtrab),
                 nrow(df_2021_acidtrab), nrow(df_2022_acidtrab), nrow(df_2023_acidtrab))
)

ggplot(mortalidade_acidtrab, aes(x = as.factor(ano), y = quantidade)) +
  geom_bar(stat = "identity", fill = "#B22222") +
  geom_text(aes(label = comma(quantidade)), vjust = -0.5, size = 4, fontface="italic") +
  labs(x = "Ano", y = "Óbitos em acidentes de trabalho", title = "Registro de óbitos por ano")+
  theme(axis.title.x = element_text(face = "italic", family = "Arial", size = 14, color = "black"),
        axis.title.y = element_text(face = "italic", family = "Arial", size = 15, color = "black"))
theme_minimal()
#----------------------------------------------------------------------------------#-----------------------------------------------------------------##

library(dplyr)
library(tidyr)

# Função para processar um conjunto de dados de um ano específico
df_mortalidade <- function(df_mortalidade_geral) {
  resultado <- df_mortalidade_geral %>%
    filter(ACIDTRAB == 1) %>%
    group_by(OCUP, ESC2010) %>%
    summarise(Qtd_Pessoas = n()) %>%
    pivot_wider(names_from = ESC2010, values_from = Qtd_Pessoas, values_fill = 0) %>%
    select("OCUP", "0", "1", "2", "3", "4", "5", "9", "NA") %>%
    select(-"NA") %>%
    rowwise() %>%
    mutate(Total_Pessoas = sum(c_across(where(is.numeric)))) %>%
    setNames(c("OCUP", "Sem_escolaridade", "Fundamental_I", "Fundamental_II", "Médio", "Superior_incompleto", "Superior_completo", "Ignorado", "Total"))
  
  return(resultado)
}

# Combina os resultados
df_tabela_ensino_combinada <- bind_rows(df_mortalidade(Mortalidade_Geral_2015), df_mortalidade(Mortalidade_Geral_2016), 
                                     df_mortalidade(Mortalidade_Geral_2017), df_mortalidade(Mortalidade_Geral_2018),
                                     df_mortalidade(Mortalidade_Geral_2019), df_mortalidade(Mortalidade_Geral_2020),
                                     df_mortalidade(Mortalidade_Geral_2021), df_mortalidade(DO22OPEN), df_mortalidade(DO23OPEN)) %>%
  group_by(OCUP)%>%
  summarise_all(sum)
  #Remove as tabelas individuais

  write.csv(df_tabela_ensino_combinada, file = "df_tabela_ensino.csv", row.names = FALSE)

  rm(tabela_ensino_combinada)