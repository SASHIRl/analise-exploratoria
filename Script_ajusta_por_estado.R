library(dplyr)
library(tidyr)
library(ggplot2)
library(data.table)
library(scales)
library(readxl)

tabela_por_estado <- Mortalidade_Geral_2021 %>%
  filter(ACIDTRAB == 1) %>%
  group_by(NATURAL, ESC2010) %>%
  summarise(Qtd_Pessoas = n()) %>%
  # Vira a tabela para agrupar por 'ESC2010'
  pivot_wider(names_from = ESC2010, values_from = Qtd_Pessoas, values_fill = 0)


tabela_por_estado <- tabela_por_estado %>%
  # Ordena as colunas da forma solicitada
  select("NATURAL", "0", "1", "2", "3", "4", "5", "9", "NA") %>%
  select(-"NA") %>%
  # Função serve para selecionar uma única coluna, nesse caso usamos para somar a quantidade de pessoas.
  rowwise() %>%
  mutate(Total_Pessoas = sum(c_across(where(is.numeric)))) %>%
  # Renomeia as colunas
  setNames(c("NATURAL", "Sem_escolaridade", "Fundamental_I", "Fundamental_II", "Médio", "Superior_incompleto", "Superior_completo", "Ignorado", "Total"))


# Classificar o dataframe com base em 'Total' em ordem decrescente
tabela_por_estado <- tabela_por_estado[order(tabela_por_estado$Total, decreasing = TRUE), ]

# Filtrar pelos 10 países com mais mortes
top_10_por_estado <- head(tabela_por_estado, 10)

# Criar o gráfico de barras
ggplot(top_10_por_estado, aes(x = NATURAL, y = Total)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Gráfico de mortes por UF",
       x = "Estados",
       y = "Mortes") +
  theme_minimal()


top_10_por_estado <- top_10_por_estado %>% 
  arrange(desc(Total)) %>% 
  select(NATURAL, Total) %>%
  pivot_wider(names_from = NATURAL, values_from = Total, values_fill = 0) %>%
  select(1:10) %>%
  select("815", "821", "823", "829", "831", "835", "841", "842", "843", "852") %>% 
  setNames(c("Pará", "Maranhão", "Ceará", "Bahia", "Minas Gerais", "São Paulo", "Paraná", "Santa Catarina", "Rio Grande do Sul", "Goiás"))

df_somas_estado <- data.frame(Coluna = names(colSums(top_10_por_estado)), Soma = colSums(top_10_por_estado))

#Cria paleta de cores
cores <- rainbow(length(unique(df_somas$Coluna)))

ggplot(df_somas_estado, aes(x = Coluna, y = Soma, fill = Coluna)) +
  geom_bar(stat = "identity") +
  labs(title = "10 Maiores Óbitos por Estados",
       x = " ",
       y = "Total de Óbitos",
       fill = "Estados") +
  scale_fill_manual(values = cores) +
  theme(legend.position = "right", axis.text.x = element_blank())

# Apenas para entendimento //
val_total_mortes <- rowSums(top_10_por_estado[1, 1:10], na.rm = TRUE)

media_geral_mortes_por_estado <- val_total_mortes / 10
print(media_geral_mortes_por_estado)

mean(val_total_mortes / (top_10_por_estado$`São Paulo`))
mean(val_total_mortes / (top_10_por_estado$`Minas Gerais`))

summary(top_10_por_estado)

mean_mg <- val_total_mortes / 360
mean_sp <- val_total_mortes / 408

print(mean_sp)
print(mean_mg)


