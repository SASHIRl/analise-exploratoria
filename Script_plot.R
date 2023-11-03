library(ggplot2)
library(tidyr)

# Classificar o dataframe com base em 'Sem_escolaridade' em ordem decrescente
df_tabela_ensino <- df_tabela_ensino[order(df_tabela_ensino$Sem_escolaridade, decreasing = TRUE), ]

# Escolher as 5 primeiras ocupações
top_5_sem_escolaridade <- head(df_tabela_ensino, 10)

# Criar o gráfico de barras
ggplot(top_5_sem_escolaridade, aes(x = OCUP, y = Sem_escolaridade)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Top 5 Ocupações com Mais Mortes (Sem Escolaridade)",
       x = "Ocupação",
       y = "Quantidade de Mortes") +
  theme_minimal()

# Gráfico de barras 10 maiores mortes por ocupação
top_10_cargos <- df_tabela_ensino %>% 
  arrange(desc(Total)) %>% 
  select(OCUP, Total) %>%
  pivot_wider(names_from = OCUP, values_from = Total, values_fill = 0) %>%
  select(1:10) %>%
  select("NA", "782510", "715210", "621005", "622020", "782305", "512105", "999993", "612005", "715615") %>% 
  setNames(c("Não informado", "MOTORISTA DE CAMINHAO", "PEDREIRO", "TRABALHADOR AGROPECUARIO", "TRABALHADOR VOLANTE DA AGRICULTURA", "MOTORISTA DE CARRO DE PASSEIO", "EMPREGADO DOMESTICO NOS SERVICOS GERAIS", "APOSENTADO", "PRODUTOR AGRICOLA POLIVALENTE", "ELETRICISTA DE INSTALACOES"))


df_somas <- data.frame(Coluna = names(colSums(top_10_cargos)), Soma = colSums(top_10_cargos))

#Cria paleta de cores
cores <- rainbow(length(unique(df_somas$Coluna)))


ggplot(df_somas, aes(x = Coluna, y = Soma, fill = Coluna)) +
  geom_bar(stat = "identity") +
  labs(title = "10 Maiores Óbitos por Ocupação",
       x = " ",
       y = "Total de Óbitos",
       fill = "Ocupações") +
  scale_fill_manual(values = cores) +
  theme(legend.position = "right", axis.text.x = element_blank())






