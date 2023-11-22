library(ggplot2)
library(dplyr)
library(tidyr)
library(reshape2)

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

mean(top_10_cargos$S)
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




# Carregar o pacote ggplot2
library(ggplot2)

# Somar os valores das colunas de escolaridade
soma_escolaridade <- df_tabela_ensino %>%
  summarise(Sem_escolaridade = sum(Sem_escolaridade),
            Fundamental_I = sum(Fundamental_I),
            Fundamental_II = sum(Fundamental_II),
            Médio = sum(Médio),
            Superior_incompleto = sum(Superior_incompleto),
            Superior_completo = sum(Superior_completo),
            Ignorado = sum(Ignorado))

# Transformar os dados em formato longo (tidy data)
df_melted <- melt(soma_escolaridade, variable.name = "Escolaridade", value.name = "Total_Mortes")

# Criar o gráfico de barras
ggplot(df_melted, aes(x = Escolaridade, y = Total_Mortes, fill = Escolaridade)) +
  geom_bar(stat = "identity") +
  labs(title = "Total de Mortes em Acidentes de Trabalho por Categoria de Escolaridade",
       x = "Escolaridade",
       y = "Total de Mortes") +
  theme_minimal()

# Comparando médias das categorias de ensino antes do ensino superior com Superior_incompleto e Superior_completo juntos
t.test(df_tabela_ensino$Sem_escolaridade + df_tabela_ensino$Fundamental_I + df_tabela_ensino$Fundamental_II + df_tabela_ensino$Médio, df_tabela_ensino$Superior_incompleto + df_tabela_ensino$Superior_completo)


