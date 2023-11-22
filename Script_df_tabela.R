library(dplyr)
library(tidyr)

top_10_cargos <- df_tabela_ensino %>% 
  arrange(desc(Total)) %>% 
  select(OCUP, Total) %>%
  pivot_wider(names_from = OCUP, values_from = Total, values_fill = 0) %>%
  select(1:10) %>%
  select("NA", "782510", "715210", "621005", "622020", "782305", "512105", "999993", "612005", "715615") %>% 
  setNames(c("Não informado", "MOTORISTA DE CAMINHAO", "PEDREIRO", "TRABALHADOR AGROPECUARIO", "TRABALHADOR VOLANTE DA AGRICULTURA", "MOTORISTA DE CARRO DE PASSEIO", "EMPREGADO DOMESTICO NOS SERVICOS GERAIS", "APOSENTADO/PENSIONISTA", "APOSENTADO", " PRODUTOR AGRICOLA POLIVALENTE", "ELETRICISTA DE INSTALACOES"))

print(top_10_cargos)