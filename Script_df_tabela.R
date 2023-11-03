soma <- sum(df_tabela_ensino$Total)

library(dplyr)

top_10_cargos <- df_tabela_ensino %>% 
  arrange(desc(Total)) %>% 
  select(OCUP, Total) %>%
  select("OCUP", "0", "1", "2", "3", "4", "5", "9", "NA") %>%
  head(10)

print(top_10_cargos)

