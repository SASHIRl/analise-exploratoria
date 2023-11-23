#LeveneTest
install.packages("car")
library(car)

#Média dos estados
rowMeans(df_acid_esc_por_estado[2:8])

media <- rowMeans(df_acid_esc_por_estado[2:8])
media_mg <- media[1] #3389
media_sp <- media[2] #3127

mg <- df_acid_esc_por_estado[1, 2:8]
mg <- unlist(mg)
mg <- as.numeric(mg)
print(mg)

sp <- df_acid_esc_por_estado[2, 2:8]
sp <- unlist(sp)
sp <- as.numeric(sp)
print(sp)

dados <- data.frame(
  Estado = rep(c("MG", "SP"), each = length(mg)),
  Valor = c(mg, sp)
)

ggplot(dados, aes(x = Estado, y = Valor, fill = Estado)) +
  geom_boxplot() +
  stat_summary(fun.y = mean, geom = "point", shape = 1, size = 3, color = "red", show.legend = FALSE) +  # Adiciona ponto para a média
  stat_summary(fun = median, geom = "point", shape = 2, size = 3, color = "blue", show.legend = FALSE) +  # Adiciona ponto para a mediana
  stat_summary(geom = "errorbar", fun.data = "mean_se", width = 0.2, color = "red", show.legend = FALSE) +  # Adiciona barras de erro para a média
  stat_summary(geom = "errorbar", fun.data = "median_hilow", width = 0.2, color = "blue", show.legend = FALSE) +  # Adiciona barras de erro para a mediana
  annotate("text", x = 1, y = mean(dados$Valor[dados$Estado == "MG"]), label = "Média MG", vjust = -0.5, hjust = 0.5, color = "red") +
  annotate("text", x = 2, y = mean(dados$Valor[dados$Estado == "SP"]), label = "Média SP", vjust = -0.5, hjust = 0.5, color = "red") +
  annotate("text", x = 1, y = median(dados$Valor[dados$Estado == "MG"]), label = "Mediana MG", vjust = 1, hjust = 0.5, color = "blue") +
  annotate("text", x = 2, y = median(dados$Valor[dados$Estado == "SP"]), label = "Mediana SP", vjust = 1, hjust = 0.5, color = "blue") +
  labs(title = "Boxplot dos Dados de MG e SP",
       x = "Estado",
       y = "Valor")

mean(sp)
mean(mg)

# Comparação por escolaridade
sp_sem_superior <- df_acid_esc_por_estado[2, 2:5]
sp_sem_superior <- unlist(sp_sem_superior)
sp_sem_superior <- as.numeric(sp_sem_superior)
print(sp_sem_superior)

sp_com_superior <- df_acid_esc_por_estado[2, 6:7]
sp_com_superior <- unlist(sp_com_superior)
sp_com_superior <- as.numeric(sp_com_superior)
print(sp_com_superior)

mg_sem_superior <- df_acid_esc_por_estado[1, 2:5]
mg_sem_superior <- unlist(mg_sem_superior)
mg_sem_superior <- as.numeric(mg_sem_superior)
print(mg_sem_superior)

mg_com_superior <- df_acid_esc_por_estado[1, 6:7]
mg_com_superior <- unlist(mg_com_superior)
mg_com_superior <- as.numeric(mg_com_superior)
print(mg_com_superior)



# Teste de hipótese / Mortes em acid de trab com/sem escolaridade?
wilcox.test(sp_sem_superior, mg_com_superior)

# Teste Shapiro para verificar se a distribuição é normal
shapiro.test(sp)
shapiro.test(mg)

# Histograma para análise visual


par(mfrow = c(1, 2))
hist(sp, main = "Histograma SP", xlab = "Valores", col = "lightblue", border = "black")
hist(mg, main = "Histograma MG", xlab = "Valores", col = "lightblue", border = "black")

#
qqnorm(sp)
qqnorm(mg)
qqline(sp)
qqline(mg)

# Seus dados
sp <- c(31, 466, 884, 998, 51, 158, 539)
mg <- c(80, 769, 792, 858, 58, 192, 640)

# Teste de Bartlett
bartlett.test(list(sp = sp, mg = mg))
