# ANÁLISE BIOLÓGICA - TELEÓSTEO
# Data: 28/04/2025

# Configuração inicial
setwd("scripts")

# Carregar pacotes
library(broom) 
library(colorspace)
library(dplyr)
library(lubridate)
library(janitor)
library(stringr)
library(tidyr)
library(purrr)
library(ggplot2)
library(ggpubr)
library(readr)
library(rstatix)
library(usethis)

#---- 1. ABRIR ARQUIVO ----
usethis::use_git()

# 1.1. Lista todos os arquivos CSV no diretório
arquivos_csv <- list.files(pattern = "\\.csv$")

# 1.2. Exibe os arquivos encontrados
print("Arquivos encontrados:")
print(arquivos_csv)

# 1.3. Lê o primeiro arquivo da lista (você pode mudar o índice ou nomear diretamente)
dados_organismo <- read.csv(arquivos_csv[1], sep = ";", header = TRUE)
dados_pesca <- read.csv(arquivos_csv[2], sep = ";", header = TRUE)

# 1.4. Fazer a junção utilizando a coluna comum
df_teleo <- Reduce(function(x, y) merge(x, y, by = "Código..AMOSTRAGEM.BIOLÓGICA...TELEÓSTEOS.", all = TRUE), 
                   list(dados_pesca, dados_organismo))

#---- 2. PADRONIZAÇÃO DOS DADOS ----
# 2.1. Padronize os nomes das colunas
df_teleo <- df_teleo %>% clean_names()

# 2.2. Remove o prefixo dos nomes das colunas de 31 a 49
names(df_teleo)[31:49] <- str_replace(
  names(df_teleo)[31:49],
  "^amostragem_biologica_organismo_a_organismo_",
  ""
)

# 2.3. Atribui o estágio de maturação "A - Imaturo" quando sexo == "Indeterminado (I)"
df_teleo <- df_teleo %>%
  mutate(
    estagio_de_maturacao_macho = if_else(
      sexo == "Indeterminado (I)",
      "A - Imaturo",
      estagio_de_maturacao_macho
    ),
    estagio_de_maturacao_femea = if_else(
      sexo == "Indeterminado (I)",
      "A - Imaturo",
      estagio_de_maturacao_femea
    )
  )

# 2.4. Atribui uma coluna "estagio_de_maturacao" única
df_teleo <- df_teleo %>%
  mutate(
    estagio_de_maturacao = case_when(
      !is.na(estagio_de_maturacao_macho) & estagio_de_maturacao_macho != "" ~ estagio_de_maturacao_macho,
      !is.na(estagio_de_maturacao_femea) & estagio_de_maturacao_femea != "" ~ estagio_de_maturacao_femea,
      TRUE ~ NA_character_
    )
  )

# 2.5. Atribuir o estágio de maturação "NIF" para todo ind. cujo o estágio não foi identificado
df_teleo <- df_teleo %>%
  mutate(
    estagio_de_maturacao = if_else(
      is.na(estagio_de_maturacao) | estagio_de_maturacao == "",
      "NIF - Não Identificado",
      estagio_de_maturacao
    )
  )

# 2.6. Atribuir o estágio de maturação "NIF" para todo ind. cujo o sexo não foi identificado
df_teleo <- df_teleo %>%
  mutate(
    sexo = if_else(
      is.na(sexo) | sexo == "",
      "NIF - Não Identificado",
      sexo
    )
  )

# 2.7. Padronizar os estágios de maturação
df_teleo <- df_teleo %>%
  mutate(
    estagio_de_maturacao = case_when(
      estagio_de_maturacao %in% c("C - Maturo", "C - Desovante") ~ "C - Maturo / Desovante",
      estagio_de_maturacao %in% c("D - Desovado", "D - Flácido") ~ "D - Desovado / Flácido",
      TRUE ~ estagio_de_maturacao
    )
  )

# 2.8. Substituir "NIF - Não Identificado" por "Indeterminado (I)" na coluna sexo
df_teleo <- df_teleo %>%
  mutate(sexo = ifelse(sexo == "NIF - Não Identificado", "Indeterminado (I)", sexo))

# 2.9. Filtrar os dados, excluindo a amostra indesejada
df_teleo <- df_teleo %>%
  filter(codigo_da_amostra != "1sb010824")


#---- 3. ÍNDICE K ----
# 3.1. Limpeza e conversão PT e CT
df_teleo <- df_teleo %>%
  mutate(
    peso_total_pt_g = as.numeric(str_replace(peso_total_pt_g, ",", ".")),
    comprimento_total_ct_cm = as.numeric(str_replace(comprimento_total_ct_cm, ",", "."))
  ) %>%
  filter(!is.na(peso_total_pt_g), !is.na(comprimento_total_ct_cm))

# 3.2. Calcular o índice de condição (K)
df_teleo <- df_teleo %>%
  mutate(
    k = 100 * peso_total_pt_g / (comprimento_total_ct_cm^3)
  )

# 3.3. Verificar resumo por espécie
resumo_k <- df_teleo %>%
  group_by(especie_amostrada_rotulo) %>%
  summarise(
    media_k = mean(k, na.rm = TRUE),
    sd_k = sd(k, na.rm = TRUE),
    n = n()
  )

print(resumo_k)

# 3.4. Gráfico boxplot do índice K por espécie
ggplot(df_teleo, aes(x = especie_amostrada_rotulo, y = k, fill = especie_amostrada_rotulo)) +
  geom_violin(trim = FALSE) + 
  geom_boxplot(width = 0.05, color = "black", fill = "black", outlier.shape = NA) + # Boxplot estreito preto
  stat_summary(fun = "mean", geom = "point", color = "white", size = 2) + # Ponto branco no centro
  geom_hline(yintercept = mean(df_teleo$k, na.rm = TRUE), linetype = "dashed") + # Linha tracejada
  labs(
    title = "Índice de Condição (K) por Espécie",
    x = "Espécie",
    y = "Índice de Condição (K)",
    fill = ""
  ) +
  theme_classic2() +
  theme(
    strip.text = element_text(face = "bold", size = 10),
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_fill_brewer(palette = "Dark2")

# 3.5. Teste ANOVA (índice K por espécie)
anova_result <- aov(k ~ especie_amostrada_rotulo, data = df_teleo)
summary(anova_result)

# 3.6. Teste de Tukey pós-hoc (comparações par-a-par)
TukeyHSD(anova_result)

# 3.7. Kruskal-Wallis (alternativa não paramétrica)
kruskal.test(k ~ especie_amostrada_rotulo, data = df_teleo)

# 3.8. Por sexo
resumo_sexo <- df_teleo %>%
  group_by(sexo) %>%
  summarise(
    media_k = mean(k, na.rm = TRUE),
    sd_k = sd(k, na.rm = TRUE),
    n = n()
  )

# 3.9. Por estádio de maturação
resumo_maturacao <- df_teleo %>%
  group_by(estagio_de_maturacao) %>%
  summarise(
    media_k = mean(k, na.rm = TRUE),
    sd_k = sd(k, na.rm = TRUE),
    n = n()
  )

print(resumo_sexo)
print(resumo_maturacao)

# 3.10. ANOVA por sexo
summary(aov(k ~ sexo, data = df_teleo))

# 3.11. ANOVA por estádio
summary(aov(k ~ estagio_de_maturacao, data = df_teleo))

# 3.12. Índice K por sexo
ggplot(df_teleo, aes(x = sexo, y = k, fill = sexo)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.05, color = "black", fill = "black", outlier.shape = NA) + # Boxplot estreito preto
  stat_summary(fun = "mean", geom = "point", color = "white", size = 2) + # Ponto branco no centro
  geom_hline(yintercept = mean(df_teleo$k, na.rm = TRUE), linetype = "dashed") + # Linha tracejada
  labs(
    title = "Índice K por Sexo", 
    x = "Sexo", 
    y = "Índice de Condição (K)",
    fill = "") +
  theme_classic2() +
  theme(
    strip.text = element_text(face = "bold", size = 10),
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) 

# 3.13. Índice K por estágio de maturação
ggplot(df_teleo, aes(x = estagio_de_maturacao, y = k, fill = estagio_de_maturacao)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.05, color = "black", fill = "black", outlier.shape = NA) + # Boxplot estreito preto
  stat_summary(fun = "mean", geom = "point", color = "white", size = 2) + # Ponto branco no centro
  geom_hline(yintercept = mean(df_teleo$k, na.rm = TRUE), linetype = "dashed") + # Linha tracejada
  labs(
    title = "Índice K por Estágio de Maturação", 
    x = "Estágio de Maturação", 
    y = "Índice de Condição (K)",
    fill = "") +
  theme_classic2() +
  theme(
    strip.text = element_text(face = "bold", size = 10),
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_fill_brewer(palette = "Set1") +
  scale_color_brewer(palette = "Set1")


# 3.14. Filtrar dados válidos
df_plot <- df_teleo %>%
  filter(
    !is.na(k),
    !is.na(comprimento_total_ct_cm),
    !is.na(sexo),
    !is.na(especie_amostrada_rotulo)
  )

# 3.15. Índice K vs CT por espécie
ggplot(df_plot, aes(x = comprimento_total_ct_cm, y = k, color = sexo)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "black", linewidth = 0.5) +
  facet_wrap(~ especie_amostrada_rotulo, scales = "free") +
  labs(
    title = "Índice K vs Comprimento Total por Espécie",
    subtitle = "Cores representam o sexo",
    x = "Comprimento Total (cm)",
    y = "Índice de Condição (K)",
    fill = "",
    color = ""
  ) +
  theme_classic2(base_size = 11) +
  theme(
    strip.text = element_text(face = "bold", size = 10),
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "bottom"
  )

#---- 4. RELAÇÕES MORFOMÉTRICAS ----
# 4.1. Filtrar dados válidos
df_hist <- df_teleo %>%
  filter(!is.na(comprimento_total_ct_cm), !is.na(especie_amostrada_rotulo), !is.na(sexo))

# 4.2. Distribuição do CT vs Espécie e Sexo (AQUI!!!!)
ggplot(df_hist, aes(x = comprimento_total_ct_cm, fill = sexo, color = sexo)) +
  geom_histogram(aes(y = after_stat(density)), linewidth = 0.25, position = "stack", bins = 30, color = "black", alpha = 0.5) +
  geom_density(linewidth = 0.5, adjust = 1.2, alpha = 0.25) +
  facet_wrap(~ especie_amostrada_rotulo, scales = "free") +
  labs(
    title = "Distribuição do Comprimento Total por Espécie e Sexo",
    subtitle = "Cores representam o sexo",
    x = "Comprimento Total (cm)",
    y = "Densidade",
    fill = "",
    color = ""
  ) +
  theme_classic2(base_size = 11) +
  theme(
    strip.text = element_text(face = "bold", size = 10),
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "bottom"
  )

# 4.3. Distribuição do CT vs Espécie e Estágio de Maturação
ggplot(df_hist, aes(x = comprimento_total_ct_cm, fill = estagio_de_maturacao, color = estagio_de_maturacao)) +
  geom_histogram(aes(y = after_stat(density)), linewidth = 0.25, position = "stack", bins = 30, color = "black", alpha = 0.5) +
  geom_density(linewidth = 0.5, adjust = 1.2, alpha = 0.25) +
  facet_wrap(~ especie_amostrada_rotulo, scales = "free") +
  labs(
    title = "Distribuição do Comprimento Total por Espécie e Estágio de Maturação",
    subtitle = "Cores representam o estágio de maturação",
    x = "Comprimento Total (cm)",
    y = "Densidade",
    fill = "",
    color = ""
  ) +
  theme_classic2(base_size = 11) +
  theme(
    strip.text = element_text(face = "bold", size = 10),
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "bottom"
  ) +
  scale_fill_brewer(palette = "Set1") +
  scale_color_brewer(palette = "Set1")


#---- 5. IGS & IHS ----
# 5.1. Converter colunas para numérico
df_teleo <- df_teleo %>%
  mutate(
    peso_da_gonoda_p_gon_g = as.numeric(gsub(",", ".", gsub("[^0-9,.]", "", peso_da_gonoda_p_gon_g))),
    peso_do_figado_p_fig_g = as.numeric(gsub(",", ".", gsub("[^0-9,.]", "", peso_do_figado_p_fig_g))),
    peso_eviscerado_p_evis_g = as.numeric(gsub(",", ".", gsub("[^0-9,.]", "", peso_eviscerado_p_evis_g)))
  )

# 5.2. Calcular os índices somáticos
df_teleo <- df_teleo %>%
  mutate(
    igs = ifelse(!is.na(peso_da_gonoda_p_gon_g) & !is.na(peso_eviscerado_p_evis_g) & peso_eviscerado_p_evis_g > 0,
                 (peso_da_gonoda_p_gon_g / peso_eviscerado_p_evis_g) * 100, NA),
    
    ihs = ifelse(!is.na(peso_do_figado_p_fig_g) & !is.na(peso_eviscerado_p_evis_g) & peso_eviscerado_p_evis_g > 0,
                 (peso_do_figado_p_fig_g / peso_eviscerado_p_evis_g) * 100, NA)
  )

# 5.3. Espécie - IGS
df_teleo %>%
  filter(!is.na(igs), igs < 50) %>%
  ggplot(aes(x = especie_amostrada_rotulo, y = igs, fill = especie_amostrada_rotulo)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.05, color = "black", fill = "black", outlier.shape = NA) + # Boxplot estreito preto
  stat_summary(fun = "mean", geom = "point", color = "white", size = 2) + # Ponto branco no centro
  geom_hline(yintercept = mean(df_teleo$k, na.rm = TRUE), linetype = "dashed") + # Linha tracejada
  labs(title = "IGS por Espécie", x = "Espécie", y = "IGS (%)", fill = "") +
  theme_classic2(base_size = 12) +
  theme(
    strip.text = element_text(face = "bold", size = 10),
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_fill_brewer(palette = "Dark2")

# 5.4. Sexo - IGS
df_teleo %>%
  filter(!is.na(igs), igs < 50) %>%
  ggplot(aes(x = sexo, y = igs, fill = sexo)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.05, color = "black", fill = "black", outlier.shape = NA) + # Boxplot estreito preto
  stat_summary(fun = "mean", geom = "point", color = "white", size = 2) + # Ponto branco no centro
  geom_hline(yintercept = mean(df_teleo$k, na.rm = TRUE), linetype = "dashed") + # Linha tracejada
  labs(title = "IGS por Sexo", x = "Sexo", y = "IGS (%)", fill = "") +
  theme_classic2(base_size = 12) +
  theme(
    strip.text = element_text(face = "bold", size = 10),
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# 5.5. Maturação - IGS
df_teleo %>%
  filter(!is.na(igs), igs < 50) %>%
  ggplot(aes(x = estagio_de_maturacao, y = igs, fill = estagio_de_maturacao)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.05, color = "black", fill = "black", outlier.shape = NA) + # Boxplot estreito preto
  stat_summary(fun = "mean", geom = "point", color = "white", size = 2) + # Ponto branco no centro
  geom_hline(yintercept = mean(df_teleo$k, na.rm = TRUE), linetype = "dashed") + # Linha tracejada
  labs(title = "IGS por Estágio de Maturação", x = "Estágio", y = "IGS (%)", fill = "") +
  theme_classic2(base_size = 12) +
  theme(
    strip.text = element_text(face = "bold", size = 10),
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_fill_brewer(palette = "Set1") +
  scale_color_brewer(palette = "Set1")

#-- Gráficos IHS --
# 5.6. Espécie - IHS
df_teleo %>%
  filter(!is.na(ihs), ihs < 50) %>%
  ggplot(aes(x = especie_amostrada_rotulo, y = ihs, fill = especie_amostrada_rotulo)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.05, color = "black", fill = "black", outlier.shape = NA) + # Boxplot estreito preto
  stat_summary(fun = "mean", geom = "point", color = "white", size = 2) + # Ponto branco no centro
  geom_hline(yintercept = mean(df_teleo$k, na.rm = TRUE), linetype = "dashed") + # Linha tracejada
  labs(title = "IHS por Espécie", x = "Espécie", y = "IHS (%)", fill = "") +
  theme_classic2(base_size = 12) +
  theme(
    strip.text = element_text(face = "bold", size = 10),
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_fill_brewer(palette = "Dark2") +
  scale_color_brewer(palette = "Dark2")

# 5.7. Sexo - IHS
df_teleo %>%
  filter(!is.na(ihs), ihs < 50) %>%
  ggplot(aes(x = sexo, y = ihs, fill = sexo)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.05, color = "black", fill = "black", outlier.shape = NA) + # Boxplot estreito preto
  stat_summary(fun = "mean", geom = "point", color = "white", size = 2) + # Ponto branco no centro
  geom_hline(yintercept = mean(df_teleo$k, na.rm = TRUE), linetype = "dashed") + # Linha tracejada
  labs(title = "IHS por Sexo", x = "Sexo", y = "IHS (%)", fill = "") +
  theme_classic2(base_size = 12) +
  theme(
    strip.text = element_text(face = "bold", size = 10),
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# 5.8. Maturação - IHS
df_teleo %>%
  filter(!is.na(ihs), ihs < 50) %>%
  ggplot(aes(x = estagio_de_maturacao, y = ihs, fill = estagio_de_maturacao)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.05, color = "black", fill = "black", outlier.shape = NA) + # Boxplot estreito preto
  stat_summary(fun = "mean", geom = "point", color = "white", size = 2) + # Ponto branco no centro
  geom_hline(yintercept = mean(df_teleo$k, na.rm = TRUE), linetype = "dashed") + # Linha tracejada
  labs(title = "IHS por Estágio de Maturação", x = "Estágio", y = "IHS (%)", fill = "") +
  theme_classic2(base_size = 12) +
  theme(
    strip.text = element_text(face = "bold", size = 10),
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_fill_brewer(palette = "Set1") +
  scale_color_brewer(palette = "Set1")

#-- Relação do IGS e IHS ao longo dos meses --
# 5.9 Atualizar a coluna de data e extrair mês
df_teleo <- df_teleo %>%
  mutate(
    data_coleta = dmy(data_da_amostragem),
    mes = month(data_coleta, label = TRUE, abbr = FALSE)
  )

# 5.10. Calcular limiares para remoção de outliers extremos
limite_igs <- quantile(df_teleo$igs, 0.99, na.rm = TRUE)
limite_ihs <- quantile(df_teleo$ihs, 0.99, na.rm = TRUE)

# 5.11. Filtrar valores válidos e remover outliers
df_igs_filtrado <- df_teleo %>%
  filter(!is.na(igs), igs <= limite_igs, !is.na(mes))
df_ihs_filtrado <- df_teleo %>%
  filter(!is.na(ihs), ihs <= limite_ihs, !is.na(mes))

#-- Gráfico IGS --
p1 <- ggplot(df_igs_filtrado, aes(x = mes, y = igs)) +
  geom_violin(trim = FALSE, fill = "#E69F00", color = "black") +
  labs(title = "IGS ao longo dos meses (sem outliers)", x = "Mês", y = "IGS") +
  theme_classic2() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

#-- Gráfico IHS --
p2 <- ggplot(df_ihs_filtrado, aes(x = mes, y = ihs)) +
  geom_violin(trim = FALSE, fill = "#56B4E9", color = "black") +
  labs(title = "IHS ao longo dos meses (sem outliers)", x = "Mês", y = "IHS") +
  theme_classic2() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

# 5.12. Combinar os gráficos
ggarrange(p1, p2, ncol = 2, nrow = 1)

#---- 6. ANÁLISES DE COMPARAÇÃO - WILCOXON E KRUSKAL-WALLIS ----
# 6.1. Função para testes entre sexo
teste_por_sexo <- function(var) {
  df <- df_teleo %>%
    filter(sexo %in% c("Fêmea (F)", "Macho (M)")) %>%
    select(valor = !!sym(var), sexo) %>%
    rename(grupo = sexo) %>%
    filter(!is.na(valor), !is.na(grupo))
  
  normalidade <- df %>%
    group_by(grupo) %>%
    summarise(p_valor = shapiro.test(valor)$p.value)
  
  if (all(normalidade$p_valor > 0.05)) {
    resultado <- t.test(valor ~ grupo, data = df)
    cat("\nTeste t para", var, "\n")
  } else {
    resultado <- wilcox.test(valor ~ grupo, data = df)
    cat("\nTeste de Wilcoxon para", var, "\n")
  }
  
  print(resultado)
}

# 6.2. Função para testes entre estágios de maturação
teste_por_maturacao <- function(var) {
  df <- df_teleo %>%
    filter(!is.na(estagio_de_maturacao)) %>%
    select(valor = !!sym(var), estagio_de_maturacao) %>%
    rename(grupo = estagio_de_maturacao) %>%
    filter(!is.na(valor), !is.na(grupo))
  
  normalidade <- df %>%
    group_by(grupo) %>%
    summarise(p_valor = shapiro.test(valor)$p.value)
  
  if (all(normalidade$p_valor > 0.05)) {
    resultado <- aov(valor ~ grupo, data = df)
    cat("\nANOVA para", var, "\n")
    print(summary(resultado))
  } else {
    resultado <- kruskal.test(valor ~ grupo, data = df)
    cat("\nKruskal-Wallis para", var, "\n")
    print(resultado)
  }
}

# 6.3. Lista de variáveis a testar
variaveis <- c("comprimento_total_ct_cm", "peso_total_pt_g", "k")

# 6.4. Executar testes por sexo
cat("=== COMPARAÇÕES ENTRE SEXOS ===\n")
lapply(variaveis, teste_por_sexo)

# 6.5. Executar testes por estágio de maturação
cat("\n=== COMPARAÇÕES ENTRE ESTÁGIOS DE MATURAÇÃO ===\n")
lapply(variaveis, teste_por_maturacao)


# 6.6. Comparação por sexo
for (var in variaveis) {
  df_sexo <- df_teleo %>%
    filter(sexo %in% c("Fêmea (F)", "Macho (M)")) %>%
    select(valor = !!sym(var), sexo) %>%
    filter(!is.na(valor), !is.na(sexo))
  
  print(
    ggplot(df_sexo, aes(x = sexo, y = valor, fill = sexo)) +
      geom_violin(trim = FALSE) +
      geom_boxplot(width = 0.05, color = "black", fill = "black", outlier.shape = NA) + # Boxplot estreito preto
      stat_summary(fun = "mean", geom = "point", color = "white", size = 2) + # Ponto branco no centro
      geom_hline(yintercept = mean(df_teleo$k, na.rm = TRUE), linetype = "dashed") + # Linha tracejada
      stat_compare_means(method = "wilcox.test", label.y = max(df_sexo$valor, na.rm = TRUE) * 1.05) +
      labs(
        title = paste("Comparação de", var, "por Sexo"),
        subtitle = "Método: Teste de Wilcoxon",
        x = "Sexo",
        y = var,
        fill = ""
      ) +
      theme_classic2() +
      theme(
        strip.text = element_text(face = "bold", size = 10),
        plot.title = element_text(face = "bold", size = 14),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
  )
}

# 6.7. Comparação por estágio de maturação
for (var in variaveis) {
  df_maturacao <- df_teleo %>%
    filter(!is.na(estagio_de_maturacao)) %>%
    select(valor = !!sym(var), estagio_de_maturacao) %>%
    filter(!is.na(valor), !is.na(estagio_de_maturacao))
  
  print(
    df_maturacao %>%
      ggplot(aes(x = estagio_de_maturacao, y = valor, fill = estagio_de_maturacao)) +
      geom_violin(trim = FALSE) +
      geom_boxplot(width = 0.05, color = "black", fill = "black", outlier.shape = NA) + # Boxplot estreito preto
      stat_summary(fun = "mean", geom = "point", color = "white", size = 2) + # Ponto branco no centro
      geom_hline(yintercept = mean(df_teleo$k, na.rm = TRUE), linetype = "dashed") + # Linha tracejada
      stat_compare_means(method = "kruskal.test", label.y = max(df_maturacao$valor, na.rm = TRUE) * 1.05) +
      labs(
        title = paste("Comparação de", var, "por Estágio de Maturação"),
        subtitle = "Método: Teste de Kuskal-Wallis",
        x = "Estágio de Maturação",
        y = var,
        fill = ""
      ) +
      theme_classic2() +
      theme(
        strip.text = element_text(face = "bold", size = 10),
        plot.title = element_text(face = "bold", size = 14),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1)
      ) +
      scale_fill_brewer(palette = "Set1") +
      scale_color_brewer(palette = "Set1")
  )
}


#---- 7. REGRESSÕES LINEARES ----
# 7.1. Conversão dos dados
df_modelo <- df_teleo %>%
  mutate(
    comprimento_total_ct_cm = as.numeric(comprimento_total_ct_cm),
    peso_total_pt_g = as.numeric(peso_total_pt_g),
    sexo = factor(sexo),
    estagio_de_maturacao = factor(estagio_de_maturacao)
  ) %>%
  filter(!is.na(comprimento_total_ct_cm), !is.na(peso_total_pt_g))

# 7.2. Geração do modelo linear
modelo_linear <- lm(peso_total_pt_g ~ comprimento_total_ct_cm + sexo + estagio_de_maturacao, data = df_modelo)

# 7.3. Resumo do modelo
summary(modelo_linear)

# 7.4. Plot dos residuais 
par(mfrow = c(2, 2))
plot(modelo_linear)

modelos_por_especie <- df_modelo %>%
  group_by(especie_amostrada_rotulo) %>%
  filter(n_distinct(sexo) > 1, n_distinct(estagio_de_maturacao) > 1) %>%  # garante variação
  nest() %>%
  mutate(
    modelo = map(data, ~ lm(peso_total_pt_g ~ comprimento_total_ct_cm + sexo + estagio_de_maturacao, data = .x)),
    resumo = map(modelo, summary)
  )

modelos_por_especie$resumo[[1]]

modelos_por_especie$especie_amostrada_rotulo

# 7.5. Gerar predições 
modelos_com_predicoes <- modelos_por_especie %>%
  mutate(predicoes = map2(modelo, data, ~ augment(.x, data = .y)))

# 7.6. Unir todas as predições em um DF 
df_predicoes <- modelos_com_predicoes %>%
  select(especie_amostrada_rotulo, predicoes) %>%
  unnest(predicoes)

# 7.7. Gráfico do modelo de predições 
ggplot(df_predicoes, aes(x = comprimento_total_ct_cm, y = peso_total_pt_g)) +
  geom_point(aes(color = sexo), alpha = 0.5) +
  #geom_line(aes(y = .fitted, color = sexo), linewidth = 0.8) +
  facet_wrap(~ especie_amostrada_rotulo, scales = "free") +
  labs(
    title = "Modelos Lineares por Espécie",
    x = "Comprimento Total (cm)",
    y = "Peso Total (g)",
    color = ""
  ) +
  theme_classic2(base_size = 11) +
  theme(
    strip.text = element_text(face = "bold", size = 10),
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "bottom"
  )


#---- 8. EXPORTAR OD DADOS ----
# 8.1. Exportar para CSV
write_csv(modelos, "relacao_morfometrica_especies.csv")

