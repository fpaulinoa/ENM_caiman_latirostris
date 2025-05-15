# Carregar pacotes necessários
library(rvest)      # Para ler e extrair informações de arquivos HTML
library(dplyr)      # Para manipulação de dados
library(tidyr)      # Para organização de dados (não utilizado diretamente, mas bom para complementar dplyr)
library(ggplot2)    # Para criação de gráficos
library(ggpubr)     # Para gráficos mais aprimorados (não usado explicitamente aqui, mas útil)
library(broom)      # Para transformar resultados estatísticos em data frames organizados

# Função para extrair a tabela de contribuição de variáveis a partir de um arquivo HTML
extrair_tabela_contribuicao <- function(caminho_arquivo) {
  html <- read_html(caminho_arquivo)       # Ler o conteúdo HTML
  tabela <- html %>%
    html_nodes("table") %>%                # Selecionar todas as tabelas
    html_table(fill = TRUE) %>%             # Converter a primeira tabela em data.frame
    .[[1]]
  colnames(tabela) <- c("Variavel", "Contribuicao_Percentual", "Importancia_Permutacao")  # Renomear colunas
  return(tabela)  # Retornar a tabela formatada
}

# Função para processar todos os arquivos de uma população
processar_populacao <- function(populacao, caminho_base) {
  arquivos <- paste0(caminho_base, "/", populacao, "/run_", 1:20, "/maxent.html")  # Criar vetor com caminhos para 20 arquivos HTML
  dados_populacao <- arquivos %>%
    lapply(extrair_tabela_contribuicao) %>% # Aplicar a função de extração em cada arquivo
    bind_rows(.id = "Repeticao")             # Unir todos os resultados em um único data frame, adicionando uma coluna 'Repeticao'
  dados_populacao <- dados_populacao %>%
    mutate(Populacao = populacao)            # Adicionar coluna indicando o nome da população
  return(dados_populacao)  # Retornar os dados
}

# Definir o caminho base onde estão os diretórios de cada população
caminho_base <- "/Users/nando/Documents/Mestrado/RFiles/output/models"

# Listar os nomes das populações (correspondentes aos diretórios)
populacoes <- c("Caimamn_gpi", "Caiman_gac")

# Processar todas as populações e consolidar todos os dados em um único data frame
dados_finais <- populacoes %>%
  lapply(function(pop) processar_populacao(pop, caminho_base)) %>%
  bind_rows()

# Criar resumo estatístico das contribuições: média e desvio padrão por variável e população
resumo_contribuicoes <- dados_finais %>%
  group_by(Populacao, Variavel) %>%
  summarise(
    Media_Contribuicao = mean(Contribuicao_Percentual, na.rm = TRUE),
    Desvio_Padrao = sd(Contribuicao_Percentual, na.rm = TRUE)
  ) %>%
  ungroup()

# Criar gráfico de barras com erro padrão (barra de erro = desvio padrão)
grafico <- ggplot(resumo_contribuicoes, aes(x = Variavel, y = Media_Contribuicao, fill = Populacao)) +
  geom_bar(stat = "identity", position = "dodge") +  # Barras agrupadas lado a lado
  geom_errorbar(aes(ymin = Media_Contribuicao - Desvio_Padrao, ymax = Media_Contribuicao + Desvio_Padrao),
                position = position_dodge(width = 0.9), width = 0.25) +  # Adicionar barras de erro
  labs(title = "Contribuição das Variáveis por População",
       x = "Variável",
       y = "Contribuição Média (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Inclinar os nomes das variáveis no eixo X

# Mostrar o gráfico
print(grafico)

# Realizar testes estatísticos
# 1. Teste ANOVA para comparar as contribuições entre populações (para cada variável)
resultados_anova <- dados_finais %>%
  group_by(Variavel) %>%
  do(tidy(aov(Contribuicao_Percentual ~ Populacao, data = .))) %>%  # Aplicar ANOVA para cada variável
  ungroup()

# 2. Teste de Kruskal-Wallis para cada variável (não assume normalidade dos dados)
resultados_kruskal <- dados_finais %>%
  group_by(Variavel) %>%
  do(tidy(kruskal.test(Contribuicao_Percentual ~ Populacao, data = .))) %>%  # Aplicar Kruskal-Wallis para cada variável
  ungroup()

# Exibir resultados da ANOVA
print("Resultados da ANOVA:")
print(resultados_anova)

# Exibir resultados do Kruskal-Wallis
print("Resultados do Teste de Kruskal-Wallis:")
print(resultados_kruskal)

# Visualizar distribuição de uma variável específica ("prox") com boxplots
variavel_exemplo <- "prox"  # Escolher uma variável específica para plotar
boxplot <- ggplot(dados_finais %>% filter(Variavel == variavel_exemplo), 
                  aes(x = Populacao, y = Contribuicao_Percentual, fill = Populacao)) +
  geom_boxplot() +
  labs(title = paste("Contribuição da Variável", variavel_exemplo, "por População"),
       x = "População",
       y = "Contribuição Percentual") +
  theme_minimal()

# Mostrar boxplot
print(boxplot)
