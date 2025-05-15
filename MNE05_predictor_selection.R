# Modelagem de nicho ecológico
# by Fernando Paulino e Carolina Loss 
# JAN - 2025

# Carrega pacotes necessários para análise
library(usdm)    # Para análise de multicolinearidade (VIF)
library(dismo)   # Para modelagem de distribuição de espécies
library(terra)   # Para manipulação de dados espaciais

# 1. CARREGAR DADOS DE OCORRÊNCIA
# Lê o arquivo CSV com os registros de ocorrência
pts <- read.csv("./data/occour/Caimans.csv")
# Padroniza nomes das espécies (substitui espaços por underscores)
pts$sp <- gsub(" ", "_", pts$sp)
# Obtém lista única de nomes de espécies
names <- unique(pts$sp)

# 2. DEFINIR ESPÉCIE PRINCIPAL PARA SELEÇÃO DE VARIÁVEIS
# Neste caso, usando Caiman_latirostris como referência
sp_principal <- "Caiman_latirostris"

# 3. PROCESSAMENTO DA ESPÉCIE PRINCIPAL (SELECÇÃO DE VARIÁVEIS)
# Define caminho dos preditores ambientais para a espécie principal
pred_folder <- paste0("./output/predictors/predmod/predmodcrop/", sp_principal, "/")
# Lista arquivos ASC (preditores)
pred_path <- list.files(path = pred_folder, pattern = '*.asc$')
# Cria caminhos completos
pred_files <- paste(pred_folder, pred_path, sep = "")

# Carrega os rasters de preditores
r_principal <- rast(pred_files)
# Seleciona um subconjunto específico de camadas (variáveis ambientais)
r_principal <- c(r_principal[[c(1, 5, 6, 7, 8, 14, 15, 16, 17, 18, 21, 22, 24, 25, 26, 27)]])

# Amostra pontos de fundo (background) para análise
set.seed(2023) # Garante reprodutibilidade
bg_principal <- spatSample(r_principal, 1000, "random", na.rm = TRUE, as.df = TRUE)

# ANÁLISE DE MULTICOLINEARIDADE (VIF)
# Realiza análise VIF com threshold de 10 (remove variáveis com VIF > 10)
v <- vifstep(bg_principal, th = 10)
# Obtém nomes das variáveis selecionadas
variaveis_selecionadas <- v@results$Variables

# SALVAR RESULTADOS DA ANÁLISE
# Cria diretório de saída se não existir
dir.create("./output/vif/", showWarnings = FALSE)
# Salva matriz de correlação
write.csv(v@corMatrix, paste0("./output/vif/vif_cormatrix_", sp_principal, ".csv"), fileEncoding = "UTF-8")
# Salva resultados do VIF
write.csv(v@results, paste0("./output/vif/vif_results_", sp_principal, ".csv"), fileEncoding = "UTF-8")

# 4. APLICAR AS MESMAS VARIÁVEIS SELECIONADAS PARA TODAS AS ESPÉCIES
for (i in 1:length(names)) {
  especie_atual <- names[i]
  
  # Carrega preditores para a espécie atual
  pred_folder <- paste0("./output/predictors/predmod/predmodcrop/", especie_atual, "/")
  pred_path <- list.files(path = pred_folder, pattern = '*.asc$')
  pred_files <- paste(pred_folder, pred_path, sep = "")
  
  r <- rast(pred_files)
  # Aplica mesma seleção inicial de camadas
  r <- c(r[[c(1, 5, 6, 7, 8, 14, 15, 16, 17, 18, 21, 22, 24, 25, 26, 27)]])
  
  # Seleciona apenas as variáveis que passaram no VIF da espécie principal
  p <- subset(r, variaveis_selecionadas)
  
  # PREPARA SAÍDA
  # Cria diretório específico para a espécie
  out.dir <- paste0("./output/vif/pred_vif/", especie_atual, "/")
  dir.create(out.dir, recursive = TRUE, showWarnings = FALSE)
  
  # Salva os rasters com as variáveis selecionadas
  writeRaster(p, paste0(out.dir, names(p), ".asc"))
}