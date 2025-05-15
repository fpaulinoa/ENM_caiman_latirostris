# Modelagem de nicho ecológico
# by Fernando Paulino e Carolina Loss 
# MAI - 2025

# Carrega os pacotes necessários
library(ENMTools)    # Ferramentas para modelagem de nicho ecológico
library(terra)       # Manipulação de dados espaciais raster e vetoriais
library(ecospat)     # Análises espaciais e de nicho
library(ade4)        # Análises multivariadas, incluindo PCA
library(factoextra)  # Visualização de resultados de análises multivariadas

# Lista com os nomes das populações a serem analisadas
populacoes <- c("flumi", "nocaa", "paran", "saof", "norma")

# Cria uma matriz vazia para armazenar os resultados de similaridade
n <- length(populacoes)  # número total de populações
resultados <- matrix(NA, nrow = n, ncol = n, 
                     dimnames = list(paste0("Caiman_", populacoes), 
                                     paste0("Caiman_", populacoes)))

# Loop para percorrer todas as combinações pares de populações
for (i in 1:(n-1)) {
  for (j in (i+1):n) {
    
    pop1 <- populacoes[i]  # população 1
    pop2 <- populacoes[j]  # população 2
    
    cat("\nProcessando:", pop1, "vs", pop2, "...\n")
    
    # Carrega os pontos de ocorrência (CSV) da população 1
    ocorrencias_pop1 <- read.csv(paste0("./output/predictors/predmod/predmodcrop/Caiman_", pop1, ".csv"), 
                                 sep = ";", header = TRUE)
    colnames(ocorrencias_pop1) <- c("x", "y")
    
    head(ocorrencias_pop1)  # mostra as primeiras linhas (opcional)
    
    # Carrega os pontos de ocorrência (CSV) da população 2
    ocorrencias_pop2 <- read.csv(paste0("./output/predictors/predmod/predmodcrop/Caiman_", pop2, ".csv"), 
                                 sep = ";", header = TRUE)
    colnames(ocorrencias_pop2) <- c("x", "y")
    
    # Carrega os rasters ambientais da população 1 (apenas algumas camadas selecionadas)
    pred_folder1 <- paste0("./output/predictors/predmod/predmodcrop/Caiman_", pop1, "/")
    env_pop1 <- rast(list.files(path = pred_folder1, pattern = '\\.asc$', full.names = TRUE))
    env_pop1 <- env_pop1[[c(1,5,6,7,8,14,15,16,17,18,21,22,24,25,26,27)]]
    
    # Carrega os rasters ambientais da população 2 (mesmas camadas)
    pred_folder <- paste0("./output/predictors/predmod/predmodcrop/Caiman_", pop2, "/")
    env_pop2 <- rast(list.files(path = pred_folder, pattern = '\\.asc$', full.names = TRUE))
    env_pop2 <- env_pop2[[c(1,5,6,7,8,14,15,16,17,18,21,22,24,25,26,27)]]
    
    # Converte pontos de ocorrência em objetos vetoriais espaciais com o mesmo CRS dos rasters
    pontos_pop1 <- vect(ocorrencias_pop1, geom = c("x", "y"), crs = crs(env_pop1))
    pontos_pop2 <- vect(ocorrencias_pop2, geom = c("x", "y"), crs = crs(env_pop2))
    
    # Padroniza os nomes das camadas ambientais para facilitar a comparação
    names(env_pop1) <- paste0("var", 1:nlyr(env_pop1))
    names(env_pop2) <- paste0("var", 1:nlyr(env_pop2))
    
    # Extrai os valores das variáveis ambientais para os pontos de ocorrência
    vals_sp1 <- as.data.frame(terra::extract(env_pop1, pontos_pop1, ID = FALSE))
    vals_sp2 <- as.data.frame(terra::extract(env_pop2, pontos_pop2, ID = FALSE))
    
    # Cria matriz de background combinando os valores ambientais das duas populações
    vals_bg1 <- as.data.frame(terra::values(env_pop1, na.rm = TRUE))
    vals_bg2 <- as.data.frame(terra::values(env_pop2, na.rm = TRUE))
    combined_env <- rbind(vals_bg1, vals_bg2)
    combined_env <- na.omit(combined_env)  # remove valores NA
    
    # Realiza uma Análise de Componentes Principais (PCA) no ambiente combinado
    pca.env <- dudi.pca(combined_env, scannf = FALSE, nf = 2)
    
    # Garante que as colunas de ocorrência estejam na mesma ordem das do ambiente combinado
    vals_sp1 <- vals_sp1[, colnames(combined_env)]
    vals_sp2 <- vals_sp2[, colnames(combined_env)]
    
    # Projeta os pontos de ocorrência no espaço PCA
    scores_sp1 <- suprow(pca.env, vals_sp1)$li
    scores_sp2 <- suprow(pca.env, vals_sp2)$li
    scores_bg <- pca.env$li  # scores do background
    
    # Cria grids climáticos para cada população no espaço PCA
    z1 <- ecospat.grid.clim.dyn(glob = scores_bg, glob1 = scores_sp1, 
                                sp = scores_sp1, R = 1000)
    z2 <- ecospat.grid.clim.dyn(glob = scores_bg, glob1 = scores_sp2, 
                                sp = scores_sp2, R = 1000)
    
    # Calcula o índice de sobreposição de nicho (D e I) entre as duas populações
    overlap <- ecospat.niche.overlap(z1, z2, cor = TRUE)
    pdf(paste0("./output/Results/mapa_sobrep_", pop1, "_vs_", pop2, ".pdf"), width = 10, height = 8) 
    # 1. Plotar sobreposição
    ecospat.plot.niche.dyn(
      z1, 
      z2, 
      quant = 0.95, 
      title = "",  # Remove o título padrão
      legend = FALSE  # remove a legenda automática
    )
    # Adiciona legenda indicando as cores das populações
    legend("bottomright", 
           legend = c(paste("População:", pop1), 
                      paste("População:", pop2), 
                      "Sobreposição"), 
           fill = c("green", "red", "purple"), 
           cex = 0.8, 
           inset = 0.08, 
           bg = "white", 
           box.lwd = 0.7,
           #xpd = TRUE
           )
    
    # Adiciona um título personalizado com margem ajustada
    mtext(
      paste("Sobreposição de Nicho", pop1, "vs", pop2), 
      side = 3,     # Topo do gráfico
      line = 2.3,   # Distância da borda (aumente para afastar mais)
      cex = 1.2,    # Tamanho do texto
      font = 2      # Negrito
    )
    dev.off()
    
    # 2. Testes de significância
    set.seed(123)  # Para reprodutibilidade
    eq_test <- ecospat.niche.equivalency.test(z1, z2, rep = 100)
    sim_test <- ecospat.niche.similarity.test(z1, z2, rep = 100)
    
    #Plotar resultados dos testes
    pdf(paste0("./output/Results/testesEQxSIM_", pop1, "_vs_", pop2, ".pdf"), width = 10, height = 5)
    par(mfrow = c(1, 2))
    ecospat.plot.overlap.test(eq_test, "D", paste("Equivalência:", pop1, "vs", pop2))
    ecospat.plot.overlap.test(sim_test, "D", paste("Similaridade:", pop1, "vs", pop2))
    dev.off()
    
    # Armazena os resultados na matriz: I no lado superior, D no lado inferior
    resultados[i, j] <- overlap$D
    resultados[j, i] <- overlap$I
    
    cat("Concluído:", pop1, "vs", pop2, 
        "- D =", round(overlap$D, 3), 
        "I =", round(overlap$I, 3), "\n")
  }
}

# Salva a matriz final de resultados em um arquivo CSV
write.csv(resultados, "./output/resultados_schoener.csv")
#print(resultados)  # imprime a matriz no console


